{-# LANGUAGE OverloadedStrings #-}

-- | Whether a GraphQL mutation is one GitHub's schema takes: the mutation
-- is there, its input has only fields the schema's input type knows and
-- every one it needs, and a word given for an enum is one of its values.
-- usta's mutations are one call each, with their input written out:
-- @name(input: {field: $x, other: WORD})@.
module Usta.Contract.GraphQL
  ( mutationConforms
  ) where

import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | 'Right' when the schema (SDL) @schema@ takes the mutation in @query@.
mutationConforms :: Text -> Text -> Either Text ()
mutationConforms schema query = do
  (name, given) <- maybe (Left ("no mutation in " <> query)) Right (call query)
  inputType <- maybe (Left ("GitHub has no mutation " <> name)) Right (inputOf schema name)
  let fields = block schema ("input " <> inputType)
  case [k | (k, _) <- given, k `notElem` map fst fields] of
    [] -> Right ()
    unknown -> Left (inputType <> " does not know " <> T.intercalate ", " unknown)
  -- Needed: a type that ends in "!", with no default after it.
  case [k | (k, t) <- fields, "!" `T.isSuffixOf` t, k `notElem` map fst given] of
    [] -> Right ()
    missing -> Left (inputType <> " needs " <> T.intercalate ", " missing)
  case [ (k, v) | (k, v) <- given, isWord v, Just t <- [lookup k fields]
       , let values = map fst (block schema ("enum " <> T.filter isAlphaNum (T.takeWhile (/= ' ') t)))
       , v `notElem` values ] of
    [] -> Right ()
    wrong -> Left ("not one of the enum's values: " <> T.intercalate ", " [k <> ": " <> v | (k, v) <- wrong])
  where
    isWord v = not (T.null v) && T.all (\c -> isUpper c || c == '_') v

-- | The mutation called, and its input's fields with what each is given.
call :: Text -> Maybe (Text, [(Text, Text)])
call query = do
  let (before, after) = T.breakOn "(input: {" query
  name <- case T.words (T.map (\c -> if c == '{' then ' ' else c) before) of
    [] -> Nothing
    ws -> Just (last ws)
  if T.null after then Nothing else do
    let inside = T.takeWhile (/= '}') (T.drop (T.length "(input: {") after)
        pairs = [(T.strip k, T.strip (T.drop 1 v)) | part <- T.splitOn "," inside, let (k, v) = T.breakOn ":" part, not (T.null (T.strip k))]
    Just (name, pairs)

-- | The input type a mutation takes.
inputOf :: Text -> Text -> Maybe Text
inputOf schema name = do
  let mutations = drop 1 (dropWhile (not . opens "type Mutation") (T.lines schema))
      from = dropWhile (not . (("  " <> name <> "(") `T.isPrefixOf`)) mutations
  case from of
    [] -> Nothing
    _ : rest -> case mapMaybe (T.stripPrefix "input: " . T.strip) (takeWhile (not . ("  ): " `T.isPrefixOf`)) rest) of
      t : _ -> Just (T.filter isAlphaNum t)
      [] -> Nothing

-- | Whether a line opens the block @what@ ("input X", "type Mutation"),
-- whatever directives follow its name.
opens :: Text -> Text -> Bool
opens what l = take 2 (T.words l) == T.words what && "{" `T.isSuffixOf` T.stripEnd l

-- | The fields (or enum values) of a block of the schema, with their
-- types: the lines indented once, outside descriptions.
block :: Text -> Text -> [(Text, Text)]
block schema opening =
  [ (T.strip k, T.strip (T.drop 1 t))
  | l <- outsideDescriptions (takeWhile (/= "}") (drop 1 (dropWhile (not . opens opening) (T.lines schema))))
  , "  " `T.isPrefixOf` l, not ("   " `T.isPrefixOf` l)
  , let (k, t) = T.breakOn ":" (T.strip l)
  , not (T.null k), T.all (\c -> isAlphaNum c || c == '_') (T.strip k)
  ]
  where
    outsideDescriptions ls = go False ls
    go _ [] = []
    go inside (l : rest)
      | "\"\"\"" `T.isPrefixOf` T.strip l = if T.count "\"\"\"" l >= 2 then go inside rest else go (not inside) rest
      | inside = go inside rest
      | otherwise = l : go inside rest
