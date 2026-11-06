{-# LANGUAGE OverloadedStrings #-}

-- | Changes between two versions of a repository, as a unified diff gives
-- them: both @git diff@ and Forgejo's @.diff@ are read by 'parseDiff'.
module Usta.Types.Diff
  ( FileDiff (..)
  , Change (..)
  , Line (..)
  , parseDiff
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as B
import Data.Char (digitToInt, isDigit, isOctDigit)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Text.Read (readMaybe)

type FileDiff :: Type
data FileDiff = FileDiff
  { path :: Text
    -- ^ Where the file is after the change; where it was, if deleted.
  , change :: Change
  , binary :: Bool
    -- ^ Binary files have no lines.
  , lines :: [Line]
  }
  deriving stock (Eq, Show)

type Change :: Type
data Change
  = Added
  | Deleted
  | Modified
  | Renamed {from :: Text}
  deriving stock (Eq, Show)

-- | One line of a hunk, with its numbers in the old and new file.
type Line :: Type
data Line
  = Hunk {header :: Text}
    -- ^ The "@@ -12,6 +12,8 @@ …" line that starts a hunk.
  | Context {old :: Int, new :: Int, text :: Text}
  | Insertion {new :: Int, text :: Text}
  | Deletion {old :: Int, text :: Text}
  deriving stock (Eq, Show)

-- | Every file of a unified diff. Lines before the first file header are
-- ignored; a malformed hunk header is an error.
parseDiff :: Text -> Either Text [FileDiff]
parseDiff = traverse file . sections . T.lines
  where
    sections ls = case dropWhile (not . T.isPrefixOf "diff --git ") ls of
      [] -> []
      start : rest ->
        let (body, next) = break (T.isPrefixOf "diff --git ") rest
         in (start : body) : sections next

file :: [Text] -> Either Text FileDiff
file section = do
  let (header, hunks) = break (T.isPrefixOf "@@") section
      after prefix = listToMaybe (mapMaybe (T.stripPrefix prefix) header)
      oldPath = after "--- " >>= pathOf
      newPath = after "+++ " >>= pathOf
      fromGitLine = gitPaths (head' section)
      change
        | any (T.isPrefixOf "new file mode") header = Added
        | any (T.isPrefixOf "deleted file mode") header = Deleted
        | Just was <- after "rename from " = Renamed {from = unquote was}
        | otherwise = Modified
      path = fromMaybe "" $ case change of
        Deleted -> oldPath <|> fmap fst fromGitLine
        _ -> newPath <|> (unquote <$> after "rename to ") <|> fmap snd fromGitLine
  numbered <- hunkLines hunks
  pure FileDiff
    { path = path
    , change = change
    , binary = any (\l -> "Binary files " `T.isPrefixOf` l || l == "GIT binary patch") header
    , lines = numbered
    }
  where
    head' = fromMaybe "" . listToMaybe

-- | "a/src/Main.hs", or Nothing for "/dev/null".
pathOf :: Text -> Maybe Text
pathOf raw
  | name == "/dev/null" = Nothing
  | otherwise = Just (stripSide name)
  where
    name = unquote (T.takeWhile (/= '\t') raw)

-- | Both paths of "diff --git a/x b/y"; used only when the ---/+++ lines
-- are missing, as for binary files and pure renames.
gitPaths :: Text -> Maybe (Text, Text)
gitPaths line = do
  rest <- T.stripPrefix "diff --git " line
  case T.breakOn "\" \"b/" rest of
    -- Both quoted: "a/x y" "b/x y".
    (before, after) | not (T.null after) -> Just (stripSide (unquote (before <> "\"")), stripSide (unquote (T.drop 2 after)))
    _ -> do
      let (before, after) = T.breakOn " b/" rest
      if T.null after then Nothing else Just (stripSide before, T.drop 3 after)

stripSide :: Text -> Text
stripSide name = fromMaybe name (T.stripPrefix "a/" name <|> T.stripPrefix "b/" name)

-- | Git quotes a path with a quote, a backslash or a control character
-- in it, C-style: "say \"hi\".txt". Its bytes come back as written.
unquote :: Text -> Text
unquote t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"' = decodeUtf8Lenient (B.pack (go (T.unpack (T.init (T.tail t)))))
  | otherwise = t
  where
    go s = case s of
      [] -> []
      '\\' : a : b : c : rest | all isOctDigit [a, b, c] -> fromIntegral (digitToInt a * 64 + digitToInt b * 8 + digitToInt c) : go rest
      '\\' : c : rest -> fromIntegral (fromEnum (escaped c)) : go rest
      c : rest -> B.unpack (encodeUtf8 (T.singleton c)) <> go rest
    escaped c = case c of
      'n' -> '\n'
      't' -> '\t'
      'r' -> '\r'
      'a' -> '\a'
      'b' -> '\b'
      'f' -> '\f'
      'v' -> '\v'
      _ -> c

hunkLines :: [Text] -> Either Text [Line]
hunkLines = go 0 0
  where
    go _ _ [] = Right []
    go o n (l : rest)
      | "@@" `T.isPrefixOf` l = case hunkStart l of
          Just (o', n') -> (Hunk {header = l} :) <$> go o' n' rest
          Nothing -> Left "a hunk header such as @@ -1,3 +1,4 @@"
      | Just t <- T.stripPrefix "+" l = (Insertion {new = n, text = t} :) <$> go o (n + 1) rest
      | Just t <- T.stripPrefix "-" l = (Deletion {old = o, text = t} :) <$> go (o + 1) n rest
      | Just t <- T.stripPrefix " " l = (Context {old = o, new = n, text = t} :) <$> go (o + 1) (n + 1) rest
      | T.null l = (Context {old = o, new = n, text = ""} :) <$> go (o + 1) (n + 1) rest
      -- "\ No newline at end of file", and anything else between hunks.
      | otherwise = go o n rest

-- | The first old and new line numbers of "@@ -12,6 +14,8 @@".
hunkStart :: Text -> Maybe (Int, Int)
hunkStart l = case T.words l of
  _ : oldRange : newRange : _
    | "-" `T.isPrefixOf` oldRange, "+" `T.isPrefixOf` newRange ->
        (,) <$> startOf oldRange <*> startOf newRange
  _ -> Nothing
  where
    startOf range = readMaybe (takeWhile isDigit (drop 1 (T.unpack range)))
