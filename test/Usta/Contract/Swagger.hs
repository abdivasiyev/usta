{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Whether a request is one an API's swagger description takes: its path
-- and method are there, its query names its own parameters, and its body
-- has only the fields the API knows and every field it needs.
module Usta.Contract.Swagger
  ( conforms
  ) where

import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Usta.Contract.Replay (Written (..))

-- | 'Right' when the swagger description @spec@ takes the request.
conforms :: Value -> Written -> Either Text ()
conforms spec w = do
  let base = text (field "basePath" spec)
      wanted = fromMaybe w.path (T.stripPrefix base w.path)
      templates = [(t, v) | (k, v) <- objectPairs (field "paths" spec), let t = Key.toText k, fits (segments t) (segments wanted)]
  (template, item) <- case sortOn (Down . literals . fst) templates of
    best : _ -> Right best
    [] -> Left ("no such path: " <> wanted)
  op <- maybe (Left (w.method <> " is not taken at " <> template)) Right (KeyMap.lookup (Key.fromText (T.toLower w.method)) (object item))
  let params = mapMaybe (resolve spec) (list (field "parameters" item) <> list (field "parameters" op))
      named place = [text (field "name" p) | p <- params, text (field "in" p) == place]
  case [k | (k, _) <- w.query, k `notElem` named "query"] of
    [] -> Right ()
    unknown -> Left (template <> " takes no query " <> T.intercalate ", " unknown)
  if "multipart/form-data" `T.isPrefixOf` w.contentType
    then case [n | n <- named "formData", ("name=\"" <> n <> "\"") `T.isInfixOf` T.pack (map (toEnum . fromIntegral) (BL.unpack w.sent))] of
      [] -> Left (template <> " takes none of the form's fields")
      _ -> Right ()
    else case ([field "schema" p | p <- params, text (field "in" p) == "body"], decode w.sent) of
      ([], Nothing) -> Right ()
      ([], Just (Object o)) | KeyMap.null o -> Right ()
      ([], Just _) -> Left (template <> " takes no body")
      (schema : _, Just (Object o)) -> fieldsOf (fromMaybe schema (resolve spec schema)) o template
      (_ : _, Just _) -> Right ()
      (_ : _, Nothing) -> if BL.null w.sent then Right () else Left (template <> " got a body that is not JSON")
  where
    fieldsOf schema o template = do
      let known = map Key.toText (KeyMap.keys (object (field "properties" schema)))
          needed = map text (list (field "required" schema))
          sent' = map Key.toText (KeyMap.keys o)
      case [k | k <- sent', k `notElem` known] of
        [] -> Right ()
        unknown -> Left (template <> " does not know " <> T.intercalate ", " unknown <> "; it knows " <> T.intercalate ", " known)
      case [k | k <- needed, k `notElem` sent'] of
        [] -> Right ()
        missing -> Left (template <> " needs " <> T.intercalate ", " missing)

segments :: Text -> [Text]
segments = filter (not . T.null) . T.splitOn "/"

fits :: [Text] -> [Text] -> Bool
fits template actual = length template == length actual && and (zipWith (\t a -> isHole t || t == a) template actual)

isHole :: Text -> Bool
isHole t = "{" `T.isPrefixOf` t && "}" `T.isSuffixOf` t

literals :: Text -> Int
literals = length . filter (not . isHole) . segments

-- | A parameter or schema, its $ref followed.
resolve :: Value -> Value -> Maybe Value
resolve spec v = case field "$ref" v of
  String ref -> case T.splitOn "/" ref of
    ["#", section, name] -> KeyMap.lookup (Key.fromText name) (object (field section spec))
    _ -> Nothing
  _ -> Just v

field :: Text -> Value -> Value
field k v = fromMaybe Null (KeyMap.lookup (Key.fromText k) (object v))

object :: Value -> KeyMap.KeyMap Value
object = \case
  Object o -> o
  _ -> KeyMap.empty

objectPairs :: Value -> [(Key.Key, Value)]
objectPairs = KeyMap.toList . object

list :: Value -> [Value]
list = \case
  Array a -> toList a
  _ -> []

text :: Value -> Text
text = \case
  String t -> t
  _ -> ""
