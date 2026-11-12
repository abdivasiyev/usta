{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Whether a request is one an API's description takes: its path and
method are there, its query names its own parameters, and its body has
only the fields the API knows and every field it needs. Swagger 2
(Forgejo) and OpenAPI 3 (GitHub) descriptions both work.
-}
module Usta.Contract.Swagger
    ( conforms
    , conformsBody
    )
where

import Data.Aeson (Value (..), decode)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

import Usta.Contract.Replay (Written (..))

-- | 'Right' when the swagger description @spec@ takes the request.
conforms :: Value -> Written -> Either Text ()
conforms spec w = do
    let
        base = text (field "basePath" spec)
        wanted = fromMaybe w.path (T.stripPrefix base w.path)
        templates =
            [ (t, v)
            | (k, v) <- objectPairs (field "paths" spec)
            , let
                t = Key.toText k
            , fits (segments t) (segments wanted)
            ]
    (template, item) <- case sortOn (Down . literals . fst) templates of
        best : _ -> Right best
        [] -> Left ("no such path: " <> wanted)
    op <- maybe (Left (w.method <> " is not taken at " <> template)) Right (KeyMap.lookup (Key.fromText (T.toLower w.method)) (object item))
    let
        params = mapMaybe (resolve spec) (list (field "parameters" item) <> list (field "parameters" op))
        -- OpenAPI 3 keeps the body apart from the parameters.
        openBody = [field "schema" (field "application/json" (field "content" b)) | Just b <- [resolve spec (field "requestBody" op)], field "requestBody" op /= Null]
        named place = [text (field "name" p) | p <- params, text (field "in" p) == place]
    case [k | (k, _) <- w.query, k `notElem` named "query"] of
        [] -> Right ()
        unknown -> Left (template <> " takes no query " <> T.intercalate ", " unknown)
    if "multipart/form-data" `T.isPrefixOf` w.contentType
        then case [n | n <- named "formData", ("name=\"" <> n <> "\"") `T.isInfixOf` T.pack (map (toEnum . fromIntegral) (BL.unpack w.sent))] of
            [] -> Left (template <> " takes none of the form's fields")
            _ -> Right ()
        else case ([field "schema" p | p <- params, text (field "in" p) == "body"] <> openBody, decode w.sent) of
            ([], Nothing) -> Right ()
            ([], Just (Object o)) | KeyMap.null o -> Right ()
            ([], Just _) -> Left (template <> " takes no body")
            (schema : _, Just (Object o)) -> conformsBody spec schema o template
            (_ : _, Just _) -> Right ()
            (_ : _, Nothing) -> if BL.null w.sent then Right () else Left (template <> " got a body that is not JSON")

{- | Whether a JSON object fits @schema@: no field it does not know, and
every one it needs. One of several schemas (oneOf, anyOf) is enough.
-}
conformsBody :: Value -> Value -> KeyMap.KeyMap Value -> Text -> Either Text ()
conformsBody spec schema0 o template = case [s | s <- list (field "oneOf" schema) <> list (field "anyOf" schema)] of
    [] -> fieldsOf schema
    options -> case [() | option <- options, Right () <- [conformsBody spec (withBase option) o template]] of
        _ : _ -> Right ()
        [] -> Left (template <> " takes none of its body's forms")
    where
        schema = fromMaybe schema0 (resolve spec schema0)
        -- A form that only says what is required takes the fields around it.
        withBase option = case (fromMaybe option (resolve spec option), field "properties" schema) of
            (Object form, Object props) | not (KeyMap.member "properties" form) -> Object (KeyMap.insert "properties" (Object props) form)
            (form, _) -> form
        fieldsOf schema' = do
            let
                known = map Key.toText (KeyMap.keys (object (field "properties" schema')))
                needed = map text (list (field "required" schema'))
                sent' = map Key.toText (KeyMap.keys o)
            case [k | k <- sent', k `notElem` known] of
                [] -> Right ()
                unknown -> Left (template <> " does not know " <> T.intercalate ", " unknown <> "; it knows " <> T.intercalate ", " known)
            case [k | k <- needed, k `notElem` sent'] of
                [] -> Right ()
                missing -> Left (template <> " needs " <> T.intercalate ", " missing)

segments :: Text -> [Text]
segments = filter (not . T.null) . T.splitOn "/"

{- | Whether a path fits a template; its last parameter may hold slashes,
as a branch's name in git/refs/{ref} does.
-}
fits :: [Text] -> [Text] -> Bool
fits template actual = case (template, actual) of
    ([t], _ : _ : _) -> isHole t
    (t : ts, a : as) -> (isHole t || t == a) && fits ts as
    ([], []) -> True
    _ -> False

isHole :: Text -> Bool
isHole t = "{" `T.isPrefixOf` t && "}" `T.isSuffixOf` t

literals :: Text -> Int
literals = length . filter (not . isHole) . segments

-- | A parameter or schema, its $ref followed.
resolve :: Value -> Value -> Maybe Value
resolve spec v = case field "$ref" v of
    String ref -> case T.splitOn "/" ref of
        "#" : steps -> resolve spec (foldl (flip field) spec steps)
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
