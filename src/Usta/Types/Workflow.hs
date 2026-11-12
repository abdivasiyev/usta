{-# LANGUAGE OverloadedStrings #-}

{- | A workflow that can be started by hand (@on: workflow_dispatch@), with
the inputs it asks for, read from its file.
-}
module Usta.Types.Workflow
    ( Workflow (..)
    , Input (..)
    , Kind (..)
    , inputsOf
    )
where

import Control.Applicative ((<|>))
import Data.Aeson (Value (..), withObject, (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml

type Workflow :: Type
data Workflow = Workflow
    { name :: Text
    , file :: Text
    -- ^ Its path, e.g. ".github/workflows/deploy.yml".
    , inputs :: [Input]
    }
    deriving stock (Eq, Show)

type Kind :: Type
data Kind = TextInput | NumberInput | BooleanInput | ChoiceInput [Text] | EnvironmentInput
    deriving stock (Eq, Show)

type Input :: Type
data Input = Input
    { name :: Text
    , description :: Text
    , kind :: Kind
    , required :: Bool
    , preset :: Maybe Text
    -- ^ Its default, as the forge takes it: "true", "1.4.2" …
    }
    deriving stock (Eq, Show)

{- | The inputs of a workflow file that can be started by hand, in the
file's order; 'Nothing' when it cannot be.
-}
inputsOf :: ByteString -> Either Text (Maybe [Input])
inputsOf bytes = case Yaml.decodeEither' bytes of
    Left e -> Left (T.pack (Yaml.prettyPrintParseException e))
    Right v -> either (Left . T.pack) (Right . fmap inFileOrder) (parseEither workflow v)
    where
        -- The parsed map keeps no order: the file's comes from where each
        -- name first appears as a key.
        inFileOrder = sortOn (\i -> T.length (fst (T.breakOn ("\n" <> i.name <> ":") (spaced text))))
        text = decodeUtf8Lenient bytes
        spaced = T.unlines . map T.stripStart . T.lines
        workflow = withObject "workflow" $ \o -> do
            -- YAML 1.1 readers may take an unquoted "on" key for true.
            trigger <- (<|>) <$> o .:? "on" <*> o .:? "true"
            case fromMaybe Null trigger of
                String "workflow_dispatch" -> pure (Just [])
                Array events | String "workflow_dispatch" `elem` V.toList events -> pure (Just [])
                Object events -> case KeyMap.lookup "workflow_dispatch" events of
                    Nothing -> pure Nothing
                    Just Null -> pure (Just [])
                    Just (Object d) -> case KeyMap.lookup "inputs" d of
                        Just (Object is) -> Just <$> traverse input (KeyMap.toList is)
                        _ -> pure (Just [])
                    Just _ -> pure (Just [])
                _ -> pure Nothing
        input :: (Key.Key, Value) -> Parser Input
        input (k, v) = case v of
            Object o -> do
                description <- o .:? "description"
                required <- o .:? "required" >>= traverse truth
                kind <- o .:? "type"
                options <- o .:? "options"
                preset <- o .:? "default"
                pure
                    Input
                        { name = Key.toText k
                        , description = fromMaybe "" description
                        , kind = case (kind :: Maybe Text) of
                            Just "boolean" -> BooleanInput
                            Just "number" -> NumberInput
                            Just "choice" -> ChoiceInput (maybe [] (map scalar) options)
                            Just "environment" -> EnvironmentInput
                            _ -> TextInput
                        , required = fromMaybe False required
                        , preset = scalar <$> preset
                        }
            _ -> pure Input {name = Key.toText k, description = "", kind = TextInput, required = False, preset = Nothing}
        truth v = case v of
            Bool b -> pure b
            String s -> pure (T.toLower s == "true")
            _ -> pure False
        scalar v = case v of
            String s -> s
            Bool b -> if b then "true" else "false"
            Number n -> either (T.pack . show @Double) (T.pack . show @Integer) (floatingOrInteger n)
            _ -> ""
