{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The server a forge runs on.
module Usta.Types.Host
    ( Host
    )
where

import Data.Aeson (FromJSON)
import Data.Char (isAsciiLower, isDigit)
import Data.Kind (Type)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Text.Read (readMaybe)

import Data.Text qualified as Text

import Usta.Types.Parse (Parse (..), Parsed (..))

{- | A server's host name, lowercase, with a port when it has one:
@github.com@, @git.example.com:3000@. No scheme and no path.
-}
type Host :: Type
newtype Host = Host Text
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, PersistField, PersistFieldSql) via Parsed Host

instance Parse Host where
    type Raw Host = Text
    parse raw
        | "://" `Text.isInfixOf` raw = Left "a host name without the scheme, such as github.com"
        | validHost lowered = Right (Host lowered)
        | otherwise = Left "a host name such as github.com or git.example.com:3000"
        where
            lowered = Text.toLower raw
    unparse (Host raw) = raw

validHost :: Text -> Bool
validHost text = case Text.splitOn ":" text of
    [name] -> validName name
    [name, port] -> validName name && validPort port
    _ -> False
    where
        validName name = Text.length name <= 253 && all validLabel (Text.splitOn "." name)
        validLabel label =
            not (Text.null label)
                && Text.length label <= 63
                && Text.all (\c -> isAsciiLower c || isDigit c || c == '-') label
                && not ("-" `Text.isPrefixOf` label)
                && not ("-" `Text.isSuffixOf` label)
        validPort port =
            Text.all isDigit port && maybe False (\p -> p >= 1 && p <= (65535 :: Int)) (readMaybe (Text.unpack port))
