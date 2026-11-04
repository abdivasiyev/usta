{-# LANGUAGE OverloadedStrings #-}

-- | A forge link, parsed into what it shows: a pull request, an issue, a
-- file or a repository. Forgejo's, GitHub's and GitLab's paths all count.
module Usta.Types.Link
  ( Target (..)
  , parseLink
  , describe
  ) where

import Data.Char (isDigit)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

type Target :: Type
data Target
  = PullAt {host :: Text, repo :: Text, number :: Int, files :: Bool}
    -- ^ @files@ for its Files tab.
  | IssueAt {host :: Text, repo :: Text, number :: Int}
  | FileAt {host :: Text, repo :: Text, branch :: Text, path :: Text, line :: Maybe Int}
  | RepositoryAt {host :: Text, repo :: Text}
  deriving stock (Eq, Show)

-- | "https://git.azizovich.uz/abdivasiyev/usta/pulls/41" and the like; a
-- missing scheme is fine, anything that is not a forge path is Nothing.
parseLink :: Text -> Maybe Target
parseLink raw = case T.splitOn "/" rest of
  host : owner : name : more | not (T.null host), not (T.null owner), not (T.null name) ->
    at host (owner <> "/" <> stripGit name) (filter (/= "-") more)
  _ -> Nothing
  where
    (beforeHash, hash) = T.breakOn "#" (T.strip raw)
    rest = T.dropWhileEnd (== '/') (dropScheme (fst (T.breakOn "?" beforeHash)))
    dropScheme t = maybe t id (T.stripPrefix "https://" t <|> T.stripPrefix "http://" t)
    stripGit n = maybe n id (T.stripSuffix ".git" n)
    lineOf = T.stripPrefix "#L" hash >>= readMaybe . T.unpack . T.takeWhile isDigit
    at host repo more = case more of
      [] -> Just (RepositoryAt host repo)
      kind : n : tab | Just number <- readMaybe (T.unpack n), kind `elem` ["pulls", "pull", "merge_requests"] ->
        Just (PullAt host repo number (take 1 tab == ["files"] || take 1 tab == ["diffs"]))
      kind : n : _ | Just number <- readMaybe (T.unpack n), kind == "issues" ->
        Just (IssueAt host repo number)
      -- Forgejo: src/branch/<branch>/<path>; GitHub and GitLab: blob/<branch>/<path>.
      "src" : "branch" : b : p | not (null p) -> Just (FileAt host repo b (T.intercalate "/" p) lineOf)
      "blob" : b : p | not (null p) -> Just (FileAt host repo b (T.intercalate "/" p) lineOf)
      _ -> Just (RepositoryAt host repo)
    a <|> b = maybe b Just a

-- | "pull request #41, Files tab", for the Settings' "Try a link".
describe :: Target -> Text
describe = \case
  PullAt {number, files} -> "pull request #" <> n number <> if files then ", Files tab" else ", Conversation tab"
  IssueAt {number} -> "issue #" <> n number
  FileAt {repo, path, line} -> path <> maybe "" ((" line " <>) . n) line <> " in " <> repo
  RepositoryAt {repo} -> "the repository " <> repo
  where
    n = T.pack . show
