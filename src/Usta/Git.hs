{-# LANGUAGE OverloadedStrings #-}

-- | Local mirrors: bare clones of a forge's repositories, which diffs,
-- commits and READMEs are read from. Every git command usta runs is here,
-- with its arguments; nothing else builds a git command line.
--
-- Git never asks on the terminal (@GIT_TERMINAL_PROMPT=0@, and ssh in
-- batch mode unless @GIT_SSH_COMMAND@ is set already) and speaks in the C
-- locale with paths unquoted, so its answers parse the same way everywhere.
-- A mirror's URL is https or ssh; over ssh the user's own keys sign in. Revisions and URLs come after @--end-of-options@ or @--@,
-- so a name that starts with "-" is never taken for an option.
module Usta.Git
  ( Kind (..)
  , GitError (..)
  , clone
  , fetch
  , resolve
  , diff
  , history
  , recent
  , readme
  ) where

import Control.Exception (Exception, throwIO, try)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process.Typed (proc, readProcess, setEnv)

import Usta.Types.Commit (Commit (..), Sha)
import Usta.Types.Diff (FileDiff, parseDiff)
import Usta.Types.Parse (parseM, unparse)

-- | How much of a repository a mirror keeps.
type Kind :: Type
data Kind
  = Full
    -- ^ Everything, fetched once: diffs work offline.
  | Partial
    -- ^ History only; file contents are fetched the first time a diff
    -- needs them.
  deriving stock (Eq, Show, Enum, Bounded)

-- | Git failed; the message is git's own, cut to its first lines.
type GitError :: Type
newtype GitError = GitError Text
  deriving stock (Show)

instance Exception GitError

-- | Runs git and returns what it printed.
git :: [String] -> IO Text
git args = do
  inherited <- getEnvironment
  let ssh = [("GIT_SSH_COMMAND", "ssh -o BatchMode=yes") | "GIT_SSH_COMMAND" `notElem` map fst inherited]
      env = ("GIT_TERMINAL_PROMPT", "0") : ("LC_ALL", "C") : ssh
        <> [kv | kv@(k, _) <- inherited, k `notElem` ["GIT_TERMINAL_PROMPT", "LC_ALL"]]
      common = ["-c", "core.quotePath=false", "-c", "color.ui=false"]
  (code, out, err) <- readProcess (setEnv env (proc "git" (common <> args)))
  case code of
    ExitSuccess -> pure (decode out)
    ExitFailure _ -> throwIO (GitError (T.intercalate "\n" (take 3 (T.lines (decode err)))))
  where
    decode = decodeUtf8Lenient . BL.toStrict

-- | Makes a bare mirror of @url@ at @path@. Every ref is kept, pull
-- requests' own (@refs/pull/…@) too, so their commits are local.
clone :: Kind -> Text -> FilePath -> IO ()
clone kind url path = do
  createDirectoryIfMissing True (takeDirectory path)
  _ <- git (["clone", "--mirror", "--quiet"] <> ["--filter=blob:none" | kind == Partial] <> ["--", T.unpack url, path])
  pure ()

-- | Brings a mirror up to date, dropping refs the forge no longer has.
fetch :: FilePath -> IO ()
fetch path = () <$ git ["-C", path, "fetch", "--prune", "--quiet", "origin"]

-- | The commit a ref names, such as "refs/pull/3/head" or "master";
-- 'Nothing' when the mirror has no such ref.
resolve :: FilePath -> Text -> IO (Maybe Sha)
resolve path ref = do
  found <- try (git ["-C", path, "rev-parse", "--verify", "--quiet", "--end-of-options", T.unpack ref <> "^{commit}"])
  case found of
    Left (GitError _) -> pure Nothing
    Right out -> Just <$> parseM out

-- | What @tip@ changes since it branched from @base@, as a pull request
-- shows it.
diff :: FilePath -> Sha -> Sha -> IO [FileDiff]
diff path base tip = do
  out <- git ["-C", path, "diff", "--no-ext-diff", "--find-renames", "--end-of-options", revs base <> "..." <> revs tip]
  either (throwIO . GitError) pure (parseDiff out)

-- | The commits @tip@ has that @base@ does not, oldest first.
history :: FilePath -> Sha -> Sha -> IO [Commit]
history path base tip =
  git ["-C", path, "log", "--reverse", format, "--end-of-options", revs base <> ".." <> revs tip] >>= commits

-- | The last @n@ commits on a ref, newest first.
recent :: FilePath -> Text -> Int -> IO [Commit]
recent path ref n =
  git ["-C", path, "log", "-n", show n, format, "--end-of-options", T.unpack ref] >>= commits

-- | The README at the top of a ref, with its file name; 'Nothing' when
-- there is none.
readme :: FilePath -> Text -> IO (Maybe (Text, Text))
readme path ref = do
  names <- T.lines <$> git ["-C", path, "ls-tree", "--name-only", "--end-of-options", T.unpack ref]
  case find isReadme names of
    Nothing -> pure Nothing
    Just name -> do
      contents <- git ["-C", path, "show", "--end-of-options", T.unpack ref <> ":" <> T.unpack name]
      pure (Just (name, contents))
  where
    isReadme name = map toLower (takeWhile (/= '.') (T.unpack name)) == "readme"

revs :: Sha -> String
revs = T.unpack . unparse

-- | Fields end in a unit separator and commits in a record separator,
-- which messages do not contain.
format :: String
format = "--format=%H%x1f%an%x1f%aI%x1f%s%x1f%b%x1e"

-- | Git writes "Z" for UTC and the author's offset, such as "+05:00",
-- otherwise; each parser takes only one of the two.
utcOf :: String -> IO UTCTime
utcOf date = case iso8601ParseM date of
  Just utc -> pure utc
  Nothing -> zonedTimeToUTC <$> iso8601ParseM date

commits :: Text -> IO [Commit]
commits out = traverse one (filter (not . T.null . T.strip) (T.splitOn "\x1e" out))
  where
    one record = case T.splitOn "\x1f" (T.dropWhile (== '\n') record) of
      [sha, author, date, subject, body] -> do
        hash <- parseM sha
        time <- utcOf (T.unpack date)
        pure Commit
          { sha = hash
          , author = author
          , authoredAt = time
          , subject = subject
          , body = T.strip body
          }
      _ -> throwIO (GitError "git printed a commit usta cannot read")
