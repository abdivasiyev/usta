{-# LANGUAGE OverloadedStrings #-}

{- | Local mirrors: bare clones of a forge's repositories, which diffs,
commits and READMEs are read from. Every git command usta runs is here,
with its arguments; nothing else builds a git command line.

Git never asks on the terminal (@GIT_TERMINAL_PROMPT=0@, and ssh in
batch mode unless @GIT_SSH_COMMAND@ is set already) and speaks in the C
locale with paths unquoted, so its answers parse the same way everywhere.
A mirror's URL is https or ssh; over ssh the user's own keys sign in,
over https usta itself is git's only credential helper
("Usta.Git.Credential"), so the token is not kept by any other helper.
An https URL names the user (@https://login\@host/owner/repo.git@), which
tells the helper whose token to give. Revisions and URLs come after @--end-of-options@ or @--@,
so a name that starts with "-" is never taken for an option.
-}
module Usta.Git
    ( Kind (..)
    , GitError (..)
    , clone
    , fetch
    , worktree
    , cloneBranch
    , resolve
    , diff
    , history
    , commitDiff
    , recent
    , page
    , branches
    , tags
    , changes
    , blame
    , fileLog
    , graph
    , readme
    , tree
    , blob
    , noreplyLogin
    )
where

import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (forM, when)
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.Process.Typed (proc, readProcess, setEnv)
import Text.Read (readMaybe)

import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Usta.Types.Branch (Branch (..), Tag (..))
import Usta.Types.Commit (Commit (..), Sha)
import Usta.Types.Diff (FileDiff, parseDiff)
import Usta.Types.Insights (Blamed (..), Change (..), Node (..))
import Usta.Types.Parse (parseM, unparse)
import Usta.Types.Tree (Entry (..))

import Usta.Types.Tree qualified as Tree

-- | How much of a repository a mirror keeps.
type Kind :: Type
data Kind
    = -- | Everything, fetched once: diffs work offline.
      Full
    | {- | History only; file contents are fetched the first time a diff
      needs them.
      -}
      Partial
    deriving stock (Bounded, Enum, Eq, Show)

-- | Git failed; the message is git's own, cut to its first lines.
type GitError :: Type
newtype GitError = GitError Text
    deriving stock (Show)

instance Exception GitError

-- | Runs git and returns what it printed.
git :: [String] -> IO Text
git args = decodeUtf8Lenient . BL.toStrict <$> gitBytes args

-- | Runs git and returns what it printed, as bytes, for file contents.
gitBytes :: [String] -> IO BL.ByteString
gitBytes args = do
    inherited <- getEnvironment
    self <- getExecutablePath
    let
        ssh = [("GIT_SSH_COMMAND", "ssh -o BatchMode=yes") | "GIT_SSH_COMMAND" `notElem` map fst inherited]
        env =
            ("GIT_TERMINAL_PROMPT", "0")
                : ("LC_ALL", "C")
                : ssh
                    <> [kv | kv@(k, _) <- inherited, k `notElem` ["GIT_TERMINAL_PROMPT", "LC_ALL"]]
        -- The empty helper drops every helper configured before it.
        helpers = ["-c", "credential.helper=", "-c", "credential.helper=!" <> quoted self <> " git-credential"]
        common = ["-c", "core.quotePath=false", "-c", "color.ui=false"] <> helpers
    (code, out, err) <- readProcess (setEnv env (proc "git" (common <> args)))
    case code of
        ExitSuccess -> pure out
        ExitFailure _ -> throwIO (GitError (T.intercalate "\n" (take 3 (map T.strip (T.lines (decode err))))))
    where
        decode = decodeUtf8Lenient . BL.toStrict
        -- A helper starting with "!" is a shell command, so the path is quoted.
        quoted path = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) path <> "'"

{- | Makes a bare mirror of @url@ at @path@. Every ref is kept, pull
requests' own (@refs/pull/…@) too, so their commits are local.
-}
clone :: Kind -> Text -> FilePath -> IO ()
clone kind url path = do
    createDirectoryIfMissing True (takeDirectory path)
    _ <- git (["clone", "--mirror", "--quiet"] <> ["--filter=blob:none" | kind == Partial] <> ["--", T.unpack url, path])
    pure ()

-- | Brings a mirror up to date, dropping refs the forge no longer has.
fetch :: FilePath -> IO ()
fetch path = () <$ git ["-C", path, "fetch", "--prune", "--quiet", "origin"]

{- | A worktree of the mirror at @dir@ on a new branch @local@, made from
@ref@ (such as "refs/pull/3/head"), tracking @upstream@ on the forge,
so a push from it updates the pull request's branch.
-}
worktree :: FilePath -> FilePath -> Text -> Text -> Text -> IO ()
worktree mirror dir local ref upstream = do
    _ <- git ["-C", mirror, "worktree", "add", "-b", T.unpack local, "--", dir, T.unpack ref]
    _ <- git ["-C", dir, "config", "branch." <> T.unpack local <> ".remote", "origin"]
    () <$ git ["-C", dir, "config", "branch." <> T.unpack local <> ".merge", "refs/heads/" <> T.unpack upstream]

-- | A clone of @url@ at @dir@ on @branch@, named @local@ there.
cloneBranch :: Text -> FilePath -> Text -> Text -> IO ()
cloneBranch url dir branch local = do
    createDirectoryIfMissing True (takeDirectory dir)
    _ <- git ["clone", "--quiet", "--branch", T.unpack branch, "--", T.unpack url, dir]
    when (local /= branch) $ () <$ git ["-C", dir, "branch", "-m", T.unpack branch, T.unpack local]

{- | The commit a ref names, such as "refs/pull/3/head" or "master";
'Nothing' when the mirror has no such ref.
-}
resolve :: FilePath -> Text -> IO (Maybe Sha)
resolve path ref = do
    found <- try (git ["-C", path, "rev-parse", "--verify", "--quiet", "--end-of-options", T.unpack ref <> "^{commit}"])
    case found of
        Left (GitError _) -> pure Nothing
        Right out -> Just <$> parseM out

{- | What @tip@ changes since it branched from @base@, as a pull request
shows it.
-}
diff :: FilePath -> Sha -> Sha -> IO [FileDiff]
diff path base tip = do
    out <- git ["-C", path, "diff", "--no-ext-diff", "--find-renames", "--end-of-options", revs base <> "..." <> revs tip]
    either (throwIO . GitError) pure (parseDiff out)

{- | What one commit changes: against its first parent for a merge, and
everything for the first commit.
-}
commitDiff :: FilePath -> Sha -> IO [FileDiff]
commitDiff path sha = do
    out <- git ["-C", path, "diff-tree", "-p", "--no-commit-id", "--root", "-M", "-m", "--first-parent", "--end-of-options", revs sha]
    either (throwIO . GitError) pure (parseDiff out)

-- | The commits @tip@ has that @base@ does not, oldest first.
history :: FilePath -> Sha -> Sha -> IO [Commit]
history path base tip =
    git ["-C", path, "log", "--reverse", format, "--end-of-options", revs base <> ".." <> revs tip] >>= commits

-- | The last @n@ commits on a ref, newest first.
recent :: FilePath -> Text -> Int -> IO [Commit]
recent path ref n =
    git ["-C", path, "log", "-n", show n, format, "--end-of-options", T.unpack ref] >>= commits

{- | Up to @n@ commits on a ref after skipping @skip@, newest first, for
a history read a page at a time.
-}
page :: FilePath -> Text -> Int -> Int -> IO [Commit]
page path ref skip n =
    git ["-C", path, "log", "--skip=" <> show skip, "-n", show n, format, "--end-of-options", T.unpack ref] >>= commits

{- | Every branch, most recently committed first, with how far each is
ahead of and behind @base@.
-}
branches :: FilePath -> Text -> IO [Branch]
branches path base = do
    out <- git ["-C", path, "for-each-ref", "--sort=-committerdate", "--format=%(refname:short)%1f%(committerdate:iso-strict)", "refs/heads"]
    forM [l | l <- T.lines out, not (T.null l)] $ \line -> do
        let
            (name, rest) = T.breakOn "\x1f" line
        when' <- either (\(_ :: SomeException) -> Nothing) Just <$> try (utcOf (T.unpack (T.drop 1 rest)))
        counts <-
            if name == base
                then pure (Just 0, Just 0)
                else do
                    counted <- try (git ["-C", path, "rev-list", "--left-right", "--count", "--end-of-options", T.unpack base <> "..." <> T.unpack name])
                    pure $ case either (\(_ :: SomeException) -> []) T.words counted of
                        [behind, ahead] -> (readMaybe (T.unpack ahead), readMaybe (T.unpack behind))
                        _ -> (Nothing, Nothing)
        pure Branch {name = name, pushed = when', ahead = fst counts, behind = snd counts}

{- | Each commit's author, date and lines added and removed on a ref,
newest first, since a time when given.
-}
changes :: FilePath -> Text -> Maybe UTCTime -> IO [Change]
changes path ref since = do
    out <-
        git
            ( ["-C", path, "log", "--no-merges", "--numstat", "--format=%x1e%an%x1f%aI"]
                <> ["--since=" <> iso t | Just t <- [since]]
                <> ["--end-of-options", T.unpack ref]
            )
    fmap concat $ forM (filter (not . T.null . T.strip) (T.splitOn "\x1e" out)) $ \record ->
        case T.lines record of
            header : stats | (author, rest) <- T.breakOn "\x1f" header -> do
                when' <- try (utcOf (T.unpack (T.drop 1 rest)))
                pure $ case when' of
                    Left (_ :: SomeException) -> []
                    Right t ->
                        -- A binary file's counts are "-", and are left out.
                        let
                            counts = [(a, d) | l <- stats, x : y : _ <- [T.words l], Just a <- [readMaybe (T.unpack x)], Just d <- [readMaybe (T.unpack y) :: Maybe Int]]
                        in
                            [Change {author = author, at = t, added = sum (map fst counts), removed = sum (map snd counts)}]
            _ -> pure []
    where
        iso = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

{- | A page of the graph, newest first in topological order: every branch
and tag, or the given branches only, the first parents only if asked.
-}
graph :: FilePath -> [Text] -> Bool -> Int -> Int -> IO [Node]
graph path scope firstParent skip n = do
    out <-
        git
            ( ["-C", path, "log", "--topo-order", "--format=%H%x1f%P%x1f%an%x1f%aI%x1f%s%x1f%D%x1e", "--skip=" <> show skip, "-n", show n]
                <> ["--first-parent" | firstParent]
                <> (if null scope then ["--branches", "--tags"] else ["--end-of-options"] <> map T.unpack scope)
            )
    fmap concat $ forM (filter (not . T.null . T.strip) (T.splitOn "\x1e" out)) $ \record ->
        case T.splitOn "\x1f" (T.dropWhile (== '\n') record) of
            [sha, parents, author, date, subject, decorations] -> do
                when' <- try (utcOf (T.unpack date))
                pure $ case when' of
                    Left (_ :: SomeException) -> []
                    Right t ->
                        [ Node
                            { sha = sha
                            , parents = T.words parents
                            , author = author
                            , at = t
                            , subject = subject
                            , refs = [r | d <- map T.strip (T.splitOn "," decorations), let
                                                                                            r = maybe d id (T.stripPrefix "HEAD -> " d), not (T.null r), r /= "HEAD"]
                            }
                        ]
            _ -> pure []

-- | Each line of a file on a ref with the commit that last changed it.
blame :: FilePath -> Text -> Text -> IO [Blamed]
blame path ref file = do
    out <- git ["-C", path, "blame", "--porcelain", T.unpack ref, "--", T.unpack file]
    pure (go Map.empty (T.lines out))
    where
        -- Porcelain names a commit's author and summary only the first time.
        go _ [] = []
        go known (header : rest) = case T.words header of
            sha : _ : _
                | T.length sha == 40 ->
                    let
                        (fields, body) = break ("\t" `T.isPrefixOf`) rest
                        field k = lookup k [(T.takeWhile (/= ' ') f, T.drop 1 (T.dropWhile (/= ' ') f)) | f <- fields]
                        (author, at, summary) = case Map.lookup sha known of
                            Just seen -> seen
                            Nothing ->
                                ( maybe "" id (field "author")
                                , maybe epoch (posixSecondsToUTCTime . fromIntegral) (field "author-time" >>= readMaybe . T.unpack :: Maybe Int)
                                , maybe "" id (field "summary")
                                )
                        known' = Map.insert sha (author, at, summary) known
                    in
                        case body of
                            l : more -> Blamed {sha = sha, author = author, at = at, summary = summary, line = T.drop 1 l} : go known' more
                            [] -> []
            _ -> go known rest
        epoch = posixSecondsToUTCTime 0

-- | The commits on a ref that changed a file, newest first.
fileLog :: FilePath -> Text -> Text -> Int -> IO [Commit]
fileLog path ref file n =
    git ["-C", path, "log", "-n", show n, format, T.unpack ref, "--", T.unpack file] >>= commits

-- | Every tag, newest first.
tags :: FilePath -> IO [Tag]
tags path = do
    out <- git ["-C", path, "for-each-ref", "--sort=-creatordate", "--format=%(refname:short)%1f%(creatordate:iso-strict)", "refs/tags"]
    forM [l | l <- T.lines out, not (T.null l)] $ \line -> do
        let
            (name, rest) = T.breakOn "\x1f" line
        when' <- either (\(_ :: SomeException) -> Nothing) Just <$> try (utcOf (T.unpack (T.drop 1 rest)))
        pure Tag {name = name, at = when'}

{- | The README at the top of a ref, with its file name; 'Nothing' when
there is none.
-}
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

-- | Every folder and file on a ref, with the files' sizes.
tree :: FilePath -> Text -> IO [Entry]
tree path ref = do
    -- NUL-separated: a name with a quote or a tab in it is not quoted.
    out <- git ["-C", path, "ls-tree", "-z", "-r", "-t", "-l", "--full-tree", "--end-of-options", T.unpack ref]
    traverse entry (filter (not . T.null) (T.splitOn "\0" out))
    where
        -- "<mode> <type> <object> <size>\t<path>"; the size is "-" for a tree.
        entry line = case (T.words meta, T.drop 1 name) of
            ([_, kind, _, size], p)
                | kind == "tree" -> pure Entry {path = p, kind = Tree.Folder, size = Nothing}
                | kind == "blob" -> pure Entry {path = p, kind = Tree.File, size = readMaybe (T.unpack size)}
            -- A submodule's commit is neither; it shows as an empty folder.
            ([_, "commit", _, _], p) -> pure Entry {path = p, kind = Tree.Folder, size = Nothing}
            _ -> throwIO (GitError "git listed a tree entry usta cannot read")
            where
                (meta, name) = T.breakOn "\t" line

-- | A file's contents on a ref.
blob :: FilePath -> Text -> Text -> IO BL.ByteString
blob path ref file = gitBytes ["-C", path, "cat-file", "blob", "--end-of-options", T.unpack ref <> ":" <> T.unpack file]

revs :: Sha -> String
revs = T.unpack . unparse

{- | Fields end in a unit separator and commits in a record separator,
which messages do not contain.
-}
format :: String
format = "--format=%H%x1f%an%x1f%ae%x1f%aI%x1f%s%x1f%b%x1e"

{- | Git writes "Z" for UTC and the author's offset, such as "+05:00",
otherwise; each parser takes only one of the two.
-}
utcOf :: String -> IO UTCTime
utcOf date = case iso8601ParseM date of
    Just utc -> pure utc
    Nothing -> zonedTimeToUTC <$> iso8601ParseM date

{- | The account a forge's private address stands for: GitHub's
"12345+login@users.noreply.github.com" or "login@users.noreply.github.com",
Forgejo's "login@noreply.<host>".
-}
noreplyLogin :: Text -> Maybe Text
noreplyLogin email = case T.breakOn "@" (T.toLower email) of
    (local, domain)
        | domain == "@users.noreply.github.com" -> Just (T.takeWhileEnd (/= '+') local)
        | "@noreply." `T.isPrefixOf` domain, not (T.null local) -> Just local
        | otherwise -> Nothing

commits :: Text -> IO [Commit]
commits out = traverse one (filter (not . T.null . T.strip) (T.splitOn "\x1e" out))
    where
        one record = case T.splitOn "\x1f" (T.dropWhile (== '\n') record) of
            [sha, author, email, date, subject, body] -> do
                hash <- parseM sha
                time <- utcOf (T.unpack date)
                pure
                    Commit
                        { sha = hash
                        , author = author
                        , authoredAt = time
                        , subject = subject
                        , body = T.strip body
                        , login = noreplyLogin email
                        }
            _ -> throwIO (GitError "git printed a commit usta cannot read")
