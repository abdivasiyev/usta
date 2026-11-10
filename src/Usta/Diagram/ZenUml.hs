{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid's ZenUML: sequence diagrams written like code, read into the
same events as a sequence diagram and drawn the same way.

@A.call(x)@ is a message from whoever is calling now; a block after it
(@{ … }@) runs inside A, which is active until the block ends.
@r = A.call()@ and @return r@ answer with a dashed message.
@A->B: text@ is an asynchronous message. @if@/@else@, @while@/@for@/
@loop@, @opt@, @par@ and @try@/@catch@/@finally@ are fragments.
-}
module Usta.Diagram.ZenUml
    ( parse
    )
where

import Data.Char (isAlphaNum)
import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Data.Text qualified as T

import Usta.Diagram.Sequence (Event (..), Line (..), Sequence (..), Tip (..), Who (..))

{- | What a "}" closes: a call, whose callee was active, a fragment, or
a group of participants, which draws nothing.
-}
type Frame :: Type
data Frame = Call Text | Fragment | Grouping

type Reading :: Type
data Reading = Reading
    { frames :: [Frame]
    , declared :: [(Text, Bool)]
    -- ^ Participants in order, and whether each is an actor.
    , mentioned :: [Text]
    , said :: [Event]
    -- ^ Newest first.
    , starter :: Maybe Text
    }

parse :: Text -> Either Text Sequence
parse source = do
    r <- go (Reading [] [] [] [] Nothing) (tokens (T.unlines (drop 1 (T.lines source))))
    let
        names = nub (map fst r.declared <> reverse r.mentioned)
        who = [Who {name = n, label = n, actor = lookup n r.declared == Just True} | n <- names]
    if null r.said then Left "The ZenUML diagram has no messages." else pure Sequence {who, events = reverse r.said}
    where
        go r ts = case ts of
            [] -> pure r
            "}" : next : rest | Just kind <- branch next -> case r.frames of
                Fragment : _ -> go (emit (Divide "else" kind) r) rest
                _ -> Left "An else, catch or finally in the ZenUML diagram follows no block."
            "}" : rest -> case r.frames of
                Call a : more -> go (emit (Activate a False) r {frames = more}) rest
                Fragment : more -> go (emit Close r {frames = more}) rest
                Grouping : more -> go r {frames = more} rest
                [] -> Left "A } in the ZenUML diagram closes nothing."
            t : rest -> statement r t >>= (`go` rest)
        -- Braces and semicolons on lines of their own; comments dropped.
        tokens t =
            [ s
            | l <- T.lines t
            , let
                code = T.strip (fst (T.breakOn "//" l))
            , s <- map T.strip (concatMap (T.splitOn ";") (T.split (== '\n') (T.replace "{" "{\n" (T.replace "}" "\n}\n" code))))
            , not (T.null s)
            ]
        -- What follows a "}" and divides its fragment: "else if(x) {",
        -- "catch(e) {", "finally {".
        branch t
            | Just rest <- T.stripPrefix "else" t = let r = T.strip rest in Just (condition (fromMaybe r (T.stripPrefix "if" r)))
            | Just rest <- T.stripPrefix "catch" t = Just (T.strip ("catch " <> condition rest))
            | "finally" `T.isPrefixOf` t = Just "finally"
            | otherwise = Nothing
        -- "(x > 1) {" as "x > 1".
        condition t = T.strip (T.dropAround (`elem` ("()" :: String)) (T.strip (T.dropWhileEnd (== '{') (T.strip t))))
        -- Who sends the next message: the callee of the innermost call, or
        -- the starter, or whoever came first.
        caller r = case [a | Call a <- r.frames] of
            a : _ -> a
            [] -> fromMaybe first r.starter
            where
                first = case map fst r.declared <> reverse r.mentioned of
                    a : _ -> a
                    [] -> "Client"
        emit e r = r {said = e : r.said}
        mention n r = r {mentioned = if n `elem` r.mentioned then r.mentioned else n : r.mentioned}
        opens t = "{" `T.isSuffixOf` t
        body t = T.strip (T.dropWhileEnd (== '{') t)
        statement r t
            | Just rest <- T.stripPrefix "@Starter(" t = pure r {starter = Just (T.strip (T.takeWhile (/= ')') rest))}
            -- "@Actor Client", "@EC2 <<BFF>> Service #E3FCEF"
            | Just rest <- T.stripPrefix "@" t
            , kind : more <- T.words rest
            , name : _ <- [w | w <- more, not ("<<" `T.isPrefixOf` w), not ("#" `T.isPrefixOf` w)] =
                pure r {declared = r.declared <> [(name, kind == "Actor")]}
            | "group" `T.isPrefixOf` t, opens t = pure r {frames = Grouping : r.frames}
            | Just _ <- T.stripPrefix "title" t = pure r
            | (word : _) <- T.words (T.replace "(" " (" (body t))
            , Just kind <- lookup word fragments =
                let
                    cond = condition (T.drop (T.length word) (body t))
                in
                    pure (emit (Open kind (if word `elem` ["try"] then "try" else cond)) r {frames = Fragment : r.frames})
            | t == "return" || "return " `T.isPrefixOf` t =
                case [a | Call a <- r.frames] of
                    a : more -> pure (emit (Message {from = a, to = callerOf more, text = T.strip (T.drop 6 t), line = Dashes, tip = Async, both = False, activate = Nothing}) r)
                    [] -> pure r
            | (lhs, rhs) <- T.breakOn "->" t
            , not (T.null rhs)
            , (target, msg) <- T.breakOn ":" (T.drop 2 rhs)
            , not (T.null msg)
            , T.all name' (T.strip lhs) =
                let
                    a = T.strip lhs
                    b = T.strip target
                in
                    pure (emit (Message {from = a, to = b, text = T.strip (T.drop 1 msg), line = Plain, tip = Async, both = False, activate = Nothing}) (mention b (mention a r)))
            | otherwise = call r t
            where
                callerOf more = case more of
                    a : _ -> a
                    [] -> caller r {frames = []}
        -- "[x = ][A->]B.method(args)[ {]"
        call r t =
            let
                (assigned, expr) = case T.breakOn "=" (body t) of
                    (v, e) | not (T.null e), T.all (\c -> name' c || c == ' ') v, not ("==" `T.isPrefixOf` e) -> (Just (T.strip v), T.strip (T.drop 1 e))
                    _ -> (Nothing, body t)
                (explicit, rest) = case T.breakOn "->" expr of
                    (a, b) | not (T.null b) -> (Just (T.strip a), T.drop 2 b)
                    _ -> (Nothing, expr)
                (target, method) = T.breakOn "." rest
                from = fromMaybe (caller r) explicit
                to = T.strip target
                created = T.takeWhile name' (T.strip (T.drop 4 expr))
            in
                if "new " `T.isPrefixOf` expr && not (T.null created)
                    then pure (emit (Message {from, to = created, text = "«create» " <> T.strip (T.drop 4 expr), line = Plain, tip = Filled, both = False, activate = Nothing}) (mention created (mention from r)))
                    else
                        if T.null method || T.null to || not (T.all name' to)
                            then
                                -- A bare name declares a participant.
                                if T.all name' (body t) && not (T.null (body t))
                                    then pure r {declared = r.declared <> [(body t, False)]}
                                    else Left ("usta could not read \"" <> T.take 40 t <> "\" in the ZenUML diagram.")
                            else do
                                let
                                    text = T.drop 1 method
                                    answer = [Message {from = to, to = from, text = v, line = Dashes, tip = Async, both = False, activate = Nothing} | Just v <- [assigned], not (opens t)]
                                    r' = mention to (mention from r)
                                    sent = emit (Message {from, to, text, line = Plain, tip = Filled, both = False, activate = if opens t || assigned /= Nothing then Just True else Nothing}) r'
                                    deactivate = [Activate to False | assigned /= Nothing, not (opens t)]
                                pure
                                    $ if opens t
                                        then sent {frames = Call to : r.frames}
                                        else foldl (flip emit) sent (answer <> deactivate)
        name' c = isAlphaNum c || c == '_'
        fragments = [("if", "alt"), ("while", "loop"), ("for", "loop"), ("forEach", "loop"), ("loop", "loop"), ("opt", "opt"), ("par", "par"), ("try", "critical")]
