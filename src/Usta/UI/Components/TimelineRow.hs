{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A timeline event as a row between the comments, as the design draws
it: an icon in a circle, who in bold, what they did, and when.
-}
module Usta.UI.Components.TimelineRow
    ( new
    , css
    )
where

import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.Gtk qualified as Gtk

import Usta.Types.Discussion (Verdict (..))
import Usta.Types.Label (Label (..))
import Usta.Types.Parse (unparse)
import Usta.Types.Timeline (Event (..), Happening (..))
import Usta.UI.Format (Clock, ago)

import Usta.UI.Components.Icon qualified as Icon

{- | The words of a row: plain text, a label chip, a struck-out old title,
or a reference.
-}
data Piece = Words Text | Chip Label | Struck Text | Ref Text

new :: Clock -> Event -> IO Gtk.Widget
new c e = do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["u-timeline-row"]]
    circle <- GI.new Gtk.Box [#valign := Gtk.AlignCenter, #cssClasses := ["circle"]]
    Icon.new 14 (iconOf e.what) >>= #append circle
    #append row circle
    text <- GI.new Gtk.Label [#useMarkup := True, #wrap := True, #xalign := 0, #hexpand := True]
    GI.set text [#label := "<b>" <> escape (unparse e.actor) <> "</b> " <> T.unwords (map piece (pieces e.what)) <> "  <span alpha='60%'>" <> escape (ago c e.at) <> "</span>"]
    #append row text
    Gtk.toWidget row

iconOf :: Happening -> Icon.Icon
iconOf h = case h of
    Labeled _ _ -> Icon.Tag
    Assigned _ _ -> Icon.Person
    ReviewRequested _ _ -> Icon.Person
    Milestoned _ _ -> Icon.Flag
    Renamed _ _ -> Icon.Pencil
    Closed -> Icon.Closed
    Reopened -> Icon.Reopened
    Merged -> Icon.Merge
    Referenced _ _ -> Icon.Chain
    Pushed _ -> Icon.Commit
    MergeScheduled _ -> Icon.Clock
    BranchDeleted -> Icon.Trash
    Reviewed _ -> Icon.Check

pieces :: Happening -> [Piece]
pieces h = case h of
    Labeled True l -> [Words "added", Chip l]
    Labeled False l -> [Words "removed", Chip l]
    Assigned True who -> [Words "assigned", Words ("<b>" <> escape (unparse who) <> "</b>")]
    Assigned False who -> [Words "unassigned", Words ("<b>" <> escape (unparse who) <> "</b>")]
    ReviewRequested True who -> [Words "asked", Words ("<b>" <> escape (unparse who) <> "</b>"), Words "for a review"]
    ReviewRequested False who -> [Words "stopped asking", Words ("<b>" <> escape (unparse who) <> "</b>"), Words "for a review"]
    Milestoned _ (Just new') -> [Words "added this to", Words ("<b>" <> escape new' <> "</b>")]
    Milestoned (Just old) Nothing -> [Words "removed this from", Words ("<b>" <> escape old <> "</b>")]
    Milestoned Nothing Nothing -> [Words "changed the milestone"]
    Renamed old _ -> [Words "changed the title from", Struck old]
    Closed -> [Words "closed this"]
    Reopened -> [Words "reopened this"]
    Merged -> [Words "merged this"]
    Referenced n title -> [Words "referenced this in", Ref ("#" <> T.pack (show (unparse n)) <> " " <> title)]
    Pushed n -> [Words ("pushed " <> T.pack (show n) <> (if n == 1 then " commit" else " commits"))]
    MergeScheduled True -> [Words "set this to merge when the checks pass"]
    MergeScheduled False -> [Words "stopped the merge when the checks pass"]
    BranchDeleted -> [Words "deleted the branch"]
    Reviewed Approve -> [Words "approved these changes"]
    Reviewed RequestChanges -> [Words "requested changes"]
    Reviewed CommentOnly -> [Words "reviewed"]

piece :: Piece -> Text
piece p = case p of
    Words w -> w
    Chip l -> "<span background='#" <> unparse l.color <> "' foreground='" <> readable (unparse l.color) <> "'> " <> escape l.name <> " </span>"
    Struck t -> "<s>" <> escape t <> "</s>"
    Ref t -> "<span foreground='#2F5FD0'>" <> escape t <> "</span>"

-- | Black or white, whichever reads on the hex colour.
readable :: Text -> Text
readable hex = if light then "#1B1D21" else "#FFFFFF"
    where
        channel i = case reads ("0x" <> T.unpack (T.take 2 (T.drop i hex))) :: [(Int, String)] of
            [(v, "")] -> v
            _ -> 0
        light = 299 * channel 0 + 587 * channel 2 + 114 * channel 4 > 150000

escape :: Text -> Text
escape = T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

css :: Text
css =
    mconcat
        [ "box.u-timeline-row { margin-left: 14px; padding: 2px 0; }"
        , "box.u-timeline-row box.circle { min-width: 26px; min-height: 26px; border-radius: 13px; background: var(--border-subtle); color: var(--text-soft); }"
        , "box.u-timeline-row box.circle image { margin: 6px; }"
        , "box.u-timeline-row label { font-size: 13px; color: var(--text-soft); }"
        ]
