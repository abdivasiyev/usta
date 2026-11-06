{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Syntax highlighting for code shown anywhere: files, diffs and code
-- blocks in Markdown. Skylighting (pandoc's highlighter, with KDE's
-- syntax definitions) splits the text into tokens; each becomes Pango
-- markup in the palette for the current theme.
module Usta.UI.Highlight
  ( Language
  , languageFor
  , languageNamed
  , languageName
  , Palette (..)
  , currentPalette
  , follow
  , markup
  , plain
  ) where

import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base.Signals (disconnectSignalHandler)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Skylighting (Syntax, TokenType (..), TokenizerConfig (..), defaultSyntaxMap, lookupSyntax, syntaxesByFilename, tokenize)
import qualified Skylighting

-- | A language Skylighting knows.
type Language :: Type
newtype Language = Language Syntax

-- | By file name, e.g. "src/Usta/Git.hs" or "flake.nix".
languageFor :: FilePath -> Maybe Language
-- Skylighting's "Default" is plain text: no language.
languageFor path = Language <$> listToMaybe [s | s <- syntaxesByFilename defaultSyntaxMap path, Skylighting.sName s /= "Default"]

-- | By the name after a Markdown fence, e.g. "haskell", or an extension
-- such as "sh".
languageNamed :: Text -> Maybe Language
languageNamed name = Language <$> lookupSyntax (T.toLower (T.strip name)) defaultSyntaxMap

languageName :: Language -> Text
languageName (Language s) = Skylighting.sName s

-- | The colors for a theme, as Pango takes them.
type Palette :: Type
data Palette = LightPalette | DarkPalette
  deriving stock (Eq, Show)

-- | The palette for the theme GTK uses now.
currentPalette :: IO Palette
currentPalette = do
  settings <- Gtk.settingsGetDefault
  scheme <- maybe (pure Gtk.InterfaceColorSchemeDefault) (`GI.get` #gtkInterfaceColorScheme) settings
  dark <- maybe (pure False) (`GI.get` #gtkApplicationPreferDarkTheme) settings
  pure (if scheme == Gtk.InterfaceColorSchemeDark || dark then DarkPalette else LightPalette)

-- | Runs @paint@ with the current palette now and again whenever the
-- theme changes while @widget@ is on screen, and when it comes back:
-- markup colors are fixed, so highlighted text is painted anew for the
-- other theme. The settings outlive every page, so it listens to them
-- only while on screen; a handler left on them kept @widget@, and all a
-- diff's lines with it, alive for good.
follow :: Gtk.IsWidget w => w -> (Palette -> IO ()) -> IO ()
follow widget paint = do
  currentPalette >>= paint
  settings <- Gtk.settingsGetDefault
  forM_ settings $ \s -> do
    w <- Gtk.toWidget widget
    handler <- newIORef Nothing
    let listen = readIORef handler >>= \case
          Just _ -> pure ()
          Nothing -> do
            h <- GI.on s (GI.PropertyNotify #gtkInterfaceColorScheme) (const (currentPalette >>= paint))
            writeIORef handler (Just h)
        stop = readIORef handler >>= mapM_ (disconnectSignalHandler s) >> writeIORef handler Nothing
    void $ GI.on w #realize (currentPalette >>= paint >> listen)
    void $ GI.on w #unrealize stop
    realized <- Gtk.widgetGetRealized w
    when realized listen

-- | One line of Pango markup per line of @text@. Without a language, or
-- when Skylighting gives up, the lines are only escaped.
markup :: Palette -> Maybe Language -> Text -> [Text]
markup palette language text = case language of
  Nothing -> map plain (T.lines text)
  Just (Language syntax) ->
    case tokenize TokenizerConfig {syntaxMap = defaultSyntaxMap, traceOutput = False} syntax text of
      Left _ -> map plain (T.lines text)
      Right tokenized -> map (foldMap token) tokenized
  where
    token (kind, piece) = case color palette kind of
      Nothing -> plain piece
      Just (hex, italic) ->
        "<span foreground=\"" <> hex <> "\"" <> (if italic then " style=\"italic\"" else "") <> ">"
          <> plain piece <> "</span>"

-- | Text escaped for Pango markup.
plain :: Text -> Text
plain = T.concatMap $ \c -> case c of
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  _ -> T.singleton c

-- | The design's syntax colors, matching the tokens' light and dark
-- values; 'True' for italic.
color :: Palette -> TokenType -> Maybe (Text, Bool)
color palette kind = case kind of
  KeywordTok -> hue keyword
  ControlFlowTok -> hue keyword
  ImportTok -> hue keyword
  PreprocessorTok -> hue keyword
  ExtensionTok -> hue keyword
  DataTypeTok -> hue type'
  BuiltInTok -> hue type'
  AttributeTok -> hue type'
  StringTok -> hue string
  CharTok -> hue string
  VerbatimStringTok -> hue string
  SpecialStringTok -> hue string
  DecValTok -> hue number
  BaseNTok -> hue number
  FloatTok -> hue number
  ConstantTok -> hue number
  SpecialCharTok -> hue number
  FunctionTok -> hue function
  CommentTok -> Just (comment, True)
  DocumentationTok -> Just (comment, True)
  CommentVarTok -> Just (comment, True)
  AnnotationTok -> Just (comment, True)
  AlertTok -> hue alert
  ErrorTok -> hue alert
  WarningTok -> hue alert
  _ -> Nothing
  where
    hue c = Just (c, False)
    dark = palette == DarkPalette
    keyword = if dark then "#B79CF2" else "#6D28D9"
    type' = if dark then "#8FB0F4" else "#1F47A8"
    string = if dark then "#7FCB94" else "#1F7A3A"
    number = if dark then "#F0A070" else "#C2410C"
    function = if dark then "#E5C07B" else "#8A5A00"
    comment = if dark then "#8A9099" else "#7A808A"
    alert = if dark then "#F28B82" else "#B42318"
