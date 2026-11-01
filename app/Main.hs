{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ImplicitParams #-}

module Main (main) where

import GI.Gtk.Objects.Application (Application(..))
import Data.GI.Base
import Control.Monad (void)

import qualified Usta.UI.Gallery as Gallery

-- Until the screens exist, the app opens the component gallery.
main :: IO ()
main = do
  app <- new Application [#applicationId := "io.github.abdivasiyev.Usta"
                         , On #activate (void $ Gallery.build ?self)]

  void $ #run app Nothing
