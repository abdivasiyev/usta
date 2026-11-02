{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ImplicitParams #-}

module Main (main) where

import GI.Gtk.Objects.Application (Application(..))
import Data.GI.Base
import Control.Monad (void)
import System.Environment (getArgs)

import qualified Usta.UI.Gallery as Gallery
import qualified Usta.UI.Window as Window

-- | @usta --gallery@ opens the component gallery instead of the app.
main :: IO ()
main = do
  args <- getArgs
  let open = if "--gallery" `elem` args then Gallery.build else Window.build
  app <- new Application [#applicationId := "io.github.abdivasiyev.Usta"
                         , On #activate (void $ open ?self)]

  void $ #run app Nothing
