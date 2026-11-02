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

-- | @usta --gallery@ opens the component gallery instead of the app;
-- @usta --welcome@ starts on the first-run screen.
main :: IO ()
main = do
  args <- getArgs
  let open
        | "--gallery" `elem` args = void . Gallery.build
        | "--welcome" `elem` args = void . Window.buildFirstRun
        | otherwise = void . Window.build
  app <- new Application [#applicationId := "io.github.abdivasiyev.Usta"
                         , On #activate (void $ open ?self)]

  void $ #run app Nothing
