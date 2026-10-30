{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import qualified GI.Gtk as Gtk

import GI.Gtk.Objects.Application (Application(..))
import Data.GI.Base
import Control.Monad (void)
import Data.Int (Int32)

pressedCB :: (?self :: Gtk.GestureClick) => Int32 -> Double -> Double -> IO ()
pressedCB nPress x y = do
    button <- #getCurrentButton ?self
    putStrLn $ "Button pressed: " <> show nPress <> " "
      <> show x <> " " <> show y <> " button: " <> show button

activate :: Gtk.Application -> IO ()
activate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  adjustment <- new Gtk.Adjustment [#value := 50, #lower := 0, #upper := 100,
                                    #stepIncrement := 1]
  slider <- new Gtk.Scale[#adjustment := adjustment, #drawValue := True]
  #append box slider
  spinButton <- new Gtk.SpinButton [#adjustment := adjustment]
  #append box spinButton

  controller <- new Gtk.GestureClick [After #pressed pressedCB]
  #addController slider controller

  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title := "Hello",
                                       #child := box]
  #show window

main :: IO ()
main = do
  app <- new Application [#applicationId := "abdivasiyev.github.io/usta"
                         , On #activate (activate ?self)]

  void $ #run app Nothing
