module Main where

import           Data.List  (sort)
import           System.IO  (BufferMode (..), IOMode (..), hClose, hGetChar,
                             hPutStr, hSetBuffering, hSetEcho, openFile)

import           Options    (Options, defineOptions, runCommand, simpleOption)

import           Text.Funzy (run)

data MainOptions = MainOptions
  { optTty :: String
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "tty" "/dev/tty"
        "DO NOT USE. Changes the in/output channels for testing"

main :: IO ()
main = runCommand $ \opts args -> do
  input <- lines <$> getContents
  tty <- openFile (optTty opts) ReadWriteMode
  hSetBuffering tty NoBuffering
  hSetEcho tty False
  hPutStr tty . unlines . ("" :) . take 5 $ sort input
  hClose tty
