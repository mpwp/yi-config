{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State.Lazy
import           Data.List                   (intersperse)
import qualified Data.Text                   as T
import           Lens.Micro.Platform
import           System.Environment          (getArgs)
import           Yi
import           Yi.Command                  (buildRun)
import           Yi.Config.Default.MiscModes (configureMiscModes)
import           Yi.Config.Default.Vim       (configureVim)
import           Yi.Config.Default.Vty       (configureVty)
import           Yi.Config.Simple
import           Yi.Config.Simple.Types
import qualified Yi.Mode.Haskell             as H
import qualified Yi.Mode.Latex               as L
import           Yi.Monad                    (gets)
import           Yi.TextCompletion           (resetComplete, wordComplete)

main :: IO ()
main = do
  files <- getArgs
  let actions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
  config <- execStateT (runConfigM (cfg >> (startActionsA .= actions))) defaultConfig
  startEditor config Nothing

cfg :: ConfigM ()
cfg = do
  configureVty
  configureVim
  configureMiscModes

  addMode L.fastMode
  addMode $ haskell H.literateMode
  addMode $ haskell H.preciseMode
  addMode $ haskell H.cleverMode
  addMode $ haskell H.preciseMode
  publishAction "hlint" hlint
  publishAction "mupdf" mupdf
  publishAction "hdevtools" hdevtools
  publishAction "compile latex" compileLatex
  publishAction "stylish" stylishHaskell

  globalBindKeys raccourcis
  modeBindKeys L.fastMode (ctrlCh 'l' ?>>! compileLatex)
  modeBindKeys L.fastMode (ctrlCh 'p' ?>>! mupdf)

raccourcis :: I Event Action ()
raccourcis = choice
  [ ctrlCh '\t' ?>>! nextWinE
  , shift (ctrlCh '\t') ?>>! prevWinE
  , ctrlCh 'n' ?>>! wordComplete >> withEditor_ resetComplete
  ]

haskell :: Mode syntax -> Mode syntax
haskell mode =
  mode { modeName = modeName mode
       , modeKeymap = topKeymapA %~ ((ctrlCh 'c' ?>>
                        choice [ ctrlCh 'l' ?>>! H.ghciLoadBuffer
                               , ctrlCh 'z' ?>>! H.ghciGet
                               , ctrlCh 'r' ?>>! H.ghciSend ":r"
                               , ctrlCh 't' ?>>! H.ghciInferType
                               , ctrlCh 'c' ?>>! hdevtools
                               , ctrlCh 'd' ?>>! hlint
                               , ctrlCh 'e' ?>>! stylishHaskell
                               ]) <||) }

compileLatex :: YiM ()
compileLatex = withFile $ \fn ->
  buildRun "xelatex" [ "-interaction=nonstopmode"
                     , "-file-line-error"
                     , T.pack fn] (const $ return ())

mupdf :: YiM ()
mupdf = let pdf = flip T.append "pdf" . T.dropEnd 3 . T.pack
  in withFile $ \fn -> buildRun "mupdf" [pdf fn] (const $ return ())

hdevtools :: YiM ()
hdevtools = withFile $ \fn -> buildRun "hdevtools" ["check", T.pack fn] (const $ return ())

hlint :: YiM ()
hlint = withFile $ \fn -> buildRun "hlint" [T.pack fn] (const $ return ())

stylishHaskell :: YiM ()
stylishHaskell = withFile $ \fn -> buildRun "stylish-haskell" ["-i", T.pack fn] (const $ return ())

withFile :: MonadEditor m => (FilePath -> m ()) -> m ()
withFile f = do
  filename <- withEditor . withCurrentBuffer $ gets file
  case filename of
    Just name -> f name
    Nothing   -> return ()