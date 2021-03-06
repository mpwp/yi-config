\begin{code}
{-# LANGUAGE OverloadedStrings #-}
\end{code}

{-|
Module : Main
Description: Custom Yi configuration
Portability: POSIX
Simple Yi configuration to ease haskell development and LaTex editing.

Dependencies:

  * xelatex

  * <https://mupdf.com/ mupdf>

  * <https://hackage.haskell.org/package/hdevtools hdevtools>

  * <https://hackage.haskell.org/package/hlint hlint>

  * <https://hackage.haskell.org/package/stylish-haskell stylish-haskell>
-}

\begin{code}
module Main
  ( main
  , config
  -- ** Global shortcuts
  , raccourcis
  -- ** Customize haskell modes
  , modifyHaskellMode
  -- ** LaTex compilation and pdf rendering
  , compileLatex
  , mupdf
  -- ** Haskell tools
  , hdevtools
  , hlint
  , stylishHaskell
  -- ** Help function
  , withFile
  ) where

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
import           Yi.Fuzzy
import qualified Yi.Mode.Haskell             as H
import qualified Yi.Mode.Latex               as L
import           Yi.Monad                    (gets)
import           Yi.TextCompletion           (resetComplete, wordComplete)
\end{code}

Run Yi with custom static configuration
\begin{code}
main :: IO ()
main = do
  files <- getArgs
  let actions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
  c <- execStateT (runConfigM (config >> (startActionsA .= actions))) defaultConfig
  startEditor c Nothing
\end{code}

Custom configuration
\begin{code}
config :: ConfigM ()
config = do
  configureVty
  configureVim
  configureMiscModes
  addMode L.fastMode
  addMode $ modifyHaskellMode H.literateMode
  addMode $ modifyHaskellMode H.preciseMode
  addMode $ modifyHaskellMode H.cleverMode
  addMode $ modifyHaskellMode H.preciseMode
  publishAction "hlint" hlint
  publishAction "mupdf" mupdf
  publishAction "hdevtools" hdevtools
  publishAction "compile latex" compileLatex
  publishAction "stylish" stylishHaskell
  globalBindKeys raccourcis
  modeBindKeys L.fastMode (ctrlCh 'l' ?>>! compileLatex)
  modeBindKeys L.fastMode (ctrlCh 'p' ?>>! mupdf)
\end{code}

'raccourcis' define global shortcuts.
\begin{code}
raccourcis :: I Event Action ()
raccourcis =
  choice
    [ ctrlCh 'n' ?>>! wordComplete >> withEditor_ resetComplete
    , ctrlCh 'p' ?>>! fuzzyOpen
    , ctrlCh 'h' ?>>! previousTabE
    , ctrlCh 'l' ?>>! nextTabE
    , ctrlCh 'x' ?>>! errorEditor "bib\nbob"
    ]
\end{code}

'modifyHaskellMode' function describe how to change default haskell modes.
\begin{code}
modifyHaskellMode :: Mode syntax -> Mode syntax
modifyHaskellMode mode =
  mode
  { modeName = modeName mode
  , modeKeymap =
      topKeymapA %~
      ((ctrlCh 'c' ?>>
        choice
          [ ctrlCh 'l' ?>>! H.ghciLoadBuffer
          , ctrlCh 'z' ?>>! H.ghciGet
          , ctrlCh 'r' ?>>! H.ghciSend ":r"
          , ctrlCh 't' ?>>! H.ghciInferType
          , ctrlCh 'c' ?>>! hdevtools
          , ctrlCh 'd' ?>>! hlint
          , ctrlCh 'e' ?>>! stylishHaskell
          , ctrlCh 'b' ?>>! brittany
          ]) <||)
  }
\end{code}

Function to compile LaTeX files in yi.
\begin{code}
compileLatex :: YiM ()
compileLatex = withFile $ \fn -> buildRun "xelatex" (T.pack fn : args) (const $ return ())
  where
    args = ["-interaction=nonstopmode", "-file-line-error"]
\end{code}

Function to display generated pdf of the currently edited LaTeX file.
\begin{code}
mupdf :: YiM ()
mupdf = withFile $ \fn -> buildRun "mupdf" [pdf fn] (const $ return ())
  where
    pdf = flip T.append "pdf" . T.dropEnd 3 . T.pack
\end{code}

Check haskell source files with hdevtools. Print results in a new buffer window.
\begin{code}
hdevtools :: YiM ()
hdevtools = withFile $ \fn -> buildRun "hdevtools" ["check", T.pack fn] (const $ return ())
\end{code}

Run hlint with the current haskell buffer. Print results in a new buffer window.
\begin{code}
hlint :: YiM ()
hlint = withFile $ \fn -> buildRun "hlint" [T.pack fn] (const $ return ())
\end{code}

Prettify haskell code using stylish-haskell
\begin{code}
stylishHaskell :: YiM ()
stylishHaskell = withFile $ \fn -> do
    b <- runProcessWithInput ("stylish-haskell -i " ++ fn) "Stylish-haskell done."
    errorEditor $ T.pack b
\end{code}

Prettify haskell code using brittany
\begin{code}
brittany :: YiM ()
brittany = withFile $ \fn -> do
    b <- runProcessWithInput ("brittany " ++ fn) "Brittany done."
    errorEditor $ T.pack b
\end{code}

'withFile' run the specified function argument on the current buffer.
\begin{code}
withFile :: MonadEditor m => (FilePath -> m ()) -> m ()
withFile f = do
  filename <- withEditor . withCurrentBuffer $ gets file
  case filename of
    Just name -> f name
    Nothing   -> return ()
\end{code}
