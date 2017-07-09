---
title: Main
---

<div id="package-header">

-   [Contents](index.html)
-   [Index](doc-index.html)

yi

</div>

<div id="content">

<div id="module-header">

  -------------- -------------
  Portability    POSIX
  Safe Haskell   None
  Language       Haskell2010
  -------------- -------------

Main

</div>

<div id="table-of-contents">

Contents

-   [Global shortcuts](#g:1)
-   [Customize haskell modes](#g:2)
-   [LaTex compilation and pdf rendering](#g:3)
-   [Haskell tools](#g:4)
-   [Help function](#g:5)

</div>

<div id="description">

Description

<div class="doc">

Simple Yi configuration to ease haskell development and LaTex editing.

Dependencies:

-   xelatex
-   [mupdf](https://mupdf.com/)
-   [hdevtools](https://hackage.haskell.org/package/hdevtools)
-   [hlint](https://hackage.haskell.org/package/hlint)
-   [stylish-haskell](https://hackage.haskell.org/package/stylish-haskell)

</div>

</div>

<div id="synopsis">

Synopsis

-   [main](#v:main) ::
    [IO](file:///usr/share/doc/ghc-8.0.2/html/libraries/base-4.9.1.0/System-IO.html#t:IO)
    ()
-   [config](#v:config) ::
    [ConfigM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Config-Simple-Types.html#t:ConfigM)
    ()
-   [raccourcis](#v:raccourcis) ::
    [I](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Interact.html#t:I)
    [Event](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Event.html#t:Event)
    [Action](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:Action)
    ()
-   [modifyHaskellMode](#v:modifyHaskellMode) ::
    [Mode](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:Mode)
    syntax -&gt;
    [Mode](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:Mode)
    syntax
-   [compileLatex](#v:compileLatex) ::
    [YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
    ()
-   [mupdf](#v:mupdf) ::
    [YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
    ()
-   [hdevtools](#v:hdevtools) ::
    [YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
    ()
-   [hlint](#v:hlint) ::
    [YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
    ()
-   [stylishHaskell](#v:stylishHaskell) ::
    [YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
    ()
-   [withFile](#v:withFile) ::
    [MonadEditor](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:MonadEditor)
    m =&gt;
    ([FilePath](file:///usr/share/doc/ghc-8.0.2/html/libraries/base-4.9.1.0/System-IO.html#t:FilePath)
    -&gt; m ()) -&gt; m ()

</div>

<div id="interface">

Documentation
=============

<div class="top">

[main]{#v:main .def} ::
[IO](file:///usr/share/doc/ghc-8.0.2/html/libraries/base-4.9.1.0/System-IO.html#t:IO)
() [\#](#v:main){.selflink}

<div class="doc">

Run Yi with custom static configuration

</div>

</div>

<div class="top">

[config]{#v:config .def} ::
[ConfigM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Config-Simple-Types.html#t:ConfigM)
() [\#](#v:config){.selflink}

<div class="doc">

Custom configuration

</div>

</div>

Global shortcuts {#g:1}
----------------

<div class="top">

[raccourcis]{#v:raccourcis .def} ::
[I](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Interact.html#t:I)
[Event](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Event.html#t:Event)
[Action](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:Action)
() [\#](#v:raccourcis){.selflink}

<div class="doc">

`raccourcis` define global shortcuts. It is used to quickly try new
tricks.

</div>

</div>

Customize haskell modes {#g:2}
-----------------------

<div class="top">

[modifyHaskellMode]{#v:modifyHaskellMode .def} ::
[Mode](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:Mode)
syntax -&gt;
[Mode](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:Mode)
syntax [\#](#v:modifyHaskellMode){.selflink}

<div class="doc">

`modifyHaskellMode` function describe how to change default haskell
modes.

</div>

</div>

LaTex compilation and pdf rendering {#g:3}
-----------------------------------

<div class="top">

[compileLatex]{#v:compileLatex .def} ::
[YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
() [\#](#v:compileLatex){.selflink}

<div class="doc">

Function to compile LaTeX files in yi.

</div>

</div>

<div class="top">

[mupdf]{#v:mupdf .def} ::
[YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
() [\#](#v:mupdf){.selflink}

<div class="doc">

Function to display generated pdf of the currently edited LaTeX file.

</div>

</div>

Haskell tools {#g:4}
-------------

<div class="top">

[hdevtools]{#v:hdevtools .def} ::
[YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
() [\#](#v:hdevtools){.selflink}

<div class="doc">

Check haskell source files with hdevtools. Print results in a new buffer
window.

</div>

</div>

<div class="top">

[hlint]{#v:hlint .def} ::
[YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
() [\#](#v:hlint){.selflink}

<div class="doc">

Run hlint with the current haskell buffer. Print results in a new buffer
window.

</div>

</div>

<div class="top">

[stylishHaskell]{#v:stylishHaskell .def} ::
[YiM](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:YiM)
() [\#](#v:stylishHaskell){.selflink}

<div class="doc">

Prettify haskell code using stylish-haskell

</div>

</div>

Help function {#g:5}
-------------

<div class="top">

[withFile]{#v:withFile .def} ::
[MonadEditor](file:///home/grp/.cabal/share/doc/x86_64-linux-ghc-8.0.2/yi-core-0.14.0/html/Yi-Types.html#t:MonadEditor)
m =&gt;
([FilePath](file:///usr/share/doc/ghc-8.0.2/html/libraries/base-4.9.1.0/System-IO.html#t:FilePath)
-&gt; m ()) -&gt; m () [\#](#v:withFile){.selflink}

<div class="doc">

`withFile` run the specified function argument on the current buffer.

</div>

</div>

</div>

</div>

<div id="footer">

Produced by [Haddock](http://www.haskell.org/haddock/) version 2.17.3

</div>
