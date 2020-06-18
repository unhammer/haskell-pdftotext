# pdftotext

The `pdftotext` package provides functions for extraction of plain text from PDF documents. It uses C++ library [Poppler](https://poppler.freedesktop.org/), which is required to be installed in the system. Output of Haskell `pdftotext` library is identical to output of Poppler's tool `pdftotext`.

## Usage

```haskell
import qualified Data.Text.IO as T
import Pdftotext

main :: IO ()
main = do
  Just pdf <- openFile "path/to/file.pdf"
  T.putStrLn $ pdftotext Physical pdf
```

## Executable

`pdftotext` comes with executable program `pdftotext.hs` which can print text extracted from PDF and basic information from the document.

```shell
$> pdftotext.hs info test/simple.pdf
File      : test/simple.pdf
Pages     : 4
Properties
  Title   : Simple document for testing
  Author  : G. Eyaeb
  Subject : Testing
  Creator : pdflatex
  Producer: LaTeX with hyperref
  Keywords: haskell,pdf
```

```shell
$> pdftotext.hs text --pages 1,4 test/simple.pdf
Simple document for testing

                  deserve neither
liberty nor safety.
```

See help for more information:

```shell
$> pdftotext.hs --help
$> pdftotext.hs text --help
$> pdftotext.hs info --help
```

## Internals

The library uses poppler via FFI, therefore internally all functions are of type `IO`. However, their non-`IO` variants (using `unsafePerformIO`) _should be_ safe to use. Module `Pdftotext.Internal` exposes all `IO`-typed functions.

## Contribute

Project is hosted at https://sr.ht/~geyaeb/haskell-pdftotext/ . The homepage provides links to [Mercurial repository](https://hg.sr.ht/~geyaeb/haskell-pdftotext), [mailing list](https://lists.sr.ht/~geyaeb/haskell-pdftotext) and [ticket tracker](https://todo.sr.ht/~geyaeb/haskell-pdftotext).

Patches, suggestions, questions and general discussions can be send to the [mailing list](https://lists.sr.ht/~geyaeb/haskell-pdftotext). Detailed information about sending patches by email can be found at [https://man.sr.ht/hg.sr.ht/email.md](https://man.sr.ht/hg.sr.ht/email.md).
