# Revision history for pdftotext

## 0.1.0.1 -- 2020-12-01

* Require C++11 (solves [#3](https://todo.sr.ht/~geyaeb/haskell-pdftotext/3), using on MacOS)
* Prevent deletion of document pointer before its pages (solves [#4](https://todo.sr.ht/~geyaeb/haskell-pdftotext/4), segmentation faults)

## 0.1.0.0 -- 2020-06-18

* Added executable `pdftotext.hs`
* Removed `xml-conduit` flag, it was bad idea, sorry for that

## 0.0.2.0 -- 2020-06-11

* Added PDF document properties (author, title etc.)
* Added flag `xml-conduit` (parse metadata using `xml-conduit`)

## 0.0.1.0 -- 2020-05-10

* First version.
