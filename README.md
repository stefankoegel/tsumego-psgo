# tsumego-psgo

This project creates PDFs from Vit Brunners ([github repository](https://github.com/tasuk/tsumego))
Tsumego Collections using `psgo`
([ctan](http://www.ctan.org/tex-archive/graphics/pstricks/contrib/psgo/)).

## Motivation

I am very thankful to Vit Brunner for the work he did on his Tsumego Collections
([website](http://tsumego.tasuki.org/)).

These collections  were created using
`sgf2dg` ([cpan](http://search.cpan.org/~reid/Games-Go-Sgf2Dg-4.211/sgf2dg)).
They depend on a certain font, that my printer does not support.
Instead of getting the fonts working,
I decided to recreate the books using `psgo`, which uses `tikz` to draw the
go boards ([go/baduk/weiqi game](http://en.wikipedia.org/wiki/Go_%28game%29))
directly.
This allowed me to change the Collections typesettings to my needs,
leading to a version of the Collections for double sided printing (long edge flip) on A4 paper to produce an A5 booklet.

In short: I had a fun weekend using Haskell, Makefiles and LaTeX.
But I don't expect this to be of much use to anyone else.

## Dependencies

You will need (at least) the following tools:

* `ghc`
* `make`
* `latex`
* `pdflatex`
* `dvipdf` (aka `dvips`)

You will also need to install

* `psgo`

from http://www.ctan.org/tex-archive/graphics/pstricks/contrib/psgo/.
Simply copy the `psgo.sty` file into this repository

## Usage

After taking care of all dependencies type `make all` on a console in the repository's directory.
This may take some time and will place the resulting PDFs in the `pdf` directory
which will be created if it does not exist.
