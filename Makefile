
# set to a low value (~50) for testing
# set to a high vlaue (>1000) for production
MAXPROBLEMS = 50

# find all templtes in the tex directory
A5TEMPLATES = $(wildcard tex/*-a5.tex)
A5PDF = $(patsubst tex/%-a5.tex, pdf/%-a5.pdf, $(A5TEMPLATES))

.PHONY: all, clean, a5

all: a5

a5: $(A5PDF)

pdf/%-a5.pdf: tmp/%-a5.dvi
	mkdir -p pdf
	dvipdf $< $@

tmp/%-a5.dvi: tex/%-a5.tex tmp/%-problems.tex
	mkdir -p tmp
	latex -output-directory=tmp $<

tmp/%-problems.tex: tsumego/books/%.tex bin/reformatter
	mkdir -p tmp
	bin/reformatter $< $@ $(MAXPROBLEMS)

# prevent make from treating the books as intermediate and deleting them
.PRECIOUS: tsumego/books/%.tex

tsumego/books/%.tex:
	git clone https://github.com/tasuk/tsumego.git

bin/reformatter: src/reformatter.hs
	mkdir -p bin
	mkdir -p tmp
	ghc $< -o $@ -outputdir tmp

clean:
	rm -rf tsumego
	rm -f tmp/*