
.PHONY: all, clean

all: bin/reformatter

tsumego/books/cho-1-elementary.tex:
	git clone https://github.com/tasuk/tsumego.git

bin/reformatter: src/reformatter.hs
	ghc $< -o $@ -outputdir tmp

clean:
	rm -rf tsumego
	rm tmp/*