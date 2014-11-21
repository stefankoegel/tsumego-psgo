
.PHONY: clean

tsumego/books/cho-1-elementary.tex:
	git clone https://github.com/tasuk/tsumego.git

clean:
	rm -rf tsumego