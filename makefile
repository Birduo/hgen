br: hgen.hs test.md
	ghc -o hgen hgen.hs
	./hgen test.md test.html header.html
	cat test.html

build: hgen.hs
	ghc -o hgen hgen.hs

run: hgen test.md
	./hgen test.md test.html header.html
	cat test.html

clean:
	rm *.o *.hi hgen test.html
