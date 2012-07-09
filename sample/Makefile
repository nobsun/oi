EXECS = echo recdircs talk catsSIO catsIO catsLIO catsOI

all	: $(EXECS)

echo	: echo.hs
	ghc --make -O0 -threaded $< -o $@

recdircs	: recdircs.hs
	ghc --make -O0 -threaded $< -o $@

talk	: talk.hs
	ghc --make -O0 -threaded $< -o $@

catsIO	: catsIO.hs
	ghc --make -O0 -threaded $< -o $@

catsSIO	: catsSIO.hs
	ghc --make -O0 -threaded $< -o $@

catsLIO	: catsLIO.hs
	ghc --make -O0 -threaded $< -o $@

catsOI	: catsOI.hs
	ghc --make -O0 -threaded $< -o $@

clean	:
	-rm $(EXECS) *.hi *.o *~ core
