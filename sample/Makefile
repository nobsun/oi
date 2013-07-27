EXECS = echo recdircs talk cats cats2 morec

all	: $(EXECS)

echo	: echo.hs
	ghc --make -O0 -threaded $< -o $@

recdircs	: recdircs.hs
	ghc --make -O0 -threaded $< -o $@

talk	: talk.hs
	ghc --make -O0 -threaded $< -o $@

cats	: cats.hs
	ghc --make -O0 -threaded $< -o $@

cats2	: cats2.hs
	ghc --make -O0 -threaded $< -o $@

morec	: morec.hs
	ghc --make -O0 -threaded $< -o $@

clean	:
	-rm $(EXECS) *.hi *.o *~ core
