
all: Declare.hs Interp.hs TypeCheck.hs Main.hs Parser
	ghc Main.hs

Parser: Parser.y
	happy Parser.y

PHONY: clean
clean:
	rm *.hi *.o Main
