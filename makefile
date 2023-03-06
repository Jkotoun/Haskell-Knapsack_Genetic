all: compile

run:
	cd src && ./flp22-fun

compile: src/flp22-fun.hs
	cd src && ghc flp22-fun.hs -Wall -o flp22-fun

clean: 
	 cd src && rm flp22-fun flp22-fun.hi flp22-fun.o