all: compile

run:
	cd src && ./flp22-fun

compile: 
	cd src && ghc Main.hs -Wall -o flp22-fun && mv flp22-fun ../flp22-fun

clean: 
	rm flp22-fun && cd src && rm *.hi *.o