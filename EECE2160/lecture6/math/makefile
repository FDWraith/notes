# Math program Makefile 

math1: mathMain.o mathfile.o
	g++ mathMain.o mathfile.o -o math1 	

mathMain.o: mathMain.cpp mathfile.h
	g++ -g -Wall -c mathMain.cpp

mathfile.o: mathfile.cpp mathfile.h
	g++ -g -Wall -c mathfile.cpp

clean:
	rm mathMain.o mathfile.o math1


