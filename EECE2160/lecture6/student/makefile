# Math program Makefile 

student1: studentMain.o student.o
	g++ studentMain.o student.o -o student1 	

studentMain.o: studentMain.cpp student.h
	g++ -g -Wall -c studentMain.cpp

student.o: student.cpp student.h
	g++ -g -Wall -c student.cpp

clean:
	rm studentMain.o student.o student1


