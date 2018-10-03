#include <iostream>
#include "student.h" 
using namespace std;

int main() {
	int a = 85;
	int b = 72;
	Student myStudent(a, b);
	cout << a << " + " << b << " = ";
	cout << myStudent.getSum();
	cout << endl;    	
}
