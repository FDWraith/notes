#include "student.h"


Student::Student(int value1, int value2) {
	data1 = value1;
	data2 = value2;
}

int Student::getSum() {
	int sum = data1 + data2;
 	return sum;
}
