#ifndef STUDENT_H
#define STUDENT_H

class Student {
    private:
		int data1;
		int data2;
	public:
		Student() {data1 = 0; data2 = 0;}
		Student(int value1, int value2);
		int getSum();
};

#endif
