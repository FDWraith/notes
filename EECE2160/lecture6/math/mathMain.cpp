#include <iostream> //For cout statement
#include "mathfile.h" //Needs the header file
using namespace std;

int main() {
	int a = 25, b = 15;
	std::cout << "The sum is = " << getSum(a, b) << endl;
	std::cout << "The difference is = " << getDifference(a, b) << endl;
	printLimit();

	return 0;
}


