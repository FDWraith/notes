## Lecture 3: Even More C++

Overloading a function can be done using different parameters

```c++
int main() {
    int num1 = 5, num2 = 10;
    double num3 = 100.5, num4 = 200.6
    getSum(num1, num2); // Calls the int function
    getSum(num4, num5); // Calls the double function
}

int getSum(int &a, int &b) {
    // ... Some code here ... //
}

double getSum(double &a, double &b) {
    // ... Some code here ... //
}
```

Use Comments to describe your code

1. At the top of your file

```c++
/**
* @file 	SortAlgorith.c
* @Author 	John Doe (j.doe@neu.edu)
* @date   	October, 2016
* @brief 	This program reads in 10 numbers and sorts 	
*		 	them in ascending order
*/
#include <stdio.h>
```

2. At the top of each function

```c++
/**
 * PrintData prints the competitors information
 *
 * @param  speed	competitor’s speed
 * @param  name	competitor’s name
 * @param  age	competitor’s age
 * @param  gender	competitor’s gender
 * @return	None   Does not return anything
 */
void PrintData(double speed, string name, int age, char gender) {
	// ... Some Code here ... //
}
```

3. Inside of functions

```c++
int main() {
	// Calls printData to print the values
	PrintData(16.3, “John”, 15, ‘M’); 
} //end main
```

#### More Libraries

```c++
// Example program
#include <iostream>
#include <string>
#include <cmath>
#include <algorithm>
#include <iomanip>

using namespace std;

int main()
{
  int x = 2;
  int y = 3;
  int z = pow(x, y);
  cout << z << endl;

  int a = max(x, y);
  cout << a << endl;

  x = 255;
  cout << dec << x << endl;
  cout << uppercase << hex << x << endl;
  cout << nouppercase << hex << x << endl;
  cout << oct << x << endl;

}
```

Sizeof finds the size of an element in **bytes**

```
int array[3] = {0, 1, 2};
sizeof(array);    // Produces 12 
sizeof(array[0]); // Produces 4 

bool b = 0;
char c = 'A';
float f = 1.234;
int i = 1
double d = 1.234;

sizeof(b);        // Produces 1
sizeof(c);        // Produces 1
sizeof(f);        // Produces 4
sizeof(i);        // Produces 4
sizeof(d);        // Produces 8
```

#### Pointers

```int * x``` creates a pointer ```x``` that points to an ```int```eger. 

Asterick is a **dereference operator**, and points to the data that resides at memory address X.

```c++
// Example program
#include <iostream>
#include <string>

using namespace std;

int main() {
  int temp = 5;
  int *X;
  
  cout << &temp << endl;
  cout << temp << endl;
  
  X = &temp; // X is storing the address of temp
}
```

Difference between passing by reference, and passing by pointers

```c++
// prototypes
int FindAverage(int *var1, int *var2);
int FindAverage(int &var1, int &var2);

int main() {
    int x = 1;
    int y = 2;
    
    FindAverage(1, 2);       // Uses the second function
    FindAverage(&1, &2);     // Uses the first function
}
```

