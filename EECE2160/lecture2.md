## Lecture 2: More C++

#### Functions

```
returnType functionName (inputParameter, ...) {
    code
}
```

Two ways to use functions: pass by value and pass by reference

```c++
// Pass by value
void multiply(int answer) {
    answer = answer * 2;
}
int main() {
    int value = 2;
    multiply(value);
    cout << value << endl;
    // Expected output: 2
}
```

```c++
// Pass by reference
void multiply(int &answer) {
    answer = answer * 2;
}
int main() {
    int value = 2;
    multiply(value);
    cout << value << endl;
    // Expected output: 4
}
```

#### Arrays and Functions

Arrays are automatically pass by reference

```c++
int generate(int arr[], int size) {
    int temp;
    for (temp = 0; temp < size; temp += 1) {
        arr[temp] = temp * size;
    }
}

int main() {
    int myArray[5] = {10, 20, 30, 40, 50};
    
    generate(myArray, 5); 
    // myArray becomes {0, 5, 10, 15, 20}
    
    // alternatively:
    generate(&myArray[0], 5);
  	// the address of myArray is the same as the address of its first element
  	
  	// THIS DOES NOT WORK
  	generate(&myArray, 5);
  	// myArray IS the address to the array. &myArray is the address to the pointer
  	// that points to myArray
}
```

#### Exercises

```c++
#include <iostream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
using namespace std;

//Randomize the content of array
void RandomArray(int array[], int size) {
    int temp;
    srand(time(NULL));
    
    // after size iterations, each index of array will have a random
    // number in the range [0, 100)
    for (temp = 0; temp < size; temp += 1) {
        array[temp] = rand() % 100;
    }
}

//Print the content of array
void PrintArray(int array[], int size) {
    int temp;
    cout << "[";
    for (temp = 0; temp < size; temp += 1) {
        if (temp != size - 1) {
            cout << " " << array[temp] << ","; 
        } else {
            cout << " " << array[temp];
        }
    }
    cout << " ]" << endl;
}    

// Helper function for SortArray
// Finds index of the smallest value in array from [leftBound, rightBound)
int minIndexInRange(int array[], int leftBound, int rightBound) {
    int temp;
    int minSoFar = array[leftBound];
    int minDex = leftBound;
    for (temp = leftBound; temp < rightBound; temp += 1) {
        if (array[temp] < minSoFar) {
            minSoFar = array[temp];
            minDex = temp;
        }
    }
    return minDex;
}

// Helper function for SortArray
// Swaps the elements at two indexes
void swap(int array[], int index1, int index2) {
    int temp = array[index1];
    array[index1] = array[index2];
    array[index2] = temp;
}

//Sort the array in ascending order
void SortArray(int array[], int size) {
    int temp;
    for (temp = 0; temp < size; temp += 1) {
        int minDex = minIndexInRange(array, temp, size);
        swap(array, temp, minDex);
    }
}
 
int main() {
    int size = 10;
    int arrayv[10];
    
    RandomArray(arrayv, size);
    PrintArray(arrayv, size);
    SortArray(arrayv, size);
    PrintArray(arrayv, size);
    
    return 0;
}
```

#### Function Prototypes

Function Prototypes alerts the program about the existence of a function, before its definition

```c++
#include <iostream>
using namespace std;

int getSum(int a, int b);  /* prototype - function declaration */

int main() {
	int num1 = 5, num2 = 10;
	int results = getSum(num1, num2);
	cout << results << endl;
}

/* function definition */
int getSum(int a, int b) {	
	return a * b;
}
```



