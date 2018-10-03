## Lecture 6: Multiple Files in C++

#### Advantages of multiple files

- reduce redundant work
- collaborative work

#### Header Files

- file with extension .h

- contains libraries that will be uses

- definitions:

  - constants
  - macros
  - function prototypes

  ```c++
  #include <iostream>
  #define SIZE 500
  
  int getSum(int data1, int data2);
  int getDifference(int data1, int data2);
  void printLimit();
  ```

- header files are not compiled

#### Makefile

```makefile
<target>: <dependency>
	<buildCommand>
```

If file <target> does not exist, or <dependency> files are younger, then execute <buildCommand>

