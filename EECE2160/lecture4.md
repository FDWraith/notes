## Lecture 4: Dynamic Memory Allocation

In C++, ```new``` and ```delete``` can be used to obtain new memory, and release memory when it's no longer needed. 

```c++
int *ptr = new int;
*ptr = 8;            // assigns 8 to memory
delete ptr;          // returns memory to the system
```

Dynamically allocated memory is taken from the heap, as opposed to the stack during static memory allocation.

Use ```delete[]``` for arrays.