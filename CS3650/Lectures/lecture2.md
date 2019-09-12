## Lecture 2

C 

- high level systems programming language (compared to assembly)
- strongly-typed
- manage your own memory
- Can inline assembly

Scoping

- variables must be declared before being used, 
- variables must be in scope {} (block-scoped)

Static Arrays

- cannot expand the storage beyond declaration
- can only store 1 type of data

Memory => Giant linear array

- each index can store 1 byte of data
- different pieces of information will take up 1, 2, 4, etc.. bytes of memory

Memory

- `sizeof` : returns the number of bytes used by a data type

- each memory has a value and an address

- `&` can be used to retrieve the address.

  ```c#
  int blah = 5;
  printf("The address of blah is: %p \n", (int*)&blah);
  ```

clang vs gcc

- gcc is better for performance
- clang is better for debugging

### Pointers

- data type that stores an address -- indirectly pointing to a value

  ```c
  int* a;
  int *a; // These are the same
  ```

- `void*` can be used to reference any pointer of any type

Dereferencing a pointer

- Get the value that the pointer is pointing at

  ```c
  int x = 5;
  int* px = &x;
  printf("px dereferenced: %d\n", *px); // Dereferencing the pointer
  //      px dereferenced:  5\n
  ```

- Modifying the dereferenced value changes original value

  ```c
  int x = 5;
  int* px = &x;
  
  // x's original value
  printf("x is: %d\n", x); // 5
  
  // Modify px
  *px = 100;
  
  // x's new value
  printf("x is: %d\n", x); // 100
  ```

Best Practice in C language is to initialize variables to some starting value (or `NULL`).

C Arrays

```c
char d[6] = {'m', 'i', 'k', 'e', 's', '\0'}
// '\0' is a null, to terminate a string
```

C is a pass by value language

- When we pass in an argument into a function, a copy of that variable is passed in
- We can modify values by passing in pointers

Stack memory

- allocated when we call a function
- anytime a function returns, the OS reclaims that memory for that process

Malloc and Free

- `malloc` is a way to request memory from the OS

- `malloc` allows us to dynamically allocate memory

- If we request memory, we should also release (`free`) the memory when done using

  ```c
  #include <stdlib.h> // include malloc/free
  
  int* memory = (int*)malloc(sizeof(int)*4); // 4 times the size of int
  memory[0] = 0;
  memory[1] = 1;
  memory[2] = 2;
  memory[3] = 3;
  
  // free the memory
  free(memory)
  ```

- Malloc'd memory is not automatically cleaned up -- will result in memory leak!

### C structs

- composite data type

  ```c
  typedef struct Student {
      int age;
      int id;
  }Student_t; 
  
  Student_t* bob = malloc(sizeof(Student_t)*1);
  
  // Arrow syntax is short hand for
  bob->age = 21;
  (*bob).age = 21; // These two are identical
  
  free(bob); // don't forget to free
  ```

- Array []'s are also dereferencing