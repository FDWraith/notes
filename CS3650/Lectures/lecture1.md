## Lecture 1

Website: http://www.mshah.io/comp/Fall19/Systems/index.html

Recommended Text: Computer Systems A Programmer's Perspective by Bryant, O'Hallaron

#### What exactly is C?

- C is a compiled lang --> using ``clang`` in this class

- `-o` flag used to name the compile output

- ``puts`` print statement (alternative: ``printf``)

  ```c
  #include <stdio.h>
  
  int main() {
      puts("hello 3650!\n");
      
      return 0;
  }
  ```

#### Compilation Process

- Source Program (hello.c)
- Pre-processor (cpp)
- Modified Source (hello.i)
- Compiler (ccl)
- Assembly program (hello.s)
- Assembler
- Relocatable object
- Linker
- Executable

Generate Assembly Code: ``clang -S hello.c`` 

Objdump: ``objdump -t hello``

- Information about the binary (being able to see all the functions in a program)

#### Hardware

- CPU: where computer instructions are executed
- Buses: transfer information from devices and memory to the CPU

#### Operating Systems (OS)

- Between Hardware + Application Software
- Communicates requests from Software to Hardware
- Resource Manager + Allocator
- Controls execution of user programs



