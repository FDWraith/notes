# Lecture 3 - Assembly

Compiler Explorer - tool to visualize how to convert C to assembly

- https://godbolt.org/

Assembly

- `movl` command in assembly moves data to a destination (in memory)
- `addl` command is to add
- `%edx` is a register (starts with % sign) -- like variables to store info
- By convention, whatever is store in a register with a suffix of 'ax' is returned from the function
  - `%eax` is an example
- `%rsp` stack pointer
- `%rbp` keeps track of arguments

Compiling

- `-g` | enables debugging 
- `-O0` | disables optimizations
- `-fno-builtin` | disables built in optimizations

Investigate binary

- `objdump` -- returns the assembly code

Objdump

- `-f` | Shows header information
- `-s` | displays summary info
- `-d` | disassembles the code
- `--source` | show the source code if possible

CISC vs RISC

- RISC better for performance and power consumption
- CISC uses complex instructions, which results in generally smaller code sizes

Vocabulary

- Registers
  - 16 named locations that store (64-bit values)
  - hold important values, including the program state
- Suffixes appended to functions to change how much data to use
  - `mov` -> `movl` moves 1 `l` or 4 bytes worth of information (for `int`s)
  - b : 1 byte
  - w : 2 byte
  - l : 4 byte
  - q : 8 byte

x86-64 Registers

- `%rax` is a 64-bit register
- can reference different parts of the register for different size of data
- `%eax` is the lower 32 bits of `%rax`
- `%ax` is the lower 16 bits of `%eax`
- `%ah` and `%al` are 8-bit separations of `%ax`

`mov`

-  mov source, destination
- (`%rax`) dereferences the register and gets the value
- `%rax` refers to the register

Some registers reserved for special use

- `%rsp` keep track of the stack
- `%rdi` first argument of function
- `%rsi` second argument
- `%rdx` third argument
- `%rip` Program counter (cannot change)

One-argument assembly is applied to `%rax`

Other Assembly Tips

- lines that start with "." are compiler directives.
  - tells the assembler soemthing about the program
  - .text is where the actual code starts
- lines that end with ":" are labels
  - useful for control flow
- .cfi stands for call frame information

Condition Code Cheat Sheet

- CF (Carry Flag for unsigned)
- SF (Carry Flag for signed)
- OF (Overflow Flag for signed)
- ZF (Zero Flag)
- Can be set using `cmp` (computes a-b)
- `test` computes a&b
- `set` reads the condition code

`movzbl` 

- zeroes out first 32 bits of `%rax` register automatically to return a `%eax` value to get rid of garbage data

Stack grows downward, using memory closer to the rest of the memory stack used for everything else.

- `%rsp` is always pointed to the top of the stack
- `pushq` always shifts stack pointer by 8 bytes



