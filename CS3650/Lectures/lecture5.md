# Lecture 5

Von Neumann-Architecture

- Fetch-Decode-Execute
- 3-stage pipeline

CPU | Fetch-Decode-Execute

- Series of operations that the computer executes to retrieve a program instruction from memory and execute

Decode

- Program Counter - register for storing address of execution
- MAR - Memory Address Register - hold address of block of memory for read/write
- MDR - Memory Data Register - holds data
- IR - Instruction Register - current instruction / opcode

Execute

- ALU - Arithmetic Logic Unit - performs integer operations
- Status - overflow, signed, zero, etc.

Memory

- Non-volatile - retains information, even without power
- volatile - loses information when power is lost
  - RAM: Dynamic and Static
- Where instructions are being fetched from

Pipelines

- Performing other parts of the fetch-decode-execute process at the same time.

Drawbacks

- One operation per CPU at a time
- Cannot fetch and execute at same time with the same piece of memory
- jump instructions may clear out entire pipeline (and go back from start)
- CPU is also faster at processing than fetching -- I/O bound

CPU Cache

- Memory close to the CPU (so that it can execute faster)

Kernel Code

- OS Code -- operates alongside your code

Exceptions

- Asynchronous (interrupts) - caused by events external to processor
- Synchronous Exceptions
  - Traps - intentional, such as breakpoints
  - Faults - unintentional, but recoverable
  - Aborts - unintentional and unrecoverable





