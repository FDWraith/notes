# Lecture 4

### Preprocessor

- Handles anything with '#'
- Does text substitution with defined strings
- Use `gcc -E -P filename` to see preprocessor code
- Brings in any library code
- You can define MACROs with #define -- a static function that takes one parameter
- Also gets rid of comments

### Compiler

- broken into 3 peices
  - front end -- generally something that a person can still read 
  - middle end -- code optimizations
  - back end -- generate machine code for architecture
- front end (Lexical Analysis)
  - Read in source code one character at a time
  - Goal: generate tokens (individual lexemes)
    - lexing or tokenization
  - broken into lexems using spaces or delimiters
- front end (syntatic analysis)
  - figure out if we have a valid program
  - Goal: convert stream of tokens into an abstract syntax tree (AST)
    - relies on rules of grammar (for syntactically correct sentences), but not semantics
  - Context-Free Grammar
    - Rules of a programming language
  - Concrete Tree and Abstract Syntax Tree
- front end (semantic analysis)
  - Programs must verifiably have a meaningful relationship
    - statements must be correct
    - type checking is an example
  - Using symbol information for type-checking
- front end (intermediate representation)
  - Similar to ASM, but more flexible and compact
- middle end
  - code optimization
  - dead code elimination
  - done by manipulating intermediate representations
- back end (code generation)
  - Generates into assembly
  - Loss of information during transformation
- back end (machine dependent code optimizer)
  - further optimizations performed on assembly instructions

### Assembler

- assembly code

### Linker

- Link different files together (especially if they reference each other)
- Static Linking
  - putting only the library code that is used into a file
- Dynamic Linking
  - executable file contains no library code
  - only when the program is running does the library code get executed
- Relocation
  - Fill in blanks so that code linked together can be run
  - move programs around so that they won't run into each other

### ELF (Executable and Linkable Format)

Header

- data: how data is ordered (little vs big endian)
- machine: machine architecture
- type: executable
- `readelf -l ./hello` gives program headers
  - GNU_Stack is temp space for storing local vars

.text section

- code

.rodata section (read-only data)

.data section

.bss section

.symtab section

- symbol table. Holds all functions and static varaible names
- `readelf --syms`

.rel.text section

- relocating library code to properly link program

.rel.data section

- same, but for data values

.debug section

- information for debugging

### Linker

- resolves duplicate symbol definitions by labelling symbols as "strong" or "weak"
  - strong - initialized
  - weak - uninitialized or 'extern'
- static makes variables global in a file -- cannot be seen elsewhere

Dynamic Libraries

- `clang -shared -fpic myfile.c -o libmylib.so`
- `-fpic` makes position independent code





