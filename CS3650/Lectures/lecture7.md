# Lecture 7

### Memory Management Unit (MMU)

- converts the virtual address (allocated for the program) into the physical memory address on the computer
- operates in terms of pages of memory (4096B)
- Virtual Memory -- lets each process get a linear piece of memory, when in reality, it might be spread across the physical space
  - processes also cannot interfere with each other.

### Page Table

- tracks the mapping of virtual to physical

- page misses / page is not in memory -> page fault

### Page Translation

- Virtual Page Offset + Virtual Page Number
- Physical Page Offset + Physical Page Number
- VPO and PPO must match, while VPN is mapped through the page table to get PPN

### Translation Lookup Buffer

- cache of recent page table entries for (small number of pages)
- 