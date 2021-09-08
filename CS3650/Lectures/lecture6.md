# Lecture 6

**Principle of Locality** - Programs tend to use data and instructions with addresses near or equal to those they have used recently.

- Temporal locality -- recently accessed data tends to be accessed again
- Spacial locality -- items with nearby addresses tend to be referenced close together in time

### Memory Hierarchy 

- For each level K in the hierarchy, serves as a cache for the K+1 level, so that we can access the K+1 level memory faster, but more limited.

### Cache Misses

- Cold Miss - when the machine first boots up, and none of the data is in the cache (yet)
- Capacity Miss - when there isn't enough space for all the data
- Conflict Miss - when two pieces of data map to the same location

