## Lecture 5: Local Search

#### Local Search

- Start with a possible candidate solution as initial state
  - possibly randomly generated
- Make a local modification to improve current state
- Repeat until a goal state is found (or out of time)
- Types of local search
  - hill climbing search or greedy local search
  - simulated annealing 
  - local beam search
  - genetic algorithms
- Evaluation function should provide gradients
- Problems
  - May have to move away from goal to find (best) solution
  - Plateaus: all neighbors look same
  - Ridges: sequence of local maxima
  - May not know the value of global optimum
- Solutions:
  - Introduce randomness / noise to get out of local maxima
  - eg. Simulated Annealing allows bad moves to be chosen randomly with decreasing frequency
    - the longer the search, the more likely we have optimal solution

#### Local Beam Search

- Keep track of k states rather than just one
- At each iteration, all successors of k states are generated
- IF any is a goal state,
  - stop
- else select k best successors from complete list and repeat

#### Genetic Algorithm

- Keeps a set of states on hand
- Relies on operators that
  -  mimic natural selection
  - cross-over properties from different states + mutation
  - fitness function to evaluate and select parent

