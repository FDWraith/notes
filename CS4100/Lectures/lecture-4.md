## Lecture 4: Informed / Heuristic Search

Heuristic

- Function that estimates how close a state is to a goal
- designed for a particular search problem
  - eg. it carries domain knowledge

#### Greedy Search

- When search expands a state, calculate heuristic h(s) of the distance to goal for each state
- Choose the state with lowest heuristic value for expanding frontier
- Cons: doesn't always choose the option with the best cost
- Not Complete: Can get itself into cycles

#### A* Search

- uses $$f(n) = g(n) + h(n)$$ as function to guide search expansion
- f(n) estimated cost of best path
- g(n) cost from start to node n
- h(n) heuristic estimate from node n to goal
- For optimal, need h estimates to be less than actual cost

Optimal?

- A using tree search is optimal if h is **admissible**
  - h is admissible if h(n) is always less than or equal to actual cost to go from state n to goal
- A using graph search if h is **consistent**
  - h(n) does not overestimate cost of each arc (not just from state n to goal)
  - consistent heuristics are also admissible

Importance of Heuristics

- If $$h(n) = 0$$, then A* turns into UCS
- The lower $$h(n)$$ is with respect to the real cost, the more A* expands
- If $$h(n)$$ is exactly equal to the cost of moving from n to the goal, then A* will only follow best possible path

Weighted A*

- $$f(n) = g(n) + W \times h(n)$$ where 1 < W < $$\infin$$

With A*

- trade-off between quality of estimate and work per node



