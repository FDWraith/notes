## Lecture 3: Uniformed Search

Model-based: Use a model of how the world evolves in response to actions

Problem Solving Agents: use atomic representation

#### Search Problems

Consists of: 

- State space
  - the set of possible states
- Transition function (with actions, possibly costs)
- Start state and a goal state
- A solution is a sequence of transitions which transforms the start state to a goal state

#### State Space Graph

Representation of search problem

- Nodes are world configurations
- arcs/edges represent transitions
- Goal is a node

#### Tree Search

- Expand potential plans
- Maintain a frontier of possible plans
- Continue until end state found or frontier empty
- **Difference from Graph Search** graph search does not revisit already explored nodes

#### Search Algo Properties

- Completeness?
- Optimal?
- Time complexity
- Space complexity

#### DFS Properties

- Time: O($$b^d$$)
- Space: O(bd)
- Completeness: Only if finite
- Optimal: No

#### BFS Properties

- Time: O($$b^s$$)
- Space: O($$b^s$$)
- Completeness: S must be finite if solution exists exists, so yes if branching is finite
- Optimal: Only if cost is 1

#### Uniform Cost search

- Expand frontier on cheapest path first
- Time and Space: O($$b^{C^*/\epsilon}$$)
- No information about goal location

#### Variations

- Limit Depth of Search
- Limit Breadth of search
- Cycle detection
- Sort Queue (by heuristics)



