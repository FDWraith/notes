## Lecture 6: Constraint Satisfaction Problems (CSP)

#### CSPs

- special subset of search problems
- state is defined by variables with domain D
- goal is specific set of constraints that need to be satisfied.
- Examples: N-Color or N-Queens

#### Constraint Graphs

- binary CSP: each constraint relates at most two variables
- binary constraint graph; nodes are variables, arcs show constraints
- use graph structure to search. 

#### CSP Variables

- can be discrete (boolean, whole numbers, integers, etc.)
- can be continuous (rational)

#### Filtering

- prunes search by checking impact of an assignment on possible future assignments

#### Exploiting Structure

- find connected components
  - each component has subproblem that can be solved separately

#### Constraint Graph (When Graph is Tree)

- ```mermaid
  graph
  
  A --- B
  C --- B
  B --- D
  D --- E
  D --- F
  ```

- Find an order of the nodes so that each variable appears after its parent in the tree (tropological sort)

- ```mermaid
  stateDiagram
  
  A --> B
  B --> C
  B --> D
  D --> E
  D --> F
  ```

- Make this graph directed arc consistent (DAC)

  - start from tail and work backwards, removing inconsistent values

- Select values (going down the order) for each node

  - since each is arc consistent, no backtracking needed

#### Reduce Constraint Graph to Tree

- Cutset Conditioning
  - choose subset S of CSP's variables, such that constraint graph becomes a tree
- Solution is to choose all possible values of S, and try to solve remaining CSP. 
  - If there is a solution, solution is CSP + values of S
  - If no solution, pick new values of S, and try again

#### Tree Decomposition

- Subproblems for each variable
- Link subproblems as a tree and solve the tree
