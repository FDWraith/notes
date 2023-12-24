## Lecture 8: Adversarial Con't

#### Monte Carlo Tree Search

- like minimax approach to model plys of the game
- but uses random playouts to acquire expected outcomes model

#### Dealing with Uncertainty: Prob and Expected Value

- $\mathbb{E}(x) = \sum_{x\in S}Pr(x)\times Val(x)$

#### Expectimax

- maximize expected value

### Midterm Topics

- Search
  - Uninformed
    - BFS, DFS, UCS
    - Tree vs Graph search
  - informed (herustics)
    - Greedy, A*
    - Admissible and Consistent Heuristics
  - Local and Constraint Satisfaction
    - backtracking
  - minimax/alpha-beta pruning
    - expectimax, expectiminimax
    - generalize techniques to multiple node types

#### Monte Carlo Tree Search

- random simulated game play to estimate value of a state

  - run multiple simulations / playouts to determine expected value of a state.

- bias over most promising moves

- $$
  \frac{w_i}{n_i} + C \times \sqrt{\frac{ln N_i}{n_i}}
  $$

- C balances exploit versus explore

- Playouts

  - random (light playouts)
  - heuristics (heavy playout)
    - known good moves or learned techniques

- picks the move with the highest number of playouts

- anytime - runs as longs as time constraint allows, but performs better with more playouts.

