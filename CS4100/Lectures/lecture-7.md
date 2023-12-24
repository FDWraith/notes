## Lecture 7: Adversarial Search

#### Competing with Another Agent

- reason with explicit model of the other

#### Zero-Sum Games

- Want to find optimal action, assuming opponent is optimal too

#### Game Trees

- Defined by
  - $S_0$ initial state
  - Actions(s)
  - Results(s,a)

#### Minimax

- Max wants optimal value
- Min wants least optimal value
- Construct tree from bottom up.
- Time complexity: O($b^d$)
- Space complexity: O(bd)

#### Alpha-beta Pruning

- prunes minimax search
- does not expand certain nodes that it already knows min / max won't take.
- Maintains two bounds which restrict possible solution
  - $\alpha$ is the best choice found so far for MAX (lower bound)
  - $\beta$ is the best choice found so far for MIN (upper bound)
  - $\alpha \leq n \leq \beta$

#### Extend to multi-agent games

- use multi-value vectors ($v_1, \dots, v_n$) instead of single value.
- values determine competition vs cooperative

#### Limited Time?

- cutoff search + use heuristic to estimate values
- At what depth to cutoff
  - option 1: cut off search at a fixed depth 
  - option 2: cutoff search at quiescent states deeper than certain threshold



