## Lecture 9: Sequential Decision under Uncertainty

- Game with change
- Incomplete knowledge
  - model of the world / environment (abstraction of the world)
- Example: Robot exploration of Terrain
  - state  = grid of tiles with danger or goals
  - actions = {left, right, up, down, exit}
    - uncertain -> don't know the next state
  - reward (~"cost" in search)
    - goal = + 1, death = -1, living reward = -0.01
    - Goal: maximize expected sum of rewards (eg. reach the goal as quickly as possible)

#### Markov Decision Process (MDP)

- State (s): A set of states 
  - eg. S = {(1,1),(1,2), ..., (3, 4)}
- Action (A): A set of action
  - A = {U, L, R, D}
- Transition Function (T):
  - T(s, a, s') = P(s' | a, s)
  - Probability that a from s leads to s'
  - action dynamic
- Reward (R):
  - R(s, a, s') $\in \mathbb{R}$
- Markov Property
  - future states only dependent on the current state
- Discount Factor ($\gamma \in [0, 1]$)
  - Decay on the reward the longer the solution takes. 
- Policy ($\pi: S \rightarrow A$)
  - mapping from state to action for every state (ignoring history)
  - there is an optimal policy $\pi^*$
- Trajectory ($\tau$)
  - seqence of s, a, r, s', a', r', ..., terminal

#### Bellman Equation

$$
\begin{align}
V^*(s) &= \max_a Q^*(s,a) \\
       &= \max_a \sum_{s'} P(s' | a, s)(R(s', a, s) + \gamma V^*(s')) \\
Q^*(s,a) &= \sum_{s'} P(s' | a, s)(R(s', a, s) + \gamma V^*(s'))
\end{align}
$$

#### Value Iteration (VI)

- Turn Bellman Equation into update
- $V^*_k(s)$ for optimal value after $k$ time steps
- Algorithm
  - Intialize $V^*_0(s) = 0$
  - Then iterate (for all states)
    - $V^*_{k+1}(s) = \max_a \sum_{s'} P(s'|a,s)(R(s,a,s') + \gamma V^*_k(s)) \forall s$
  - Repeat until value converges or time runs out.
- Runtime of each iteration is $O(|S|^2|A|)$

