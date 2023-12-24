## Lecture 11: Reinforcement Learning

#### Model-based Learning

- learn the model (T and R) from the samples
- then apply value-iteration/policy-iteration
- How to learn:
  - calculate expectation/prob from samples
  - $T(s,a,s') = P(s'|s,a) = \frac{\text{\# of times for s,a,s'}}{\text{\# of times for s,a}}$
  - $R(s,a,s') = \frac{\sum_i r_i(s,a,s')}{\# r(s,a,s')}$

#### Model-Free Learning

- Try to learn other quantities (V and Q) to find $\pi^*$
- Evaluation: evaluate $\pi$

#### Direct Evaluation (Monte Carlo)

- $V^{\pi}(s)$ is the expected sum of reward from s and following $\pi$
  - calculate from samples
- Idea: Average all observed value from samples:
  - Act according to $\pi$
  - For each episode, calculate $V(s)$ for all visited s
    - $V_{\tau}(s) = r_1 + \gamma r_2 + \gamma^2 r_3 + ... + \gamma^{\tau} r_{\tau + 1}$
  - Calculate the average from all episodes
    - $\hat{V}(s) = \frac{\sum_{\forall \tau} V_{\tau}(s)}{\#\tau}$

#### Temporal Difference (TD)

- Key Idea: Learn from every sample sequentially

  -  rolling average for $V(s) \forall s$

  $$
  \begin{align}
  \bar{x}_{n+1} &= \frac{x_1 + x_2 + ... + x_n + x{n + 1}}{n+1} \\
                &= \frac{n}{n} \times \frac{x_1 + ... + x_n}{n+1} + \frac{x_{n+1}}{n+1} \\
                &= \frac{n \times \bar{x}_n}{n+1} + \frac{x_{n+1}}{n+1} \\
                 &= \frac{n}{n+1}\bar{x}_n + \frac{1}{n+1}x_{n+1} \\
                &= (1 - \alpha)\bar{x}_n + \alpha x_{n+1}
  \end{align}
  $$

- Set $\alpha <1$ to make recent sample more important (early estimates are inaccurate)

#### Q-Learning

- Q-VI: $Q_{k+1}(s,a) = \sum_{s'} P(s'|s,a) [R(s,a,s') + \gamma \max_{a'} Q(s',a')]$
- Learn Q(s,a) from samples same as TD
  - receive sample (s,a, r, s')
  - sample = $R(s,a,s') + \gamma max_{a'} Q(s',a')$
  - Update $Q(s,a) = Q(s,a) + \alpha(\text{sample} - Q(s,a))$

#### $\varepsilon$-greedy exploration

- randomly explore from time to time
- every time step
  - explore with probability $\varepsilon < 1$.
  - otherwise, exploit (optimal action) with probability $1 - \varepsilon$
- lower $\varepsilon$ over time to converge to optimal policy

#### Approximate Q-Learning

- Problem: too many states, and some states are similar
- describe a state using a vector of features
- use a linear value function to reduce vector to value (with weights)