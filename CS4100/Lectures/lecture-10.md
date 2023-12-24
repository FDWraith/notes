## Lecture 10: Two Key Problem in RL

- Control: find $\pi^*$
- Evaluation: eval $\pi$ 

#### Policy Evaluation

- Compute $V^{\pi}$ for any $\pi$
- $V^{\pi}(s) = \sum_{s'} P(s'|s,\pi(s))[R(s,\pi(s),s') + \gamma V^{\pi}(s')]$

#### Policy Extraction

- Given $V^{\pi}(s)$, how should we act?
- $\pi^*(s) = \arg\max_a \sum_{s'} P(s'|s,a)[R(s,a,s') + \gamma V^*(s')]$ 

#### Policy Iteration (PI)

- Solve for $V^*(s)$
  - Step 1: Policy Evaluation (with some random policy) 
    - $V^{\pi_i}(s) = \sum_{s'} P(s'|s,\pi_i(s))[R + \gamma V^{\pi_i}(s')]$
  - Step 2: Policy Improvement (Update policy with one-step look ahead)
    - $\pi_{i+1}(s) = \arg\max_a \sum_{s'} P(s'|s,a)[R + \gamma V^{\pi_i}(s')]$ 
  - Iterate until policy converges
  - Will eventually converge - number of policies is O($|A|^{|S|}$)

#### Reinforcement Learning

- Know S and A
- Don't know T and R
- Can interact with world to get trajectories ($\tau$ = s,a,r,s',a',r',s'',...)