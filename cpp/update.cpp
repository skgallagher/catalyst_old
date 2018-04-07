/*
Update functions including
- update agents
- update environments

April 7, 2018
SKG
 */

#include "update.hpp"


/*
Get probability vector of transferring from current state to next state at this time step.  These probabilities are based on the CM
INPUTS:
T - total amount of time
N - number of agents
K - number of agent states
tt - current time step from 0 to T, non-inclusive
agent_status - N x (T+1) matrix of agent states.  
base_probs - T x K x K probability of transitions from state i to j at time tt to tt+1
OUTPUT:
Modified agent_probs - a N x K matrix of probabilities from transitioning from agent's current state to states 0, .., K-1
 */
void get_agent_probs_cm(int T, int N, int K,
		     int tt, int agent_status[][1000],
			double base_probs[][100][100],
			double agent_probs[][K]){
  int current_status;
  for(int ii = 0; ii < N; ii++){
    for(int kk=0; kk < K; kk++){
      current_status = agent_status[tt][ii];
      agent_probs[ii][kk] = base_probs[tt][current_status][kk];
    }
  }


}

/*
  (Potentially) Change states of agents states at the next time step (t+1)
  INPUTS:
  t -- current time step
  N -- total number of agents
  K -- totalnumber of compartments
  agent_probs -- NxK array of probabilities of updating for each agent conditioned on their current state
  agent_status -- Nx(T+1) array of agent's status at time t
  OUTPUTS: modified agent_status with new agent states at time t+1
*/
void update_agents(int t, int N, int K,
		   double agent_probs[][100],
		   int agent_status[][1000]){
  double probs[K];
  for(int ii=0; ii < N; ii++){
    for(int kk=0; kk < K; kk++){
      probs[kk] = agent_probs[ii][kk];
    }
    agent_status[t+1][ii] = draw_multinom(probs, K);
  }
}


