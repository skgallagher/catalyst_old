/*
The AM Portion of the program
- Find susceptibles and infectious
- Loop over infectious and infect neighbors by modifying probabilitites
- Include environmental interventions (TODO)

SKG
April 9, 2018
 */

/*
Run the individualized portion of the model
INPUTS:
tt - current timestep
N - number of agents
K - number of states
agent_status - N x (T+1) matrix of agent status at each time step
base_probs - T x K x K - entry tij is probability of transferring from current state i to state j from time t to t+1 based on the CM
env - N x E - entry ij is agent i's environment for env j.  0 is a null assignment
neighbor_graph - list of pre-computed potential neighbors
infectous_states - vector of categories pertaining to infectious states
susceptible_states - vector of categories pertaining to susceptible states
agent_probs - N x K matrix where entry ij is agent i's probability from its current state to state j
OUTPUTS:
modified agent_probs for use in update_agents
 */
void run_am_step(int tt, int N, int K,
		 int agent_status[][1000],
		 double base_probs[][100][100],
		 int env[][100],
		 Graph neighbor_graph,
		 int infectious_states[],
		 int susceptible_states[],
		 double agent_probs[][100]){
  /*
   Find infectious agents
   Loop over infectious agents
   - Extract neighbors
   - Have them (possibly) infect susceptible neighbors if not already infected
   */
  infectious_inds = extract_infectious_inds(tt, N,
					    infectious_states,

}
		 
