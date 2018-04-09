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
n_inf_states - total number of infectious tates
n_sus_states - total number of susceptible states
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
		 int n_inf_states,
		 int n_sus_states,
		 double agent_probs[][100]){
  /*
    Find infectious agents
    Loop over infectious agents
    - Extract neighbors
    - Have them (possibly) infect susceptible neighbors if not already infected
  */
  int infectious_inds[N];
  n_inf_inds = extract_infectious_inds(tt, N,
				       infectious_states,
				       agent_status,
				       infectious_inds);

  int neighbor_inds[N];
  bool is_susceptible;
  bool is_already_inf;
  int infected_state; // the agent who is being infected
  int infector_state; // the agent doing the infecting
  
  for(int ii=0; ii < n_inf; ii++){
    nbr_inds = neighbor_graph.something();
    n_nbrs = neighbor_graph.somethingelse();
    for(int jj=0; jj < n_nbrs; jj++){
      // This can be improved to not check susceptibility every time
      // But if neighbors are sparse, it's ok
      infected_stat
	is_susceptible = check_susceptible(tt, jj,
					   agent_status);
      is_already_inf = check_already_inf(jj, agent_probs);
      if(is_susceptible && !is_already_inf){
	infect_agent(tt, N, K,
		     ii, jj,
		     env,
		     infected_state,
		     infector_state,
		     infectious_states,
		     susceptible_states,
		     base_probs,
		     agent_probs);
      }					 
    } 
  }
}

/*
Return the infected indices at the current time step
INPUTS:
tt - current time step
N - total number of agents
infectious_states - indices of different infectious states
n_inf_states - total number of ifnectious states
agent_status - N x(T+1) matrix where entry nt is agent n's status at time t
infectious_inds - an initially empty vector of the infectious indicies at this time step
OUTPUTS:
the total number of infectious and updated vector of infectious_inds
 */
int extract_infectious_inds(int tt, int N,
			    int infectious_states[],
			    int n_inf_states,
			    int agent_status[][1000],
			    int infectious_inds[]){
  int n_inf_inds=0;
  int inf_state;
  int current_state;
  for(int ii=0; ii < N; ii++){
    current_state = agent_status[ii][tt];
    for(int jj=0; jj < n_inf_states; jj++){
      inf_state = infectious_states[jj];
      if(inf_state == current_state){
	infectious_inds[n_inf_inds] = ii;
	n_inf_inds++;
	break;
      }
    }
  }
  return n_inf_inds;

}
