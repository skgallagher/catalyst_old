/*
Initialize functions including
- initialize_agents
- initialize_envs
- initialize_nbr_dict
- initialize_base_probs

March 21, 2018
SKG

Compilation command:
 */


#include "initialize.hpp"

/*
Set up an array of initial agent states
INPUTS:
N - total number of agents
T - total number of time steps from 0, 1, ..., T-1
K - total number of compartments
agent_status - T x N array where entry tn is agent n's status at time t
init_state_counts - 1d array of length K of the number of agents in each state at time 0.  That is entry k is the number of agents in state k at time 0.  Should add to N.
OUTPUTS:
agent_status array
 */
void initialize_agents(int N, int T, int K,
		  int agent_status[][1000],
		  int init_state_counts[]){
  // Initialize agents at time 0
  // so the first m_0 are in state 0, the next
  // m_1 are in state 1, ..., and the final m_K are in state K
  // The rest of the time steps are set to zero
  // WILL throw an error if there are fewer than 2 states
  int lower_bd = 0;
  int upper_bd = init_state_counts[0];
  for (int kk=0; kk < K; kk++){
    for (int ii=lower_bd; ii < upper_bd; ii++){
      agent_status[ii][0] = kk;
    }
    if (kk < K-1){
      lower_bd = upper_bd;
      upper_bd = upper_bd + init_state_counts[kk+1];
    }
  }




}



/*
  Initialize environments for the activities.  This is a N x E array where entry
  n,e is the nth agent's e^th environment assignment.  All 0s constitute a NULL assignment.
  @arg N number of agents
  @arg E number of environment types (e.g. schools + workplaces -> E =2)
  @arg max_env is the max number of different  categories
  @arg init_env_counts is a 2D E x max_env  array where entry j, i means that environment j has init_env_counts[i,j] agents with assignment i.
  @return a modified env variable, a N x E array.
 */
void initialize_envs(int N, int E, int max_env,
		     int env[][100],
		     int init_env_counts[][100]){

  for(int ee=0; ee < E; ee++){
    int lower_bd = 0;
    int upper_bd = init_env_counts[ee][0];
    for (int kk=0; kk < max_env; kk++){
      //     printf("upper_bd %d\n", upper_bd);
      if( upper_bd > N){
	break;
      }
      for (int ii=lower_bd; ii < upper_bd; ii++){
	env[ii][ee] = kk;
      }
      lower_bd = upper_bd;
      upper_bd = upper_bd + init_env_counts[ee][kk+1];
    }

  }


}


/*
Return graph (adjacency list) of neigbhors
INPUTS:
N - number of total agents
E - number of total environments
env - N x E array where entry ne is agent n's value for env e.  0 is a NULL assignment
OUTPUTS:
Graph object from boost library where edges represent being neighbors
 */

Graph initialize_nbr_graph(int N, int E, int env[][100]){
  Graph g(N);
  // For every agent
  // - Find neighbor of agent
  // - Add them to graph
  for(int ii=0; ii < N; ii++){

    for(int jj=0; jj < N; jj++){ // Loop over possible neighbors
      for(int ee=0; ee < E; ee++){ // Loop over environments
	int current_env = env[ii][ee];
	// NULL assignments (env =0) are NOT neighbors)
	// NEIGHBORS should not include self
	if( current_env > 0 & ii !=jj &
	    env[jj][ee] == current_env){
	  // Add neighbors to graph and break since we don't like repeat neighbors
	  boost::add_edge(ii, jj, g);
	  break;
	}
      }
    }

  }
  return g;
}
