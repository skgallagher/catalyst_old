/*
SKG
February 22, 2018
Update functions for the AM part
*/


#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <glib.h>
#include "update-functions.h"
#include "helper-functions.h"
#include "random-draws-functions.h"



/* Find out if first argument is in the array of the second.  integer version
INPUTS: 
x -- some integer
a -- some integer array
N -- length of a
OUTPUT:
1 if x is in a and 0 otherwise */
int in_array(int x,
	     int a[], int N){
  for(int ii=0; ii < N; ii++){
    if(a[ii] == x){
      return 1;
    }
  }
  return 0;
}

/* Check to see if agent is already infected based on their agent_probs
An agent is infected if there is an entry of 1.0 in an infectious state
INPUTS: 
sus_ind -- index of susceptible agent in agent_probs
K -- number of compartments
n_inf_states -- total number of infectious states
agent_probs N x K array of transition probabilities
inf_state_cats -- array of category numbers of infectious states
OUTPUTS: 1 if agent is already infected and zero otherwise
 */
int already_inf(int sus_ind, int K, int n_inf_states,
		double agent_probs[][K], int inf_state_cats[]){
  for(int kk=0; kk < K; kk++){
    // If agent both has prob 1 of transitioning and
    // that 1.0 is to an infectious state
    
    //    print_array(inf_state_cats, n_inf_states);
    if((agent_probs[sus_ind][kk] == 1.0) &&
       (in_array(kk, inf_state_cats, n_inf_states) == 1)){
      //printf("category is %d\n", kk);
      //printf("prob of transitioning to %d is %.2f\n",
      //	     kk, agent_probs[sus_ind][kk]);
      //printf("agent %d is already infected\n", sus_ind);
      return 1;
    }
  }
  return 0;
}
    
/*
  (Potentially) Change states of agents states at the next time step (t+1)
  INPUTS:
  t -- current time step
  N -- total number of agents
  K -- totalnumber of compartments
  agent_probs -- NxK array of probabilities of updating for each agent conditioned on their current state
  agent_status -- TxN array of agent's status at time t
  OUTPUTS: modified agent_status with new agent states at time t+1
*/
void update_agents(int t, int N, int K,
		   double agent_probs[][K],
		   int agent_status[][N]){
  double probs[K];
  for(int ii=0; ii < N; ii++){
    for(int kk=0; kk < K; kk++){
      probs[kk] = agent_probs[ii][kk];
    }
    agent_status[t+1][ii] = draw_multinom(probs, K);
  }
}





/*
Loop over the agents at time t and find which of them are in infectious states
Inputs:
N - number of total agents
t - the current time step.
infection_state_cats array of indices corresponding to the infectious states.  Ex. for the SIR model. The only infectious state is "I" which is category 1.
agent_status T x N array of agent states where entry tn corresponds to agent n's state at time t.
inf_inds -- pointer to array of indices of infectious agents at current time step 

Output: modified inf_inds and n_inf_inds -- updated to show the current infectious agens
 */
int find_infected_agents(int N, int t, int infection_state_cats[],
			  int n_inf_states,
			  int agent_status[][N], int inf_inds[]){

  int n_inf_inds = 0;
  
  for(int ii=0; ii < N; ii++){
    for(int jj = 0; jj < n_inf_states; jj++){
      if( agent_status[t][ii] == infection_state_cats[jj]){
	inf_inds[n_inf_inds] = ii;
	n_inf_inds++;
      }
    }
  }
  return n_inf_inds;
  

}



/*
Initialize current probabilities of transition conditioned on agent's initial state
INPUTS:
T - total number of time steps
N - total number of agents
K - total number of compartments
t - current time step
agent_status - TxN array of agent status where entry A_{tn} \in {1, \dots, K} is the agent n's status at time t
base_probs - TxKxK array where entry A_{tij} is the probability of transitioning from compartment i to j at time t to t+1. 
agent_probs - NxK array where entry A_{nk} is agent n's probability of transitioning to state k conditioned on their current state.  This is also the pointer for the object that will be modified
OUTPUT: updated agent_probs initialized at time t=0
 */
void get_agent_probs(int T, int N, int K, int t,
			  int agent_status[][N],
			  double base_probs[][K][K],
			  double agent_probs[N][K]){
  int current_status;
  for(int ii = 0; ii < N; ii++){
    for(int kk=0; kk < K; kk++){
      current_status = agent_status[t][ii];
      agent_probs[ii][kk] = base_probs[t][current_status][kk];
    }
  }
}


/*
Chance to infect susceptible agent by infectious agent.  Updates agent_probs accordingly
INPUTS:
n - index of infectious agent
m - index of susceptible agent
t - current time step, t=0, ..., T
N - total number of agents
K - total number of compartments
agent_status TxN array where A_{tn} is agent n's current status at time t
E - total number of environments
env -  N x E array where entry
  n,e is the nth agent's e^th environment assignment.  All 0s constitute a NULL assignment.
env_inds - array of indices of environments agents share
inf_cats- indices of states which correspond to infectious states
sus_cats - indices of states which correspond to susceptible states
infection_probs - KxKxK where entry ijk is probability of agent with status i infecting agent with status j to status k.  Should be a sparse matrix. As only entries ijk > 0 if i is an infectious state, j is a susceptible state,
OUTPUTS:
agent_probs N x K array of transition probabilities
*/
void infect_agent(int n, int m,
		  int t, 
		  int N, int K,
		  int agent_status[][N],
		  int E, int env[][E],
		  int env_inds[],
		  int inf_cats[],
		  int sus_cats[],
		  double infection_probs[][K][K],
		  double agent_probs[][K]){

  // Extract the probability of agent n infecting agent m, conditioned on environments, etc
  // //printf("interacting infected agent %d and susceptible agent %d\n", n, m);
  double updated_probs[K];
  get_inf_probs(n, m,
		t, 
		N, K,
		agent_status,
		E, env,
		env_inds,
		inf_cats,
		sus_cats,
		agent_probs,
		infection_probs,
		updated_probs);
  ////printf("Agent %d's new probs of transitioning are\n", m);
  // Actually perform a multinomial draw to see whether agent was infected
  for(int kk=0; kk < K; kk++){
    //printf("%.2f ", updated_probs[kk]);
  }
  //printf("\n");
  int new_sus_state;
  new_sus_state = draw_multinom(updated_probs, K);
  //printf("agent %d's new state is going to be %d\n", m, new_sus_state);
  // Update agent's new probabilities of transitioning. We ensure an infection by setting that state's probability to 1
  for(int kk=0; kk < K; kk++){
    if( kk == new_sus_state){
      agent_probs[m][kk] = 1.0;
    } else {
      agent_probs[m][kk] = 0.0;
    }
  }
}



/*
Extract the relevant probabilities of an infectious agent in a certain state to a susceptible agent in another state of becoming infected. 
INPUTS:
n - index of infectious agent
m - index of susceptible agent
t - current time step, t=0, ..., T
N - total number of agents
K - total number of compartments
agent_status TxN array where A_{tn} is agent n's current status at time t
E - total number of environments
env -  N x E array where entry
  n,e is the nth agent's e^th environment assignment.  All 0s constitute a NULL assignment.
env_inds - array of indices of environments agents share
inf_cats- indices of states which correspond to infectious states
sus_cats - indices of states which correspond to susceptible states
agent_probs N x K array of transition probabilities
infection_probs - KxKxK where entry ijk is probability of agent with status i infecting agent with status j to status k.  Should be a sparse matrix. As only entries ijk > 0 if i is an infectious state, j is a susceptible state.
OUTPUTS: updated_probs a 1d array of length K which are the probabilities of an agent being infected 
TODO: need to put base_probs in this
*/
void get_inf_probs(int n, int m,
		   int t, 
		   int N, int K,
		   int agent_status[][N],
		   int E, int env[][E],
		   int env_inds[],
		   int inf_cats[],
		   int sus_cats[],
		   double agent_probs[][K],
		   double infection_probs[][K][K],
		   double updated_probs[]){
  int inf_status;
  int sus_status;
  for(int kk=0; kk < K; kk++){
    inf_status = agent_status[t][n];
    sus_status = agent_status[t][m];
    updated_probs[kk] = infection_probs[inf_status][sus_status][kk];
  }

  


}



