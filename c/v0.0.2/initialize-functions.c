/*
Version 0.0.2
Initialize agents, neighbors, diseae, and environment
February 19, 2018
SKG
 */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <glib.h>
#include "random-draws-functions.h"
#include "helper-functions.h"
#include "initialize-functions.h"
#include "sir-functions.h"


void initialize_agents(int T, int N, int K,
		       int agent_status[][N],
		       int init_state_counts[]){
  // Initialize agents at time 0
  // so the first m_0 are in state 0, the next
  // m_1 are in state 1, ..., and the final m_K are in state K
  // The rest of the time steps are set to zero
  // WILL throw an error if there are less than 2 states
  int lower_bd = 0;
  int upper_bd = init_state_counts[0];
  for (int kk=0; kk < K; kk++){
    for (int ii=lower_bd; ii < upper_bd; ii++){
      agent_status[0][ii] = kk;
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
  @arg E number of environment types (e.g. schools + workplaces -> E =2)
  @arg N number of agents
  @arg max_env is the max number of different  categories
  @arg init_env_counts is a 2D E x max_env  array where entry j, i means that environment j has init_env_counts[i,j] agents with assignment i.
  @return a modified env variable, a N x E array.
 */
void initialize_envs(int E, int N, int max_env,
		     int env[][E],
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
Initialize dictionary of neighbors in dict.  Here dict is GHashtable with integer keys corresponding to the agent index of reference and corresponding values that are a integer GArray of indices of neighbors.  N is the number of agents
 */ 
void make_nbr_dict(int N, int E, int env[][E], GHashTable *dict){
   
  for(int ii = 0; ii < N; ii++){

    for(int jj=0; jj < N; jj++){

      for(int ee=0; ee < E; ee++){
	int current_env = env[ii][ee];
	// NULL assignments (env =0) are NOT neighbors)
	// NEIGHBORS should not include self
	if( current_env > 0 & ii !=jj & 
	    env[jj][ee] == current_env){
	  gint* my_ind = g_new(gint, 1);
	  *my_ind = ii;
	  gint* my_nbr = g_new(gint, 1);
	  *my_nbr = jj;
	  g_hash_table_insert(dict, my_ind,
	  		      g_slist_append(g_hash_table_lookup(dict, my_ind),
	  				     my_nbr));
	  break;
	}
      }
    }
  }
}


/*
INPUTS:
base_probs  - 3d array of dimension TxKxK. entry ijk is the probability of an agent in compartment j moving to compartment k from time i to i+1.
 */
void initialize_base_probs(int T, int N, int K,
			   int P, int D,
			   int init_state_counts[],
			   double p[], double step_size,
			   double eps_abs, double eps_rel,
			   int agent_status[][N],
			   char base_probs_fn[],
			   double base_probs[][K][K]
			   ){

  int S = (int)(T / step_size) + 1;
  double f_mat[S][P+1]; // SIR values at from 0 to S * step_size
  double f_est[T+1][P+1]; // SIR values at time 0, 1, ..., T
  double init_vals[K];
  for(int ii=0; ii < K; ii++){
    init_vals[ii] = 1.0 * init_state_counts[ii];
  }

  // Run the ODE
  ode_vals(T, P, D, step_size,
		1.0 * N, init_vals,
		eps_abs,  eps_rel,
		p, f_mat);

  // print_float_2d(S, P+1, f_mat);


  // Fill in time values in f_est
  for(int ii=0; ii < (T+1); ii++){
    f_est[ii][0] = 1.0 * ii;
  }

  // Subset ODE to whole time points
  int matching_inds[T+1];
  find_matching_inds(0, 0,
		     T+1, S,
		     P+1, P+1,
		     f_est, f_mat,
		     1, 1.e-6,
		     matching_inds);
  /* printf("matching inds are\n"); */
  /* print_int_1d(S, matching_inds); */
  subset_array_d(T+1, S,
		 matching_inds, P+1,
		 f_mat, f_est);

  
  // Extract probabilities from ODE
  // Currently hardcoded for SIR
  // Prob of S-> I = beta * I / N
  // Prob of I -> R = gamma
  extract_sir_probs(T, 1.0 * N, K, P,
		    f_est,
		    p,
		    base_probs);
}




void extract_sir_probs(int T, double N, int K, int P,
		       double sir_vals[][P+1],
		       double p[],
		       double base_probs[][K][K]){
  for(int tt=0; tt < T; tt++){
    //printf("tt %d I %.2f N %.2f\n", tt, sir_vals[tt][2], N);
    double beta = p[0];
    double gamma = p[1];
    
    base_probs[tt][0][1] = beta *
      sir_vals[tt][2] / N; // S to I
    base_probs[tt][0][2] = 0.0; // S to R
    base_probs[tt][0][0] = 1.0 - base_probs[tt][0][1]; // S to S
    base_probs[tt][1][0] = 0.0; // I to S
    base_probs[tt][1][1] = 1.0 - gamma; // I to I
    base_probs[tt][1][2] = gamma; // I to R
    base_probs[tt][2][0] = 0.0; // R to S
    base_probs[tt][2][1] = 0.0; // R to I
    base_probs[tt][2][2] = 1.0; // R to R
    
      
  }
  
}

/*
Extract the neighbors of the agent into an integer array.  also count the number
INPUTS:
nbr_dict dictionary of neighbors where the key is a GInt of the current agent index and the value is a g_slist of neighbor indices
inf_ind -- integer index of current agent we are looking for neighbors of
nbr_inds -- integer array of indices of neighbors from hashtable
OUTPUT: modified nbr_inds and n_nbrs -- the total number of neighbors for this agent
 */
int extract_neighbors(GHashTable* nbr_dict, int inf_ind, int nbr_inds[]){
  gint n_nbrs;
  gpointer value;
  gint* my_ind = g_new(gint, 1);
  *my_ind = inf_ind;
  printf("Agent index is %d\n", inf_ind);
  value = g_hash_table_lookup(nbr_dict, GINT_TO_POINTER(my_ind));
  n_nbrs = g_slist_length(value);
  printf("The number of neighbors is %d\n", n_nbrs);
  for(int ii=0; ii < n_nbrs; ii++){
    nbr_inds[ii] = *(int*)g_slist_nth(value, ii)->data;
  }
  return n_nbrs;
}
