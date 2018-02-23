/*
Version 0.0.2
Run the CM/AM hybrid, catalyst
February 19, 2018
SKG

Compilation command:


gcc `pkg-config --cflags --libs glib-2.0`   catalyst.c random-draws-functions.c helper-functions.c initialize-functions.c sir-functions.c minimize-functions.c read-write-functions.c update-functions.c -o catalyst -lgsl -lgslcblas -lm
 */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <glib.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multimin.h>
#include "random-draws-functions.h"
#include "helper-functions.h"
#include "initialize-functions.h"
#include "sir-functions.h"
#include "minimize-functions.h"
#include "read-write-functions.h"
#include "update-functions.h"

int main(){

  // Set seed for random number generation
  srand(time(NULL));

  int N = 100; // number of agents
  int T = 100; // number of time steps
  int K = 3; // number of states
  int P = K;
  int D = 2; // dimension of params to be minimized
  int agent_status[T][N];
  int init_state_counts[] = {95, 5, 0}; // S=950, I = 50, R =0
  int inf_state_cats[] = {1}; // 1 is the only infection state
  int sus_state_cats[] = {0}; //0 is the only susceptible state
  int init_inf_inds[N];
  int n_inf_inds = 0;
  int n_inf_states = 1;
  int n_sus_states = 1;
  double p[] = {.1, .03}; // {beta, gamma}
  double step_size = .001;
  double eps_abs = 1.e-6;
  double eps_rel = 1.e-6;



  initialize_agents(T, N, K,
  		    agent_status,
  		    init_state_counts);

  // environments
  int E = 3;
  int max_env = 5;
  int env[N][E];
  // magic number 100.  If there are more than 100^2 total ECs this will break
  // Everyone gets a neighbor
  int init_env_counts[100][100]  = {{0, 100, 0, 0, 0},
  				    {0, 100, 0, 0, 0},
  				    {0, 100, 0, 0, 0}};
  
  
  initialize_envs(E, N, max_env,
  		  env, init_env_counts);




  GHashTable* nbr_dict = g_hash_table_new(g_int_hash, g_int_equal);
  make_nbr_dict(N, E, env, nbr_dict);

  printf("There are %d keys in the hash\n", g_hash_table_size(nbr_dict));

  // Print neighbor info
  // g_hash_table_foreach(nbr_dict, print, NULL);

  // Initialize base probabilities

  double base_probs[T][K][K];
  char base_probs_fxn[] = "null";
  initialize_base_probs(T, N, K,
			P, D,
			init_state_counts,
			p, step_size,
			eps_abs, eps_rel,
  			agent_status,
  			base_probs_fxn,
  			base_probs);

  char base_probs_fn[] = "base_probs.csv";
  // write_base_probs(T, K, base_probs, base_probs_fn);

  /*
  // Run the simulation
  */
  
  double agent_probs[N][K]; // prob of transitioning from current state to state k
  get_agent_probs(T, N, K, 0,
		  agent_status,
		  base_probs,
		  agent_probs);

  //printf("initial agent probs of transition conditioned on current state\n");
  //  print_2d_array(N, K, agent_probs);


  // Find the initial infected agents

  n_inf_inds = find_infected_agents(N, 0, inf_state_cats,
		       n_inf_states, agent_status,
		       init_inf_inds);
  //  print_array(init_inf_inds, n_inf_inds);


  // Actually loop over agents and infect them

  double infection_probs[K][K][K];
  // probs of infection
  infection_probs[1][0][1] = 1.0; // chance of infected infecting
  // susceptible to being infected is 1

  int inf_ind;
  int sus_ind;
  int n_nbrs; // # of neighbors of current infected agent
  int nbr_inds[N]; // indices of neighbors initialization
  int env_inds[E];

  int do_am = 0; // if 1, we do the AM specific calculations
  // Else , use the base probabilities to udpate


  for(int t=0; t < (T-1) ; t++){
    if(do_am == 1){
      printf("In the AM portion\n");
      for(int ii=0; ii < n_inf_inds; ii++){
	// Loop over infectious
	// Loop over neigbhors
	// Check if neighbor is susceptible
	// Check if susceptible hasn't already been infected, e.g.
	// agent_probs isn't 1 in an infectious category
	// infect agent (e.g. update agent_probs)


	inf_ind = init_inf_inds[ii];
	//	printf("\nExtracting neighbors for agent %d\n", inf_ind);
	n_nbrs = extract_neighbors(nbr_dict, inf_ind, nbr_inds);
	//	printf("The neighbors are\n");
	//	print_array(nbr_inds, n_nbrs);
    
	for(int jj=0; jj < n_nbrs; jj++){
	  sus_ind = nbr_inds[jj];
	  printf("the neighbor index is %d\n", sus_ind);
	  // check if agent is currently in susceptible state
	  if (in_array(agent_status[t][sus_ind], 
		       sus_state_cats, n_sus_states) == 1){
	    printf("neighbor %d is susceptible\n", sus_ind);
	    // check if agent is not already infected by another agent
	    if(already_inf(sus_ind, K, n_inf_states,
			   agent_probs, inf_state_cats) != 1){
	      printf("neighbor %d is not already infected\n", sus_ind);
	    
	      infect_agent(inf_ind, sus_ind,
			   t, 
			   N, K,
			   agent_status,
			   E, env,
			   env_inds,
			   inf_state_cats,
			   sus_state_cats,
			   infection_probs,
			   agent_probs);
	    }
	  }
	}
      }
    }
    //  printf("step %d\n", t);
    //   print_float_2d(N, K, agent_probs); 
    
    // Do draws to see how agents update based on their probs
    /* printf("Updating agents!\n"); */
    update_agents(t, N, K,  agent_probs, agent_status);
    // Update the infectious indices
    n_inf_inds = find_infected_agents(N, t, inf_state_cats,
				      n_inf_states, agent_status,
				      init_inf_inds);
    // Update agent probs
    get_agent_probs(T, N, K, t+1,
		    agent_status,
		    base_probs,
		    agent_probs);


  }
  char fn[] = "agents-out-2.csv";
  write_agents(fn, N, T, agent_status);

}
