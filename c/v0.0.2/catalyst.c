/*
Version 0.0.2
Run the CM/AM hybrid, catalyst
February 19, 2018
SKG

Compilation command:


gcc `pkg-config --cflags --libs glib-2.0`   catalyst.c random-draws-functions.c helper-functions.c initialize-functions.c sir-functions.c minimize-functions.c read-write-functions.c -o catalyst -lgsl -lgslcblas -lm
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

int main(){

  int N = 1000; // number of agents
  int T = 100; // number of time steps
  int K = 3; // number of states
  int P = K;
  int D = 2; // dimension of params to be minimized
  int agent_status[T][N];
  int init_state_counts[] = {950, 50, 0}; // S=950, I = 50, R =0
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
  int init_env_counts[100][100]  = {{0, 1000, 0, 0, 0},
  				    {0, 1000, 0, 0, 0},
  				    {0, 1000, 0, 0, 0}};
  
  
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
  write_base_probs(T, K, base_probs, base_probs_fn);

}
