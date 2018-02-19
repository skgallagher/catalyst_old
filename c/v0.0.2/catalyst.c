/*
Version 0.0.2
Run the CM/AM hybrid, catalyst
February 19, 2018
SKG

Compilation command:


gcc `pkg-config --cflags --libs glib-2.0`   catalyst.c random-draws-functions.c helper-functions.c initialize-functions.c sir-functions.c minimize-functions.c -o catalyst -lgsl -lgslcblas -lm
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

int main(){

  int N = 100; // number of agents
  int T = 10; // number of time steps
  int K = 3; // number of states
  int agent_status[T][N];
  int init_state_counts[] = {950, 50, 0}; // S=950, I = 50, R =0
  int inf_state_cats[] = {1}; // 1 is the only infection state
  int sus_state_cats[] = {0}; //0 is the only susceptible state
  int init_inf_inds[N];
  int n_inf_inds = 0;
  int n_inf_states = 1;
  int n_sus_states = 1;

  printf("hello world\n");

  /* initialize_agents(T, N, K, */
  /* 		    agent_status, */
  /* 		    init_state_counts); */
  /* for (int nn=0; nn < N; nn++){ */
  /*   printf("agent %d: status: %d\n", */
  /* 	   nn, agent_status[0][nn]); */
  /* } */

  /* // environments */
  /* int E = 3; */
  /* int max_env = 5; */
  /* int env[N][E]; */
  /* // magic number 100.  If there are more than 100^2 total ECs this will break */
  /* int init_env_counts[100][100]  = {{500, 500, 0, 0, 0}, */
  /* 				    {200, 300, 500, 0, 0}, */
  /* 				    {100, 0, 600, 200, 100}}; */
  
  
  /* initialize_envs(E, N, max_env, */
  /* 		  env, init_env_counts); */




  /* GHashTable* nbr_dict = g_hash_table_new(g_int_hash, g_int_equal); */
  /* make_nbr_dict(N, E, env, nbr_dict); */

  /* printf("There are %d keys in the hash\n", g_hash_table_size(nbr_dict)); */

  /* g_hash_table_foreach(nbr_dict, print, NULL); */

  /* // Initialize base probabilities */
  /* double base_probs[T][K][K]; */
  /* char base_probs_fn[] = "null"; */
  /* initialize_base_probs(T, N, K,
     init_state_counts,
  /* 			agent_status, */
  /* 			base_probs_fn, */
  /* 			base_probs); */

}
