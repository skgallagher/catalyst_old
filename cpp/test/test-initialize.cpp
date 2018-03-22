/*
Tests for initialize.cpp
SKG
March 22, 2018

Compilation commands:
g++ test-initialize.cpp ../print.cpp ../initialize.cpp -o test-inits
 */

#include <iostream>
#include <map>
#include "../initialize.hpp"
#include "../print.hpp"
using namespace std;


int main(){

  int N = 10; // number of agents
  int T = 10; // number of time steps
  int K = 3; // number of states
  int P = K;
  int D = 2; // dimension of params to be minimized
  int agent_status[N][1000]; // if we have more than 1000 time points we're in trouble
  int init_state_counts[] = {9, 1, 0}; // S=950, I = 50, R =0
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



  initialize_agents(N, T, K,
  		    agent_status,
  		    init_state_counts);

  print_agents(N, T, agent_status);

  // // environments
  // int E = 3;
  // int max_env = 5;
  // int env[N][E];
  // // magic number 100.  If there are more than 100^2 total ECs this will break
  // int init_env_counts[100][100]  = {{20, 10, 10, 0, 60},
  // 				    {0, 25, 25, 50, 0},
  // 				    {100, 0, 0, 0, 0}};
  
  
  // //initialize_envs(E, N, max_env,
  // //		  env, init_env_counts);




  // std::map<int,int*> nbr_dict;
  // // make_nbr_dict(N, E, env, nbr_dict);



  return 0;
}
