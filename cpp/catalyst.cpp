/*
Version 1.0.1
Run the CM/AM hybrid, catalyst
Now in cpp
March 21, 2018
SKG

Compilation command:

g++ catalyst.cpp initialize.cpp print.cpp random.cpp sir.cpp -o catalyst
 */

#include <iostream>
#include <cstdlib>
#include <sys/time.h>
#include "initialize.hpp"
#include "print.hpp"
#include "random.hpp"
#include "sir.hpp"
#include <boost/array.hpp>
#include <boost/numeric/odeint.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/adjacency_iterator.hpp>


using namespace std;
using namespace boost::numeric::odeint;

int main(){

  // RANDOM SEED - seed set by microsecond.  Could lead to issues down the road when setting jobs in parallel.  May help to discard first few runs.
  struct timeval t1;
  gettimeofday(&t1, NULL);
  srand(t1.tv_usec * t1.tv_sec);
  //

  // Read in controls
  // TODO

  // CONTROL SETTINGS
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
  double step_size = 1.0;
  double eps_abs = 1.e-6;
  double eps_rel = 1.e-6;
  int E = 3; // Number of environments
  int max_env = 5; // the max types of environment for each category
  int env[N][100];
  // magic number 100.  If there are more than 100^2 total ECs this will break
  int init_env_counts[10][100]  = {{2, 1, 1, 0, 6},
				   {0, 3, 2, 5, 0},
				   {10, 0, 0, 0, 0}};
  


  //  Initialize agents
  initialize_agents(N, T, K,
		    agent_status,
		    init_state_counts);

  std::cout << "agents \n";
  print_agents(N, T, agent_status);

  // Initialize environments
  initialize_envs(N, E, max_env,
		  env, init_env_counts);
  std::cout << "envs \n";
  print_envs(N, E, env);


  // Set up neighbor dictionary/graph
  
  Graph g(N);
  std::cout << "neighbor graph \n";
  
  g = initialize_nbr_graph(N, E, env);
  print_graph_nbrs(g, N);

  // Setting up the SIR
  // TODO: match up with above params
  state_type x = { 950.0 , 50.0 , 0.0 }; // initial conditions
  runge_kutta4< state_type > stepper;
  std::array<double,3> params;
  params[0] = .1;
  params[1] = .03;
  params[2] = 1000.0;
  sir sys(params);

    
  //integrate_const(stepper, sys , x , 0.0 , 100.0 , 1.0, write_sir);

  // Storing results in a container
  vector<state_type> cm_vals;
  vector<double> times;
  double S = 1.0*T;
  

  cm_vals = run_sir(sys, x,
		   stepper,
		    step_size,
		    S,
		    cm_vals,
		    times);

  print_cm_vals(T, K, cm_vals, step_size);


  // // Initialize base probabilities of transition
  double base_probs[T][100][100];
  initialize_base_probs(T, K, params,
			cm_vals,
			base_probs);

  print_base_probs(T, K, base_probs);

  // for(int t=0; t < (T-1); t++){
  //   if(do_am == 1){
  //     // Run AM portion (individualized portion)
  //     run_am_step();
  //   }
  //   // Update everything
  //   update_agents();
  //   update_agent_probs();
  //   update_envs();
        
  // }
  
  // // Write out stuff
  // write_agents();

  
  return 0;
}
