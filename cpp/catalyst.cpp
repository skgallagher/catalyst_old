/*
Version 1.0.1
Run the CM/AM hybrid, catalyst
Now in cpp
March 21, 2018
SKG

Compilation command:


gcc `pkg-config --cflags --libs glib-2.0`   catalyst.c random-draws-functions.c helper-functions.c initialize-functions.c sir-functions.c minimize-functions.c read-write-functions.c update-functions.c -o catalyst -lgsl -lgslcblas -lm
 */

#include <iostream>
using namespace std;

int main(){

  // Read in controls
  // TODO
  read_control_file();

  //  Initialize agents
  initialize_agents(T, N, K,
		    agent_status,
		    init_state_counts);

  // Initialize environments
  initialize_envs(E, N, max_env,
		  env, init_env_counts);

  // Set up neighbor dictionary
  initialize_nbr_dict();

  // Initialize base probabilities of transition
  initialize_base_probs();

  for(int t=0; t < (T-1); t++){
    if(do_am == 1){
      // Run AM portion (individualized portion)
      run_am_step();
    }
    // Update everything
    update_agents();
    update_agent_probs();
    update_envs();
        
  }
  
  // Write out stuff
  write_agents();

  
  return 0;
}
