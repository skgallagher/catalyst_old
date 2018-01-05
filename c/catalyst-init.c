/*
Version 0.0.1 of catalyst: compartment and agent tracing and analysis through time
January 5, 2018
SKG
 */

#include <stdio.h>
#include <string.h>

int catalyst(char control_file)

int main(){

  char *control_file = "catalyst_control.txt";
  catalyst(control_file);

}


/* Run the CM-AM hybrid */
int catalyst(char control_file){

  inputs = read_inputs(control_file);

  // Loop over time steps
  for (int tt = 0; tt < T; tt++){
    // Loop over environments
    for (int ee = 0; ee < E; ee++){
      agent_status = infect_agents(input, agent_chars,
				   agents_status,
				   tt, ee);
    }
    // Update input and environments (e.g. add prevention strats)
    agent_status = update_agents(agent_chars, agents_status, tt);
    inputs = update_inputs(inputs);
    envs = update_envs(inputs, envs);
  }

  // Write out current agents specified
  write_agents(agent_status, inputs)

  return 1;


}
