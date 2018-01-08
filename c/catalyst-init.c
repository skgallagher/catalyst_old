/*
Version 0.0.1 of catalyst:
 compartment and agent tracing and analysis through time
January 5, 2018
SKG
 */

#include <stdio.h>
#include <string.h>




int catalyst(char control_file);
int update_agents(float agent_probs,
		  int agent_status,
		  int tt);
int infect_agents(catal_input inputs,
		  float agent_probs,
		  float agent_chars,
		  int agent_status,
		  int tt,
		  int ee);
catal_input update_inputs(catal_input inputs);
catal_envs update_envs(catal_envs inputs);
int update_agent(float weights);
float normalize_probs(float weights);
int draw_multinom(probs);
float float_rand( float min, float max )
  
		      

int main(){


  srand(time(NULL)); // is this necessary to start the random draws?
  char *control_file = "catalyst_control.txt";
  int out = catalyst(control_file);

}


/* Run the CM-AM hybrid */
int catalyst(char control_file){



  inputs = read_inputs(control_file);

  // Loop over time steps
  for (int tt = 1; tt < T; tt++){
    // Loop over environments
    for (int ee = 0; ee < E; ee++){
      agent_status = infect_agents(inputs, agent_probs,
				   agent_chars,
				   agent_status,
				   tt, ee);
    }
    // Update input and environments (e.g. add prevention strats)
    agent_status[, tt] = update_agents(agent_probs,
				       agent_status, tt);
    inputs = update_inputs(inputs);
    envs = update_envs(inputs);
  }

  // Write out current agents specified
  write_agents(agent_status, inputs);

  return 1;


}

/* Draw from multinomials to get the new status 
Output: updated array of agent_status at time tt*/
int update_agents(float agent_probs,
		  int agent_status,
		  int tt){
  int N = size(agent_probs)[1];
  int new_agent_status[N];
  for (ii=0; ii < N; ii++){
    new_agent_status[ii] = update_agent(agent_probs[ii,agent_status[ii],]);
  }
  return new_agent_status;

}


/* Update agent according to a multinomial draw */
int update_agent(float weights){

  int N = size(weights);
  float probs[N];
  int new_agent_status;
  probs = normalize_probs(weights, N);
  new_agent_status = draw_multinom(probs);
  return new_agent_status;

}

/* turn a vector into probabilities that add to 1 */
float normalize_probs(float weights, N){
  float tot = 0;
  float probs[N];
  for (ii=0; ii < N; ii++){
    tot = tot + weights[ii];
  }
  for (ii=0; ii < N; ii++){
    probs = probs[ii] / tot;
  }
  return probs;
}

/* Return an integer corresponding to the drawn multinomial category */
int draw_multinom(probs){
  int multinom_draw;
  float cum_prob = 0;
  M =  size(probs); // number of categories
  draw = float_rand(0.0, 1.0);
  /* If the draw is in the 1st interval of cumulative probability, then 1 is the new status,
  If it is in the 2nd interval, then 2 is new status...,
  If it is in the Nth interval, N is the new status
  */
  for (mm=0; mm < M; mm++){
    cum_prob = cum_prob + probs[mm];
    if (draw < cum_prob){
      multinom_draw = mm;
    }
  }
  return multinom_draw;

}


float float_rand( float min, float max ){
    float scale = rand() / (float) RAND_MAX; /* [0, 1.0] */
    return min + scale * ( max - min );      /* [min, max] */
}
