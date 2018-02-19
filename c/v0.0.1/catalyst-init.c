v/*
Version 0.0.1 of catalyst:
 compartment and agent tracing and analysis through time
January 5, 2018
SKG
 */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

/*
Make an input struct, eventually to be read from control file
 */
struct catal_input{
  int N; // the number of agents
  int T; // the number of total time steps from 0,1, ...,T
  float disease_params[10]; // TODO: make generically large
  int K; // number of compartments in model
  char fn[50]; // file name to print agents
}; // note the semi-colon





int catalyst(char control_file[]);

/* int infect_agents(catal_input inputs, */
/* 		  float agent_probs[][][], */
/* 		  float agent_chars, */
/* 		  int agent_status[][], */
/* 		  int tt, */
/* 		  int ee); */
// catal_input update_inputs(catal_input inputs);
//catal_envs update_envs(catal_envs inputs);
int draw_multinom(float probs[], int K);
float float_rand( float min, float max );
struct catal_input read_inputs(char control_file[]);




/* 
Read the input file and return it as a struct
 */
struct catal_input read_inputs(char control_file[]){
  struct catal_input inputs;
  
  inputs.T = 10;
  inputs.N = 100;
  inputs.disease_params[1] =.1; // beta
  inputs.disease_params[2] = .03; // gamma 
  inputs.K = 3; // # number of compartments S, I, R
  strcpy(inputs.fn, "agents.csv");
  return inputs;

}




 
		      

int main(){


  srand(time(NULL)); // is this necessary to start the random draws?
  char control_file[] = "catalyst_control.txt";
  int out = catalyst(control_file);

}


/* Run the CM-AM hybrid */
int catalyst(char control_file[]){

  struct catal_input inputs = read_inputs(control_file);
  int N = inputs.N;
  int T = inputs.T;
  int K = inputs.K;

  /* initialize agents
     For this example, 950 are S, and 50 are I
     TODO: move to control file/input and made more generic
   */
  int S = 90;
  int I = 10;
  int agent_status[T][N];
  for(int tt=0; tt < T; tt++){
    for(int ii=0; ii < N; ii++){
      if (ii < S){
	agent_status[tt][ii] = 0;
      } else {
	agent_status[tt][ii] = 1;
      }
    }
  }

  /* Given agent_probs.  Currently, they are all the same for a given time t
     agents. Function to call initial probs
   */
  float agent_probs[T][K][K];
  for(int tt=0; tt < T; tt++){ // All S move to I and I move to R, R to R
    agent_probs[tt][0][0] = 0.5;
    agent_probs[tt][0][1] = 0.5;
    agent_probs[tt][0][2] = 0.0;
    agent_probs[tt][1][0] = 0.0;
    agent_probs[tt][1][1] = 0.5;
    agent_probs[tt][1][2] = 0.5;
    agent_probs[tt][2][0] = 0.0;
    agent_probs[tt][2][1] = 0.0;
    agent_probs[tt][2][2] = 1.0;
  }

  

  // Loop over time steps
  for(int tt = 1; tt < T; tt++){ // note initial step is at time 1 not 0
    /* // Loop over environments */
    /* for(int int ee = 0; ee < E; ee++){ */
    /*   agent_status = infect_agents(inputs, agent_probs, */
    /* 				   agent_chars, */
    /* 				   agent_status, */
    /* 				   tt, ee); */
    /* } */
    // Update input and environments (e.g. add prevention strats)
    // Update agent status
    for(int ii=0; ii < N; ii++){
      int status = agent_status[tt-1][ii];
      float probs[K];
      for(int kk=0; kk < K; kk++){
	probs[kk] = agent_probs[tt][status][kk];
      }
      
      agent_status[tt][ii] =  draw_multinom(probs, K);
    }
    /* inputs = update_inputs(inputs); */
    /* envs = update_envs(inputs); */
  }

  // Write out current agents specified
  // TODO: delete old inputs.fn if it exists
  printf("Writing out agents to %s\n", inputs.fn );

  FILE *f = fopen(inputs.fn, "w");
  for (int tt=0; tt < T; tt++){
    for (int ii=0; ii < N; ii++){
      fprintf(f, "%d,", agent_status[tt][ii]);
      fflush(stdout);
    }
    fprintf(f, "\n");
    fflush(stdout);
  }
  fclose(f);




  return 1;


}





/* Return an integer corresponding to the drawn multinomial
   category */
int draw_multinom(float probs[], int K){
  int multinom_draw;
  float cum_prob = 0.0;
  float draw = float_rand(0.0, 1.0);
  /* If the draw is in the 1st interval of cumulative probability, then 1 is the new status,
  If it is in the 2nd interval, then 2 is new status...,
  If it is in the Nth interval, N is the new status
  */
  for(int kk=0; kk < K; kk++){
    cum_prob = cum_prob + probs[kk];
    if (draw < cum_prob){
      multinom_draw = kk;
      break;
    }
  }
  return multinom_draw;

}


float float_rand( float min, float max ){
    float scale = rand() / (float) RAND_MAX; /* [0, 1.0] */
    return min + scale * ( max - min );      /* [min, max] */
}


