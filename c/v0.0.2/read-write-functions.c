/*
catalyst v0.0.2
Read/write functions
February 20, 2018
SKG

 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "read-write-functions.h"


/*
Write out the base probabilities transitions as a csv
INPUTS:
T -- last time step T.  time steps are 0, 1, 2, ..., T-1
K -- number of compartments
base_probs  - 3d array of dimension TxKxK. entry ijk is the probability of an agent in compartment j moving to compartment k from time i to i+1.
fn - string of where file is going to be written to
OUTPUTS:
a written csv. format is
time, current compartment i, prob from i to compt 1, ..., prob from i to compt K
 no return
 */
void write_base_probs(int T, int K,
		      double base_probs[T][K][K],
		      char fn[]){

   printf("Writing out base_probs to %s\n", fn);

   FILE *f = fopen(fn, "w");
   for (int tt=0; tt < T; tt++){
     for (int ii=0; ii < K; ii++){
       fprintf(f, "%d,%d,", tt, ii); 
       for(int jj=0; jj < K; jj++){
	 fprintf(f, "%.10f,", jj, base_probs[tt][ii][jj]);
	 fflush(stdout);
       }
       fprintf(f, "\n");
     }
    
     fflush(stdout);
   }
   fclose(f);

}


/*
Write out agents to a csv
Rows are time steps
Columns are agents
 */
void write_agents(char fn[], int N, int T,
		  int agent_status[][N]){
  printf("Writing out agents to %s\n", fn );

  FILE *f = fopen(fn, "w");
  for (int tt=0; tt < T; tt++){
    for (int ii=0; ii < N; ii++){
      fprintf(f, "%d,", agent_status[tt][ii]);
      fflush(stdout);
    }
    fprintf(f, "\n");
    fflush(stdout);
  }
  fclose(f);
}
