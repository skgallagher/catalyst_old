/*
catalyst v0.0.2
Read/write functions
February 20, 2018
SKG

HEADER
 */


void write_base_probs(int T, int K,
		      double base_probs[T][K][K],
		      char fn[]);

void write_agents(char fn[], int N, int T,
		  int agent_status[][N]);
