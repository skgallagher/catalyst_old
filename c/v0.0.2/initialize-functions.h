/*
Version 0.0.2
Initialize agents, neighbors, diseae, and environment
February 19, 2018
SKG
HEADER
 */



void initialize_agents(int T, int N, int K,
		       int agent_status[][N],
		       int init_state_counts[]);

void initialize_envs(int E, int N, int max_env,
		     int env[][E],
		     int init_env_counts[][100]);

void make_nbr_dict(int N, int E, int env[][E], GHashTable *dict);

void initialize_base_probs(int T, int N, int K,
			   int P, int D,
			   double p[], double step_size,
			   double eps_abs, double eps_rel,
			   int agent_status[][N],
			   char base_probs_fn[],
			   double base_probs[][K][K],
			   );

void extract_sir_probs(int T, double N, int K, double sir_vals[][P+1],
		       double p[],
		       double base_probs[][K][K]);
