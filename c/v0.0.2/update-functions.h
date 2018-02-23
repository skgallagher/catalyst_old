/*
SKG
February 22, 2018
Update functions for the AM part
HEADER
*/


int in_array(int x,
	     int a[], int N);

int already_inf(int sus_ind, int K, int n_inf_states,
		double agent_probs[][K], int inf_state_cats[]);

void update_agents(int t, int N, int K,
		   double agent_probs[][K],
		   int agent_status[][N]);


int find_infected_agents(int N, int t, int infection_state_cats[],
			 int n_inf_states,
			 int agent_status[][N], int inf_inds[]);

void get_agent_probs(int T, int N, int K, int t,
		     int agent_status[][N],
		     double base_probs[][K][K],
		     double agent_probs[N][K]);

void get_agent_probs(int T, int N, int K, int t,
		     int agent_status[][N],
		     double base_probs[][K][K],
		     double agent_probs[N][K]);
void infect_agent(int n, int m,
		  int t, 
		  int N, int K,
		  int agent_status[][N],
		  int E, int env[][E],
		  int env_inds[],
		  int inf_cats[],
		  int sus_cats[],
		  double infection_probs[][K][K],
		  double agent_probs[][K]);

void get_inf_probs(int n, int m,
		   int t, 
		   int N, int K,
		   int agent_status[][N],
		   int E, int env[][E],
		   int env_inds[],
		   int inf_cats[],
		   int sus_cats[],
		   double agent_probs[][K],
		   double infection_probs[][K][K],
		   double updated_probs[]);
