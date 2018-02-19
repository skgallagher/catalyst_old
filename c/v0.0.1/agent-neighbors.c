/*
Version 0.0.1 of catalyst:
Functions of agents and their neighbors
January 17, 2018
SKG
 */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <glib.h>


/*
Prototypes
 */
void initialize_agents(int T, int N, int K,
		       int agent_status[][N],
		       int init_state_counts[]);
void initialize_envs(int E, int N, int max_env,
		     int env[][E],
		     int init_env_counts[][100]);
void make_nbr_dict(int N, int E, int env[][E], GHashTable *dict);
int find_infected_agents(int N, int t, int infection_state_cats[],
			  int n_inf_states,
			 int agent_status[][N], int init_inf_inds[]);
int extract_neighbors(GHashTable* nbr_dict, int inf_ind, int nbr_inds[]);

void initialize_base_probs(int T, int N, int K,
			   int agent_status[][N],
			   char base_probs_fn[],
			   double base_probs[][K][K]);
void get_agent_probs(int T, int N, int K, int t,
			   int agent_status[][N],
			   double base_probs[][K][K],
			   double agent_probs[][K]);
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
		   double infection_probs[][K][K],
		   double updated_probs[]);

int draw_multinom(double probs[], int K);
double double_rand(double min, double max );

int in_array(int x, int a[],
	      int N);
int already_inf(int sus_ind, int K, int n_inf_states,
		double agent_probs[][K], int inf_state_cats[]);
void update_agents(int t, int N, int K,
		   double agent_probs[][K], int agent_status[][N]);

void write_agents(char fn[], int N, int T, int agent_status[][N]);

// Printing helpers
void prt(GArray* a);
void print(gpointer key, gpointer value, gpointer data);
void print_array(int a[], int N);
void print_2d_array(int I, int J, double a[][J]);

///////////////////////////////////////////////////
int main(){
  int N = 10; // number of agents
  int T = 10; // number of time steps
  int K = 3; // number of states
  int agent_status[T][N];
  int init_state_counts[] = {9, 1, 0}; // S=9, I = 1, R =0
  int inf_state_cats[] = {1}; // 1 is the only infection state
  int sus_state_cats[] = {0}; //0 is the only susceptible state
  int init_inf_inds[N];
  int n_inf_inds = 0;
  int n_inf_states = 1;
  int n_sus_states = 1;

  

  initialize_agents(T, N, K,
		    agent_status,
		    init_state_counts);
  for (int nn=0; nn < N; nn++){
    printf("agent %d: status: %d\n",
	   nn, agent_status[0][nn]);
  }

  // environments
  int E = 3;
  int max_env = 5;
  int env[N][E];
  // magic number 100.  If there are more than 100^2 total ECs this will break
  int init_env_counts[100][100]  = {{5, 5, 0, 0, 0},
				    {2, 3, 5, 0, 0},
				    {1, 0, 6, 2, 1}};
  
  for (int ii=0; ii < E; ii++){
    for (int jj=0; jj < max_env; jj++){
      printf("%d",
	     init_env_counts[ii][jj]);
    }
    printf("\n");
  }

  
  initialize_envs(E, N, max_env,
		  env, init_env_counts);


  for (int nn=0; nn < N; nn++){
    printf("%d %d %d\n",
	   env[nn][0], env[nn][1],
	   env[nn][2]);
  }

  GHashTable* nbr_dict = g_hash_table_new(g_int_hash, g_int_equal);
  make_nbr_dict(N, E, env, nbr_dict);

  printf("There are %d keys in the hash\n", g_hash_table_size(nbr_dict));

  g_hash_table_foreach(nbr_dict, print, NULL);

  // Initialize base probabilities
  double base_probs[T][K][K];
  char base_probs_fn[] = "null";
  initialize_base_probs(T, N, K,
			   agent_status,
			   base_probs_fn,
			   base_probs);

  double agent_probs[N][K]; // prob of transitioning from current state to state k
  get_agent_probs(T, N, K, 0,
		       agent_status,
		       base_probs,
		       agent_probs);

  printf("initial agent probs of transition conditioned on current state\n");
  print_2d_array(N, K, agent_probs);


  // Find the initial infected agents

  n_inf_inds = find_infected_agents(N, 0, inf_state_cats,
		       n_inf_states, agent_status,
		       init_inf_inds);
  print_array(init_inf_inds, n_inf_inds);


  // Actually loop over agents and infect them

  double infection_probs[K][K][K];
  // probs of infection
  infection_probs[1][0][1] = 1.0; // chance of infected infecting
  // susceptible to being infected is 1

  int inf_ind;
  int sus_ind;
  int n_nbrs; // # of neighbors of current infected agent
  int nbr_inds[N]; // indices of neighbors initialization
  int env_inds[E];

  for(int t=0; t < T ; t++){
    for(int ii=0; ii < n_inf_inds; ii++){
      // Loop over infectious
      // Loop over neigbhors
      // Check if neighbor is susceptible
      // Check if susceptible hasn't already been infected, e.g.
      // agent_probs isn't 1 in an infectious category
      // infect agent (e.g. update agent_probs)


      inf_ind = init_inf_inds[ii];
      printf("\nExtracting neighbors for agent %d\n", inf_ind);
      n_nbrs = extract_neighbors(nbr_dict, inf_ind, nbr_inds);
      printf("The neighbors are\n");
      print_array(nbr_inds, n_nbrs);
    
      for(int jj=0; jj < n_nbrs; jj++){
	sus_ind = nbr_inds[jj];
	printf("the neighbor index is %d\n", sus_ind);
	// check if agent is currently in susceptible state
	if (in_array(agent_status[t][sus_ind], 
		     sus_state_cats, n_sus_states) == 1){
	  printf("neighbor %d is susceptible\n", sus_ind);
	  // check if agent is not already infected by another agent
	  if(already_inf(sus_ind, K, n_inf_states,
			 agent_probs, inf_state_cats) != 1){
	    printf("neighbor %d is not already infected\n", sus_ind);
	    
	    infect_agent(inf_ind, sus_ind,
			 t, 
			 N, K,
			 agent_status,
			 E, env,
			 env_inds,
			 inf_state_cats,
			 sus_state_cats,
			 infection_probs,
			 agent_probs);
	      }
	}
      }
    }
    printf("after interacting at time step %d, agent probs of transition conditioned on current state\n", t);
    print_2d_array(N, K, agent_probs);
    
    // Do draws to see how agents update based on their probs
    printf("Updating agents!\n");
    update_agents(t, N, K,  agent_probs, agent_status);
    // Update the infectious indices
    n_inf_inds = find_infected_agents(N, t, inf_state_cats,
				      n_inf_states, agent_status,
				      init_inf_inds);
    // Update agent probs
    get_agent_probs(T, N, K, t+1,
		    agent_status,
		    base_probs,
		    agent_probs);


  }
  char fn[] = "agents-out.csv";
  write_agents(fn, N, T+1, agent_status);
  


}


/* Find out if first argument is in the array of the second.  integer version
INPUTS: 
x -- some integer
a -- some integer array
N -- length of a
OUTPUT:
1 if x is in a and 0 otherwise
 */
int in_array(int x,
	     int a[], int N){
  for(int ii=0; ii < N; ii++){
    if(a[ii] == x){
      return 1;
    }
  }
  return 0;
}

/* Check to see if agent is already infected based on their agent_probs
An agent is infected if there is an entry of 1.0 in an infectious state
INPUTS: 
sus_ind -- index of susceptible agent in agent_probs
K -- number of compartments
n_inf_states -- total number of infectious states
agent_probs N x K array of transition probabilities
inf_state_cats -- array of category numbers of infectious states
OUTPUTS: 1 if agent is already infected and zero otherwise
 */
int already_inf(int sus_ind, int K, int n_inf_states,
		double agent_probs[][K], int inf_state_cats[]){
  for(int kk=0; kk < K; kk++){
      // If agent both has prob 1 of transitioning and
      // that 1.0 is to an infectious state

    print_array(inf_state_cats, n_inf_states);
    if((agent_probs[sus_ind][kk] == 1.0) &&
       (in_array(kk, inf_state_cats, n_inf_states) == 1)){
      printf("category is %d\n", kk);
      printf("prob of transitioning to %d is %.2f\n",
	     kk, agent_probs[sus_ind][kk]);
      printf("agent %d is already infected\n", sus_ind);
      return 1;
    }
  }
  return 0;
}
    
/*
  (Potentially) Change states of agents states at the next time step (t+1)
  INPUTS:
  t -- current time step
  N -- total number of agents
  K -- totalnumber of compartments
  agent_probs -- NxK array of probabilities of updating for each agent conditioned on their current state
  agent_status -- TxN array of agent's status at time t
  OUTPUTS: modified agent_status with new agent states at time t+1
*/
void update_agents(int t, int N, int K,
		   double agent_probs[][K],
		   int agent_status[][N]){
  double probs[K];
  for(int ii=0; ii < N; ii++){
    for(int kk=0; kk < K; kk++){
      probs[kk] = agent_probs[ii][kk];
    }
    agent_status[t+1][ii] = draw_multinom(probs, K);
  }
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



/*
Extract the neighbors of the agent into an integer array.  also count the number
INPUTS:
nbr_dict dictionary of neighbors where the key is a GInt of the current agent index and the value is a g_slist of neighbor indices
inf_ind -- integer index of current agent we are looking for neighbors of
nbr_inds -- integer array of indices of neighbors from hashtable
OUTPUT: modified nbr_inds and n_nbrs -- the total number of neighbors for this agent
 */
int extract_neighbors(GHashTable* nbr_dict, int inf_ind, int nbr_inds[]){
  gint n_nbrs;
  gpointer value;
  gint* my_ind = g_new(gint, 1);
  *my_ind = inf_ind;
  printf("Agent index is %d\n", inf_ind);
  value = g_hash_table_lookup(nbr_dict, GINT_TO_POINTER(my_ind));
  n_nbrs = g_slist_length(value);
  printf("The number of neighbors is %d\n", n_nbrs);
  for(int ii=0; ii < n_nbrs; ii++){
    nbr_inds[ii] = *(int*)g_slist_nth(value, ii)->data;
  }
  return n_nbrs;
}

void initialize_agents(int T, int N, int K,
		       int agent_status[][N],
		       int init_state_counts[]){
  // Initialize agents at time 0
  // so the first m_0 are in state 0, the next
  // m_1 are in state 1, ..., and the final m_K are in state K
  // The rest of the time steps are set to zero
  // WILL throw an error if there are less than 2 states
  int lower_bd = 0;
  int upper_bd = init_state_counts[0];
  for (int kk=0; kk < K; kk++){
    for (int ii=lower_bd; ii < upper_bd; ii++){
      agent_status[0][ii] = kk;
    }
    if (kk < K-1){
      lower_bd = upper_bd;
      upper_bd = upper_bd + init_state_counts[kk+1];
    }
  }


}

/*
  Initialize environments for the activities.  This is a N x E array where entry
  n,e is the nth agent's e^th environment assignment.  All 0s constitute a NULL assignment.
  @arg E number of environment types (e.g. schools + workplaces -> E =2)
  @arg N number of agents
  @arg max_env is the max number of different  categories
  @arg init_env_counts is a 2D E x max_env  array where entry j, i means that environment j has init_env_counts[i,j] agents with assignment i.
  @return a modified env variable, a N x E array.
 */
void initialize_envs(int E, int N, int max_env,
		     int env[][E],
		     int init_env_counts[][100]){

  for(int ee=0; ee < E; ee++){
    int lower_bd = 0;
    int upper_bd = init_env_counts[ee][0];
    for (int kk=0; kk < max_env; kk++){
      printf("upper_bd %d\n", upper_bd);
      if( upper_bd > N){
	break;
      }
      for (int ii=lower_bd; ii < upper_bd; ii++){
	env[ii][ee] = kk;
      }
      lower_bd = upper_bd;
      upper_bd = upper_bd + init_env_counts[ee][kk+1];
    }

  }


}


/*
Initialize dictionary of neighbors in dict.  Here dict is GHashtable with integer keys corresponding to the agent index of reference and corresponding values that are a integer GArray of indices of neighbors.  N is the number of agents
 */ 
void make_nbr_dict(int N, int E, int env[][E], GHashTable *dict){


   
  for(int ii = 0; ii < N; ii++){

    for(int jj=0; jj < N; jj++){

      for(int ee=0; ee < E; ee++){
	int current_env = env[ii][ee];
	// NULL assignments (env =0) are NOT neighbors)
	// NEIGHBORS should not include self
	if( current_env > 0 & ii !=jj & 
	    env[jj][ee] == current_env){
	  gint* my_ind = g_new(gint, 1);
	  *my_ind = ii;
	  gint* my_nbr = g_new(gint, 1);
	  *my_nbr = jj;
	  g_hash_table_insert(dict, my_ind,
	  		      g_slist_append(g_hash_table_lookup(dict, my_ind),
	  				     my_nbr));
	  break;
	}
      }
    }
  }
}


void prt(GArray* a) {
 int ii;
 for (ii = 0; ii < a->len; ii++){
   printf("%d ", g_array_index(a, int, ii));
 }
 printf("\n");
}


void print(gpointer key, gpointer value, gpointer data) {
  printf("Here are some neighbors of: %d", *(gint*)key);
  printf("\n");
 
  printf("The last item is '%d'\n",
	 *(gint*)g_slist_last(value)->data); 
}


/*
Loop over the agents at time t and find which of them are in infectious states
Inputs:
N - number of total agents
t - the current time step.
infection_state_cats array of indices corresponding to the infectious states.  Ex. for the SIR model. The only infectious state is "I" which is category 1.
agent_status T x N array of agent states where entry tn corresponds to agent n's state at time t.
inf_inds -- pointer to array of indices of infectious agents at current time step 

Output: modified inf_inds and n_inf_inds -- updated to show the current infectious agens
 */
int find_infected_agents(int N, int t, int infection_state_cats[],
			  int n_inf_states,
			  int agent_status[][N], int inf_inds[]){

  int n_inf_inds = 0;
  
  for(int ii=0; ii < N; ii++){
    for(int jj = 0; jj < n_inf_states; jj++){
      if( agent_status[t][ii] == infection_state_cats[jj]){
	inf_inds[n_inf_inds] = ii;
	n_inf_inds++;
      }
    }
  }
  return n_inf_inds;
  

}


void print_array(int a[], int N){
  for(int ii=0; ii < N; ii++){
    printf("%d ", a[ii]);
  }
  printf("\n");

}

void print_2d_array(int I, int J, double a[][J]){
  for(int ii=0; ii < I; ii++){
    for(int jj=0; jj < J; jj++){
      printf("%.2f ", a[ii][jj]);
    }
    printf("\n");
  }

}



/*
Create the initial probabilities for compartment transitions
INPUTS:
T - total number of time steps
N - total number of agents
K - total number of compartments
agent_status - TxN array of agent status where entry A_{tn} \in {1, \dots, K} is the agent n's status at time t
base_probs_fn - character string of the saved base probabilities.  Default is "null" meaning we initialize our own probs
base_probs - TxKxK array where entry A_{tij} is the probability of transitioning from compartment i to j at time t to t+1.  This is also a pointer that will be updated and returned
OUTPUTS: updated base_probs which is a TxKxK array where sum_j=1^K A_{tij} = 1 for all t,i.
 */
void initialize_base_probs(int T, int N, int K,
			   int agent_status[][N],
			   char base_probs_fn[],
			   double base_probs[][K][K]){
 
    // Hard coded for a SIR model with 50% of transitioning from S to I and I to R
    for(int tt=0; tt < T; tt++){ // Some S move to I and I move to R, All R to R
      base_probs[tt][0][0] = 1.0;
      base_probs[tt][0][1] = 0.0;
      base_probs[tt][0][2] = 0.0;
      base_probs[tt][1][0] = 0.0;
      base_probs[tt][1][1] = 0.5;
      base_probs[tt][1][2] = 0.5;
      base_probs[tt][2][0] = 0.0;
      base_probs[tt][2][1] = 0.0;
      base_probs[tt][2][2] = 1.0;
    }
 
}


/*
Initialize current probabilities of transition conditioned on agent's initial state
INPUTS:
T - total number of time steps
N - total number of agents
K - total number of compartments
t - current time step
agent_status - TxN array of agent status where entry A_{tn} \in {1, \dots, K} is the agent n's status at time t
base_probs - TxKxK array where entry A_{tij} is the probability of transitioning from compartment i to j at time t to t+1. 
agent_probs - NxK array where entry A_{nk} is agent n's probability of transitioning to state k conditioned on their current state.  This is also the pointer for the object that will be modified
OUTPUT: updated agent_probs initialized at time t=0
 */
void get_agent_probs(int T, int N, int K, int t,
			  int agent_status[][N],
			  double base_probs[][K][K],
			  double agent_probs[N][K]){
  int current_status;
  for(int ii = 0; ii < N; ii++){
    for(int kk=0; kk < K; kk++){
      current_status = agent_status[t][ii];
      agent_probs[ii][kk] = base_probs[t][current_status][kk];
    }
  }
}


/*
Chance to infect susceptible agent by infectious agent.  Updates agent_probs accordingly
INPUTS:
n - index of infectious agent
m - index of susceptible agent
t - current time step, t=0, ..., T
N - total number of agents
K - total number of compartments
agent_status TxN array where A_{tn} is agent n's current status at time t
E - total number of environments
env -  N x E array where entry
  n,e is the nth agent's e^th environment assignment.  All 0s constitute a NULL assignment.
env_inds - array of indices of environments agents share
inf_cats- indices of states which correspond to infectious states
sus_cats - indices of states which correspond to susceptible states
infection_probs - KxKxK where entry ijk is probability of agent with status i infecting agent with status j to status k.  Should be a sparse matrix. As only entries ijk > 0 if i is an infectious state, j is a susceptible state,
*/
void infect_agent(int n, int m,
		  int t, 
		  int N, int K,
		  int agent_status[][N],
		  int E, int env[][E],
		  int env_inds[],
		  int inf_cats[],
		  int sus_cats[],
		  double infection_probs[][K][K],
		  double agent_probs[][K]){

  // Extract the probability of agent n infecting agent m, conditioned on environments, etc
  printf("interacting infected agent %d and susceptible agent %d\n", n, m);
  double updated_probs[K];
  get_inf_probs(n, m,
		t, 
		N, K,
		agent_status,
		E, env,
		env_inds,
		inf_cats,
		sus_cats,
		infection_probs,
		updated_probs);
  printf("Agent %d's new probs of transitioning are\n", m);
  // Actually perform a multinomial draw to see whether agent was infected
  for(int kk=0; kk < K; kk++){
    printf("%.2f ", updated_probs[kk]);
  }
  printf("\n");
  int new_sus_state;
  new_sus_state = draw_multinom(updated_probs, K);
  printf("agent %d's new state is going to be %d\n", m, new_sus_state);
  // Update agent's new probabilities of transitioning. We ensure an infection by setting that state's probability to 1
  for(int kk=0; kk < K; kk++){
    if( kk == new_sus_state){
      agent_probs[m][kk] = 1.0;
    } else {
      agent_probs[m][kk] = 0.0;
    }
  }
}



/*
Extract the relevant probabilities of an infectious agent in a certain state to a susceptible agent in another state of becoming infected. 
INPUTS:
n - index of infectious agent
m - index of susceptible agent
t - current time step, t=0, ..., T
N - total number of agents
K - total number of compartments
agent_status TxN array where A_{tn} is agent n's current status at time t
E - total number of environments
env -  N x E array where entry
  n,e is the nth agent's e^th environment assignment.  All 0s constitute a NULL assignment.
env_inds - array of indices of environments agents share
inf_cats- indices of states which correspond to infectious states
sus_cats - indices of states which correspond to susceptible states
infection_probs - KxKxK where entry ijk is probability of agent with status i infecting agent with status j to status k.  Should be a sparse matrix. As only entries ijk > 0 if i is an infectious state, j is a susceptible state.
OUTPUTS: updated_probs a 1d array of length K which are the probabilities of an agent being infected 
*/
void get_inf_probs(int n, int m,
		   int t, 
		   int N, int K,
		   int agent_status[][N],
		   int E, int env[][E],
		   int env_inds[],
		   int inf_cats[],
		   int sus_cats[],
		   double infection_probs[][K][K],
		   double updated_probs[]){
  int inf_status;
  int sus_status;
  for(int kk=0; kk < K; kk++){
    inf_status = agent_status[t][n];
    sus_status = agent_status[t][m];
    updated_probs[kk] = infection_probs[inf_status][sus_status][kk];
  }

  


}







/* Return an integer corresponding to the drawn multinomial
   category */
int draw_multinom(double probs[], int K){
  int multinom_draw;
  double cum_prob = 0.0;
  double draw = double_rand(0.0, 1.0);
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


double double_rand(double min, double max ){
    double scale = rand() / (double) RAND_MAX; /* [0, 1.0] */
    return min + scale * ( max - min );      /* [min, max] */
}






/* Comipliation command
gcc `pkg-config --cflags --libs glib-2.0` -o agent-neighbors agent-neighbors.c
Source: https://www.ibm.com/developerworks/linux/tutorials/l-glib/
*/
