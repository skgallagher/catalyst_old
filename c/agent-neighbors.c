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
void prt(GArray* a);
void print(gpointer key, gpointer value, gpointer data);

///////////////////////////////////////////////////
int main(){
  int N = 10; // number of agents
  int T = 10; // number of time steps
  int K = 5; // number of states
  int agent_status[T][N];
  int init_state_counts[] = {4, 3, 2, 0, 1};

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

  GHashTable* dict = g_hash_table_new(g_int_hash, g_int_equal);
  make_nbr_dict(N, E, env, dict);

  printf("There are %d keys in the hash\n", g_hash_table_size(dict));

  g_hash_table_foreach(dict, print, NULL);


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

    GArray* neighbors = g_array_sized_new(FALSE, FALSE, sizeof(int), N);
    for(int jj=0; jj < N; jj++){

      for(int ee=0; ee < E; ee++){
	int current_env = env[ii][ee];
	// NULL assignments (env =0) are NOT neighbors)
	// NEIGHBORS should not include self
	if( current_env > 0 & ii !=jj & 
	    env[jj][ee] == current_env){
	  g_array_append_val(neighbors, jj);
	  /* gint* my_ind = g_new(gint, 1); */
	  /* *my_ind = ii; */
	  /* gint* my_nbr = g_new(gint, 1); */
	  /* *my_nbr = jj; */
	  /* g_hash_table_insert(dict, my_ind, */
	  /* 		      g_slist_append(g_hash_table_lookup(dict, my_ind), */
	  /* 				     my_nbr)); */
	  break;
	}
      }
    }
    if (neighbors->len > 0){
      printf("neighbors of agent %d\n", ii);
      prt(neighbors);
    }

    

    gint* k_one = g_new(gint, 1);
    *k_one = ii;
    g_hash_table_insert(dict, k_one, neighbors);

    

    g_array_free(neighbors, FALSE);
    
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
}
