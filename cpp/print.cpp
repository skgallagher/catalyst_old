/*
Printing functions for helping test

March 21, 2018
SKG

Compilation command:
 */

#include "print.hpp"



void print_agents(int N, int T,
		  int agent_status[][1000]){

  for(int nn=0; nn < N; nn++){
    for(int tt=0; tt < T; tt++){
      std::cout << agent_status[nn][tt] << ' ';
    }
    std::cout << '\n';
  }

}


void print_envs(int N, int E, int env[][100]){
  for(int nn=0; nn < N; nn++){
    for(int ee=0; ee < E; ee++){
      std::cout << env[nn][ee] << ' ';
    }
    std::cout << '\n';
  }
}


void print_graph_nbrs(Graph g, int N){
  for(int ii=0; ii < N; ii++){
    std::pair<adjacency_iterator, adjacency_iterator> neighbors =
      boost::adjacent_vertices(vertex(ii,g), g);


    IndexMap index = get(boost::vertex_index, g);
 

    cout << ii << ": ";
    for(; neighbors.first != neighbors.second; ++neighbors.first){
      std::cout << index[*neighbors.first] << " ";
    }
    std::cout << '\n';
  }
  
}


/*
Print CM vals
INPUTS: 
T - total amount of time
K - number of state 
cm_vals - (T / step_size + 1) x (K+1) matrix where col 0 is time, 1 is state 1, 2 is state 2, etc.
step_size - time step size
 */
void print_cm_vals(int T, int K, vector<state_type> cm_vals,
		   double step_size){
  int n_steps;
  n_steps = (int) (T / step_size + 1);
  for( int ii=0; ii< n_steps; ii++ ){
    std::cout << step_size * ii << ' ';
    for(int kk=0; kk < K; kk++){
      std::cout << cm_vals[ii][kk] << ' ';
    }
    std::cout << '\n';
  }
   

}

void print_base_probs(int T, int K, double base_probs[][100][100]){
  for(int tt=0; tt < T; tt++){
    std::cout << "t=" << tt << '\n';
    for(int ii=0; ii < K; ii++){
      for(int jj=0; jj < K; jj++){
	std::cout << base_probs[tt][ii][jj] << ' ';
      }
      std::cout << '\n';
    }
  }

}
