/*
Tests for initialize.cpp
SKG
March 22, 2018

Compilation commands:
g++ test-initialize.cpp ../print.cpp ../initialize.cpp -o test-inits
 */

#include <iostream>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/adjacency_iterator.hpp>
#include "../initialize.hpp"
#include "../print.hpp"
using namespace std;

typedef boost::property<boost::vertex_property_tag, double> VertexProperty;
typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, VertexProperty> Graph;
typedef boost::property_map<Graph, boost::vertex_index_t>::type IndexMap;
typedef boost::graph_traits < Graph >::adjacency_iterator adjacency_iterator;
    

 

int main(){



  int N = 10; // number of agents
  int T = 10; // number of time steps
  int K = 3; // number of states
  int P = K;
  int D = 2; // dimension of params to be minimized
  int agent_status[N][1000]; // if we have more than 1000 time points we're in trouble
  int init_state_counts[] = {9, 1, 0}; // S=950, I = 50, R =0
  int inf_state_cats[] = {1}; // 1 is the only infection state
  int sus_state_cats[] = {0}; //0 is the only susceptible state
  int init_inf_inds[N];
  int n_inf_inds = 0;
  int n_inf_states = 1;
  int n_sus_states = 1;
  double p[] = {.1, .03}; // {beta, gamma}
  double step_size = .001;
  double eps_abs = 1.e-6;
  double eps_rel = 1.e-6;



  initialize_agents(N, T, K,
		    agent_status,
		    init_state_counts);

  std::cout << "agents \n";
  print_agents(N, T, agent_status);

  int E = 3;
  int max_env = 5; // the max k
  int env[N][100];
  // magic number 100.  If there are more than 100^2 total ECs this will break
  int init_env_counts[10][100]  = {{2, 1, 1, 0, 6},
				   {0, 3, 2, 5, 0},
				   {10, 0, 0, 0, 0}};
  
  
  initialize_envs(N, E, max_env,
		  env, init_env_counts);

  std::cout << "envs \n";
  print_envs(N, E, env);


  

  Graph g(N);
  std::cout << "neighbor graph \n";
  
  g = initialize_nbr_graph(N, E, env);

  print_graph_nbrs(g, N);


  




  return 0;
}
