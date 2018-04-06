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
