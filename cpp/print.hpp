#ifndef PRINT_H
#define PRINT_H

#include <iostream>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/adjacency_iterator.hpp>
#include <boost/array.hpp>

using namespace std;

typedef boost::property<boost::vertex_property_tag, double> VertexProperty;
typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, VertexProperty> Graph;
typedef boost::property_map<Graph, boost::vertex_index_t>::type IndexMap;
typedef boost::graph_traits < Graph >::adjacency_iterator adjacency_iterator;
    
typedef boost::array<double, 3> state_type; // e.g. (S, I, R)
 

void print_agents(int N, int T,
		  int agent_status[][1000]);
void print_envs(int N, int E, int env[][100]);


void print_graph_nbrs(Graph g, int N);

void print_cm_vals(int T, int K,
		   vector<state_type> cm_vals,
		   double step_size);


void print_base_probs(int T, int K, double base_probs[][100][100]);

#endif
