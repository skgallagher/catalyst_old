#include <iostream>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/adjacency_iterator.hpp>

using namespace std;

typedef boost::property<boost::vertex_property_tag, double> VertexProperty;
typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, VertexProperty> Graph;
typedef boost::property_map<Graph, boost::vertex_index_t>::type IndexMap;
typedef boost::graph_traits < Graph >::adjacency_iterator adjacency_iterator;
    

 

void print_agents(int N, int T,
		  int agent_status[][1000]);
void print_envs(int N, int E, int env[][100]);


void print_graph_nbrs(Graph g, int N);
