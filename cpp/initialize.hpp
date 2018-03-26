#include <iostream>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/adjacency_iterator.hpp>

using namespace std;

typedef boost::property<boost::vertex_property_tag, double> VertexProperty;
 
/*
adjacency_list<OutEdgeContainer, VertexContainer, Directedness,
               VertexProperties, EdgeProperties,
               GraphProperties, EdgeList>
*/
typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, VertexProperty> Graph;


void initialize_agents(int N, int T, int K,
		       int agent_status[][1000],
		       int init_state_counts[]);



void initialize_envs(int N, int E, int max_env,
		     int env[][100],
		     int init_env_counts[][100]);

Graph initialize_nbr_graph(int N, int E, int env[][100]);

