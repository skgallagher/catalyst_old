#ifndef AGENT_STEP_H
#define AGENT_STEP_H

#include "initialize.hpp"
#include <boost/array.hpp>
#include <boost/numeric/odeint.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/adjacency_iterator.hpp>


void run_am_step(int tt, int N, int K,
		 int agent_status[][1000],
		 double base_probs[][100][100],
		 int env[][100],
		 Graph neighbor_graph,
		 int inf_states[],
		 int sus_states[],
		 double agent_probs[][100]);

#endif
