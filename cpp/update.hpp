#ifndef UPDATE_H
#define UPDATE_H

#include <iostream>
#include "random.hpp"


void get_agent_probs_cm(int T, int N, int K,
		     int tt, int agent_status[][1000],
			double base_probs[][100][100],
			double agent_probs[][K]);

void update_agents(int t, int N, int K,
		   double agent_probs[][100],
		   int agent_status[][1000]);

#endif
