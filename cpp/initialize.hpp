#include <iostream>
#include <map>

using namespace std;


void initialize_agents(int N, int T, int K,
		       int agent_status[][1000],
		       int init_state_counts[]);



void initialize_envs(int N, int E, int max_env,
		     int env[][100],
		     int init_env_counts[][100]);

std::map<int,int*> init_nbr_dict(int N, int E, int env[][100],
		   std::map<int,int*> map);
