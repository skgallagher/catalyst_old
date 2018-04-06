#include "../sir.hpp"

int main(int argc, char **argv)
{
     // Setting up the SIR
  // TODO: match up with above params
  state_type x = { 950.0 , 50.0 , 0.0 }; // initial conditions
  runge_kutta4< state_type > stepper;
  std::array<double,3> params;
  params[0] = .1;
  params[1] = .03;
  params[2] = 1000.0;
  sir sys(params);

    
  //integrate_const(stepper, sys , x , 0.0 , 100.0 , 1.0, write_sir);

  // Storing results in a container
  vector<state_type> cm_vals;
  vector<double> times;

  

  int out = run_sir(sys, x,
	  stepper,
	  cm_vals,
	  times);

  
}
