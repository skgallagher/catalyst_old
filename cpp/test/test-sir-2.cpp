#include "../sir.hpp"

/*
COMPILATION
g++ test-sir-2.cpp ../sir.cpp -o sir2
 */

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

  

  cm_vals = run_sir(sys, x,
		    stepper,
		    1.0,
		    100.0,
		    cm_vals,
		    times);

  /* output */
  int steps = 100 / 1 + 1;
  for( size_t i=0; i < steps; i++ )
    {
      cout << i  << '\t' << cm_vals[i][0] << ' ' << cm_vals[i][1] << ' ' << cm_vals[i][2] << '\n';
    }


  
}
