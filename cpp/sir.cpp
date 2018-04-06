/*
Integrate the SIR function
SKG
March 26, 2018
 */


#include "sir.hpp"

/*
INPUTS:
sys - a sir object with params (beta, gamma, N) and an operator with diff eqs
init_vals - initial values of the SIR
stepper - the type of stepper for use in the ODE integrator
cm_vals - a container for the outputted vals
times - set of time values
T - max amount of time
step_size - 1 is the step size
OUTPUTS:
a filled cm_vals matrix of size (T+1) x (# state + 1) (col 0 is time, 1 is state 1, 2 is 2, etc.)
 */
state_type run_sir(sir sys, state_type init_vals,
		   runge_kutta4<state_type> stepper,
		   vector<state_type> cm_vals,
		   vector<double> times,
		   double T,
		   double step_size){

}
