/*
SKG
February 7, 2018
Minimizing the SIR function

Compilation:
  gcc fit-sir.c minimize-functions.c sir-functions.c -lgsl -lgslcblas -lm -o fit-sir
 */


#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multimin.h>
#include <math.h>
#include "minimize-functions.h"
#include "sir-functions.h"
#include "helper-functions.h"



int main(void){

  int T = 10;
  int P = 3;
  double step_size = .01;
  int S = (int)(T / step_size) + 1;
  double p[3];
  double x[S][P+1];
  double f_mat[S][P+1];
  double tt=0.0;
  int D = 2; // dimension

  // Initialize time points to fill
  x[0][0] = tt;

  // Initial values
  x[0][1] = 950.;
  x[0][2] = 50.;
  x[0][3] = 0.;
  for(int ss=1; ss <= S; ss++){
    x[ss][0] = x[ss-1][0] + step_size;
    f_mat[ss][0] = x[ss][0];
  }
  

  // Initialize params
  p[0] = .1;
  p[1] = .03;
  p[2] = 1000.;
  ode_wrapper(S, P, x,
	      p, f_mat);

  // extracting a smaller set of data from the ODE
  double data[T+1][P+1];
  int step = (int) (1.0 / step_size);
  int ss=0;
  for(int tt = 0; tt < T+1; tt++){
    for(int pp=0; pp < (P+1); pp++){
      data[tt][pp] = f_mat[ss][pp];
    }
    ss = ss + step;
  }

  printf("observed data is\n");
  // print_float_2d(T+1, P+1, data);

  double data2[T+1][P+1];
  ode_wrapper(T, P, data, p, data2);



  // trying the wrapper
  double init_params[2] = {.6, .03}; // initial guesses
  double thresh = 1e-4;
  double init_step_size = .01;
  double n_iters = 1000; // max number
  int print_steps = 1; // 1 is print. anything else is don't print
  double best_params[D];
  optimize_ode(T+1, P, D, data,
	       init_params, thresh,
	       init_step_size, n_iters,
	       print_steps, best_params);


  
  return 0;
  

}
