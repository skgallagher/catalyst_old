/*
Working with minimizing cost functions without derivatives from gsl
The goal is to minimized ODEs also made in GSL, dependent on parameters
ugh
January 30, 2018
SKG
Compilation command:
 gcc minimize-test-4.c minimize-functions.c helper-functions.c sir-functions.c -o minimize-test-4 -lgsl -lgslcblas -lm

V4
This is testing the limits of my C knowledge
We attempt to minimize f(x) = b1x + b0 where b0 = 1 and b1 =4
the data is
t f(x)
0 1
1 5
2 9
3 13
Want to find {b0, b1}
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
  int S = 4, P = 1; // number of data points, number of Ys
  int D = 2; // dimension
  

  // Data
  double data[S][P+1];
  data[0][0] = 0.0;
  data[0][1] = 1.0; 
  data[1][0] = 1.0;
  data[1][1] = 5.0;
  data[2][0] = 2.0;
  data[2][1] = 9.0;
  data[3][0] = 3.0;
  data[3][1] = 13.0;
  print_float_2d(S, P+1, data);


  // trying the wrapper
  double init_params[2] = {-4.0, 92.0}; // initial guesses
  double thresh = 1e-3;
  double init_step_size = 1.0;
  double n_iters = 1000; // max number
  int print_steps = 1; // 1 is print anything else is don't print
  double best_params[D];
  optimize_ode(S, P, D, data,
	       init_params, thresh,
	       init_step_size, n_iters,
	       print_steps, best_params);
  
  
  return 0;
  

}
