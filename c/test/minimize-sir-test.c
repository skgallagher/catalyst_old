/*
Working with minimizing cost functions without derivatives from gsl
The goal is to minimized ODEs also made in GSL, dependent on parameters
ugh
January 30, 2018
SKG
Compilation command:
  gcc minimize-sir-test.c helper-functions.c sir-functions.c minimize-functions.c -lgsl -lgslcblas -lm -o minimize-sir-test

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
  // TODO:  why does going from Time steps T=17 to 18 cause the last entry to zero out?
  int T = 20;
  int P = 3;
  int D = 2;
  double step_size = .001;
  double p[100];
  double N = 1000.;
  double init_vals[100];
  int S = (int) (T / step_size) + 1;
  double f_mat[S][P+1];
  double tt=0.0;
  double eps_abs = 1.e-8;
  double eps_rel = 1.e-10;


  // Initial values
  init_vals[0] = 950.;
  init_vals[1] = 50.;
  init_vals[2] = 0.;

  // Initialize params
  p[0] = .3;
  p[1] = .15;

  // goooooo
  ode_vals(T, P, D, step_size,
	   N, init_vals,
	   eps_abs, eps_rel,
	   p, f_mat);
  // print_float_2d(S, P+1, f_mat);

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



  // trying the wrapper
  double init_params[2] = {.22, .16}; // initial guesses
  double thresh = 1.e-4;
  double init_step_size = .1;
  double n_iters = 1000; // max number
  int print_steps = 1; // 1 is print. anything else is don't print
  double best_params[D];
  optimize_ode(T+1, P, D, data,
	       init_params, thresh,
	       init_step_size, n_iters,
	       print_steps, best_params);
  
  return 0;
  

}
