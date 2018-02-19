/* From https://gist.github.com/Imxset21/40a00f80c25d703fa117 */

/*
  Example adapted from the GNU Scientific Library Reference Manual
  Edition 1.1, for GSL Version 1.1
  9 January 2002
  URL: gsl/ref/gsl-ref_25.html#SEC381
   
  Revisions by:  Dick Furnstahl  furnstahl.1@osu.edu
                 Pedro Rittner   pr273@cornell.edu
 
  Revision history:
  11/03/02  changes to original version from GSL manual
  11/14/02  added more comments
  06/09/14  changed build suggestions to comply with modern gcc linkage for GSL/BLAS
*/

/* 
   Compile and link with:
   gcc test-sir.c helper-functions.c sir-functions.c -lgsl -lgslcblas -lm -o test-sir

   Alternatively:
   gcc -std=c99 -Wall -O0 -g3 ode_test.c -o ode_test.bin -lgsl -lblas
*/

/* 
   The following program solves the second-order nonlinear 
   Van der Pol oscillator equation (see Landau/Paez 14.12, part 1),

   x"(t) + \mu x'(t) (x(t)^2 - 1) + x(t) = 0

   This can be converted into a first order system suitable for 
   use with the library by introducing a separate variable for 
   the velocity, v = x'(t).  We assign x --> y[0] and v --> y[1].
   So the equations are:
   x' = v                  ==>  dy[0]/dt = f[0] = y[1]
   v' = -x + \mu v (1-x^2) ==>  dy[1]/dt = f[1] = -y[0] + mu*y[1]*(1-y[0]*y[0])
*/

#include <stdio.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_odeiv.h>
#include "minimize-functions.h"
#include "sir-functions.h"
#include "helper-functions.h"



int main (void){
  int T = 100;
  int P = 3;
  int D = 2;
  double step_size = .1;
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
  p[0] = .1;
  p[1] = .03;

  // goooooo
  ode_vals(T, P, D, step_size,
	   N, init_vals,
	   eps_abs, eps_rel,
	   p, f_mat);
  print_float_2d(S, P+1, f_mat);
}

