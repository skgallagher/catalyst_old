/*
February 2, 2018
Sorting out the functions for the minimizer
SKG
 */

#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multimin.h>
#include <math.h>
#include "minimize-functions.h"
#include "sir-functions.h"
#include "helper-functions.h"

// TODO: lin reg -> some ode.
/* 
First make function based on parameters
f(x; b0, b1) that returns matrix from steps x[0], x[1], ..., x[S]
x - S x P+1 array of data where first col is time (which is really what we need)
p = [b0, b1]
S = total number of obs
P = total number of dimensions to minimize genrally 1
f_mat -- output of data
 */
void lin_reg(int S, int P, double x[][P+1],
	     double p[], double f_mat[][P+1]){
  for(int ss=0; ss < S; ss++){
    f_mat[ss][0] = x[ss][0];
    for(int pp=1; pp <= P; pp++){
      f_mat[ss][pp] = p[0] + x[ss][0] * p[1];
    }
  }
}



/* there's probably a better way to call the params as a struct or something but oh well
INPUTS:
D - number of dims to be minimized
S - total number of time points (can be duplicates)
P - number of outputs of function.  Typically 1 but for the SIR we have 3 because one t will give us a S, I, and R value
data - array of S x P+1 where col 1 is the time, and the next P are the different outputs
OUTPUTS:
1d array of parameters. first three entries are the dimension of betas to be minimized D,
total number of points S, and dimension of outputs P.
The next D are the actual betas to be minmized
The next S are the x value of points, the following S are the first column of outputs, the next S are the second, etc*.
So total length is 3 + D + S(P+1)*/
double data_to_params(int D, int S, int P, double betas[],
		      double data[S][P+1], double params[]){
  params[0] = (double)D;
  params[1] = (double)S;
  params[2] = (double)P;
  for(int dd=0; dd < D; dd++){
    params[3 + dd] = betas[dd];
  }
  for(int pp=0; pp < (P+1); pp++){
    for(int ss=0; ss < S; ss++){
      params[D + 3 + ss + pp * S] = data[ss][pp];
    }
  }
  
}

void params_to_data(double params[], int S, int P,
		      double data[S][P+1]){
  int D = (int)params[0];
  for(int pp=0; pp < (P+1); pp++){
    for(int ss=0; ss < S; ss++){
      data[ss][pp] = params[3 + D + ss + pp*S];
    }
  }

}


/*
cost function to minimize, in this case sse
 */
double sum_squares(const gsl_vector *v, void *params){

  double *p = (double *)params;
  int D = (int)p[0];
  int S = (int)p[1];
  int P = (int)p[2];
  double data[S][P+1];
  params_to_data(p, S, P,
		 data);
  /* printf("converted data\n"); */
  /* print_float_2d(S, P+1, data); */

  // Extract the min_vars from v
  double min_vars[D];
  for(int ii=0; ii < D; ii++){
    min_vars[ii] = gsl_vector_get(v, ii);
  }
   /* printf("Trying beta=%.4f, gamma=%.4f\n", */
   /* 	 min_vars[0], min_vars[1]); */

  // Get max T
  double T;

  T = data[S-1][0];
  /* printf("T is %.3f\n", T); */
  /* printf("S is %d\n", S); */
  
  // Extract initial values and N
  double N;
  double init_vals[P];
  for(int ii=0; ii < P; ii++){
    N = N + data[0][ii+1];
    init_vals[ii] = data[0][ii+1];
  }
  
   // Compute the true function given min_vars
  double step_size = .001;
  int n_ode_rows;
  n_ode_rows = ((int)(T / step_size)) + 1;
  double eps_abs = 1.e-8;
  double eps_rel = 1.e-10;
  double f_mat[n_ode_rows][P+1];
  int ode_T;
  ode_T = (int)(T+.5);

  ode_vals(ode_T, P, D, step_size,
	   N, init_vals, eps_abs, eps_rel,
	   min_vars, f_mat);
  

  /* printf("last time val of fmat is %.3f\n", f_mat[n_ode_rows-1][0]); */

  // Subset f_mat to match time points of data
  double f_est[S][P+1];
  // Propagate f_est time values from the data
  for(int ii=0; ii < S; ii++){
    f_est[ii][0] = data[ii][0];
  }

    /*  printf("ode with those times\n"); */
    /* print_float_2d(S, P+1, f_est); */
    /*      printf("the data is\n"); */
    /* print_float_2d(S, P+1, data); */

    /* printf("n_ode_rows is %d\n", n_ode_rows); */

  // Extract the matching time point indices from data and function 
  int matching_inds[S];
  find_matching_inds(0, 0,
		     S, n_ode_rows,
		     P+1, P+1,
		     data, f_mat,
		     1, 1.e-6,
		     matching_inds);
  /* printf("matching inds are\n"); */
  /* print_int_1d(S, matching_inds); */
  subset_array_d(S, n_ode_rows,
		 matching_inds, P+1,
		 f_mat, f_est);
  
  
  /* printf("ode with those vals\n"); */
  /* print_float_2d(S, P+1, f_est); */

  // the actual sum of squares
  double SSE = 0.0;
  for(int ss=0; ss < S; ss++){
    for(int pp=1; pp < (P+1); pp++){
      //SSE = SSE + abs(data[ss][pp] - f_est[ss][pp]); // abs val
       SSE = SSE + pow(data[ss][pp] - f_est[ss][pp], 2);
      /* printf("ss %d pp %d SSE: %.3f\n", ss, pp, SSE);  */
    }
  }
  //printf("SSE: %.3f\n", SSE);
  return SSE;
}





/* Wrapper for the Nelder-Mead optimization for a function with unknown gradient provided by gsl.  The cost function is SSE.
INPUTS:
S - total number of time steps in the data (can be duplicate times)
P - total number of Ys (usually 1.  3 in the SIR case, 4 in SEIR, etc)
D - Dimension of parameters to be optimized
data - Sx(P+1) where first column is the time and the following columns are the different observed Ys.
init_params - our initial guesses for the parameters.
thresh - threshold of error to be optimized too.  Smaller will require more steps but more accuracy.  Default is .001
init_step_size - how large initial steps N-M should take.  Default is 1.0
n_iters - max amount of iterations to be done by NM
print_steps - if 1 we will print out the steps of optimization
best_params - array of length D.  array to be returned with optimized params
OUTPUTS:
best_params array of length D with the minimized parameters
*/
void optimize_ode(int S, int P, int D, double data[][P+1],
		  double init_params[], double thresh,
		  double init_step_size, int n_iters,
		  int print_steps, double best_params[]){

  // Put data into the form of params
  double params[3 + D + S * (P+1)]; // 3 for length of {S, P, D}
  data_to_params(D, S, P, init_params,
		 data, params);
    
  /*NELDER-MEAD simplex minimization.  Don't need deriv */
  const gsl_multimin_fminimizer_type *T =
    gsl_multimin_fminimizer_nmsimplex2;
  gsl_multimin_fminimizer *s = NULL;
  gsl_vector *ss, *x;
  gsl_multimin_function minex_func;

  size_t iter = 0;
  int status;
  double size;
  /* Starting point */
  x = gsl_vector_alloc(D);
  for(int ii=0; ii < D; ii++){
    gsl_vector_set(x, ii, init_params[ii]);
  }
  /* Set initial step sizes to 1 */
  ss = gsl_vector_alloc(D);
  gsl_vector_set_all(ss, init_step_size);
  /* Initialize method and iterate */
  minex_func.n = D;
  minex_func.f = sum_squares;
  minex_func.params = params;
  s = gsl_multimin_fminimizer_alloc(T, D);
  gsl_multimin_fminimizer_set(s, &minex_func, x, ss);
  do
    {
      iter++;
      status = gsl_multimin_fminimizer_iterate(s);
      if (status)
	break;
      size = gsl_multimin_fminimizer_size(s);
      status = gsl_multimin_test_size(size, thresh);  // threshold
      if(print_steps == 1){
	if (status == GSL_SUCCESS){
	    printf ("converged to minimum at\n");
	}
	printf("%5d ", iter);
	for(int ii=0; ii < D; ii++){
	  printf("%.4f ", gsl_vector_get(s->x, ii));
	}
	printf("SSE = %7.3f size = %.4f\n", s->fval, size);
      }
    }
  while (status == GSL_CONTINUE && iter < n_iters);

  if(iter >= n_iters){
    printf("WARNING: Optimization did not converge\n");
  }
  for(int ii=0; ii < D; ii++){
    best_params[ii] = gsl_vector_get(s->x, ii);
  }
  
  gsl_vector_free(x);
  gsl_vector_free(ss);
  gsl_multimin_fminimizer_free (s);

  
}


