/*
Working with minimizing cost functions without derivatives from gsl
The goal is to minimized ODEs also made in GSL, dependent on parameters
ugh
January 30, 2018
SKG
Compilation command:
 gcc minimize-test-3.c -o minimize-test-3 -lgsl -lgslcblas -lm

V3
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

#include<stdio.h>
#include<stdlib.h>
#include<gsl/gsl_vector.h>
#include<gsl/gsl_multimin.h>
#include <math.h>

// prototypes
double extract_ft(double t, int p, int S, int P,
		  double f_mat[][P+1], double eps);
double abs_val(double x);
void optimize_ode(int S, int P, int D, double data[][P+1],
		  double init_params[], double thresh,
		  double init_step_size, int n_iters,
		  int print_steps, double best_params[]);


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


void print_float_2d(int M, int N, double a[M][N]){
  for(int ii=0; ii < M; ii++){
    for(int jj=0; jj < N; jj++){
      printf("%.1f ", a[ii][jj]);
    }
    printf("\n");
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
  double betas[D];
  for(int dd=0; dd < D; dd++){
    betas[dd] = p[dd + 3];
  }
  double data[S][P+1];
  params_to_data(p, S, P,
		 data);
  //  printf("converted data\n");
  //  print_float_2d(S, P+1, data);

  // extract the min_vars from v
  double min_vars[D];
  for(int ii=0; ii < D; ii++){
    min_vars[ii] = gsl_vector_get(v, ii);
  }
  //  printf("Trying b0=%.1f, b1=%.1f\n",
  //	 min_vars[0], min_vars[1]);
  
  
  // Compute the true function given v
  double f_mat[S][P+1];
  lin_reg(S, P, data,
	  min_vars, f_mat);
  
  //printf("lin reg with those vals\n");
  //print_float_2d(S, P+1, f_mat);

  // the actual sum of squares
  double SSE = 0.0;
  double f_tp;
  for(int ss=0; ss < S; ss++){
    for(int pp=1; pp < (P+1); pp++){
      f_tp=100.0;
      double t;
      t = data[ss][0];
      double eps = 1e-4;
      f_tp = extract_ft(t, pp, S, P, f_mat, eps);
      //printf("f_tp=%.2f\n", f_tp);
      SSE = SSE + pow(data[ss][pp] - f_tp, 2);
      //printf("data=%.2f fxn=%.2f\n", data[ss][pp], f_tp);
      // printf("t=%.2f, SSE=%.2f\n", t, SSE);
    }
  }
  //  printf("SSE: %.3f\n", SSE);
  return SSE;
}

/*
extract the corresponding time point in column p of the function matrix f_mat
Probably a bit slow
TODO: speed up
INPUTS:
t - time point we are looking at
p - which column we need.  Typically the second column (so 1)
S - total number of time steps in f_mat
P - total number of outputs of the function
eps - a threshold for equality.  In general should be pretty small
OUTPUTS:
proper value of the function in column p and time t
*/
double extract_ft(double t, int p, int S, int P,
		  double f_mat[][P+1], double eps){
  for (int ss=0; ss < S; ss++){
    if( abs_val(f_mat[ss][0] - t) < eps){
      // printf("returning val\n");
      // printf("s=%d, t=%.2f, f=%.2f\n", ss, t, f_mat[ss][p+1]);
      return f_mat[ss][p];
    }
  }
}

/*
Absolute value function
 */
double abs_val(double x){
  if( x < 0.0){
    return -1.0 * x;
  } else{
    return x;
  }

}





void print_float_1d(int N, double a[]){
  for(int ii=0; ii < N; ii++){
    printf("%.2f ", a[ii]);
  }
  printf("\n");
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
  gsl_vector_set_all(ss, 1);
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
	  printf("%10.3e ", gsl_vector_get(s->x, ii));
	}
	printf("SSE = %7.3f size = %.3f\n", s->fval, size);
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



int main(void){
  int S = 4, P = 1;
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
  double f_mat[S][P+1];
  double betas[2] = {4.0, 1.0};
  double params[3 + D + S * (P+1)];
  

  
  data_to_params(D, S, P, betas,
		 data, params);
  params_to_data(params, S, P, data);

  // trying the wrapper
  double init_params[2] = {-4.0, 92.0};
  double thresh = 1e-3;
  double init_step_size = 1.0;
  double n_iters = 1000;
  int print_steps = 1;
  double best_params[D];
  optimize_ode(S, P, D, data,
	       init_params, thresh,
	       init_step_size, n_iters,
	       print_steps, best_params);
  
  
  return 0;
  

}
