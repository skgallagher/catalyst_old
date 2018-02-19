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
   gcc -c ode_test.c
   gcc sir-test-2.c minimize-functions.c -lgsl -lgslcblas -lm -o sir-test-2

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

/* function prototypes */
int rhs (double t, const double y[], double f[], void *params_ptr);
int jacobian (double t, const double y[], double *dfdy,
              double dfdt[], void *params_ptr);
void ode_wrapper(int S, int P, double x[][P+1],
		 double p[], double f_mat[][P+1]);


/* SIR struct */
/* TODO: make generic for any ode */
struct sir {
  double params[100];
  double N;
};



/*************************** main program ****************************/

/* 
This is a wrapper for the ODE functions.  This will integrate with the optimizer to find best fit of parameters in the ODE. 
First make function based on parameters
f(x; p) that returns matrix from steps x[0][0], x[1][0], ..., x[S][0]
x - S x P+1 array of data where first col is time (which is really what we need)
INPUTS:
p = [param1, param2, ...] (like {step_size, beta, gamma, N}).  Currently this is [beta, gamma, N]
S = total number of obs
P = total number of outputs (Ys) to minimize. genrally 1
f_mat -- output of data
OUTPUTS:
modified f_mat with function at given values of t for given parameters p
 */
void ode_wrapper(int S, int P, double x[][P+1],
		 double p[], double f_mat[][P+1]){
  int dimension = P;		/* number of differential equations */
  
  double eps_abs = 1.e-8;	/* absolute error requested */
  double eps_rel = 1.e-10;	/* relative error requested */

  /* define the type of routine for making steps: */
  const gsl_odeiv_step_type *type_ptr = gsl_odeiv_step_rkf45;
  /* 
     allocate/initialize the stepper, the control function, and the
     evolution function.
  */
  gsl_odeiv_step *step_ptr = gsl_odeiv_step_alloc (type_ptr, dimension);
  gsl_odeiv_control *control_ptr = gsl_odeiv_control_y_new (eps_abs, eps_rel);
  gsl_odeiv_evolve *evolve_ptr = gsl_odeiv_evolve_alloc(dimension);

  gsl_odeiv_system my_system;	/* structure with the rhs function, etc. */

  // TODO: make generic
  struct sir disease_params;
  
  disease_params.params[0] = p[0]; // beta
  disease_params.params[1] = p[1]; // gamma
  double N = 0.;
  for(int ii=0; ii < P; ii++){ // N is taken from initial states
    N = N + x[0][ii+1];
  }
  disease_params.N = N; // N
  double y[P];			/* current solution vector */

  double t, t_next;		/* current and next independent variable */
  double tmin, tmax, delta_t;	/* range of t and step size for output */

  double h = 1e-6;		/* starting step size for ode solver */

  /* load values into the my_system structure */
  my_system.function = rhs;	/* the right-hand-side functions dy[i]/dt */
  my_system.jacobian = jacobian;	/* the Jacobian df[i]/dy[j] */
  my_system.dimension = dimension;	/* number of diffeq's */
  my_system.params = &disease_params;	/* parameters to pass to rhs and jacobian */


    

  tmin = x[0][0];		/* starting t value */
  tmax = x[S][0];	        /* final t value */
  delta_t = .1;

    
  for(int ii=0; ii < P; ii++){
    y[ii] = x[0][ii + 1]; // Initial values taken from data at time 0
    f_mat[0][ii+1] = x[0][ii+1];
  }

  t = tmin;             /* initialize t */

  

  int ss = 1;
  /* step to tmax from tmin */

  for (t_next = tmin + delta_t; t_next <= tmax; t_next += delta_t){

    while (t < t_next)	/* evolve from t to t_next */
      {
	gsl_odeiv_evolve_apply (evolve_ptr, control_ptr, step_ptr,
				&my_system, &t, t_next, &h, y);
      }

    // add to matrix if proper step size
    if(abs_val(f_mat[ss][0] - t) < .0001){
      for(int ii=0; ii < (P); ii++){
	f_mat[ss][ii+1] = y[ii];
      }
      ss = ss+1;
    }
  }

  /* all done; free up the gsl_odeiv stuff */
  gsl_odeiv_evolve_free (evolve_ptr);
  gsl_odeiv_control_free (control_ptr);
  gsl_odeiv_step_free (step_ptr);
}

int
main (void)
{
  int T = 100;
  int P = 3;
  double step_size = .1;
  int S = (int)(T / step_size);
  double p[3];
  double x[S][P+1];
  double f_mat[S][P+1];
  double tt=0.0;

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
  print_float_2d(S, P+1, f_mat);
}

/*************************** rhs ****************************/
/* 
   Define the array of right-hand-side functions y[i] to be integrated.
   The equations are:
   (S, I, R)
   s' = - beta*s*i/N dy[0]/dt = f[0] 
   i' = beta*s*i/N - gamma * i = f[1]
   r' = gamma*i = f[2]
  
   * params is a void pointer that is used in many GSL routines
   to pass parameters to a function
*/
int rhs (double t, const double y[], double f[], void *params_ptr){

  
  /* get parameter(s) from params_ptr; here, just a double */
  struct sir *my_params_pointer = params_ptr;
  double beta = my_params_pointer->params[0];
  double gamma = my_params_pointer->params[1];
  double N = my_params_pointer->N;
  
    
  /* evaluate the right-hand-side functions at t */
  f[0] = -beta * y[0] * y[1] / N;
  f[1] = beta * y[0] * y[1] / N - gamma * y[1];
  f[2] = gamma * y[1];

  return GSL_SUCCESS;		/* GSL_SUCCESS defined in gsl/errno.h as 0 */
}

/*************************** Jacobian ****************************/
/*
  Define the Jacobian matrix using GSL matrix routines.
  (see the GSL manual under "Ordinary Differential Equations") 
  
  * params is a void pointer that is used in many GSL routines
  to pass parameters to a function
*/
int
jacobian (double t, const double y[], double *dfdy,
          double dfdt[], void *params_ptr){
  
  /* get parameter(s) from params_ptr; here, just a double */
  struct sir *my_params_pointer = params_ptr;
  double beta = my_params_pointer->params[0];
  double gamma = my_params_pointer->params[1];
  double N = my_params_pointer->N;

  gsl_matrix_view dfdy_mat = gsl_matrix_view_array (dfdy, 3, 3); //TODO: make generic not 3x3

  gsl_matrix *m_ptr = &dfdy_mat.matrix;	/* m_ptr points to the matrix */

  /* fill the Jacobian matrix as shown */
  gsl_matrix_set (m_ptr, 0, 0, -beta * y[1] / N);	/* df[0]/dy[0] = beta * I /N*/
  gsl_matrix_set (m_ptr, 0, 1, -beta * y[0] / N);	/* df[0]/dy[1] = -beta * S / N */
  gsl_matrix_set (m_ptr, 0, 2, 0.0); /* df[0]/dy[2] = 0 */
  gsl_matrix_set (m_ptr, 1, 0, beta * y[1] / N); /* df[1]/dy[0] = beta * I / N  */
  gsl_matrix_set (m_ptr, 1, 1, beta * y[0] / N - gamma); /* df[1]/dy[1] = beta * S / N -gamma */
  gsl_matrix_set (m_ptr, 1, 2, 0.0);     /* df[1]/dy[2] = 0 */
  gsl_matrix_set (m_ptr, 2, 2, 0.0);     /* df[2]/dy[0] = 0 */
  gsl_matrix_set (m_ptr, 2, 2, gamma);     /* df[2]/dy[2] = gamma */

  /* set explicit t dependence of f[i] */
  dfdt[0] = 0.0;
  dfdt[1] = 0.0;
  dfdt[2] = 0.0;

  return GSL_SUCCESS;		/* GSL_SUCCESS defined in gsl/errno.h as 0 */
}

/* Comipliation command
gcc ode-test.c -lgsl -lgslcblas -lm
 */
