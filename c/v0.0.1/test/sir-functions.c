/*
SKG
February 6, 2018
Catalyst v0.0.1
Functions for making SIR curves for given beta, gamma, init vals
Made so as to easily fit into the minimizer function

Notes: This was adapted from sir-test-2.c which was adapted from some smart guy on the internet
Look at that file for more details

Compile and link with:
gcc sir-functions.c minimize-functions.c -lgsl -lgslcblas -lm -o run-sir

TODO:
 Don't hard code derivatives
 */

#include <stdio.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_odeiv.h>
#include "sir-functions.h"
#include "helper-functions.h"

/* SIR struct */
/* TODO: make generic for any ode */
struct si_params {
  double params[100]; // {beta, gamma, ...}
  double N;
};



/* 
This is a wrapper for the ODE functions.  This will integrate with the optimizer to find best fit of parameters in the ODE. 
First make function based on parameters
f(x; p) that returns matrix from steps x[0][0], x[t+ step_size][0], ..., x[T][0]
INPUTS:
T - max time
P - number of differential equations to minimize = number of Ys (e.g. in SIR, 3)
D - number of disease parameters to MINIMIZE (so no N)
step_size - for the ODE integrater.  how big should our steps be?
N - number of agents
init_vals - array of length P of the initial values at time 0
eps_abs - absolute error requested 
eps_rel - relative error requested 
p = [param1, param2, ...] (like {beta, gamma, N}).  Currently this is [beta, gamma, N]
f_mat -- output of data double array of dimension (T / step_size + 1) x P+1
OUTPUTS:
modified f_mat with function at given values of t for given parameters p
TODO:  Make this function into smaller pieces
 */
void ode_vals(int T, int P, int D, double step_size,
	      double N, double init_vals[],
	      double eps_abs, double eps_rel,
	      double p[], double f_mat[][P+1]){
  int dimension;
  dimension = P;

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
  struct si_params disease_params;

   // printf("beta %.3f gamma %.3f\n", p[0], p[1]);
  for(int ii=0; ii < D; ii++){
    disease_params.params[ii] = p[ii];  // put the disease params in the struct
  }
  disease_params.N = N; // N
  double y[P];			/* current solution vector */
  // Fill y initial values
  f_mat[0][0] = 0.0;
  for(int ii=0; ii < P; ii++){
    y[ii] = init_vals[ii];
    f_mat[0][ii+1] = y[ii];
  }

  /* printf("T is %d\n", T); */
  
  double t, t_next;		/* current and next independent variable */
  double tmin, tmax, delta_t;	/* range of t and step size for output */

  double h = 1e-6;		/* starting step size for ode solver */

  /* load values into the my_system structure */
  my_system.function = rhs;	/* the right-hand-side functions dy[i]/dt */
  my_system.jacobian = jacobian;	/* the Jacobian df[i]/dy[j] */
  my_system.dimension = dimension;	/* number of diffeq's */
  my_system.params = &disease_params;	/* parameters to pass to rhs and jacobian */

 
  tmin = 0.0;		/* starting t value */
  tmax = (double)T;	        /* final t value */
  /* printf("tmax is %.3f\n", tmax); */
  delta_t = step_size;
  t = tmin;             /* initialize t */
  int ss = 1;
  for (t_next = tmin + delta_t; t_next <= (tmax+delta_t); t_next += delta_t){
    while (t < t_next){	/* evolve from t to t_next */
	gsl_odeiv_evolve_apply (evolve_ptr, control_ptr, step_ptr,
				&my_system, &t, t_next, &h, y);
    }
    f_mat[ss][0] = t_next;
    for(int ii=0; ii < (P); ii++){
      f_mat[ss][ii+1] = y[ii];
    }
    /* printf("current time is %.3f\n", f_mat[ss][0]); */
    ss = ss+1;
  }

  /* all done; free up the gsl_odeiv stuff */
  gsl_odeiv_evolve_free (evolve_ptr);
  gsl_odeiv_control_free (control_ptr);
  gsl_odeiv_step_free (step_ptr);

  
  
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
  struct si_params *my_params_pointer = params_ptr;
  double beta = my_params_pointer->params[0];
  double gamma = my_params_pointer->params[1];
  double N = my_params_pointer->N;

  //  printf("beta %.3f gamma %.3f N %3f\n", beta, gamma, N);
    
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
int jacobian (double t, const double y[], double *dfdy,
          double dfdt[], void *params_ptr){
  
  /* get parameter(s) from params_ptr; here, just a double */
  struct si_params *my_params_pointer = params_ptr;
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
