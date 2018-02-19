/* function prototypes */
int rhs (double t, const double y[], double f[], void *params_ptr);
int jacobian (double t, const double y[], double *dfdy,
              double dfdt[], void *params_ptr);
void ode_wrapper(int S, int P, double x[][P+1],
		 double p[], double f_mat[][P+1]);
void ode_vals(int T, int P, int D, double step_size,
	      double N, double init_vals[],
	      double eps_abs, double eps_rel,
	      double p[], double f_mat[][P+1]);
