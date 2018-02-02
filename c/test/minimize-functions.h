/*
February 2, 2018
Sorting out the functions for the minimizer
This is the header file
SKG
 */

void lin_reg(int S, int P, double x[][P+1],
	     double p[], double f_mat[][P+1]);

void print_float_2d(int M, int N, double a[M][N]);

double data_to_params(int D, int S, int P, double betas[],
		      double data[S][P+1], double params[]);

void params_to_data(double params[], int S, int P,
		    double data[S][P+1]);

double sum_squares(const gsl_vector *v, void *params);

double extract_ft(double t, int p, int S, int P,
		  double f_mat[][P+1], double eps);

double abs_val(double x);

void print_float_1d(int N, double a[]);

void optimize_ode(int S, int P, int D, double data[][P+1],
		  double init_params[], double thresh,
		  double init_step_size, int n_iters,
		  int print_steps, double best_params[]);


