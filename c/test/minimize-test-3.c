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
We attempt to minimize f(x) = b0x + b1 where b0 = 4 and b1 =1
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




/* 
First make function based on parameters
f(x; b0, b1) that returns matrix from steps x[0], x[1], ..., x[S]
p = [b0, b1]
S = total number of obs
P = total number of dimensions to minimize genrally 1
 */
void lin_reg(int S, int P, double x[][P+1],
	     double p[], double f_mat[S][P+1]){
  for(int ss=0; ss < S; ss++){
    f_mat[ss][0] = x[ss][0];
    for(int pp=1; pp <= P; pp++){
      f_mat[ss][pp] = p[0] + x[ss][pp] * p[1];
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
  printf("converted data\n");
  print_float_2d(S, P+1, data);

  // extract the min_vars from v
  double min_vars[D];
  for(int ii=0; ii < D; ii++){
    min_vars[ii] = gsl_vector_get(v, ii);
  }
  
  
  // Compute the true function given v
  double f_mat[S][P+1];
  lin_reg(S, P, data,
	  min_vars, f_mat);

  // the actual sum of squares
  double SSE = 0.0;
  double f_x = 0.0;
  for(int ss=0; ss < S){
    for(pp=1; pp < (P+1)){
      double t;
      t = data[ss][0];
      f_t = extract_ft(t, S, p, f_mat);
      SSE = SSE + pow(data[ss][pp] - f_x);
    }
  }
  
  
  

  return SSE;

}

// probably a bit slow
double extract_ft(double t, int S, int p, double f_mat[S][P+1], double eps){
  for (int ss=0; ss < S; ss++){
    if( f_mat[ss][0] - t < eps |
	-f_mat[ss][0] + t > eps){
      f_mat[ss][p+1];
    }
  }
}


void print_float_1d(int N, double a[]){
  for(int ii=0; ii < N; ii++){
    printf("%.2f ", a[ii]);
  }
  printf("\n");
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

  
  printf("converted params\n");
  print_float_1d(3+D+S*(P+1), params);
  printf("converted data\n");
  print_float_2d(S, P+1, data);
  double p[2] = {4.0, 1.0};
  lin_reg(S, P, data,
	  p, f_mat);

  printf("sum of squares\n");
  sum_squares(params);
  
  


  
  return 0;
}
