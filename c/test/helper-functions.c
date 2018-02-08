/*
SKG
February 8, 2018
General helper functions for testing and better code practice
Trying to do things right..
 */



#include <stdio.h>
#include <stdlib.h>
#include "helper-functions.h"


/* Subset an array of doubles based on the indices given
INPUTS:
N_inds - number of indices to be subsetted to
N_y - number of total obs currently in y.  
inds - array of indices to subset y to
My - number of columns in y and new_y
y - array of dim N_y x M
new_y - subsetted array of dim N_inds x M_y
OUTPUTS:
new_y subsetted y to given inds
*/
void subset_array_d(int N_inds, int Ny, int inds[N_inds],
		  int My,
		  double y[Ny][My],
		  double new_y[N_inds][My]){
  for(int ii=0; ii < N_inds; ii++){
    for(int mm = 0; mm < My; mm++){
      new_y[ii][mm]= y[inds[ii]][mm];
    }
  } 
}


/* return number of indices that are >= 0 
INPUTS: 
A - total length of a
a - the array
*/
int length_int_inds(int A, int a[]){
  int num_non_neg = 0;
  for(int ii=0; ii < A; ii++){
    if (a[ii] >= 0){
      num_non_neg++;
    }
  }
  return num_non_neg;
}


/*
Return the row indices of elements in array y in column Cy that match the elements in array x, column Cx.  If element in Cx has no matching index, return -1.  We assume there is at most one matching element in y.
INPUTS:
Cx - the column index of x we are looking at
Cy - the column index of y we are looking at
Nx - number of rows in x
Ny - number of rows in y
Mx - number of columns in x
My - number of columns in y
x - array of dimension Nx x Mx
y - array of dimension Ny x My
is_ordered - if 1, we assume the elements in column Cx in x AND the elements in column Cy in y are in non-decreasing order.
eps - a very small number
OUTPUTS: 1d array matching_inds of length Nx.  Entry i corresponds to the index of the matching element of x found in y.
 */
void find_matching_inds(int Cx, int Cy,
			int Nx, int Ny,
			int Mx, int My,
			double x[][Mx], double y[][My],
			int is_ordered, double eps,
			int matching_inds[]){
  double el_x, el_y;
  double abs_diff;
  int jj_start=0;
  if(is_ordered == 1){
    for(int ii=0; ii < Nx; ii++){
      el_x = x[ii][Cx];
      for(int jj=jj_start; jj < Ny; jj++){
	el_y = y[jj][Cy];
	if(abs_val(el_x - el_y) < eps){
	  matching_inds[ii] = jj;
	  jj_start = jj;
	  break;
	} else if(el_y > el_x){
	  matching_inds[ii] = -1;
	  jj_start = jj;
	  break;
	}
	matching_inds[ii] = -1;
	jj_start = jj;
      }

    }
  }
}


/*
Absolute value function for a double x
 */
double abs_val(double x){
  if( x < 0.0){
    return -1.0 * x;
  } else{
    return x;
  }
}

/* print 1d array of doubles */
void print_float_1d(int N, double a[]){
  for(int ii=0; ii < N; ii++){
    printf("%.2f ", a[ii]);
  }
  printf("\n");
}

/* print 1d array of doubles */
void print_int_1d(int N, int a[]){
  for(int ii=0; ii < N; ii++){
    printf("%d ", a[ii]);
  }
  printf("\n");
}


/* print 2d arrray of doubles */
void print_float_2d(int M, int N, double a[M][N]){
  for(int ii=0; ii < M; ii++){
    for(int jj=0; jj < N; jj++){
      printf("%.6f ", a[ii][jj]);
    }
    printf("\n");
  }
}
