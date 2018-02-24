/*
SKG
February 8, 2018
General helper functions for testing and better code practice
Trying to do things right..
 */



#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
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
  int jj_start=0;
  if(is_ordered == 1){
    for(int ii=0; ii < Nx; ii++){
      el_x = x[ii][Cx];
      /* printf("element x is %.3f\n", el_x); */
      for(int jj=jj_start; jj < Ny; jj++){
	/* printf("jj is %d\n", jj); */
	/* printf("Ny is %d\n", Ny); */
	el_y = y[jj][Cy];
	/* printf("element y is %.3f\n", el_y); */
	/* printf("x: %.3f y: %.3f\n", el_x, el_y); */
	if(abs_val(el_x - el_y) < eps){
	  matching_inds[ii] = jj;
	  jj_start = jj;
	  break;
	} else if(el_y > el_x){
	  /* printf("y > x"); */
	  matching_inds[ii] = -1;
	  jj_start = jj;
	  break;
	}
	matching_inds[ii] = -1;
	/* jj_start = jj; */
	/* printf("jj_start is %d\n ", jj_start); */
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




void prt(GArray* a) {
 int ii;
 for (ii = 0; ii < a->len; ii++){
   printf("%d ", g_array_index(a, int, ii));
 }
 printf("\n");
}


void print(gpointer key, gpointer value, gpointer data) {
  printf("Here are some neighbors of: %d", *(gint*)key);
  printf("\n");
 
  printf("The last item is '%d'\n",
	 *(gint*)g_slist_last(value)->data); 
}


/*
Count the number of agents in each compartment at each time step, for each run
INPUTS:
ll - current run
T - total number of time steps.  
N - total number of agents
K - total number of compartments labeled 0, 1, ..., K-1
L - total number of runs 
agent_status - TxN array where entry ij is the the jth's agent's status at time i
compt_counts - LxTxK array where entry ijk is the number of agents in compartment k at time j and run i
OUTPUTS: updated compt_counts for run ll
 */
void count_compts(int ll,
		  int T, int N, int K, int L,
		  int agent_status[][N],
		  int compt_counts[][T][K]){
  
  int counter;

  for(int kk=0; kk < K; kk++){
    for(int tt=0; tt < T; tt++){
      counter = 0;
      for(int ii=0; ii < N; ii++){
	if(agent_status[tt][ii] = kk){
	  counter++
	}
      }
      compt_counts[ll][tt][kk] = counter;
    }
  }
}
