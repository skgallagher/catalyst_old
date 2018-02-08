/*
SKG
February 8, 2018
General helper functions for testing and better code practice
Trying to do things right..

Compilation:
gcc test-helper-functions.c helper-functions.c -o test-helper
 */



#include <stdio.h>
#include <stdlib.h>
#include "helper-functions.h"


void main(void){

  printf("test 1 x and y have identical els\n");
  // x and y elements are completely matching
  int Cx = 4, Cy = 2;
  int Nx = 10, Ny = 10;
  int Mx = 6, My = 3;
  double x[20][Mx], y[20][My];
  double step = .02;
  
  for(int ii=0; ii < Nx; ii++){
    x[ii][Cx] = ii * step;
    y[ii][Cy] = ii * step;
  }

  int matching_inds[Nx];
  double eps=.00001;
  int is_ordered=1;

  
  find_matching_inds(Cx, Cy,
		     Nx, Ny,
		     Mx, My,
		     x, y,
		     is_ordered, eps,
		     matching_inds);
  printf("x looks like\n");
  print_float_2d(Nx, Mx, x);
  printf("y looks like\n");
  print_float_2d(Ny, My, y);
  printf("the matching inds are\n");
  print_int_1d(Nx, matching_inds);
		     
  ///////////////////////
  printf("test 2 -- x is larger\n");
  // x is larger than y
  Nx = 11;
  for(int ii=0; ii < Nx; ii++){
    x[ii][Cx] = ii * step;
  }

  find_matching_inds(Cx, Cy,
		     Nx, Ny,
		     Mx, My,
		     x, y,
		     is_ordered, eps,
		     matching_inds);
  printf("x looks like\n");
  print_float_2d(Nx, Mx, x);
  printf("y looks like\n");
  print_float_2d(Ny, My, y);
  printf("the matching inds are\n");
  print_int_1d(Nx, matching_inds);
  int n;
  n= length_int_inds(Nx, matching_inds);
  printf("# non neg inds %d\n", n);

   ///////////////////////
  printf("test 3 -- x has duplicates\n");
  // x is 
  Nx = 10;
  for(int ii=0; ii < Nx; ii++){
    x[ii][Cx] = ii * step;
  }
  x[4][Cx] = .06;

  find_matching_inds(Cx, Cy,
		     Nx, Ny,
		     Mx, My,
		     x, y,
		     is_ordered, eps,
		     matching_inds);
  printf("x looks like\n");
  print_float_2d(Nx, Mx, x);
  printf("y looks like\n");
  print_float_2d(Ny, My, y);
  printf("the matching inds are\n");
  print_int_1d(Nx, matching_inds);

  ////////////////////
  printf("test 4 -- subsetting an array\n");
  int N_inds = 5;
  int inds[5] = {0, 0, 3, 7, 8};
  double new_x[20][Mx];
  
  subset_array_d(N_inds, Ny, inds,
		 Mx,
		 x, new_x);
  printf("x looks like\n");
  print_float_2d(Nx, Mx, x);
  printf("new x looks like\n");
  print_float_2d(N_inds, Mx, new_x);
		     
}
