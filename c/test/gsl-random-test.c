/*
Testing the gsl package random distributions
SKG
January 9, 2017
 */

#include <stdio.h>
#include <string.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <sys/time.h>


gsl_rng * r; /* global generator */

unsigned long int random_seed();

int main(){

  unsigned long int my_seed;


  const gsl_rng_type * T;
  gsl_rng * r;

  gsl_rng_env_setup();
  T = gsl_rng_default;
  my_seed = random_seed();
  printf("my seed %d\n", my_seed);
  gsl_rng_default_seed = my_seed;
  r = gsl_rng_alloc (T);

  printf ("generator type: %s\n", gsl_rng_name (r));
  printf ("seed = %lu\n", my_seed);
  printf ("first value = %lu\n", gsl_rng_get (r));

  int N = 10^7;
  double p[4] = {1, 1, 1, 1};
  unsigned int m[4];


  unsigned int m_array[N][4];


  for (int ii=0; ii < N; ii++){

    gsl_ran_multinomial(r, 4, 1, p, m);
    for( int jj=0; jj < 4; jj++){
      m_array[ii][jj] = m[jj];
    }
    
  }
  printf("[%d, %d, %d, %d]\n", m_array[N-1][0], m_array[N-1][1],
	 m_array[N-1][2], m_array[N-1][3]);

  
  gsl_rng_free (r);
  
 
}


unsigned long int random_seed(){
 struct timeval tv;
 gettimeofday(&tv,0);
 return (tv.tv_sec + tv.tv_usec);
}

/* Comipliation command
gcc gsl-random-test.c -lgsl -lgslcblas -lm
 */

/* random seed ref
https://stackoverflow.com/questions/9768519/gsl-uniform-random-number-generator
 */
