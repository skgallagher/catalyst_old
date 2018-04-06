/*
Random numbers and fun times

April 8, 2018
SKG

Compilation command:

SET THE SEED ONCE IN MAIN

#include <cstdlib>
#include <sys/time.h>
 struct timeval t1;
  gettimeofday(&t1, NULL);
  srand(t1.tv_usec * t1.tv_sec);
 */


#include "random.hpp"


/* Return an integer corresponding to the drawn multinomial
   category.  We are drawing a multinomial of size 1
INPUTS: 
probs -- a 1d array of length K.  nonneg reals between 0 and 1 and add to 1
K - number of categories/ length of probs
OUTPUTS: an integer between 0 and K-1 corresponding to the draw
*/
int draw_multinom(double probs[], int K){
  int multinom_draw;
  double cum_prob = 0.0;
  double draw = double_rand(0.0, 1.0);
  /* If the draw is in the 1st interval of cumulative probability, then 1 is the new status,
  If it is in the 2nd interval, then 2 is new status...,
  If it is in the Nth interval, N is the new status
  */
  for(int kk=0; kk < K; kk++){
    cum_prob = cum_prob + probs[kk];
    if (draw < cum_prob){
      multinom_draw = kk;
      break;
    }
  }
  return multinom_draw;

}


double double_rand(double min, double max ){
  double scale = rand() / (double) RAND_MAX; /* [0, 1.0] */
  return min + scale * ( max - min );      /* [min, max] */
}

