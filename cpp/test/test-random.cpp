/*
Testing random draws with boost,bernoulli, binomial, and multinomial

SKG
March 28, 2018
 */

#include <iostream>
#include <boost/random/uniform_real_distribution.hpp>
#include <boost/random.hpp>
#include <ctime>
#include <random>
using namespace std;

int draw_multinom(double probs[], int K, auto rng);

/*
Thanks Stack Exchange!
https://codereview.stackexchange.com/questions/109260/seed-stdmt19937-from-stdrandom-device
 */
auto RandomlySeededMersenneTwister () {
    // Magic number 624: The number of unsigned ints the MT uses as state
    std::vector<unsigned int> random_data(624);
    std::random_device source;
    std::generate(begin(random_data), end(random_data), [&](){return source();});
    std::seed_seq seeds(begin(random_data), end(random_data));
    std::mt19937 seededEngine (seeds);
    return seededEngine;
}


http://www.boost.org/doc/libs/1_37_0/libs/random/random_demo.cpp



int main(){

  

  auto rng = RandomlySeededMersenneTwister();
  int multinom_draw;
  int K=2;
  double probs[K]= {.5, .5};
  for(int ii=0; ii < 10; ii++){
    multinom_draw = draw_multinom(probs, K, rng);
    cout << multinom_draw << '\n';
  }

  return 0;
}

/* Return an integer corresponding to the drawn multinomial
   category.  We are drawing a multinomial of size 1
INPUTS: 
probs -- a 1d array of length K.  nonneg reals between 0 and 1 and add to 1
K - number of categories/ length of probs
OUTPUTS: an integer between 0 and K-1 corresponding to the draw
*/
int draw_multinom(double probs[], int K, auto rng){
  int multinom_draw;
  double cum_prob = 0.0;

  boost::random::uniform_real_distribution<> unif(0.0, 1.0);
  double draw = unif(rng);
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
