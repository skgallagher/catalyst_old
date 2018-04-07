#ifndef RANDOM_H
#define RANDOM_H

#include <iostream>
#include <cstdlib>
#include <sys/time.h>

using namespace std;

int draw_multinom(double probs[], int K);
double double_rand(double min, double max );

#endif
