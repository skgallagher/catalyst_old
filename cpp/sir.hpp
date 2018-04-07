#ifndef SIR_H
#define SIR_H


#include <iostream>
#include <boost/array.hpp>
#include <boost/numeric/odeint.hpp>
using namespace std;
using namespace boost::numeric::odeint;



typedef boost::array<double, 3> state_type; // e.g. (S, I, R)



class sir {
  std::array<double,3> m_params; // e.g. (beta, gamma, N)

public:
  sir( std::array<double,3> params ) : m_params(params) { }

  void operator() (state_type &x, state_type &dxdt, double /* t */){
    dxdt[0] = -1.0 * m_params[0] * x[0] * x[1] / m_params[2];
    dxdt[2] = m_params[1] * x[1];
    dxdt[1] = -1.0 * dxdt[0] - dxdt[2];
  }

};

struct sir_vals{
    std::vector< state_type >& m_states;
    std::vector< double >& m_times;

    sir_vals( std::vector< state_type > &states , std::vector< double > &times )
    : m_states( states ) , m_times( times ) { }

    void operator()( const state_type &x , double t )
    {
        m_states.push_back( x );
        m_times.push_back( t );
    }
};

vector<state_type> run_sir(sir sys, state_type init_vals,
		   runge_kutta4<state_type> stepper,
		   double step_size,
		   double T,
		   vector<state_type> cm_vals,
		   vector<double> times);


#endif
