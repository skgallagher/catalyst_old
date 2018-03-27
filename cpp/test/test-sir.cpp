#include <iostream>
#include <boost/array.hpp>

#include <boost/numeric/odeint.hpp>

using namespace std;
using namespace boost::numeric::odeint;



typedef boost::array< double , 3 > state_type;



class sir {
  std::array<double,3> m_params;

public:
  sir( std::array<double,3> params ) : m_params(params) { }

  void operator() ( const state_type &x, state_type &dxdt, const double /* t */){
    dxdt[0] = -1.0 * m_params[0] * x[0] * x[1] / m_params[2];
    dxdt[2] = m_params[1] * x[1];
    dxdt[1] = -1.0 * dxdt[0] - dxdt[2];
  }

};


void write_sir( const state_type &x , const double t )
{
    cout << t << '\t' << x[0] << '\t' << x[1] << '\t' << x[2] << endl;
}

int main(int argc, char **argv)
{
    state_type x = { 950.0 , 50.0 , 0.0 }; // initial conditions
    runge_kutta4< state_type > stepper;
    std::array<double,3> params;
    params[0] = .1;
    params[1] = .03;
    params[2] = 1000.0;
    sir sys(params);
    
    integrate_const( stepper, sys , x , 0.0 , 100.0 , 1.0, write_sir);
}
