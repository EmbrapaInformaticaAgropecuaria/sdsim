#include <Rcpp.h>
#include <vector>
#include "../inst/include/LSODA.h"
#include <iostream>
#include <R.h>
#include <Rdefines.h>

using namespace Rcpp;

Environment ENV;

int NEQ;

extern "C" {
  SEXP teste(SEXP x) {
    LSODA a;
    bool r = a.abs_compare(2,3);
    return x;
  }
}

// SEXP func(SEXP a) {
//   lsoda.lsoda_update(sys, NEQ, y, out, &t0, tf, &istate, nullptr, rtol, atol)
//   return 2 * a;
// }


// 
// void sys(double t, double *y, double *dydt) {
//   Function f = ENV["ode"];
//   
//   std::vector<double> yR(y,y + NEQ);
//   std::vector<double> result = as<std::vector<double>>(f(t, yR));
//   for(int i = 0; i < NEQ; i++) {
//     dydt[i] = result[i];
//   }
// }
// 
// extern "C" {
// 	SEXP Lsoda(SEXP t0_S, SEXP tf_S, SEXP y_S, SEXP rtol_S, SEXP atol_S, SEXP env) {
// 	  ENV = as<Environment>(env);
// 	  NEQ = LENGTH(y_S);
// 	  
// 	  double t0 = as<double>(t0_S);
// 	  double tf = as<double>(tf_S);
// 	  std::vector<double> y = as<std::vector<double>>(y_S);
// 	  double rtol = as<double>(rtol_S);
// 	  double atol = as<double>(atol_S);
// 	  int istate = 1;
// 
// 	  std::vector<double> out;
// 	  SEXP yout;
// 	  PROTECT(yout = NEW_NUMERIC(NEQ));
// 
//     lsoda.lsoda_update(sys, NEQ, y, out, &t0, tf, &istate, nullptr, rtol, atol);
// 
// 	  for (int i = 0; i < NEQ; i++) {
// 	    DOUBLE_DATA(yout)[i] = out[i+1];
// 	  }
// 	  UNPROTECT(1);
// 	  return yout;
// 	}
// }
