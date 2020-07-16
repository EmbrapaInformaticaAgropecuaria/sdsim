#include <Rcpp.h>
#include <vector>
#include "../inst/include/LSODA.h"
#include <iostream>
#include <R.h>
#include <Rdefines.h>

using namespace Rcpp;

LSODA lsoda;

Environment ENV;

int NEQ;

void sys(double t, double *y, double *dydt) {
  Function f = ENV["ode"];
  
  NumericVector yR(y, y + NEQ);
  double parms = 0;
  NumericVector result = as<NumericVector>(f(t, yR, parms));
  for(int i = 0; i < NEQ; i++) {
    dydt[i] = result[i];
  }
}

extern "C" {
  SEXP Lsoda(SEXP time_S, SEXP y_S, SEXP rtol_S, SEXP atol_S, SEXP env) {
    ENV = as<Environment>(env);
    NEQ = LENGTH(y_S);
    
    std::vector<double> time = as<std::vector<double>>(time_S);
    std::vector<double> y = as<std::vector<double>>(y_S);
    double rtol = as<double>(rtol_S);
    double atol = as<double>(atol_S);
    
    int np = time.size() - 1;
    int istate = 1;

    vector<double> Y(np * (NEQ + 1),0);

    for (int i = 0; i < np; i++) {
      
      vector<double> out;

      lsoda.lsoda_update(sys, NEQ, y, out, &time[i], time[i + 1], &istate, nullptr, rtol, atol);
      
      Y[(NEQ + 1) * i] = time[i];
      for (int j = 0; j < NEQ; j++) {
        Y[(NEQ + 1) * i + 1 + j] = out[j+1];
        y[j] = out[j+1];
      }
    }
    
    ENV["out"] = Y;
    return 0;
  }
}



