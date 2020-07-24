#include <Rcpp.h>
#include <vector>
#include "../inst/include/LSODA.h"
#include <iostream>
#include <R.h>
#include <Rdefines.h>

using namespace Rcpp;
using namespace std;

Environment ENV;

int NEQ;

void sys(double t, double *y, double *dydt) {
  Function ode = ENV["ode"];

  vector<double> yR(y, y + NEQ);
  double parms = 1;
  vector<double>result = as<vector<double>>(ode(t, yR, parms));

  for(int i = 0; i < NEQ; i++) {
    dydt[i] = result[i];
  }
}

extern "C" {
  SEXP Lsoda(SEXP time_S, SEXP y_S, SEXP rtol_S, SEXP atol_S, SEXP env) {
    ENV = as<Environment>(env);
    NEQ = LENGTH(y_S);

    vector<double> time = as<vector<double>>(time_S);
    vector<double> y = as<vector<double>>(y_S);
    double rtol = as<double>(rtol_S);
    double atol = as<double>(atol_S);
    int np = time.size() - 1;
    
    Function trigger = ENV["trigger"];
    Function event = ENV["event"];
    
    vector<double> Y(np * (NEQ + 1),0);
    vector<double> out;
    
    LSODA lsoda;
    
    for (int i = 0; i < np; i++) {
      int istate = 1;

      lsoda.lsoda_update(sys, NEQ, y, out, &time[i], time[i + 1], &istate, nullptr, rtol, atol);
      vector<double> yOut(&out[1], &out[NEQ + 1]);

      bool triggerQ = as<bool>(trigger(time[i + 1], yOut, 1));
      if(triggerQ == false) {
        yOut = as<vector<double>>(event(time[i + 1], yOut, 1));
      }
      
      Y[(NEQ + 1) * i] = time[i];
      for (int j = 0; j < NEQ; j++) {
        Y[(NEQ + 1) * i + 1 + j] = yOut[j];
        y[j] = yOut[j];
      }
    }
    
    ENV["out"] = Y;
    return 0;
  }
}



