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

int NAUX;

// vector<vector<double>> AUX;
// 
// int counter = 0;

void sysLS(double t, double *y, double *dydt) {
  Function f = ENV["ode"];
  
  NumericVector yR(y, y + NEQ);
  double parms = 0;
  NumericVector result = as<NumericVector>(f(t, yR, parms));
  for(int i = 0; i < NEQ; i++) {
    dydt[i] = result[i];
  }
  
  // if(NAUX != 0) {
  //   AUX[counter].resize(NAUX);
  //   
  //   for(int i = 0; i < NAUX; i++) {
  //     AUX[counter][i] = result[i + NEQ];
  //   }
  // }

}

void sys(double t, double *y, double *dydt) {
  Function f = ENV["ode"];

  NumericVector yR(y, y + NEQ);
  double parms = 0;
  NumericVector result = as<NumericVector>(f(t, yR, parms));
  for(int i = 0; i < NEQ; i++) {
    dydt[i] = result[i];
  }

    NumericVector aux(NAUX);
  
  for(int i = 0; i < NAUX; i++) {
    aux[i] = result[i + NEQ];
  }
  
  ENV["aux"] = aux;
}

extern "C" {
  SEXP Lsoda_step(SEXP t0_S, SEXP tf_S, SEXP y_S, SEXP rtol_S, SEXP atol_S, SEXP env) {
	  ENV = as<Environment>(env);
	  NEQ = LENGTH(y_S);
	  NAUX = LENGTH(ENV["aux"]);

	  double t0 = as<double>(t0_S);
	  double tf = as<double>(tf_S);
	  std::vector<double> y = as<std::vector<double>>(y_S);
	  double rtol = as<double>(rtol_S);
	  double atol = as<double>(atol_S);
	  int istate = 1;

	  std::vector<double> out;
	  SEXP yout;
	  // PROTECT(yout = NEW_NUMERIC(NEQ));

    lsoda.lsoda_update(sys, NEQ, y, out, &t0, tf, &istate, nullptr, rtol, atol);
// 
// 	  for (int i = 0; i < NEQ; i++) {
// 	    DOUBLE_DATA(yout)[i] = out[i+1];
// 	  }

	  ENV["out"] = out;

	  // UNPROTECT(1);
	  return 0;
	}
}

extern "C" {
  SEXP Lsoda(SEXP time_S, SEXP y_S, SEXP rtol_S, SEXP atol_S, SEXP env) {
    ENV = as<Environment>(env);
    NEQ = LENGTH(y_S);
    NAUX = 2;
    
    std::vector<double> time = as<std::vector<double>>(time_S);
    std::vector<double> y = as<std::vector<double>>(y_S);
    double rtol = as<double>(rtol_S);
    double atol = as<double>(atol_S);
    
    int np = time.size();
    int istate = 1;

    vector<vector<double>> Y(np);
    // AUX.resize(np);

    Y[0] = y;
    
    for (int i = 0; i < np - 1; ++i) {
      
      vector<double> out;

      lsoda.lsoda_update(sysLS, NEQ, Y[i], out, &time[i], time[i + 1], &istate, nullptr, rtol, atol);
      
      Y[i + 1].resize(NEQ);
      for (int j = 0; j < NEQ; j++) {
        Y[i + 1][j] = out[j+1];
      }
    }
    
    ENV["out"] = Y;
    // ENV["aux"] = AUX;
    return 0;
  }
}



