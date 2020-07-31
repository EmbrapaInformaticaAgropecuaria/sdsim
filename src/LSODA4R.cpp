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

extern "C" {
  SEXP initLSODA()
  {
    XPtr<LSODA> ptr(new LSODA(), true);
    return(ptr);
  }
}

void sys(double t, double *y, double *dydt) {
  // Get ODE function
  Function ode = ENV["ode"];

  vector<double> yR(y, y + NEQ);
  double parms = 1;
  
  // Evaluate differentials
  vector<double>result = as<vector<double>>(ode(t, yR, parms));
  for(int i = 0; i < NEQ; i++) {
    dydt[i] = result[i];
  }
}

extern "C" {
  SEXP runLSODA(SEXP obj, SEXP time_S, SEXP y_S, SEXP rtol_S, SEXP atol_S, SEXP env) {
    // Set global variables
    ENV = as<Environment>(env);
    NEQ = LENGTH(y_S);

    // Change SEXP to c++ types
    vector<double> time = as<vector<double>>(time_S);
    vector<double> y = as<vector<double>>(y_S);
    double rtol = as<double>(rtol_S);
    double atol = as<double>(atol_S);
    
    // Number of points to evaluate
    int np = time.size() - 1; 
    
    // Return vector (time_1 st1_1 st2_1 st3_1 ... time_np st1_np st2_np st3_np) 
    vector<double> Y(np * (NEQ + 1), 0);
    
    // lsoda_update return
    vector<double> out; 
  
    // LSODA object
    XPtr<LSODA> ptr(obj);
    
    int istate = ENV["istate"]; // Istate value
    string triggerT = ENV["triggerT"]; // trigger type
    string eventT = ENV["eventT"]; // event type
    
    for (int i = 0; i < np; i++) {
      // Calculate a state given previous state and time range
      double tin = time[i];
      ptr->lsoda_update(sys, NEQ, y, out, &tin, time[i + 1], &istate, nullptr, rtol, atol);
      vector<double> yOut(&out[1], &out[NEQ + 1]); // Discard out first element (it is always zero)

      // Check trigger
      double root = 1;
      if(triggerT == "function") { // If trigger is a function check root
        Function trigger = ENV["trigger"];
        root = as<double>(trigger(time[i], yOut, 1));
      } else if(triggerT == "numeric") { // If trigger is a timeEvent vector check if there is an event in time[i]
        vector<double> timeEvent = as<vector<double>>(ENV["trigger"]);
        for(int j = 0; j < timeEvent.size(); j++) {
          if(fabs(timeEvent[j] - time[i]) < atol) {
            root = 0;
            continue;
          }
        }
      }

      // Evaluate event function
      if(eventT == "function") {
        Function event = ENV["event"];
        if(root <= 0 || fabs(root) < atol) {
          yOut = as<vector<double>>(event(time[i], yOut, 1));
          istate = 1;
        }
      }
      
      ENV["istate"] = istate;
      
      // Save output
      Y[(NEQ + 1) * i] = time[i + 1];
      for (int j = 0; j < NEQ; j++) {
        Y[(NEQ + 1) * i + 1 + j] = yOut[j];
        y[j] = yOut[j];
      }
    }
    return wrap(Y);
  }
}