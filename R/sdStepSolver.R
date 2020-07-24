runStep <- function(ode, trigger, event, time, y, rtol = 1e-6, atol = 1e-6) {
  e <- environment(ode)
  naux <- length(e$aux)
  
  newEnv <- new.env(parent = parent.env(globalenv()))
  
  assign("out", c(), newEnv)
  assign("ode", ode, newEnv)
  
  if(!is.null(trigger)) 
    assign("trigger", trigger, newEnv)
  else
    assign("trigger", function(time, st, parms) {return(1)}, newEnv)

  if(!is.null(event)) 
    assign("event", event, newEnv)
  else
    assign("event", function(time, st, parms) {return(1)}, newEnv)

  
  .Call("Lsoda", as.double(time), as.double(y), as.double(rtol), as.double(atol), newEnv)

  return(list(state = newEnv$out))
}
