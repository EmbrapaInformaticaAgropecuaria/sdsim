runStep <- function(ode, trigger, event, time, y, rtol = 1e-6, atol = 1e-6) {
  # New environment for ODE, trigger and event functions
  newEnv <- new.env(parent = parent.env(globalenv()))
  assign("ode", ode, newEnv)
  
  if(is.function(trigger)) {
    assign("trigger", trigger, newEnv)
    assign("triggerQ", TRUE, newEnv)
  }
  else {
    assign("triggerQ", FALSE, newEnv)
  }
    

  if(is.function(event)) {
    assign("event", event, newEnv)
    assign("eventQ", TRUE, newEnv)
  }
  else {
    assign("eventQ", FALSE, newEnv)
  }
  
  out <- .Call("Lsoda", as.double(time), as.double(y), as.double(rtol), as.double(atol), newEnv)

  return(list(state = out))
}
