runStep <- function(ode, time, y, rtol = 1e-6, atol = 1e-6) {
  e <- environment(ode)
  naux <- length(e$aux)
  
  newEnv <- new.env(parent = parent.env(globalenv()))
  
  assign("out", c(), newEnv)
  assign("ode", ode, newEnv)
  
  .Call("Lsoda", as.double(time), as.double(y), as.double(rtol), as.double(atol), newEnv)

  return(list(state = newEnv$out))
}
