oneStep <- function(ode, from, to, state, rtol = 1e-6, atol = 1e-6) {
  e <- environment(ode)
  naux <- length(e$aux)

  newEnv <- new.env(parent = parent.env(globalenv()))

  assign("out", c(), newEnv)
  assign("aux", rep(0, naux), newEnv)
  assign("ode", ode, newEnv)

  .Call("Lsoda_step", as.double(from), as.double(to), as.double(state), as.double(rtol), as.double(atol), newEnv)

  return(list(state = newEnv$out[-1], aux = newEnv$aux))
}

allSteps <- function(ode, time, y, rtol = 1e-6, atol = 1e-6) {
  e <- environment(ode)
  naux <- length(e$aux)
  
  newEnv <- new.env(parent = parent.env(globalenv()))
  
  assign("out", c(), newEnv)
  assign("aux", c(), newEnv)
  assign("ode", ode, newEnv)
  
  .Call("Lsoda", as.double(time), as.double(y), as.double(rtol), as.double(atol), newEnv)
  
  outDF <- as.data.frame(do.call(rbind, newEnv$out))
  colnames(outDF) <- (names(y))
  
  return(list(state = outDF, aux = newEnv$aux))
}
