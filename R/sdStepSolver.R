step_solver <- function(ode, t0, tf, y, naux = 0, rtol = 1e-6, atol = 1e-6) {
  newEnv <- new.env(parent = parent.env(globalenv()))

  assign("out", c(), newEnv)
  assign("aux", c(), newEnv)
  assign("func", ode, newEnv)

  .Call("Lsoda", as.double(t0), as.double(tf), as.double(y), as.integer(naux), as.double(rtol), as.double(atol), newEnv)

  return(list(state = newEnv$out[-1], aux = newEnv$aux))
}
