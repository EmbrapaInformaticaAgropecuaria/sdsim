testando <- function(x) {
  a <- .Call("teste", as.double(x))
  return(a)
}

step_solver <- function(ode, t0, tf, y, rtol = 1e-5, atol = 1e-6) {
  .Call("Lsoda", as.double(t0), as.double(tf), as.double(y), as.double(rtol), as.double(atol), environment())
}
