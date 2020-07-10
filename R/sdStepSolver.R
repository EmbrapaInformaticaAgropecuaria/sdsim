stepSolver <- function(ode, t0, tf, y, rtol = 1e-6, atol = 1e-6) {
  e <- environment(ode)
  naux <- length(e$aux)
  
  newEnv <- new.env(parent = parent.env(globalenv()))
  
  
  # func <- function (t, st, c)
  # {
  #   st <- as.list(st)
  #   names(st) <- c("y1", "y2", "dy1", "dy2")
  #     
  #   d1 <- ((st$y1 + 1.9)^2 + st$y2^2)^(3/2)
  #   
  #   d2 <- ((st$y1 - 1.2)^2 + st$y2^2)^(3/2)
  #   
  #   dy3 <- st$y1 + 2 * st$dy2 - 3.2 * (st$y1 + 3.8)/d1 - 
  #     9.4 * (st$y1 - 2.5)/d2
  #   dy4 <- st$y2 - 2 * st$dy1 - 3.2 * st$y2/d1 - 2.1 * 
  #     st$y2/d2
  #   
  #   return(c(y1 = st$dy1, y2 = st$dy2, dy1 = dy3, dy2 = dy4))
  # }
  
  assign("out", c(), newEnv)
  assign("aux", rep(0, naux), newEnv)
  assign("ode", ode, newEnv)

  .Call("Lsoda", as.double(t0), as.double(tf), as.double(y), as.double(rtol), as.double(atol), newEnv)

  return(list(state = newEnv$out[-1], aux = newEnv$aux))
}
