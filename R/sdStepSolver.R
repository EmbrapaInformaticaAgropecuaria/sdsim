initLSODA <- function() {
  obj <- .Call("initLSODA", PACKAGE = "liblsoda")
}

runLSODA <- function(obj, ode, aux, auxLength, times, state, trigger = NULL, event = NULL, atol = 1e-6, rtol = 1e-6) {
  # New environment for ODE, istate, trigger and event
  newEnv <- new.env(parent = parent.env(globalenv()))
  assign("aux", NULL, newEnv)
  assign("ode", ode, newEnv)
  assign("istate", obj$istate, newEnv)

  if(!is.null(trigger)) {
    assign("trigger", trigger, newEnv)
    if(is.function(trigger)) {
      assign("triggerT", "function", newEnv)
    } else if (is.numeric(trigger)) {
      assign("triggerT", "numeric", newEnv)
      triggerInTime <- trigger[which(trigger > times[1] & trigger < times[length(times)])]
      times <- sort(bazar::almost.unique(c(triggerInTime, times)))
    }
  } else {
    assign("triggerT", "none", newEnv)
  }

  if(!is.null(event)) {
    assign("event", event, newEnv)
    if(is.function(event))
      assign("eventT", "function", newEnv)
  } else {
    assign("eventT", "none", newEnv)
  }

  if(!is.null(aux)) {
    assign("auxFunc", aux, newEnv)
    assign("auxLength", auxLength, newEnv)
    assign("auxT", "true", newEnv)
  } else {
    assign("auxT", "false", newEnv)
    assign("auxLength", 0, newEnv)
  }

  out <- .Call("runLSODA", obj$lsoda, as.double(times), as.double(state), as.double(rtol), as.double(atol), newEnv, PACKAGE = "liblsoda")
  return(list(state = out, aux = newEnv$aux, istate = newEnv$istate))
}