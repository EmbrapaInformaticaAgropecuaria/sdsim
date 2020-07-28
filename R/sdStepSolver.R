initLSODA <- function() {
  obj <- .Call("initLSODA")
}

runLSODA <- function(obj, ode, trigger, event, time, y, rtol = 1e-6, atol = 1e-6) {
  # New environment for ODE, trigger and event functions
  newEnv <- new.env(parent = parent.env(globalenv()))
  assign("ode", ode, newEnv)
  assign("istate", obj$istate, newEnv)
  
  if(!is.null(trigger)) {
    assign("trigger", trigger, newEnv)
    if(is.function(trigger)) {
      assign("triggerT", "function", newEnv)
    } else if (is.numeric(trigger)) {
      assign("triggerT", "numeric", newEnv)
      if(!all(trigger %in% time) && trigger >= time[1] && trigger <= time[length(time)]) {
        time <- checkEventTime(trigger, time)
      }
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
  out <- .Call("runLSODA", obj$lsoda, as.double(time), as.double(y), as.double(rtol), as.double(atol), newEnv)

  return(list(state = out, istate = newEnv$istate))
}

checkEventTime <- function(trigger, time) {
  checkInterval <- function(trigger) {
    if(trigger >= time[1] && trigger <= time[length(time)]) {
      sort(c(trigger, time))
    }
  }
  
  res <- lapply(trigger, checkInterval)
  time <- sort(unique(unlist(res)))
  return(time)
}

