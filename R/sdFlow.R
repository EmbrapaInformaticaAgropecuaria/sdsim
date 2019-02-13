sdFlowClass <- R6::R6Class(
  classname = "sdFlow",
  public = list(
    initialize = function()
    {
      
    },
    print = function()
    {
      # TODO
    },
    getOdeFunction = function() {
      
    },
    setStateVariables = function() {
      
    },
    saveXml = function()
    {
      # TODO
    }
  ),
  active = list(),
  private = list(
    generate_flows = function(source, sink, st) {
      nStates <- length(st)
      st_names <- st
      
      source_idx <- sapply(source, function(x)
        match(x, st_names))
      sink_idx   <- sapply(sink,   function(x)
        match(x, st_names))
      
      inflow <- list()
      outflow <- list()
      for (i in 1:nStates) {
        inflow[[i]] <- which(sink_idx == i)
        outflow[[i]] <- which(source_idx == i)
      }
      
      return(list(inflow = inflow, outflow = outflow))
    },
    makeFlowOdeFunction = function(source, sink, flow_rate)
    {
      flows <-
        private$generate_flows(source, sink, private$stateVariables)
      
      ode <- function(t, st, ct, par, inp, sw, aux)
      {
        # Calc flow quantity
        flow_qty <- sapply(flow_rate, eval, envir = environment())
        
        # Calc differentials
        inflow_qty <-
          sapply(flows$inflow, function(x)
            sum(flow_qty[x]))
        outflow_qty <-
          sapply(flows$outflow, function(x)
            sum(flow_qty[x]))
        
        dS_dt <- inflow_qty - outflow_qty
        
        return(list(dS_dt))
      }
      
      return(ode)
    },
    pOdeFunction = NULL,
    pOdeFlows = NULL,
    stateVariables = NULL,
    modelEnvironment = NULL
  )
)