sdOdeClass <- R6::R6Class(
  classname = "sdOde",
  public = list(
    initialize = function(ode = NULL, 
                          modelEnvironment = NULL)
    {
      if(is.function(ode))
        private$pOdeFunction <- ode
      else if(is.list(ode))
      {
        private$pOdeFlows <- ode$flows
        private$stateVariables <- ode$stateVariables
      }
      
      private$modelEnvironment <- modelEnvironment
    },
    print = function()
    {
      # TODO
    },
    test = function()
    {
      print(ls(environment(self$getOdeFunction())))
    },
    getOdeFunction = function() {
      if(!is.null(private$pOdeFunction))
      {
        ode <- private$pOdeFunction
      }
      else if(!is.null(private$pOdeFlows))
      {
        if(is.null(private$stateVariables))
          stop("TODO")
        ode <- private$makeFlowOdeFunction(private$pOdeFlows)
      }
      
      if(!is.null(private$modelEnvironment))
        parent.env(environment(ode)) <- private$modelEnvironment
      
      return(ode)
    },
    getVerifyFunction = function() {
      
    },
    setStateVariables = function() {
      
    },
    saveXml = function()
    {
      # TODO
    }),
  active = list(
  ),
  private = list(
    generate_flows = function(source, sink, st) {
      nStates <- length(st)
      st_names <- st
      
      source_idx <- sapply(source, function(x) match(x, st_names))
      sink_idx   <- sapply(sink,   function(x) match(x, st_names))
      
      inflow <- list()
      outflow <- list()
      for (i in 1:nStates){
        inflow[[i]] <- which(sink_idx == i)
        outflow[[i]] <- which(source_idx == i)
      }
      
      return(list(inflow = inflow, outflow = outflow))
    },
    makeFlowOdeFunction = function(df)
    {
      flows <- private$generate_flows(df$source, df$sink, private$stateVariables)
      
      ode <- function(t, st, ct, par, inp, sw, aux)
      {
        # Calc flow quantity
        flow_qty <- sapply(df$flow_rate, function(x) {
          expr <- parse(text = x)
          eval(expr)
        })
        
        # Calc differentials
        inflow_qty <- sapply(flows$inflow, function(x) sum(flow_qty[x]))
        outflow_qty <- sapply(flows$outflow, function(x) sum(flow_qty[x]))
        
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