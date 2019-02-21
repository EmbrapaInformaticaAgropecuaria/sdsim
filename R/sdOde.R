sdOdeClass <- R6::R6Class(
  classname = "sdOde",
  public = list(
    initialize = function(ode = NULL,
                          modelEnvironment = NULL) { 
      if (is.function(ode)) {
        private$pOdeFunction <- ode
      } else if(inherits(ode, sdFlowClass$classname)) {
        private$pOdeFlows <- ode
      } else if (is.list(ode)) { 
        # TODO: differential equations list
      }
      
      private$modelEnvironment <- modelEnvironment
    },
    getOdeFunction = function() {
      if (!is.null(private$pOdeFunction)) { 
        ode <- private$pOdeFunction
      } else if (!is.null(private$pOdeFlows)) { 
        ode <- private$pOdeFlows$getOdeFunction()
      }
      
      if (!is.null(private$modelEnvironment))
        parent.env(environment(ode)) <- private$modelEnvironment
      
      return(ode)
    },
    print = function() { 
      # TODO
    },
    saveXml = function() { 
      # TODO
    }
  ),
  
  active = list(),
  private = list(
    pOdeFunction = NULL,
    pOdeFlows = NULL,
    modelEnvironment = NULL
  )
)