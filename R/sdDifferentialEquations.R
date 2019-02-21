sdDifferentialEquationsClass <- R6::R6Class(
  
  classname = "sdDifferentialEquations",
  
  public = list(
    initialize = function(differentialEquations) { 
      # add verifications
      
      private$pDifferentialEquations <- differentialEquations
    },
    getOdeFunction = function() {
      # private$pDifferentialEquations
      eqList <- list(
        "\td0 <- st$x ^ 1/2",
        "\td1 <- st$y ^ 1/4"
      )
      
      # function(t, st, ct, par, inp, sw, aux) {
      #   d0 <- st$x ^ 1/2
      #   d1 <- st$y ^ 1/4
      #   
      #   return(list(c(d0, d1)))
      # }
      
      strParms<- "function(t, st, ct, par, inp, sw, aux) {\n"
      eqStr <- paste(eqList, collapse = "\n")
      strReturn <- "\n\n\treturn(list(c(d0, d1)))\n}"
      
      strFun <- paste0(strParms, eqStr, strReturn)
      
      ode <- eval(parse(text = strFun))
      
      return(ode)
    },
    print = function() {
      print(self$getOdeFunction())
    },
    saveXml = function() { 
      # TODO
    }
  ),
  
  active = list(),
  
  private = list(
    pDifferentialEquations = NULL
  )
)