sdDifferentialEquationsClass <- R6::R6Class(
  
  classname = "sdDifferentialEquations",
  
  public = list(
    initialize = function(differentialEquations) { 
      # verifications of argument 'differentialEquations'
      
      if(is.null(differentialEquations))
        stop(sprintf(auxiliaryMsg$sdDifferentialEquationsClass2))
      
      if(!is.list(differentialEquations))
        stop(sprintf(auxiliaryMsg$sdDifferentialEquationsClass1))
      
      
      private$pDifferentialEquations <- differentialEquations
    },
    getOdeFunction <- function(){
      
      EqList <- private$pDifferentialEquations
      
      #transform a list of equations in differential equations 
      
      for(i in 1:length(EqList)){
        EqList[i] <- paste0("\td",toString(i-1)," <- ", EqList[i])
      }
      
      strParms<- "function(t, st, ct, par, inp, sw, aux) {\n"
      eqStr <- paste0(EqList, collapse = "\n")
      strReturn <- "\n\n\treturn(list(c(d0"
      
      for(i in 1:(length(EqList)-1)){
        strReturn <- paste0(strReturn, ", d", toString(i))
      }
      strReturn <- paste0(strReturn, ")))\n}")
  
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