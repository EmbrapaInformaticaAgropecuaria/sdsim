sdOdeClass <- R6::R6Class(
  
  classname = "sdOde",
  
  public = list(
    initialize = function(ode) { 
      # verifications of argument 'ode'
      
      if(is.null(ode))
        stop(sprintf(auxiliaryMsg$sdOdeClass2))
      
      if(!is.list(ode))
        stop(sprintf(auxiliaryMsg$sdOdeClass1))
      
      
      private$pOde <- ode
    },
    getOdeFunction = function(){
      
      EqList <- private$pOde
      
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
    pOde = NULL
  )
)