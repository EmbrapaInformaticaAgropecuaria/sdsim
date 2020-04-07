sdFunctionOdeClass <- R6::R6Class(
  classname = "sdFunctionOde",
  
  inherit = sdOdeClass,
  
  public = list(
    
    initialize = function(func = NULL) {
      
      # TODO: add messages
      if(missing(func) || is.null(func)) {
        stop(sprintf(""))
      }
      
      if(!is.function(func)) {
        stop(sprintf(""))
      }
      
      if (!all(private$pFunDefaultArgs %in% 
               names(formals(func)))) {
        stop(sprintf(""))
      }
      
      private$pOde <- func
    },
    
    getOdeFunction = function() {
      return(private$pOde)
    },
    
    print = function() {
      cat(FunToString(private$pOde))
    },
    
    saveXml = function() {
      doc = XML::newXMLDoc()
      rootOde <- XML::newXMLNode(class(self)[[1]], doc = doc)
      lOde <- list(ode = FunToString(private$pOde))
      ListToXML(rootOde, lOde)
      invisible(rootOde)
    }
    
  ),
  
  active = list(
    ode = function() {
      return(private$pOde)
    }
  ),
  
  private = list(
    pOde = NULL,
    pFunDefaultArgs = c("t", "st", "ct", "par", "inp", "sw", "aux", "global")
  )
)