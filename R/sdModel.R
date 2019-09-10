sdModelClass <- R6::R6Class(
  classname = "sdModel",
  
  public = list(print = function(){},
                verifyModel = function() {},
                saveXml = function(){}),
  
  active = list(
    id = function(id) { 
      if (missing(id)) {
        
        return(private$pId)
      } else if (is.null(id)) { 
        
        id <- gsub("\\s", "", paste(class(self)[[1]], Sys.Date()), perl = TRUE)
        warning(sprintf(sdModelMsg$id1, id))
        
      } else if (!is.character(id)) { 
        
        id <- gsub("\\s", "", paste(class(self)[[1]], Sys.Date()), perl = TRUE)
        warning(sprintf(sdModelMsg$id2, id))
        
      } else if (make.names(gsub("\\s", "", id, perl = TRUE)) %in% 
                 sdsimReserved) { 
        
        # reserved name
        warning(sprintf(sdModelMsg$id3, 
                        make.names(gsub("\\s", "", id, perl = TRUE)),
                        gsub("\\s", "", paste(class(self)[[1]], Sys.Date()), 
                             perl = TRUE)))
        id <- gsub("\\s", "", paste(class(self)[[1]], Sys.Date()), perl = TRUE)
      }
      
      private[["pId"]] <- make.names(gsub("\\s", "", 
                                          id, 
                                          perl = TRUE))
    },
    description = function(description) { 
      if (missing(description)) {
        return(private$pDescription)
      } else { 
        if (is.character(description))
          private$pDescription <- description
        else
          warning(sprintf(sdModelMsg$description, private$pId))
      }
    },
    defaultScenario = function(defaultScenario) { 
      if (missing(defaultScenario)) {
        return(private$pDefaultScenario)
      } else { 
        if (is.character(defaultScenario))
          defaultScenario <- sdLoadScenario(defaultScenario)
        
        # scenario must be a scenario object 
        if (inherits(defaultScenario, sdScenarioClass$classname)) { 
          private$pDefaultScenario <- defaultScenario$clone()
          private$pDefaultScenario$id <- "Default"
          private$flagVerify <- FALSE
        } else {
          warning(sprintf(sdOdeModelMsg$defaultScenario,private$pId))
        } 
      }
    },
    isVerified = function() { 
      return(private$flagVerify)
    }
  ),
  private = list(pId = NULL,
                 pDefaultScenario = NULL,
                 pDescription = NULL,
                 flagVerify = FALSE)
)