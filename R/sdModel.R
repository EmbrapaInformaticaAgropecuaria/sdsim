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
        warning(sprintf(fmt = sdModelMsg$id1, id), call. = FALSE)
        
      } else if (!is.character(id)) { 
        
        id <- gsub("\\s", "", paste(class(self)[[1]], Sys.Date()), perl = TRUE)
        warning(sprintf(fmt = sdModelMsg$id2, id), call. = FALSE)
        
      } else if (make.names(gsub("\\s", "", id, perl = TRUE)) %in% 
                 sdsimReserved) { 
        
        # reserved name
        warning(sprintf(fmt = sdModelMsg$id3, 
                        make.names(gsub("\\s", "", id, perl = TRUE)),
                        gsub("\\s", "", paste(class(self)[[1]], Sys.Date()), 
                             perl = TRUE)), call. = FALSE)
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
          warning(sprintf(fmt = sdModelMsg$description, private$pId), call. = F)
      }
    },
    defaultScenario = function(defaultScenario) { 
      if (missing(defaultScenario)) {
        return(private$pdefaultScenario)
      } else { 
        if (is.character(defaultScenario))
          defaultScenario <- sdLoadScenario(defaultScenario)
        
        # scenario must be a scenario object 
        if (inherits(defaultScenario, sdScenarioClass$classname)) { 
          private$pdefaultScenario <- defaultScenario$clone()
          private$pdefaultScenario$id <- "Default"
          private$flagVerify <- FALSE
        } else {
          sdOdeModelMsg$defaultScenario(private$pId)
        } 
      }
    },
    isVerified = function() { 
      return(private$flagVerify)
    }
  ),
  private = list(pId = NULL,
                 pdefaultScenario = NULL,
                 pDescription = NULL,
                 flagVerify = FALSE)
)