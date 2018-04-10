sdModelClass <- R6::R6Class(
  classname = "sdModel",
  public = list(print = function(){},
                verifyModel = function() {},
                saveXML = function(){}),
  active = list(
    id = function(id) 
    {
      if (missing(id))
        return(private$pid)
      else if (is.null(id))
      {
        id <- gsub("\\s", "", paste(class(self)[[1]], Sys.Date()), 
                   perl = TRUE)
        warning(sprintf(fmt = sdModelMsg$id1, id), call. = F)
      }
      else if (!is.character(id))
      {
        id <- gsub("\\s", "", paste(class(self)[[1]], Sys.Date()), 
                   perl = TRUE)
        warning(sprintf(fmt = sdModelMsg$id2, id), call. = F)
      }
      
      private[["pid"]] <- make.names(gsub("\\s", "", 
                                          id, 
                                          perl = TRUE))
    },
    description = function(description) 
    {
      if (missing(description))
        return(private$pdescription)
      else
      {
        if (is.character(description))
          private$pdescription <- description
        else
          warning(sprintf(fmt = sdModelMsg$description, private$pid), call. = F)
      }
    },
    defaultScenario = function() {},
    isVerified = function()
    {
      return(private$flagVerify)
    }
  ),
  private = list(pid = NULL,
                 pdescription = NULL,
                 flagVerify = FALSE)
)