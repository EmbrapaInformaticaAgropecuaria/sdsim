# Create an empty data frame for state, constants, switches e parameters
CreateVarDataFrame <- function(nRows) {
  data.frame(Variable = character(nRows), 
             Value = character(nRows), 
             Unit = character(nRows), 
             Description = character(nRows), 
             stringsAsFactors = FALSE, row.names = NULL)
}

# Create an empty data frame for inputs
CreateInputDataFrame <- function(nRows) {
  data.frame(Variable = character(nRows), 
             Value = character(nRows), 
             Unit = character(nRows), 
             Description = character(nRows),
             Interpolation = character(nRows),
             stringsAsFactors = FALSE, row.names = NULL)
}

# Create an empty data frame for coupled model connections
CreateConnectionsDataFrame <- function(nRows) {
  data.frame('Connection ID' = character(nRows), 
             'Receiver Component ID' = character(nRows), 
             'Receiver Input' = character(nRows), 
             'Sender Component ID' = character(nRows),
             'Sender Output' = character(nRows),
             stringsAsFactors = FALSE, row.names = NULL, check.names = F)
}

# Create an empty data frame for coupled model components
CreateComponentsDataFrame <- function(nRows) {
  data.frame('Component ID' = character(nRows),
             stringsAsFactors = FALSE, row.names = NULL, check.names = F)
}

# Initalize reactive script data
# InitScriptData <- function(session) {
#   odeTxt <- "function(t, st, ct, par, inp, sw, aux) {
#   # Calculate the differentials
#   
#   # Return the differentials to the integrator
#   return(list(c()))
# }"
#   
#   initVarsTxt <- "# # Uncomment to use initialization function
# # function(st, ct, par, inp, sw, aux) 
# # {
# #   # Initialize variables
# #    
# #   # Return the differentials to the integrator
# #   return(list(st = st, ct = ct, inp = inp, par = par, sw = sw))
# # }"
#   
#   rootTxt <- "# # Uncomment to use root function
# # function(t, st, ct, par, inp, sw, aux) 
# # {
# #   # return roots
# #   return(c())
# # }"
#   
#   eventTxt <- "# # Uncomment to use events (Root function must be defined)
# # function (t, st, ct, par, inp, sw, aux) 
# # {
# #   # Return the altered state
# #   return(st)
# # }"
#   
#   descriptionTxt <- ""
#   
#   globalFunctionsTxt <- ""
#   
#   # Update script fields
#   CustomAceUpdate(session, "description", value = descriptionTxt)
#   CustomAceUpdate(session, "ode", value = odeTxt)
#   CustomAceUpdate(session, "initVars", value = initVarsTxt)
#   CustomAceUpdate(session, "root", value = rootTxt)
#   CustomAceUpdate(session, "event", value = eventTxt)
#   CustomAceUpdate(session, "globalFunctions", value = globalFunctionsTxt)
# }

# Customized update value for shinyAce scripts. 
# Send custom message to javascript for updating. The default ace editor 
# javascript function doesn't update the scripts for empty values.
CustomAceUpdate <- function (session, editorId, value, theme, readOnly, mode, 
                             fontSize, wordWrap, 
                             border = c("normal", "alert", "flash"), 
                             autoComplete = c("disabled", "enabled", "live"), 
                             autoCompleteList = NULL) {
  if (missing(session) || missing(editorId)) {
    stop("Must provide both a session and an editorId to update Ace.")
  }
  theList <- list(id = editorId)
  if (!missing(value)) {
    theList["value"] <- value
  }
  
  session$sendCustomMessage("shinyAceUpdate", theList)
}

# Render RHandsontable with the given data parameter
UpdateRHandsontable <- function(data, tableName, output) {
  output[[tableName]] <- rhandsontable::renderRHandsontable({
    if (!is.null(data)) {
      return(rhandsontable::rhandsontable(data, stretchH = "all"))
    }
  })
}
