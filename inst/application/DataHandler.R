# Create an empty data frame for state, constants, switches e parameters
CreateVarDataFrame <- function(nRows) {
  data.frame(Variable = character(nRows), 
             Value = character(nRows), 
             Unit = character(nRows), 
             Description = character(nRows), 
             stringsAsFactors = FALSE, row.names = NULL)
}

CreateFlowDataFrame <- function(nRows) {
  data.frame(Stocks = character(nRows), 
             Flows = character(nRows), 
             FlowRate = character(nRows),
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

UpdateVisNetWork <- function(hot, grName, output, layoutOpt) {

  if(!is.null(hot))
    odeFlow <- rhandsontable::hot_to_r(hot)
  else{
    return(NULL)
  }
  
  stocks <- odeFlow$Stocks[!is.element(odeFlow$Stocks, c(NA,""))]
  flows <- odeFlow$Flows[!is.element(odeFlow$Flows, c(NA,""))]
  flowRate <- odeFlow$FlowRate[!is.element(odeFlow$FlowRate, c(NA,""))]

  id <- NULL
  label <- NULL
  group <- NULL
  
  from <- NULL
  to <- NULL
  arrows <- NULL
  
  if(length(flows)!=0) {
    numBoundaries <- strsplit(flows, split = "\\h*->\\h*", perl = TRUE)
    for(i in 1:length(numBoundaries)) {
      
      # Append empty string if boundary is described as ""
      if(length(numBoundaries[[i]]) == 1)
        numBoundaries[[i]] <- c(numBoundaries[[i]], "")
      
      # Enumerate boundaries
      numBoundaries[[i]] <- sub("boundary", paste0("boundary", i), numBoundaries[[i]])
      numBoundaries[[i]][nchar(numBoundaries[[i]])==0] <- paste0("boundary", i)
      
      # Insert flowrate between a flow and split it OR just split flow 
      if(!is.na(flowRate[i]) && flowRate[i] != "") {
        from <- c(from, numBoundaries[[i]][1])
        to <- c(to, flowRate[i])
        
        from <- c(from, flowRate[i])
        to <- c(to, numBoundaries[[i]][2])
        
      } else {
        from <- c(from, numBoundaries[[i]][1])
        to <- c(to, numBoundaries[[i]][2])
      }
    }
    
    # Set edges arrows type
    arrows <- unlist(lapply(to, function(x) {
      if(!is.na(match(x, flowRate))) {
        return("")
      } else {
        return("to")
      }
    }))
  }
  
  id <- unique(c(stocks, flowRate, from, to))
  nboundary <- length(id) - length(stocks) - length(flowRate)
  label <- c(stocks, flowRate, rep("", nboundary))
  
  nodes <- data.frame(id = id, 
                      label = label,
                      group = c(rep("stocks", length(stocks)), 
                                rep("flowRate", length(flowRate)),
                                rep("boundaries", nboundary)))
  
  edges <- data.frame(from = from, 
                      to = to, 
                      arrows = arrows)
  
  output[[grName]] <- renderVisNetwork({
    visNetwork(nodes, edges, width = "100%")%>%
      visInteraction(navigationButtons = TRUE) %>%
      visPhysics(enabled = FALSE, hierarchicalRepulsion = list(centralGravity = 0)) %>%
      visGroups(groupname = "boundaries",
                shape = "icon",
                icon = list(code = "f0c2",
                            color = "black",
                            size = 30)) %>%
      visGroups(groupname = "flowRate",
                shape = "icon",
                icon = list(code = "f0b0",
                            color = "black",
                            size = 30)) %>%
      visGroups(groupname = "stocks",
                size = 100,
                color = list(background = "lightgray",
                             border = "black"),
                shape = "box") %>%
      addFontAwesome(name = "font-awesome-visNetwork") %>%
      visHierarchicalLayout(enabled = layoutOpt, sortMethod = "directed", direction = "LR")
  })
}
