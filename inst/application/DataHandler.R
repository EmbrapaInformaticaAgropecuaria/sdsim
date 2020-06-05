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

UpdateVisNetWork <- function(hot, grName, output) {

  odeFlow <- rhandsontable::hot_to_r(hot)
  
  stocks <- odeFlow$Stocks[!is.element(odeFlow$Stocks, c(NA,""))]
  flows <- odeFlow$Flows[!is.element(odeFlow$Flows, c(NA,""))]
  flowRate <- odeFlow$FlowRate[!is.element(odeFlow$FlowRate, c(NA,""))]
  
  if(length(stocks) == 0 && length(flows) == 0 && length(flowRate) == 0) {
    return(NULL)
  }
  
  id <- NULL
  label <- NULL
  group <- NULL
  
  from <- NULL
  to <- NULL
  arrows <- NULL
  
  if(length(flows)!=0) {
    # Insert a flowrate node between a flow edge
    edgesAux <- unlist(lapply(flows, function(x) {
      # get flow index and split flow
      index <- match(x,flows)
      edge <- unlist(strsplit(x, split = "\\h*->\\h*", perl = TRUE))
      if(length(edge) == 1)
        edge <- c(edge, "")
      #if there is a flowrate for index, insert it between the flow edge
      if(!is.na(flowRate[index]) && flowRate[index] != "") {
        return(c(paste(edge[1], "->", flowRate[index]), paste(flowRate[index], "->", edge[2])))
      } else {
        return(x)
      }
    }))
    
    # separate edges
    split_flow <- strsplit(edgesAux, split = "\\h*->\\h*", perl = TRUE)
    for(i in 1:length(split_flow)) {
      if(length(split_flow[[i]]) == 1)
        split_flow[[i]] <- c(split_flow[[i]], "")
    }
    from <- unlist(lapply(split_flow, `[[`, 1))
    to <- unlist(lapply(split_flow, `[[`, 2))
    
    # insert source and sink ids
    for(i in 1:length(from)) {
      if(from[i] == "")
        from[i] <- paste0("source", i)
      if(to[i] == "")
        to[i] <- paste0("sink", i)
    }
    
    # set edges arrows type
    arrows <- unlist(lapply(to, function(x) {
      if(!is.na(match(x,flowRate))) {
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
      visHierarchicalLayout(sortMethod = "directed", direction = "LR")
  })
}
