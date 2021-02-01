# Create an empty data frame for state, constants, switches e parameters
CreateVarDataFrame <- function(nRows) {
  data.frame(Variable = character(nRows), 
             Value = character(nRows), 
             Unit = character(nRows), 
             Description = character(nRows), 
             stringsAsFactors = FALSE, row.names = NULL)
}

CreateFlowDataFrame <- function(nRows) {
  data.frame(Source = character(nRows),
             Sink = character(nRows),
             # Stocks = character(nRows),
             # Flows = character(nRows), 
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

RhandsonToDF <- function(hot, trimWhites = NULL, variableCol = "Variable") {
  if(is.null(hot))
    return(NULL)
  
  df <- rhandsontable::hot_to_r(hot)
  
  if(is.null(df))
    return(NULL)
  
  for(col in trimWhites) {
    df[col] <- as.data.frame(apply(df[col], c(1, 2), function(x) gsub('\\s+', '',x)), stringsAsFactors = F)
  }
  
  df <- df[-which(grepl(EMPTY_PERL_REGEX, df[[variableCol]], perl = T)), , drop = FALSE]
  row.names(df) <- NULL
  
  return(df)
}

UpdateVisNetWork <- function(hot, grName, output, layoutOpt) {

  # if(!is.null(hot)) {
  #   df <- rhandsontable::hot_to_r(hot)
  # }
  # else{
  #   return(NULL)
  # }
  # 
  # id <- NULL
  # label <- NULL
  # group <- NULL
  # 
  # from <- NULL
  # to <- NULL
  # arrows <- NULL
  # 
  # for(r in 1:nrow(df)) {
  #   if(any(df[r,] != "")) {
  #     if(!is.na(df$FlowRate[r]) && df$FlowRate[r] != "") {
  #       from <- c(from, df$Source[r])
  #       to <- c(to, df$FlowRate[r])
  #       arrows <- c(arrows, "")
  #       
  #       from <- c(from, df$FlowRate[r])
  #       to <- c(to, df$Sink[r])
  #       arrows <- c(arrows, "to")
  #     } else {
  #       from <- c(from, df$Source[r])
  #       to <- c(to, df$Sink[r])
  #       arrows <- c(arrows, "to")
  #     }
  #   }
  # }
  # 
  # if(!is.null(from))
  #   for(i in 1:length(from)) {
  #     if(any(from[i] == c("boundary", "")))
  #       from[i] <- paste0("source", i)
  #   }
  # 
  # if(!is.null(to))
  #   for(i in 1:length(to)) {
  #     if(any(to[i] == c("boundary", "")))
  #       to[i] <- paste0("sink", i)
  #   }
  # 
  # stocks <- setdiff(union(df$Source,df$Sink),c("boundary",""))
  # rate <- df$FlowRate[!is.element(df$FlowRate, c(NA,""))]
  # 
  # id <- unique(c(stocks, rate, from, to))
  # nboundary <- length(id) - length(stocks) - length(rate)
  # label <- c(stocks, rate, rep("", nboundary))
  # 
  # 
  # nodes <- data.frame(id = id, 
  #                     label = label,
  #                     group = c(rep("stocks", length(stocks)), 
  #                               rep("flowRate", length(rate)),
  #                               rep("boundaries", nboundary)))
  # 
  # edges <- data.frame(from = from, 
  #                     to = to, 
  #                     arrows = arrows)
  # 

  
  # output[[grName]] <- renderVisNetwork({
    # visNetwork(nodes, edges, width = "100%")%>%
    #   visInteraction(navigationButtons = TRUE) %>%
    #   visPhysics(enabled = FALSE, hierarchicalRepulsion = list(centralGravity = 0)) %>%
    #   visGroups(groupname = "boundaries",
    #             shape = "icon",
    #             icon = list(code = "f0c2",
    #                         size = 30)) %>%
    #   visGroups(groupname = "flowRate",
    #             shape = "icon",
    #             icon = list(code = "f0b0",
    #                         size = 30)) %>%
    #   visGroups(groupname = "stocks",
    #             size = 100,
    #             shape = "box") %>%
    #   addFontAwesome(name = "font-awesome-visNetwork") %>%
    #   visHierarchicalLayout(enabled = layoutOpt, sortMethod = "directed", direction = "LR")
  # })
}
