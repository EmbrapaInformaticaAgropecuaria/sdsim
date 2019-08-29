# Read a text file
#
# @param fileName Text file name
# @param header Flag TRUE if file has a header, FALSE if not
# @return The file data in a data.frame
# ReadDataTxt <- function(fileName = "fileName", header = TRUE, sep = ",", 
#                         dec = ".")
# {
#   data <- tryCatch(
#     {
#       read.table(file = fileName, header = header, sep = sep, 
#                  dec = dec, strip.white = TRUE, stringsAsFactors = FALSE)
#     },
#     warning = function(w)
#     {
#       warning(w, call. = F)
#       return(NULL)
#     },
#     error = function(e)
#     {
#       warning(e, call. = F)
#       return(NULL)
#     })
#   
#   
#   return(data)
# }

# Convert a data.frame to a list 
# 
# The data.frame should have at least two columns: one with the variable name.
# And another wiht the variable value.
#
# @param dataFrame Data frame with at least these columns: Variable and 
# Value
# @param variableCol The name of the column with the variable name
# @param valueCol The name of the column with the variable value)
# @return A list of the variables and their values
ConvertDataFrameToList <- function(dataFrame, variableCol = "Variable", 
                                   valueCol = "Value", dec = ".") { 
  dataFrame <- as.data.frame(dataFrame, stringsAsFactors = FALSE)
  
  dataList <- NULL
  if (!is.null(dataFrame) && nrow(dataFrame) > 0 && 
      !is.null(dataFrame[[valueCol]]) && !is.null(dataFrame[[variableCol]]) &&
      !all(dataFrame[[variableCol]] %in% "")) { # if file is not empty
    # remove invalid rows
    dataFrame <- dataFrame[!(dataFrame[[valueCol]] %in% "") **
                             !sapply(dataFrame[[valueCol]], is.null, 
                                     USE.NAMES = F),]
    dataFrame <- dataFrame[!is.na(dataFrame[[valueCol]]),]
    # convert to list
    dataList[as.character(dataFrame[[variableCol]])] <- 
      as.list(dataFrame[[valueCol]])
  } else {
    warning(sprintf(readInputDataMsg$ConvertDataFrameToList))
  }
  
  return(dataList)
}

# Read input as excel file
# 
# First get all the available sheets and then read them all
# 
# @param fileName Excel file name
# @return A list with the sheets as data.frames
ReadDataExcel <- function(fileName  = "DGM Inputs/dgmParameterInput.xlsx") { 
  # read data from excel file with one or more sheets
  sheets <- readxl::excel_sheets(fileName)
  
  modelParms <- lapply(sheets, function(x) { 
    tryCatch( { 
      df <- readxl::read_excel(path = fileName, sheet = x, 
                               trim_ws = T, col_types = "text")
      row.names(df) <- NULL
      return(df)
    },
    error=function(e) {
      warning(sprintf(readInputDataMsg$ReadDataExcel1, fileName, e))
      return(data.frame())
    },
    warning=function(w) { 
      warning(sprintf(readInputDataMsg$ReadDataExcel2,fileName,w))
      return(data.frame())
    })
  })
  names(modelParms) <- sheets
  
  return(modelParms)
}

# Convert a list of data frames into a list of lists
# Extracts the description and unit columns into a separate list and
# when there is any time series input also generates a list with the 
# interpolation methods
ConvertListDataFrameToList <- function(listDataFrames,
                                       variableCol = "Variable",
                                       valueCol = "Value",
                                       descriptionCol = "Description", 
                                       unitCol = "Interpolation",
                                       interpolationCol = "Interpolation",
                                       dec = ".") { 
  # convert data.frames variables into lists of variables, with each value
  # converted to its correct type
  listInputs <- list()
  
  for (dfName in tolower(names(listDataFrames))) { 
    parm <- listDataFrames[[dfName]]
    
    if (!is.null(parm) && nrow(parm) > 0) { # if file is not empty
      # inputs
      if (dfName == "input") { 
        # if there is time series also save the interpolation methods
        if (interpolationCol %in% names(parm) && 
            !is.null(parm[[interpolationCol]]))  
          listInputs[["interpolation"]] <- ConvertDataFrameToList(
            parm, 
            variableCol = variableCol, 
            valueCol = interpolationCol,
            dec = dec)
      }
      
      listInputs[[dfName]] <- ConvertDataFrameToList(parm, 
                                                     variableCol = variableCol, 
                                                     valueCol = valueCol,
                                                     dec = dec)
      
      # add description and unit
      if (!is.null(parm[[descriptionCol]]) &&
          !all(parm[[descriptionCol]] %in% ""))
        listInputs[["description"]] <- c(listInputs[["description"]], 
                                         ConvertDataFrameToList(
                                           parm, 
                                           variableCol = variableCol, 
                                           valueCol = descriptionCol,
                                           dec = dec))
      if (!is.null(parm[[unitCol]]) && 
          !all(parm[[unitCol]] %in% ""))
        listInputs[["unit"]] <- c(listInputs[["unit"]], 
                                  ConvertDataFrameToList(
                                    parm, 
                                    variableCol = variableCol, 
                                    valueCol = unitCol,
                                    dec = dec))
    }
  }
  
  return(listInputs)
}

# Read Files in .xlsx, .txt or all .txt's inside a Folder
# 
# If the filePath is a directory, it will read all the files inside the 
# directory into data frames. The name of the files will be the name of the data 
# frames.
# If the file ends in .xlsx (excel file), it will read all the sheets from the 
# excel file into data frames. The name of the sheets will be the name of the 
# data frames.
# If the file exists but is not any of the above cases it assumes that the file
# is a text file and read it into a data frame.
# Further the data frames will be converted into the vars lists, preserving the
# names.
#
# @param filePath Path to the input excel file, text file or directory with text 
# files inside.
# @return A list of data frames containing the data loaded from the filePath
# LoadFile <- function(filePath, 
#                      sep = ",", 
#                      dec = ".")
# {
#   # Detect if file extension is equal ".xlsx"
#   isXlsx <- grepl("\\.(xlsx|xls)$", filePath, perl=TRUE) 
#   
#   if (dir.exists(filePath))  # Files inside a directory
#   {
#     fileNames <- list.files(path = filePath)
#     
#     data <- lapply(fileNames, function(fileName)
#     {
#       data <- ReadDataTxt(fileName = paste0(filePath, "/", fileName), 
#                           sep = sep, dec = dec)
#       return(data)
#     })
#     fileNames <- lapply(fileNames, function(x)
#     {
#       gsub(".txt", "", x)
#     })
#     names(data) <- fileNames
#   }
#   else if (isXlsx)  # File is a xlsx
#   {
#     data <- ReadDataExcel(filePath)
#   }
#   else if (file.exists(filePath))  # Read as a text file
#   {
#     fileName <- gsub(".*\\/", "", filePath)  # Remove directory from file path
#     data <- list(fileName = ReadDataTxt(filePath, sep = sep, dec = dec))
#   }
#   else
#   {
#     stop("Load scenario file ", filePath, 
#         ": The external input file must be an excel file, a text file, or ",
#         "a directory with the text input files inside. Check if your ",
#          "external inputs are in the correct format and try to init ",
#          "your model again.")
#   }
#   
#   return(data)
#   }


# Read the Model Variables from EXCEL Files
# 
# It will read all the sheets from the excel file into a list of data.frames 
# (one data.frame per sheet) where the name of the sheets will be used to name 
# the data.frames. The sheet names must be informed in the args list or follow 
# the defaults, not valid sheet names will be ignored.
# 
# Then, it will convert the inputs and the defaults lists of data.frames to a 
# list of lists where each element of the inner lists contain the variable 
# value and it is named with the variable name specified in the data file. It 
# also stores the interpolation methods present in the inputSheet and all the
# descriptions and units
# 
# @param file the excel file
# @param dec The decimal point character used in the input files. Default is 
# ".".
# @param stateSheet A character string containing the name of the xlsx sheet
# that corresponds to the state variables sheet. (Only used if the file is a 
# xlsx)
# @param constantSheet A character string containing the name of the xlsx sheet
# that corresponds to the constants sheet. (Only used if the file is a 
# xlsx)
# @param inputSheet A character string containing the name of the xlsx sheet
# that corresponds to the inputs sheet. (Only used if the file is a 
# xlsx)
# @param parameterSheet A character string containing the name of the xlsx 
# sheet that corresponds to the parameters sheet. (Only used if the file is a 
# xlsx)
# @param switchSheet A character string containing the name of the xlsx sheet
# that corresponds to the switches sheet. (Only used if the file is a 
# xlsx)
# @param simulationSheet A character string containing the name of the xlsx 
# sheet that corresponds to the simulation configurations sheet. (Only used if 
# the file is a xlsx)
# @param variableCol A character string with the name of the column containing 
# the variables names.
# @param valueCol A character string with the name of the column containing 
# the variables values.
# @param unitCol A character string with the name of the column containing 
# the variables units.
# @param descriptionCol A character string with the name of the column 
# containing the variables descriptions.
# @param interpolationCol A character string with the name of the column 
# containing the input time series variables interpolation method.
# @section EXCEL Format: The EXCEL file representing a \code{sdScenario} must
# have one sheet per type of variable and a sheet for the simulation 
# configurations, e.g. the \code{stateSheet}, \code{constantSheet}, 
# \code{inputSheet}, \code{parameterSheet}, \code{switchSheet} and the 
# \code{simulationSheet}. A sheet can be missing if the scenario do not contain
# the corresponding type of variable.
# 
# Each sheet must follow this guidelines:
# 
# \itemize{
#   \item Have header
#   \item Have 4 columns, with default labels as bellow: 
#   \enumerate{
#     \item Variable - the column with the variables name
#     \item Value - the column with the variables value
#     \item Unit - the column with the variables unit
#     \item Description - the column with the variables description
#   }
#   With the exception of the \code{inputSheet} that should also contain a 
#   fifth column, labeled 'Interpolation' containing the interpolation method 
#   to be used in the time series variables. Each variable that have a valid
#   method will be passed to the \code{\link{sdTemporalFunctionList}} to be 
#   automatically converted into a temporal function.
#   \item The default decimal point character is '.'
#   
#   The sheet names must follow the defaults or be specified in the arguments.
# }
LoadModelScenario <- function(file,
                              dec = ".",
                              stateSheet = "state",
                              constantSheet = "constant",
                              inputSheet = "input",
                              parameterSheet = "parameter",
                              switchSheet = "switch",
                              simulationSheet = "simulation",
                              variableCol = "Variable", 
                              valueCol = "Value", 
                              unitCol = "Unit", 
                              descriptionCol = "Description", 
                              interpolationCol = "Interpolation") {  
  scen <- list()
  
  # read defaults
  if (file.exists(file)) { 
    modelScenario <- ReadDataExcel(file)
    
    # convert the sheet names to the expected names (hard coded as bellow)
    sheetNames <- c(stateSheet, constantSheet, inputSheet, parameterSheet, 
                    switchSheet, simulationSheet)
    expectedSheets <- c("state", "constant", "input", "parameter", 
                        "switch", "simulation")
    # remove unvalid sheets
    validSheets <- match(names(modelScenario), sheetNames, nomatch = 0)
    modelScenario <- modelScenario[validSheets > 0]
    # rename the valid sheets correctly
    validSheets <- validSheets[validSheets > 0]
    names(modelScenario) <- expectedSheets[validSheets]
    
    # Convert data.frames to list extracting description, unit and interpolation
    scen <- ConvertListDataFrameToList(modelScenario, 
                                       variableCol = variableCol, 
                                       valueCol = valueCol, 
                                       interpolationCol = interpolationCol,
                                       descriptionCol = descriptionCol,
                                       unitCol = unitCol,
                                       dec = dec) 
    
    # get the simulation options
    if (!is.null(scen$simulation)) { 
      scen$times <- scen$simulation[names(scen$simulation) %in% 
                                      c("from", "to", "by")]
      scen$method <- scen$simulation$method
      scen$id <- scen$simulation$id
      scen$simulation <- NULL
      
      # move the times description and units from the 'from', 'to' and 'by'
      if (!is.null(unlist(scen$description[c("from", "to", "by")]))) { 
        scen$description$times <- paste(scen$description[c("from", "to", "by")], 
                                        collapse = " ")
        scen$description[c("from", "to", "by")] <- NULL
      }
      if (!is.null(unlist(scen$unit[c("from", "to", "by")]))) { 
        scen$unit$times <- paste(scen$unit[c("from", "to", "by")], 
                                 collapse = " ")
        scen$unit[c("from", "to", "by")] <- NULL
      }
    }
  } else { 
    warning(sprintf(readInputDataMsg$LoadModelScenario1, file))
  }
  
  return(scen)
}
