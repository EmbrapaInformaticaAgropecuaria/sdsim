# shinyUI
source("AuxiliaryUI.R", local = TRUE)
# library(DiagrammeR)
require(visNetwork)

# Interface dimensions
# Width of sidebar and page title
sideBarWidth <- 241

# Dimensions of rhandsontables
tableHeight <- "540px"
tableWidth <- "100%"

# Dimensions of script areas
scriptHeight <- "540px"
# 
# # Number of rows of each table
# tableRows <- 50

# Shiny header
header <- shinydashboard::dashboardHeader(title = "sdsim", titleWidth = sideBarWidth)

# Shiny sidebar
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id = "sidebar",
    div(
      uiOutput("selectModelOutput"),
      style = "height: 75px;"
    ),
    div(
      tags$button(
        id = "newModel",
        class = "btn action-button shiny-bound-input",
        img(src = "icon_add.png",
            height = "35px"),
        style = "border-radius: 50%; padding: 0px 0px 0px 0px; height: 30px; 
        width: 30px; border: 0px; background-color: transparent; 
        display: inline-block;vertical-align:top;"
      ),
      tags$button(
        id = "deleteModel",
        class = "btn action-button shiny-bound-input",
        img(src = "icon_remove.png",
            height = "35px"),
        style = "border-radius: 50%; padding: 0px 0px 0px 0px; height: 30px; 
        width: 30px; border: 0px; background-color: transparent; 
        display: inline-block;vertical-align:top;"
      ),
      tags$button(
        id = "saveModel",
        class = "btn action-button shiny-bound-input",
        img(src = "icon_save.png",
            height = "34px"),
        style = "border-radius: 50%; padding: 0px 0px 0px 0px; height: 30px; 
        width: 30px; border: 0px; background-color: transparent; 
        display: inline-block;vertical-align:top;"
      ),
      style = "padding: 0px 0px 0px; 0px; text-align: center;"
    ),
    
    shinydashboard::menuItem(
      "Edit Model", 
      tabName = "odePage", icon = icon("cogs")),
    shinydashboard::menuItem(
      "Description", 
      tabName = "descriptionPage", icon = icon("commenting")),
    
    div(style = "border: 1px solid grey"),
    
    div(
      uiOutput("selectScenarioOutput"),
      style = "height: 75px;"
    ),
    div(
      tags$button(
        id = "newScenario",
        class = "btn action-button shiny-bound-input",
        img(src = "icon_add.png",
            height = "35px"),
        style = "border-radius: 50%; padding: 0px 0px 0px 0px; height: 30px; 
        width: 30px; border: 0px; background-color: transparent; 
        display: inline-block;vertical-align:top;"
      ),
      tags$button(
        id = "deleteScenario",
        class = "btn action-button shiny-bound-input",
        img(src = "icon_remove.png",
            height = "35px"),
        style = "border-radius: 50%; padding: 0px 0px 0px 0px; height: 30px; 
        width: 30px; border: 0px; background-color: transparent; 
        display: inline-block;vertical-align:top;"
      ),
      
      tags$button(
        id = "saveScenario",
        class = "btn action-button shiny-bound-input",
        img(src = "icon_save.png",
            height = "34px"),
        style = "border-radius: 50%; padding: 0px 0px 0px 0px; height: 30px; 
        width: 30px; border: 0px; background-color: transparent; 
        display: inline-block;vertical-align:top;"
      ),
      style = "padding: 0px 0px 0px; 0px; text-align: center;"
    ),
    shinydashboard::menuItem(
      "Edit Scenario",
      tabName = "scenarioPage", icon = icon("th")),
    
    div(style = "border: 1px solid grey"),
    shinydashboard::menuItem(
      strong("Simulation"), 
      tabName = "simulationPage", icon = icon("line-chart")),
    
    div(style = "border: 1px solid grey"),
    
    shinydashboard::menuItem(
      "Help", 
      tabName = "helpPage", icon = icon("question-circle")),
    shinydashboard::menuItem(
      "About",
      tabName = "aboutPage", icon = icon("info-circle"))
  ),
  width = sideBarWidth
)

# Simulation execution page
simulationPage <- shinydashboard::tabItem(
  tabName = "simulationPage",
  fluidPage(
    shinydashboard::box(
      title = "System Dynamics Model Simulation",
      solidHeader = T,
      width = "100%",
      status = "primary",
      fluidRow(
        # # Simulation name output
        # column(7, strong(textOutput("simNameOut")), style = "font-size: 35px")
        column(7, h3("Model Simulation"))
      ),
      br(), # newline
      div(
        div(
          
          id = "methodInputDiv",
          selectInput("method", "Method", 
                      c("lsoda", "lsode", "lsodes", "lsodar", "vode",
                        "daspk", "euler", "rk4", "ode23", "ode45", 
                        "radau", "bdf", "bdf_d", "adams", "impAdams", 
                        "impAdams_d")),
          style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px; width: 150px;"
        ),
        div(
          textInput("initialTime", "Initial Time", value = 0),
          style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px; width: 100px;"
        ),
        div(
          textInput("finalTime", "Final Time", value = 100),
          style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px; width: 100px;"
        ),
        div(
          textInput("step", "Time Step", value = 1),
          style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px; width: 100px;"
        )
      ),
      # Execute simulation button
      div(
        actionButton("execSim", 
                     "Start Simulation"),
        style = "display: inline-block;vertical-align:top; padding: 0px 10px 15px 0px;"
      ),
      div(
        textOutput("simulationProgress"),
        style = "display: inline-block;vertical-align:top; padding: 5px 0px 5px 0px; font-weight: bold; font-size: 100%; color: #00631f; z-index: 105;"
      ),
      br(),
      
      textOutput("errorTitle"),
      
      pre(
        id = "errorLog",
        style = "max-height: 100px;
        white-space: pre-wrap;",
        class = "shiny-text-output noplaceholder",
        colapse = " "
      ),
      
      # Simulation Results
      shinydashboard::tabBox(
        title = "", 
        width = "100%",
        
        # Trajectory Table
        tabPanel(
          "Trajectory",
          shinydashboard::tabBox(
            title = "", width = "100%",
            tabPanel("Result",
                     dataTableOutput("outTrajectory"),
                     uiOutput("exportOutputTrajBt")
            ),
            tabPanel("Auxiliary Variables",
                     dataTableOutput("auxTrajectory"),
                     uiOutput("exportAuxTrajBt")
            ),
            tabPanel("Time Series",
                     dataTableOutput("timeSeriesTrajectory"),
                     uiOutput("exportTimeSeriesTrajBt")
            )
          )
        ),
        
        # Plots tab
        tabPanel(
          "Plots",
          div(textInput("plotTitle", "Plot Title", placeholder = "Optional"),
              style = "display: inline-block;vertical-align:top; width: 250px; padding: 0px;"),
          div(textInput("plotXLabel", "X-Axis Label", placeholder = "Optional"),
              style = "display: inline-block;vertical-align:top; width: 250px; padding: 0px;"),
          div(textInput("plotYLabel", "Y-Axis Label(s)", placeholder = "Optional"),
              style = "display: inline-block;vertical-align:top; width: 250px; padding: 0px;"),
          
          br(),
          div(
            selectInput("selVarPlot", "Y-Axis Variable(s) *", 
                        choices = "No Variables Available", 
                        multiple = T, 
                        selected = "No Variables Available"),
            style = "display: inline-block;vertical-align:top;
            width: 250px; padding: 0px;"),
          div(
            selectInput("selectXAxisPlot", 
                        choices = "No Variables Available", 
                        "X-Axis Variable *"),
            style = "display: inline-block;vertical-align:top; 
            width: 250px; padding: 0px;"),
          br(),
          
          div(
            radioButtons("plotType", 
                         "Plot Type", 
                         choices = c("line", "point"),
                         inline = T),
            style = "display: inline-block;vertical-align:top; 
            width: 250px; padding: 0px;"),
          div(
            checkboxInput("showUnitToggle",
                          "Display Variable's Units",
                          value = T),
            style = "display: inline-block;vertical-align:top; 
            width: 250px; padding-top: 15px;"),
          div(
            id = "multipleAxisTooltipDiv",
            checkboxInput("multipleAxisToggle",
                          "Display Multiple Y-Axis",
                          value = T),
            style = "display: inline-block;vertical-align:top; 
            padding-top: 15px;"),
          conditionalPanel(
            "input.selVarPlot != 'No Variables Available'",
            plotOutput("customPlot"),
            actionButton(inputId = "savePlot", "Save Plot")
          ),
          conditionalPanel(
            "input.selVarPlot == 'No Variables Available'",
            h5("No Variables to Plot, you must first simulate a model.")
          )
        )
      )
    )
  )
)

# Note: Every input id for rhandsontables and ace editor scripts must have the 
# name of the corresponding data variable followed by 'Input'. 
# Example: 'odeInput' corresponds to 'ode' reactive variable
scenarioPage <- shinydashboard::tabItem(
  tabName = "scenarioPage",
  fluidPage(
    shinydashboard::box(
      title = "Scenario Definition",
      solidHeader = T,
      status = "primary",
      width = "100%",
      # h5("Define variables to be used in the simulation."),
      
      div(
        id = "editScenarioIdDiv",
        tags$button(
          id = "editScenarioId",
          class = "btn action-button shiny-bound-input",
          HTML("Change Scenario ID"),
          img(src = "icon_edit.png", height = "30px"),
          style = "padding: 1px 0px 20px 10px; height: 30px; 
          border: 0px; display: inline-block; vertical-align:bottom;"
        )
      ),
      div(style = "padding: 3px;"),
      shinydashboard::tabBox(
        id = "configBox", title = "", width = "100%",
        tabPanel(
          "State", 
          rhandsontable::rHandsontableOutput("state",
                                             height = tableHeight, 
                                             width = tableWidth),
          br(),
          shinydashboard::box(
            title = ("Usage"),
            solidHeader = T, 
            status = "primary",
            width = "100%",
            collapsible = T,
            h5('The "State" sheet is used to define the state 
               variables of the system and their initial values 
               in the simulation.'),
            h5('The state variables are used to describe the 
               mathematical "state" of the system. The continuous 
               rate of change of these variables is determined 
               by the differential equations function.'),
            h5("The variables must be declared one per sheet line. The sheet's 
               columns represent:"),
            tags$ul(
              tags$li("Variable: the name of the state variable."),
              tags$li("Value: the initial value of the state variable."),
              tags$li("Unit: the measurement unit of the state variable."),
              tags$li("Description: the description of the state variable.")
            ),
            h5('The defined state variables are accessible 
               by the model\'s functions, inside the list "st". 
               Example: st$y')
          )
          
        ),
        tabPanel(
          "Constant", 
          rhandsontable::rHandsontableOutput("constant", 
                                             height = tableHeight, 
                                             width = tableWidth),
          br(),
          shinydashboard::box(
            title = ("Usage"),
            solidHeader = T, 
            status = "primary",
            width = "100%",
            collapsible = T,
            h5('The "Constant" sheet is used to define the constants
               of the system and their values.'),
            h5('Constants are variables whose values remain constant
               throughout the simulation.'),
            h5("The constants must be declared one per sheet line. The sheet's 
               columns represent:"),
            tags$ul(
              tags$li("Variable: the name of the constant."),
              tags$li("Value: the value of the constant."),
              tags$li("Unit: the measurement unit of the constant."),
              tags$li("Description: the description of the constant.")
            ),
            h5('The defined constants are accessible 
               by the model\'s functions, inside the list "ct". 
               Example: ct$k.')
          )
        ),
        tabPanel(
          "Parameter", 
          rhandsontable::rHandsontableOutput("parameter", 
                                             height = tableHeight, 
                                             width = tableWidth),
          br(),
          shinydashboard::box(
            title = ("Usage"),
            solidHeader = T, 
            status = "primary",
            width = "100%",
            collapsible = T,
            h5('The "Parameter" sheet is used to define the
               parameters of the system and their values.'),
            h5('Parameters are variables that contain calibrated
               values for specific scenarios.'),
            h5("The parameters must be declared one per sheet line. The sheet's 
               columns represent:"),
            tags$ul(
              tags$li("Variable: the name of the parameter."),
              tags$li("Value: the value of the parameter."),
              tags$li("Unit: the measurement unit of the parameter."),
              tags$li("Description: the description of the parameter.")
            ),
            h5('The defined parameters are accessible 
               by the model\'s functions, inside the list "par". 
               Example: par$p.')
          )
          
        ),
        tabPanel(
          "Input", 
          fluidRow(
            column(2, fileInput("importTimeSeries", "Upload time series files", 
                                multiple = T, width = 300)),
            column(2, actionButton("viewTimeSeries", 
                                   label = "View Uploaded Time Series"),
                   style = "margin-top: 25px;")
          ),
          # br(),
          rhandsontable::rHandsontableOutput("input", 
                                             height = tableHeight, 
                                             width = tableWidth),
          br(),
          shinydashboard::box(
            title = ("Usage"),
            solidHeader = T, 
            status = "primary",
            width = "100%",
            collapsible = T,
            h5('The "Input" sheet is used to define the inputs
               of the system and their values.'),
            h5('Inputs are variables that contain the input values
               of each simuation.'),
            h5("The inputs can also be defined as time series. 
               This can be done by uploading a time series file and
               typing its name in the value field."),
            h5("The inputs that contain time series have its
               values interpolated. The interpolation methods available are:"),
            tags$ul(
              tags$li("linear"),
              tags$li("constant"),
              tags$li("fmm"),
              tags$li("natural"),
              tags$li("periodic"),
              tags$li("monoH.FC"),
              tags$li("hyman"),
              style = "columns: 100px"
            ),
            h5("The inputs must be declared one per sheet line. The sheet's 
               columns represent:"),
            tags$ul(
              tags$li("Variable: the name of the input."),
              tags$li("Value: the value of the input."),
              tags$li("Unit: the measurement unit of the input."),
              tags$li("Description: the description of the input."),
              tags$li("Interpolation: the interpolation method (only used for
                      time series).")
            ),
            h5('The defined inputs are accessible 
               by the model\'s functions, inside the list "inp".
               Example: inp$i')
          )
        ),
        tabPanel(
          "Switch", 
          rhandsontable::rHandsontableOutput("switch", 
                                             height = tableHeight, 
                                             width = tableWidth),
          br(),
          shinydashboard::box(
            title = ("Usage"),
            solidHeader = T, 
            status = "primary",
            width = "100%",
            collapsible = T,
            h5('The "Switch" sheet is used to define the switches
               of the system and their values.'),
            h5('Switches are conditional variables that are used
               as selectors in the model\'s functions'),
            h5("The switches must be declared one per sheet line. The sheet's 
               columns represent:"),
            tags$ul(
              tags$li("Variable: the name of the switch"),
              tags$li("Value: the value of the switch"),
              tags$li("Unit: the measurement unit of the switch"),
              tags$li("Description: the description of the switch")
            ),
            h5('The defined switches are accessible 
               by the model\'s functions, inside the list "sw". 
               Example: sw$s.')
          )
        )
      )
    )
  )
)

# Note: Every input id for rhandsontables and ace editor scripts must have the 
# name of the corresponding data variable followed by 'Input'. 
# Example: 'odeInput' corresponds to 'ode' reactive variable
odePage <- shinydashboard::tabItem(
  tabName = "odePage",
  fluidPage(
    shinydashboard::box(
      title = "Edit Model",
      solidHeader = T,
      width = "100%",
      status = "primary",
      # h5("Define the model functions and plotting configuration."),
      
      tags$button(
        id = "editModelId",
        class = "btn action-button shiny-bound-input",
        HTML("Change Model ID"),
        img(src = "icon_edit.png", height = "30px"),
        style = "padding: 1px 0px 20px 10px; height: 30px; 
        border: 0px; display: inline-block; vertical-align:bottom;"
      ),
      div(style = "padding: 3px;"),
      div(id = "odeModelPage",
          shinydashboard::tabBox(
            id = "modelBox", title = "Ode Model", width = "100%",
            tabPanel(
              "Differential Equations",
              radioButtons("odeType", NULL, 
                           choices = c("Function", "Flow Map"),
                           selected = "Function",
                           inline = T),
              AceEditorCustom("odeFunction", height = scriptHeight),
              rhandsontable::rHandsontableOutput("odeFlow",
                                                 height = "260px",
                                                 width = tableWidth),
              checkboxInput("showFlowDiagram",
                            "Display Flow Diagram",
                            value = FALSE),
              conditionalPanel(
                "input.showFlowDiagram != false",
                visNetworkOutput("flowDiagram")
              ),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("Computes the values of the state 
                                               variables derivatives in the ODE system (the model 
                                               definition) at time t."),
                h5("It is defined as the function:"),
                tags$ul(h5("function(t, st, ct, par, inp, sw, aux)")),
                h5("Where,"),
                tags$ul(
                  tags$li("t: the current time instant in the 
                                                         integration;"),
                  tags$li("st: a list with the current estimate of 
                                                         the variables in the ODE system;"),
                  tags$li("ct: a list of constants;"),
                  tags$li("par: a list of parameters;"),
                  tags$li("inp: a list of inputs;"),
                  tags$li("sw: a list of switches;"),
                  tags$li("aux: a list of auxiliary variables.")
                ),
                h5("The return value of this function must be a list, 
                   of which the first element is a vector containing the 
                   derivatives of the state variables with respect to 
                   time, and the next elements are auxiliary values 
                   that are computed at each time step and will be 
                   included in the simulation output. The derivatives 
                   must be specified in the same order as the state 
                   variables."),
                br(),
                h5("Example from Arenstorf Model:"),
                AceEditorCustom("odeExample",
                                theme = "idle_fingers",
                                showLineNumbers = F,
                                showPrintMargin = F,
                                readOnly = T, 
                                height = "180px",
                                highlightActiveLine = F, 
                                value = odeExampleStr)
              )
              
            ),
            tabPanel(
              "Parameter Initialization",
              AceEditorCustom("initVars", height = scriptHeight),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("An optional function to create or change the 
                   model variables before the simulation."),
                h5("It is defined as the function:"),
                tags$ul(h5("function(st, ct, par, inp, sw, aux)")),
                h5("Where,"),
                tags$ul(
                  tags$li("st: a list with the initial values of 
                                                         state variables"),
                  tags$li("ct: a list of constants;"),
                  tags$li("par: a list of parameters;"),
                  tags$li("inp: a list of inputs;"),
                  tags$li("sw: a list of switches;"),
                  tags$li("aux: a list of auxiliary variables.")
                ),
                h5("The return value of initVars is a list 
                   containing the modified variable lists.
                   e.g. return(list(st = st, ct = ct, inp = inp, par = par
                   , sw = sw, aux = aux))."),
                br(),
                h5("Example from Arenstorf Model:"),
                AceEditorCustom("parmInitExample",
                                theme = "idle_fingers",
                                showLineNumbers = F, 
                                showPrintMargin = F,
                                readOnly = T,
                                height = "158px", 
                                highlightActiveLine = F, 
                                value = parmInitExampleStr)
              )
            ),
            tabPanel(
              "Trigger", AceEditorCustom("trigger", height = scriptHeight),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("The trigger function, or Root function, is an optional 
                   R-function that triggers an Event Function when it returns 
                   zero."),
                h5("It is defined as the function:"),
                tags$ul(h5("function(t, st, ct, par, inp, sw, aux)")),
                h5("Where,"),
                tags$ul(
                  tags$li("t: the current time instant in the 
                             integration;"),
                  tags$li("st: a list with the current estimate 
                             of the variables in the ODE system;"),
                  tags$li("ct: a list of constants;"),
                  tags$li("par: a list of parameters;"),
                  tags$li("inp: a list of inputs;"),
                  tags$li("sw: a list of switches;"),
                  tags$li("aux: a list of auxiliary variables.")
                ),
                h5("If no Event Function is defined, when the trigger function 
                   returns zero, the simulation stops. "),
                h5("The return must be numeric vector. If any element of 
                   this vector is zero an event is trigged."),
                br(),
                h5("Example from Bouncing Ball Model:"),
                AceEditorCustom("rootExample",
                                theme = "idle_fingers",
                                showLineNumbers = F, 
                                showPrintMargin = F,
                                readOnly = T, 
                                height = "110px", 
                                highlightActiveLine = F, 
                                value = rootExampleStr)
              )
            ),
            tabPanel(
              "Event", AceEditorCustom("event", height = scriptHeight),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("An optional function that specifies the event that
                   is triggered when the trigger function returns 0. It can
                   be used to change state values during the execution
                   of the simulation."),
                h5("It is defined as the function:"),
                tags$ul(h5("function(t, st, ct, par, inp, sw, aux)")),
                h5("Where,"),
                tags$ul(
                  tags$li("t: the current time instant in the 
                             integration;"),
                  tags$li("st: a list with the current estimate of 
                             the variables in the ODE system;"),
                  tags$li("ct: a list of constants;"),
                  tags$li("par: a list of parameters;"),
                  tags$li("inp: a list of inputs;"),
                  tags$li("sw: a list of switches;"),
                  tags$li("aux: a list of auxiliary variables.")
                ),
                h5("It should return the state-values (some of which might
                   have been modified by the function), as a vector with 
                   the variables in the right order."),
                h5("If no Event Function is defined, when the trigger function 
                   returns zero, the simulation stops. "),
                br(),
                h5("Example from Bouncing Ball Model:"),
                AceEditorCustom("eventExample",
                                theme = "idle_fingers",
                                showLineNumbers = F, 
                                showPrintMargin = F,
                                readOnly = T, 
                                height = "210px", 
                                highlightActiveLine = F, 
                                value = eventExampleStr)
              )
            ),
            tabPanel(
              "Auxiliaries", 
              rhandsontable::rHandsontableOutput("aux",
                                                 height = tableHeight,
                                                 width = tableWidth),
              br(),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("'aux' represents a set of equations for calculating
                   the corresponing auxiliary variables at each step of 
                   the simulation."),
                h5("The parameters available to be used in the auxiliary
                   equations are:"),
                tags$ul(
                  tags$li("t: the current time instant in the 
                             integration;"),
                  tags$li("st: a list with the current estimate of 
                             the variables in the ODE system;"),
                  tags$li("ct: a list of constants;"),
                  tags$li("par: a list of parameters;"),
                  tags$li("inp: a list of inputs;"),
                  tags$li("sw: a list of switches;"),
                  tags$li("aux: a list of auxiliary variables.")
                ),
                h5("The auxiliary variables will be automatically
                   calculated at each step of the simulation, and the aux
                   list will contain its results."),
                h5("The auxiliary variables are evaluated in order of 
                   dependency. This means that an auxiliary variable 'x' depends
                   on another auxiliary 'y', the auxiliary 'y' will be sorted to
                   be evaluated before 'x'."),
                h5("The auxiliary variables must be declared one per sheet line. 
                   The sheet's columns represent:"),
                tags$ul(
                  tags$li("Variable: the name of the variable."),
                  tags$li("Value: the equation that calculates the variable."),
                  tags$li("Unit: the measurement unit of the variable."),
                  tags$li("Description: the description of the variable.")
                ),
                h5("Tip: While editing the variable tables Alt+Enter can 
                   be used to insert a new line inside a single cell.")
              )
              
            ),
            tabPanel(
              "Global Functions",
              AceEditorCustom("globalFunctions", height = scriptHeight),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("Global functions are functions that can be executed
                   by the other model's functions."),
                h5("The global functions must be written in the script 
                   area using R programming language. After defined
                   the functions can be executed in any function and
                   auxiliary variables by writing the name of the function
                   followed by the arguments inside paranthesis."),
                h5("Example of the definition of global functions:"),
                AceEditorCustom("globalFunctionDefiningExample",
                                theme = "idle_fingers",
                                showLineNumbers = F, 
                                showPrintMargin = F,
                                readOnly = T, 
                                height = "125px", 
                                highlightActiveLine = F, 
                                value = globalExampleStr),
                h5("Example of function execution in the 
                   Differential Equations:"),
                AceEditorCustom("globalFunctionCallExample",
                                theme = "idle_fingers",
                                showLineNumbers = F, 
                                showPrintMargin = F,
                                readOnly = T, 
                                height = "140px", 
                                highlightActiveLine = F, 
                                value = globalCallExampleStr)
              )
            )
          ) 
      ),
      div(id = "staticModelPage",
          shinydashboard::tabBox(
            id = "staticModelBox", title = "Static Model", width = "100%",
            tabPanel(
              "Equations",
              rhandsontable::rHandsontableOutput("staticAux", 
                                                 height = tableHeight, 
                                                 width = tableWidth),
              br(),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("'Equations' represents a set of equations for calculating 
                   the corresponing variables at each step of the 
                   simulation."),
                h5("The parameters available to be used in the equations are:"),
                tags$ul(
                  tags$li("t: the current time instant in the 
                             integration;"),
                  tags$li("ct: a list of constants;"),
                  tags$li("par: a list of parameters;"),
                  tags$li("inp: a list of inputs;"),
                  tags$li("sw: a list of switches;"),
                  tags$li("equations: a list of containing the results
                             after evaluation.")
                ),
                h5("The equations will be automatically
                   calculated at each step of the simulation, and the equations
                   list will contain its results."),
                h5("The equations are evaluated in order of dependency. This
                   means that an equation 'x' depends on another equation 'y',
                   the equation 'y' will be sorted to be evaluated before 'x'."),
                h5("Tip: While editing the variable tables Alt+Enter can 
                   be used to insert a new line inside a single cell.")
              )
            ),
            tabPanel(
              "Parameter Initialization",
              AceEditorCustom("staticInitVars", height = scriptHeight),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("An optional function to create or change the model variables
                   before the simulation."),
                h5("It is defined as the function:"),
                tags$ul(h5("function(ct, par, inp, sw, aux)")),
                h5("Where,"),
                tags$ul(
                  tags$li("ct: a list of constants;"),
                  tags$li("par: a list of parameters;"),
                  tags$li("inp: a list of inputs;"),
                  tags$li("sw: a list of switches;"),
                  tags$li("aux: a list of auxiliary variables.")
                ),
                h5("The return value of initVars is a list 
                   containing the modified variable lists.
                   e.g. return(list(ct = ct, inp = inp, par = par, 
                   sw = sw, aux = aux))."),
                br(),
                h5("Example from Arenstorf Model:"),
                AceEditorCustom("staticParmInitExample",
                                theme = "idle_fingers",
                                showLineNumbers = F, 
                                showPrintMargin = F,
                                readOnly = T,
                                height = "158px", 
                                highlightActiveLine = F, 
                                value = staticParmInitExampleStr)
              )
            ),
            tabPanel(
              "Global Functions",
              AceEditorCustom("staticGlobalFunctions", height = scriptHeight),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("Global functions are functions that can be executed
                   by the other model's functions."),
                h5("The global functions must be written in the script 
                   area using R programming language. After defined
                   the functions can be executed in any function or
                   equations by writing the name of the function
                   followed by the arguments inside paranthesis."),
                h5("Example of the definition of global functions:"),
                AceEditorCustom("staticGlobalFunctionDefiningExample",
                                theme = "idle_fingers",
                                showLineNumbers = F, 
                                showPrintMargin = F,
                                readOnly = T, 
                                height = "125px", 
                                highlightActiveLine = F, 
                                value = globalExampleStr),
                h5("Example of function execution in the 
                   Differential Equations:"),
                AceEditorCustom("staticGlobalFunctionCallExample",
                                theme = "idle_fingers",
                                showLineNumbers = F, 
                                showPrintMargin = F,
                                readOnly = T, 
                                height = "140px", 
                                highlightActiveLine = F, 
                                value = globalCallExampleStr)
              )
            )
          ) 
      ),
      div(id = "coupledModelPage",
          shinydashboard::tabBox(
            id = "coupledModelBox", title = "Coupled Model", width = "100%",
            tabPanel(
              "Components",
              rhandsontable::rHandsontableOutput("componentIds",
                                                 height = tableHeight,
                                                 width = tableWidth),
              br(),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("The “Components” sheet is used to define the component
                   models IDs. The component IDs should be defined one per line.
                   All IDs listed must correspond to a model that is loaded in 
                   the application.")
              )
            ),
            tabPanel(
              "Connections",
              rhandsontable::rHandsontableOutput("connections",
                                                 height = tableHeight,
                                                 width = tableWidth),
              br(),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("The “Connections” list is used to define the connections
                 between the model's components. The connections must be defined
                   one per line. Each connection parameter is defined in its
                   respective column. The connection parameters are:"),
                tags$ul(
                  tags$li("Connection ID: the identification of the connection"),
                  tags$li("Receiver Component ID: The identification of the 
                          receiver component."),
                  tags$li("Receiver Input: the name of the input variable from
                          the receiver component."),
                  tags$li("Sender Component ID: the identification of the sender
                          component."),
                  tags$li("Sender Output: the name of the connected state variable
                          or, auxiliary or algebraic, equation with prefix st$, 
                          aux$ or eq$, respectively, indicating the output type 
                          from the sender component, e.g. st$<varName>,
                          aux$<eqName> or eq$<eqName>.")
                )
              )
            )
          )
      )
    )
  )
)

descriptionPage <- shinydashboard::tabItem(
  tabName = "descriptionPage",
  fluidPage(
    shinydashboard::box(
      title = "Model Description",
      solidHeader = T,
      status = "primary",
      width = "100%",
      collapsible = F,
      AceEditorCustom("description", showPrintMargin = T,
                      showLineNumbers = F, mode = "", height = scriptHeight,
                      wordWrap = T)
    )
  )
)

managePage <- shinydashboard::tabItem(
  tabName = "managePage",
  fluidPage(
    shinydashboard::box(
      title = "Manage Models",
      solidHeader = T, 
      status = "primary",
      width = "100%",
      collapsible = F,
      selectInput("uploadedModelsManage", "Uploaded Models", choices = c("a", "b", "c")),
      actionButton("deleteModel", "Delete Model"),
      actionButton("renameModel", "Rename Model")
    )
  )
)

helpPage <- shinydashboard::tabItem(
  tabName = "helpPage",
  fluidPage(
    shinydashboard::box(
      title = "Instructions",
      solidHeader = T, 
      status = "primary",
      width = "100%",
      collapsible = F,
      br(),
      fluidPage(
        fluidRow(
          column(
            width = 3,
            tags$ul(
              style = "padding-left: 17px",
              tags$li(a(href = "#Interface_Usability", "Interface Usability")),
              tags$li(a(href = "#Programming_Language", "Programming Language")),
              tags$li(a(href = "#Model", "Model")),
              tags$li(
                style="list-style-type:none",
                tags$ul(
                  tags$li(a(href = "#ODE_Model", "ODE Model")),
                  tags$li(a(href = "#Static_Model", "Static Model")),
                  tags$li(a(href = "#Coupled_Model", "Coupled Model")),
                  tags$li(a(href = "#Managing_Models", "Managing Models"))
                )
              ),
              tags$li(a(href = "#Scenario", "Scenario")),
              tags$li(
                style="list-style-type:none",
                tags$ul(
                  tags$li(a(href = "#Default_Scenario", "Default Scenario")),
                  tags$li(a(href = "#Alternate_Scenario", "Alternate Scenario")),
                  tags$li(a(href = "#Managing_Scenarios", "Managing Scenarios"))
                )
              ),
              tags$li(a(href = "#Simulation", "Simulation")),
              tags$li(
                style="list-style-type:none",
                tags$ul(
                  tags$li(a(href = "#Parameters", "Parameters")),
                  tags$li(a(href = "#Execution", "Execution")),
                  tags$li(a(href = "#Results", "Results"))
                )
              ),
              tags$li(a(href = "#References", "References"))
            )
          ),
          column(
            width = 9,
            div(
              style = "height: 80vh; overflow-y: scroll;",
              h4(id = "Interface_Usability", strong("Interface Usability")),
              h5("This section provides a general explanation of this application, and 
                      introduces the modeling and simulation approach of the sdsim package. 
                      Detailed instructions of the application's functionalities are 
                      available under the tab “Usage” at each menu tab."),
              br(),
              h4(id = "Programming_Language", strong("Programming Language")),
              h5("This is a companion application to the sdsim package, developed for 
                      the programming language R. Therefore, everything created in this 
                      application must be written in R."),
              br(),
              h4(id = "Model", strong("Model")),
              h5("In a modeling and simulation context, a model is conceived as 
                      mathematical representation of a system. It is a set of instructions, 
                      rules, equations, or constraints for generating input/output behavior 
                      . This application supports three types of 
                      models: ODE models, static models and coupled models."),
              br(),
              h5(id = "ODE_Model", strong("ODE Model")),
              h5("An ODE (ordinary differential equation) model has its behaviour 
                      determined by its current state, which is represented by state 
                      variables, and by a system of ODEs which specifies the rate of change 
                      of the model state. Along with a scenario containing the trajectories 
                      of the driver variables, the ODE model can be simulated to forecast the
                      evolution of the real system’s state over time."),
              h5("An ODE model is specified by:"),
              tags$ul(
                tags$li("ID: the identification name of the model."),
                tags$li("Description: (optional) a text description of the model."),
                tags$li("Differential Equations: an R-function that computes the values 
                             of the state variables derivatives in the ODE system (the model 
                             definition) at time t. The calculated derivatives are used to 
                             calculate the growth of the model's state throughout the 
                             simulation."),
                tags$li("Initialization Function: (optional) an R-function that 
                             initializes or changes the initial state values and/or other 
                             model variables before the solver call when running a 
                             simulation. It can be used to initialize variables whose values 
                             are calculated depending on other variables from the system."),
                tags$li("Trigger Function: (optional) an R-function that triggers an 
                             Event Function when it returns zero. If no Event Function is 
                             defined, when zero is returned the simulation stops."),
                tags$li("Event Function: (optional) an R-function that specifies the 
                             event."),
                tags$li("Auxiliary Variables: (optional) a set of equations that are 
                             used to calculate auxiliary variables at each step of the 
                             simulation. These variables represent intermediary values used 
                             to compute the derivatives of the differential equations."),
                tags$li("Global Functions: (optional) a set of R-functions that can be 
                             executed in the scope of other functions defined in the model.")
              ),
              br(),
              h5(id = "Static_Model", strong("Static Model")),
              h5("A static model calculates the system in equilibrium, and thus is 
                      time-invariant. It represents a steady-state (no state variables) 
                      operation in which the system variables are assumed to remain constant 
                      in time", 
                 Popover(
                   id = "ref_Karnopp_et_al_2006",
                   text = "(Karnopp et al., 2006)", 
                   title = "System Dynamics: Modeling and Simulation of Mechatronic Systems", 
                   content = "Dean C. Karnopp, Donald L. Margolis, and Ronald C. 
                        Rosenberg. 2006. System Dynamics: Modeling and Simulation of 
                        Mechatronic Systems. John Wiley & Sons, Inc., New York, NY, USA.",
                   href = "#note_Karnopp_et_al_2006"), 
                 ". A static model is defined by algebraic equations with static 
                      behaviour."),
              h5("A static model is specified by:"),
              tags$ul(
                tags$li("ID: the identification name of the model."),
                tags$li("Description: (optional) a text description of the model."),
                tags$li("Equations: a set of algebraic equations that are calculated 
                             during the simulation."),
                tags$li("Initialization Function: (optional) an R-function that 
                             initializes or changes the initial state values and/or other
                             model variables before the solver call when running a 
                             simulation. It can be used to initialize variables whose values 
                             are calculated depending on other variables from the system."),
                tags$li("Global Functions: (optional) a set of R-functions that can be 
                             executed in the scope of other functions or equations defined 
                             in the model.")
              ),
              h5("In this definition a static model provides the system’s response to a 
                      specific set of input conditions, specified by the scenario. As a 
                      result, assuming that no exogenous sources are specified, such as time 
                      series or connections between models, the algebraic equations will
                      always output the same result, meaning its output trajectory will be 
                      constant."
                 # Popover(text = "(sdsim)", title = "sdsim", content = "sdsim")
              ),
              br(),
              h5(id = "Coupled_Model", strong("Coupled Model")),
              h5("A coupled model is a set of models that are coupled together to define
                      a complex system. The flow of information between the component models 
                      of a coupled system is defined through input and output connections. 
                      ", 
                 Popover(
                   id = "ref_Zeigler_et_al_2000",
                   text = "(Zeigler et al., 2000)", 
                   title = "Theory of modeling and simulation", 
                   content = "ZEIGLER, B. P.; PRAEHOFER, H.; KIM, T. G. Theory of 
                        modeling and simulation: integrating discrete event and continuous 
                        complex dynamic systems. [S.l.]: Academic press, 2000.",
                   href = "#note_Zeigler_et_al_2000")),
              h5("A coupled model is specified by:"),
              tags$ul(
                tags$li("ID: the identification name of the model."),
                tags$li("Description: (optional) a text description of the model."),
                tags$li("Components: a set of IDs of existing atomic, static or coupled 
                             models. The component models must be previously loaded in the 
                             application."),
                tags$li("Connections: a data table describing the connections between 
                             the components of the model. The connections determine loops of 
                             information feedback and circular causality for conceptualizing 
                             the structure of a complex system and for communicating 
                             model-based insights.")
              ),
              br(),
              h5(id = "Managing_Models", strong("Managing Models")),
              h5("Models can be managed using the menu under “Model”, located at the 
                      sidebar menu. The dropdown input is used to switch between loaded 
                      models."),
              h5("Models can be created or loaded clicking the plus sign button (", 
                 img(src = "icon_add.png", height = "20px", 
                     style = "padding: 0 0 3px 0"),
                 "), located “Model”. New models can be created 
                      empty, loaded from existing models xml files, loaded from one of the 
                      examples available, or cloned from the currently selected model."),
              h5("The selected model's specification can be viewed and modified in the
                      “Edit Model” menu. The model’s description can be modified in the 
                      “Description” menu."),
              h5("The selected model's ID can be changed in the “Edit Model” menu using 
                      the “Change Model ID” button."),
              h5("The selected model can be deleted from the application by clicking the
                      minus sign button (",
                 img(src = "icon_remove.png", height = "20px", 
                     style = "padding: 0 0 3px 0"),
                 "), under “Model”."
              ),
              h5("The selected model can be saved to a XML file by clicking the button 
                      with the folder icon (",
                 img(src = "icon_save.png", height = "20px", 
                     style = "padding: 0 0 3px 0"),
                 "), under “Model”, and downloading the file. This file
                      can be used to
                      reload the model into the application. It can also be loaded to R using
                      the functions exported by the sdsim package."),
              br(),
              h4(id = "Scenario",strong("Scenario")),
              h5("A scenario contains the variables and values that describe the 
                      environment in which a system is embedded."),
              h5("A scenario is specified by:"),
              tags$ul(
                tags$li("State: the initial value of state variables, which will change 
                             over time during the simulation according to the value of its 
                             respective derivative. Every ODE model must have at least one 
                             state variable. Static models scenarios do not have state 
                             variables"),
                tags$li("Constants: remain immutable across different simulations and 
                             cannot be calibrated by statistical methods."),
                tags$li("Parameters: variables that can be calibrated using statistical 
                             methods."),
                tags$li("Switches: discrete variables that are used as conditional 
                             selectors in the model. Switches can be used, for instance, to 
                             change parameter initialization values or to change the control 
                             flow of the model (i.e. to change the parts of the code which 
                             will be used by the model)."),
                tags$li("Inputs: exogenous variables (also called drivers). These 
                             variables can be numeric values, time series or forcing 
                             functions (i.e. functions that only depends on time). Time 
                             series should be uploaded as csv (comma separated values) format
                             files with time in the first column and the respective variable 
                             value in the second column. Both columns must have the 
                             headers.")
              ),
              h5("This application supports two types of scenarios: default scenarios 
                      and alternate scenarios."),
              br(),
              h5(id = "Default_Scenario",strong("Default Scenario")),
              h5("Default cenarios contain the base values of each 
                      model's variables. Every ode and static models have default 
                      scenarios. Coupled models use the defaults of its components, 
                      thus it does not have its own default scenario. The ID of
                      default scenarios cannot be changed, and will always be named 
                      “Default”."),
              br(),
              h5(id = "Alternate_Scenario",strong("Alternate Scenario")),
              h5("Alternate scenarios contain the value for at least
                      one of the model's variables. This scenario can be used to run
                      different simulations using the same model, without the need
                      to alter the default scenario. If a variable from the model is
                      not specified in the alternate scenario, the simulation will
                      be executed using that variable's value from the default
                      scenario."),
              br(),
              h5(id = "Managing_Scenarios", strong("Managing Scenarios")),
              h5("Scenarios can be managed using the menu under “Scenario”, located at 
                      the sidebar menu. The dropdown input is used to switch between loaded 
                      scenarios. Each model has a different set of scenarios, and the 
                      available scenarios will change according to the selected model."),
              h5("Scenarios can be created or loaded clicking the plus sign button(", 
                 img(src = "icon_add.png", height = "20px", 
                     style = "padding: 0 0 3px 0"),
                 "), under “Scenario”. New scenarios can be created empty or loaded from
                      existing scenarios xml or xlsx files."),
              h5("The selected scenario's state variables, constants, inputs and 
                      switches can be viewed and modified in the “Edit Scenario” menu."),
              h5("The selected scenario's ID can be changed in the “Edit Scenario” menu 
                      by clicking the “Change Scenario ID” button."),
              h5("The selected scenario can be deleted from the application by clicking 
                      the minus sign button (",
                 img(src = "icon_remove.png", height = "20px", 
                     style = "padding: 0 0 3px 0"),
                 "), under “Scenario”. Default scenarios cannot be deleted."),
              h5("The selected scenario can be saved into a XML or xlsx file by clicking
                      the button with the folder icon (",
                 img(src = "icon_save.png", height = "20px", 
                     style = "padding: 0 0 3px 0"),
                 "), under “Scenario”, and downloading the file. This file can be used 
                      to reload the model into the application. It can also be loaded to R 
                      using the functions exported by the sdsim package."),
              br(),
              h4(id = "Simulation", strong("Simulation")),
              h5("After models are loaded, they can be simulated in the “Simulation” 
                      menu, located at the sidebar."),
              br(),
              h5(id = "Parameters", strong("Parameters")),
              h5("The integration method can be chosen under “Method”. The available
                      methods are:"),
              tags$ul(
                tags$li("lsoda"),
                tags$li("lsode"),
                tags$li("lsodes"),
                tags$li("lsodar"),
                tags$li("vode"),
                tags$li("daspk"),
                tags$li("euler"),
                tags$li("rk4 (Runge-Kutta)"),
                tags$li("ode23"),
                tags$li("ode45"),
                tags$li("radau"),
                tags$li("bdf"),
                tags$li("bdf_d"),
                tags$li("adams"),
                tags$li("impAdams"),
                tags$li("impAdams_d"),
                style = "columns: 130px;"
              ),
              h5("Models that require events cannot work with some of the methods. The
                      available methods for models with events are:"),
              tags$ul(
                tags$li("lsoda"),
                tags$li("lsode"),
                tags$li("lsodes"),
                tags$li("lsodar"),
                tags$li("radau"),
                style = "columns: 130px;"
              ),
              h5("The simulation times can be set under “Initial Time”, determines 
                      time when the simulations begins, usually 0, “Final 
                      Time”, determines when the simulation ends, and “Time Step”, the
                      interval between each step of the simulation."),
              br(),
              h5(id = "Execution", strong("Execution")),
              h5("The selected model, together with the selected scenario, can be 
                      simulated by pressing the “Start Simulation” button. If any errors or 
                      warnings occur during the simulation, they will be logged and displayed
                      inside a text panel."),
              br(),
              h5(id = "Results", strong("Results")),
              h5("The simulation results are displayed under the “Trajectory” tab, 
               which contains three other tabs: “Result”, where the trajectory of 
               the state variables or algebraic equations are displayed; “Auxiliary 
               Variables”, where the trajectory of the auxiliary variables is 
               displayed; and “Time Series”, where the trajectory of time series 
               inputs is displayed. Each trajectory can be saved using the button 
               “Export CSV” located at the bottom of each of these tabs."),
              h5("The simulation results can be plotted using the “Plot” tab. The
               plot parameters are:"),
              tags$ul(
                tags$li("Plot Title: The text displayed at the top of the plot."),
                tags$li("X-Axis Label: The text displayed under the horizontal axis."),
                tags$li("Y-Axis Label: The text displayed to the left of the vertical
                      axis. If there are multiple Y-Axis variables, the label of each
                      axis can be separated by commas. Example: 'Variable X, 
                      Variable Y, Variable Z'"),
                tags$li("Y-Axis Variables: One or more variables that will be 
                      plotted."),
                tags$li("X-Axis Variable: The variable that will be represented as the
                      X-Axis."),
                tags$li("Plot Type: Choose if the plot is drawn using points or line."),
                tags$li("Display Variable's Units: Choose if the variable's units are
                      displayed at the side of the Y-Axis Label."),
                tags$li("Display Multiple Y-Axis: This is only used for plots with more 
                      than one variable. Choose if each variable is plotted 
                      using different Y-Axis value intervals, or if all variables are
                      plotted using the same Y-Axis value interval. This can be
                      enabled if the difference of value between the plotted variables
                      is too large.")
              ),
              br(),
              h4(id = "References", strong("References")),
              tags$ul(
                tags$li(
                  h5(id = "note_Karnopp_et_al_2006",
                     tags$a(href = "#ref_Karnopp_et_al_2006", strong("^")),
                     "Dean C. Karnopp, Donald L. Margolis, and Ronald C. Rosenberg. 
                   2006. System Dynamics: Modeling and Simulation of Mechatronic 
                   Systems. John Wiley & Sons, Inc., New York, NY, USA.")),
                tags$li(
                  h5(id = "note_Zeigler_et_al_2000",
                     tags$a(href = "#ref_Zeigler_et_al_2000", strong("^")), 
                     "Zeigler, B. P.; Praehofer, H.; Kim, T. G. Theory of modeling and 
                   simulation: integrating discrete event and continuous complex 
                   dynamic systems. [S.l.]: Academic press, 2000."))
              )
            )
          )
        )
      )
    )
  )
)

aboutPage <- shinydashboard::tabItem(
  tabName = "aboutPage",
  fluidPage(
    shinydashboard::box(
      title = ("About"),
      solidHeader = T, 
      status = "primary",
      width = "100%",
      collapsible = F,
      h4("System Dynamics Simulator User Interface"),
      h5("This is a companion application for the sdsim package, designed
         to assist the implementation and simulation of System Dynamics models. 
         It makes use of the R language package sdsim and shares its standards 
         for Differential Equations and file formats. It also relies on the 
         simulation algorithms and other functionalities of the sdsim 
         package."),
      br(),
      h4("Our Team"),
      tags$ul(
        tags$li("Adauto Luiz Mancini - Embrapa Agricultural Informatics"),
        tags$li("Bruno Henrique Pereira - Federal University of São Carlos"),
        tags$li("Cristina Freitas Bazzano - University of Campinas"),
        tags$li("Luis Gustavo Barioni - Embrapa Agricultural Informatics"),
        tags$li("Márcio Nicolau - Embrapa Wheat")
      ),
      br(),
      h4("Contact"),
      h5("luis.barioni@embrapa.br")
    )
  )
)


# Dashboard layout
body <- shinydashboard::dashboardBody(
  shinydashboard::tabItems(managePage, simulationPage, scenarioPage, odePage, 
                           descriptionPage, helpPage, aboutPage),
  
  # Add javascript functions to sidebar
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  tags$script(src = "custom.js")
)

# Interface
ui <- shinydashboard::dashboardPage(
  header,
  sidebar,
  body,
  skin = "green"
)
