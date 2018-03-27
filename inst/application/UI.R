# shinyUI
source("AuxiliaryUI.R", local = TRUE)

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
        img(src = "icon_save2.png",
            height = "34px"),
        style = "border-radius: 50%; padding: 0px 0px 0px 0px; height: 30px; 
        width: 30px; border: 0px; background-color: transparent; 
        display: inline-block;vertical-align:top;"
      ),
      style = "padding: 0px 0px 0px; 0px; text-align: center;"
    ),
    
    shinydashboard::menuItem(
      "Edit Model", 
      tabName = "DifferentialEquationsPage", icon = icon("cogs")),
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
        img(src = "icon_save2.png",
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
        id = "simulationParametersDiv",
        div(
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
                     dataTableOutput("inpTrajectory"),
                     uiOutput("exportTimeSeriesTrajBt")
            )
          )
        ),
        
        # Plots tab
        tabPanel(
          "Plots",
          div(textInput("plotTitle", "Plot Title", placeholder = "Optional"),
              style = "display: inline-block;vertical-align:top; width: 250px; padding: 0px;"),
          div(textInput("plotXLabel", "X Axis Label", placeholder = "Optional"),
              style = "display: inline-block;vertical-align:top; width: 250px; padding: 0px;"),
          div(textInput("plotYLabel", "Y Axis Label(s)", placeholder = "Optional"),
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
                         "Choose The Plot Type", 
                         choices = c("line", "point"),
                         inline = T),
            style = "display: inline-block;vertical-align:top; 
            width: 250px; padding: 0px;"),
          div(
            checkboxInput("showUnitToggle",
                          "Display variable's units",
                          value = T),
            style = "display: inline-block;vertical-align:top; 
            width: 250px; padding-top: 15px;"),
          div(
            id = "multipleAxisTooltipDiv",
            checkboxInput("multipleAxisToggle",
                          "Display multiple Y-Axis",
                          value = T),
            style = "display: inline-block;vertical-align:top; 
            padding-top: 15px;"),
          conditionalPanel(
            "input.selVarPlot != 'No Variables Available' && input.selVarPlot != null",
            plotOutput("customPlot"),
            actionButton(inputId = "savePlot", "Save Plot")
          )
        )
      )
    )
  )
)

# Note: Every input id for rhandsontables and ace editor scripts must have the 
# name of the corresponding data variable followed by 'Input'. 
# Example: 'DifferentialEquationsInput' corresponds to 'DifferentialEquations' reactive variable
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
            tags$ul(style = "columns: 100px",
                    tags$li("linear"),
                    tags$li("constant"),
                    tags$li("fmm"),
                    tags$li("natural"),
                    tags$li("periodic"),
                    tags$li("monoH.FC"),
                    tags$li("hyman")
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
# Example: 'DifferentialEquationsInput' corresponds to 'DifferentialEquations' reactive variable
DifferentialEquationsPage <- shinydashboard::tabItem(
  tabName = "DifferentialEquationsPage",
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
      div(id = "atomicModelPage",
          shinydashboard::tabBox(
            id = "modelBox", title = "Atomic Model", width = "100%",
            tabPanel(
              "Differential Equations",
              AceEditorCustom("DifferentialEquations", height = scriptHeight),
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
                  tags$li(h5("t: the current time instant in the 
                                                         integration;")),
                  tags$li(h5("st: a list with the current estimate of 
                                                         the variables in the ODE system;")),
                  tags$li(h5("ct: a list of constants;")),
                  tags$li(h5("par: a list of parameters;")),
                  tags$li(h5("inp: a list of inputs;")),
                  tags$li(h5("sw: a list of switches;")),
                  tags$li(h5("aux: a list of auxiliary variables."))
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
                AceEditorCustom("DifferentialEquationsExample",
                                theme = "idle_fingers",
                                showLineNumbers = F,
                                showPrintMargin = F,
                                readOnly = T, 
                                height = "180px",
                                highlightActiveLine = F, 
                                value = DifferentialEquationsExampleStr)
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
                  tags$li(h5("st: a list with the initial values of 
                                                         state variables")),
                  tags$li(h5("ct: a list of constants;")),
                  tags$li(h5("par: a list of parameters;")),
                  tags$li(h5("inp: a list of inputs;")),
                  tags$li(h5("sw: a list of switches;")),
                  tags$li(h5("aux: a list of auxiliary variables."))
                ),
                h5("The return value of InitVars is a list 
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
              "Trigger", AceEditorCustom("root", height = scriptHeight),
              shinydashboard::box(
                title = ("Usage"),
                solidHeader = T, 
                status = "primary",
                width = "100%",
                collapsible = T,
                h5("An optional function that becomes zero when a root 
                   occurs. When a root is found, the simulation triggers 
                   an event by calling the EventFunction. "),
                h5("It is defined as the function:"),
                tags$ul(h5("function(t, st, ct, par, inp, sw, aux)")),
                h5("Where,"),
                tags$ul(
                  tags$li(h5("t: the current time instant in the 
                             integration;")),
                  tags$li(h5("st: a list with the current estimate 
                             of the variables in the ODE system;")),
                  tags$li(h5("ct: a list of constants;")),
                  tags$li(h5("par: a list of parameters;")),
                  tags$li(h5("inp: a list of inputs;")),
                  tags$li(h5("sw: a list of switches;")),
                  tags$li(h5("aux: a list of auxiliary variables."))
                ),
                h5("If no EventFunction is defined, when a root is found 
                   the simulation stops."),
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
                   is triggered when the root function returns 0. It can
                   be used to change state values during the execution
                   of the simulation."),
                h5("It is defined as the function:"),
                tags$ul(h5("function(t, st, ct, par, inp, sw, aux)")),
                h5("Where,"),
                tags$ul(
                  tags$li(h5("t: the current time instant in the 
                             integration;")),
                  tags$li(h5("st: a list with the current estimate of 
                             the variables in the ODE system;")),
                  tags$li(h5("ct: a list of constants;")),
                  tags$li(h5("par: a list of parameters;")),
                  tags$li(h5("inp: a list of inputs;")),
                  tags$li(h5("sw: a list of switches;")),
                  tags$li(h5("aux: a list of auxiliary variables."))
                ),
                h5("It should return the state-values (some of which might
                   have been modified by the function), as a vector with 
                   the variables in the right order."),
                h5("If no EventFunction is defined, when a root is found 
                   the simulation stops."),
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
                  tags$li(h5("t: the current time instant in the 
                             integration;")),
                  tags$li(h5("st: a list with the current estimate of 
                             the variables in the ODE system;")),
                  tags$li(h5("ct: a list of constants;")),
                  tags$li(h5("par: a list of parameters;")),
                  tags$li(h5("inp: a list of inputs;")),
                  tags$li(h5("sw: a list of switches;")),
                  tags$li(h5("aux: a list of auxiliary variables."))
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
                  tags$li(h5("t: the current time instant in the 
                             integration;")),
                  tags$li(h5("ct: a list of constants;")),
                  tags$li(h5("par: a list of parameters;")),
                  tags$li(h5("inp: a list of inputs;")),
                  tags$li(h5("sw: a list of switches;")),
                  tags$li(h5("equations: a list of containing the results
                             after evaluation."))
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
                  tags$li(h5("ct: a list of constants;")),
                  tags$li(h5("par: a list of parameters;")),
                  tags$li(h5("inp: a list of inputs;")),
                  tags$li(h5("sw: a list of switches;")),
                  tags$li(h5("aux: a list of auxiliary variables."))
                ),
                h5("The return value of InitVars is a list 
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
                h5("The \"Components\" sheet is used to define the component
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
                h5("The \"Connections\" list is used to define the connections
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
      h4(strong("Programming Language")),
      h5("This is a companion application to the sdsim package, developed for 
         the programming language R. Therefore, everything created in this 
         application must be written in R."),
      br(),
      h4(strong("Model")),
      h5("In this application a model is a set of components that can be used to
          simulate the behavior of a system. This application supports three
          types of models:  atomic, static and coupled."),
      br(),
      h5(strong("Atomic model")),
      h5("Represents an atomic system dynamics model that consists of functions 
         describing the system flows and a default scenario describing the 
         system environment (variables and values)."),
      h5("An atomic model is composed by:"),
      tags$ul(
        tags$li("Model ID: the model identification name."),
        tags$li("Model Description: a text description of the model."),
        tags$li("Differential Equations: an R-function that computes the values 
                of the state variables derivatives in the ODE system (the model
                definition) at time t."),
        tags$li("Variable Intialization: (optional) an R-function that 
                initialize or change the initial state values and/or other model
                variables before the solver call when running a simulation. It 
                can be used, for example, to compute some dependent parameter 
                variables or the initial state variables, using the 
                arguments."),
        tags$li("Trigger Function: (optional) an R-function that triggers an 
                Event Function when it returns zero. If no Event Function is 
                defined, when zero is returned the simulation stops."),
        tags$li("Event Function: (optional) an R-function that specifies the event."),
        tags$li("Auxiliary Variables: (optional) a set of equations that are used to 
                calculate auxiliary variables at each step of the simulation. 
                These variables represent intermediary values used to compute 
                the derivatives of the differential equations."),
        tags$li("Global Functions: (optional) a set of R-functions that can be executed in 
                the scope of other functions defined in the model.")
      ),
      h5("The system is solved by integrating the derivatives resultant from the
         differential equations to the state variables at each step of the 
         simulation."),
      br(),
      h5(strong("Static model")),
      h5("Represents a static (or steady-state, no state variables) model that 
         consists of algebraic equations and a default scenario describing the 
         system environment (variables and values). A static model calculates 
         the system in equilibrium, and thus is time-invariant."),
      h5("A static model is composed by:"),
      tags$ul(
        tags$li("Model ID: the model identification."),
        tags$li("Model Description: (optional) a text description of the model."),
        tags$li("Equations: a set of algebraic equations that are calculated 
                during the simulation."),
        tags$li("Variable Intialization:  (optional) an R-function that 
                initialize or change the initial state values and/or other
                model variables before the solver call when running a 
                simulation. It can be used, for example, to compute some 
                dependent parameter variables or the initial state variables, 
                using the arguments."),
        tags$li("Global Functions: (optional) a set of R-functions that can be 
                executed in the scope of other functions or equations defined 
                in the model.")
      ),
      br(),
      h5(strong("Coupled Model")),
      h5("Represents a coupled system dynamics model composed by a set of models."),
      h5("A coupled model is composed by:"),
      tags$ul(
        tags$li("Model ID: the model identification."),
        tags$li("Model Description: (optional) a text description of the 
                model."),
        tags$li("Components: a set of IDs atomic, static or coupled models. The 
                component models must be loaded in the application."),
        tags$li("Connections: a table describing the connections between the 
                components of the model. The connections determine loops of 
                information feedback and circular causality for conceptualizing 
                the structure of a complex system and for communicating 
                model-based insights.")
      ),
      h5("The complex system is solved by integrating all the coupled system 
         components simultaneously, updating the connections at each time 
         step."),
      br(),
      h5(strong("Managing Models")),
      h5("Models can be managed using the menu under \"Model\", located at the 
         sidebar menu. The dropdown input is used to switch between loaded
         models."),
      h5("Models can be created or loaded using the plus sign button. New models can 
         be created empty, loaded from existing models xml files, 
         loaded from one of the examples available, or cloned from the
         currently selected model."),
      h5("The currently selected model's differential equations, parameter 
         initilization function, trigger function, event function, auxiliary
         variables and global functions are available in the \"Edit 
         Model\" menu. The model's description is available in the
         \"Description\" menu."),
      h5("The model's ID can be changed in the \"Edit Model\" menu using
         the \"Change Model ID\" button."),
      h5("Deleting the current model from the application can be done by 
         clicking the minus sign button."),
      h5("Saving the current model into a XML file can be done by clicking
         the folder button and downloading the file. This file 
         can be used to load the model back to the application. It 
         can also be loaded to R, using the sdsim package."),
      br(),
      h4(strong("Scenario")),
      h5("A scenario includes the initial values of state variables and all 
         the other variables values not calculated by the model equations."),
      h5("A scenario is composed of:"),
      tags$ul(
        tags$li("State: the initial value of state variables, which will change over 
                time during the simulation according to the values of their 
                respective differential equations. State variables are the main 
                variables of the system and represent the simulation result 
                trajectory. Every dynamic model must have at least one state 
                variable."),
        tags$li("Constants: remain imutable across simulations and are 
                unavailable to statistical calibration."),
        tags$li("Parameters: remain imutable across a simulation, but are 
                available for statistical calibration."),
        tags$li("Switches: discrete values used as conditional selectors in 
                the model. Swiches can be used, for instance to change parameter
                initialization values or to change the control flow of the model
                (i.e. to change the parts of the code which will be used by the
                model)."),
        tags$li("Inputs: exogenous variables (also called drivers). These 
                variables can be numeric values, time series or \"forcing 
                functions (i.e. functions that only depends on time)\". 
                Time series should be \"csv\" (comma separated values) format 
                files with time in the first column and the respective variable 
                value in the second column. The columns must have the headers.")
      ),
      h5("There are two types of scenarios: default scenarios and alternate 
         scenarios."),
      tags$ul(
        tags$li("Default: scenarios which contain the base values of each 
                model's variables. Every atomic and static models have default 
                scenarios. Coupled models use the defaults of its components, 
                thus it does not have its own default scenario. The ID of
                default scenarios cannot be changed, and will always be named 
                \"Default\"."),
        tags$li("Alternate: scenarios which contain the value for at least
                one of the model's variables. This scenario can be used to run
                different simulations using the same model, without the need
                to alter the default scenario. If a variable from the model is
                not specified in the alternate scenario, the simulation will
                be executed using that variable's value from the default
                scenario.")
      ),
      h5(strong("Managing Scenarios")),
      h5("Scenarios can be managed using the menu under \"Scenario\", located at
         the sidebar menu. The dropdown input is used to switch between loaded
         scenarios. Each model has a different set of scenarios, and the
         available scenarios will change according to the selected model."),
      h5("Scenarios can be created or loaded using the plus sign button. New 
         scenarios can be created empty or loaded from existing scenarios xml
         or xlsx files."),
      h5("The currently selected scenario's state variables, constants, 
         inputs and switches are available in the \"Edit Scenario\" menu."),
      h5("The scenario's ID can be changed in the \"Edit Scenario\" menu using
         the \"Change Scenario ID\" button. This button is not available when
         a default scenario is selected."),
      h5("Deleting the current scenario from the application can be done by 
         clicking the minus sign button. Default scenarios cannot be deleted."),
      h5("Saving the current scenario into a XML or xlsx file can be done by 
         clicking the folder button and downloading the file. This file 
         can be used to load the scenario back to the application. It 
         can also be loaded to R, using the sdsim package.")
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
  shinydashboard::tabItems(managePage, simulationPage, scenarioPage, DifferentialEquationsPage, 
                           descriptionPage, helpPage, aboutPage),
  
  # Add javascript functions to sidebar
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  tags$script(src = "custom.js")
)

# Interface
ui <- shinydashboard::dashboardPage(
  header,
  sidebar,
  body
)
