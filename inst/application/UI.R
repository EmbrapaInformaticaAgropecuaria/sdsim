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
    div(id = "test", actionButton("testButton", "TEST"), hidden = NA),
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
      "Simulation", 
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
                           values interpolated. The interpolation method can be 
                           chosen in the \"Interpolation\" column.
                           The interpolation methods available are:"),
            tags$ul(style = "columns: 100px",
                    tags$li("linear"),
                    tags$li("constant"),
                    tags$li("fmm"),
                    tags$li("natural"),
                    tags$li("periodic"),
                    tags$li("monoH.FC"),
                    tags$li("hyman")
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
                  tags$li(h5("t is the current time instant in the 
                                                         integration;")),
                  tags$li(h5("st is a list with the current estimate of 
                                                         the variables in the ODE system;")),
                  tags$li(h5("ct is a list of constants;")),
                  tags$li(h5("par is a list of parameters;")),
                  tags$li(h5("inp is a list of inputs;")),
                  tags$li(h5("sw is a list of switches;")),
                  tags$li(h5("aux is a list of auxiliary variables."))
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
                  tags$li(h5("st is a list with the initial values of 
                                                         state variables")),
                  tags$li(h5("ct is a list of constants;")),
                  tags$li(h5("par is a list of parameters;")),
                  tags$li(h5("inp is a list of inputs;")),
                  tags$li(h5("sw is a list of switches;")),
                  tags$li(h5("aux is a list of auxiliary variables."))
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
                  tags$li(h5("t is the current time instant in the 
                                                         integration;")),
                  tags$li(h5("st is a list with the current estimate 
                                                         of the variables in the ODE system;")),
                  tags$li(h5("ct is a list of constants;")),
                  tags$li(h5("par is a list of parameters;")),
                  tags$li(h5("inp is a list of inputs;")),
                  tags$li(h5("sw is a list of switches;")),
                  tags$li(h5("aux is a list of auxiliary variables."))
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
                  tags$li(h5("t is the current time instant in the 
                                                         integration;")),
                  tags$li(h5("st is a list with the current estimate of 
                                                         the variables in the ODE system;")),
                  tags$li(h5("ct is a list of constants;")),
                  tags$li(h5("par is a list of parameters;")),
                  tags$li(h5("inp is a list of inputs;")),
                  tags$li(h5("sw is a list of switches;")),
                  tags$li(h5("aux is a list of auxiliary variables."))
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
                  tags$li(h5("t is the current time instant in the 
                                                         integration;")),
                  tags$li(h5("st is a list with the current estimate of 
                                                         the variables in the ODE system;")),
                  tags$li(h5("ct is a list of constants;")),
                  tags$li(h5("par is a list of parameters;")),
                  tags$li(h5("inp is a list of inputs;")),
                  tags$li(h5("sw is a list of switches;")),
                  tags$li(h5("aux is a list of auxiliary variables."))
                ),
                h5("The auxiliary variables will be automatically
                                               calculated at each step of the simulation, and the aux
                                               list will contain its results."),
                h5("The variables are calculated in the same order as
                                               they are listed in the table Therefore if an auxiliary
                                               depends on another auxiliary the former must be listed
                                               first."),
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
                h5("'aux' represents a set of equations for calculating
                                               the corresponing auxiliary variables at each step of 
                                               the simulation."),
                h5("The parameters available to be used in the auxiliary
                                               equations are:"),
                tags$ul(
                  tags$li(h5("t is the current time instant in the 
                                                         integration;")),
                  tags$li(h5("st is a list with the current estimate of 
                                                         the variables in the ODE system;")),
                  tags$li(h5("ct is a list of constants;")),
                  tags$li(h5("par is a list of parameters;")),
                  tags$li(h5("inp is a list of inputs;")),
                  tags$li(h5("sw is a list of switches;")),
                  tags$li(h5("aux is a list of auxiliary variables."))
                ),
                h5("The auxiliary variables will be automatically
                                               calculated at each step of the simulation, and the aux
                                               list will contain its results."),
                h5("The variables are calculated in the same order as
                                               they are listed in the table Therefore if an auxiliary
                                               depends on another auxiliary the former must be listed
                                               first."),
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
                h5("An optional function to create or change the 
                                               model variables before the simulation."),
                h5("It is defined as the function:"),
                tags$ul(h5("function(st, ct, par, inp, sw, aux)")),
                h5("Where,"),
                tags$ul(
                  tags$li(h5("st is a list with the initial values of 
                                                         state variables")),
                  tags$li(h5("ct is a list of constants;")),
                  tags$li(h5("par is a list of parameters;")),
                  tags$li(h5("inp is a list of inputs;")),
                  tags$li(h5("sw is a list of switches;")),
                  tags$li(h5("aux is a list of auxiliary variables."))
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
      div(id = "coupledModelPage",
          shinydashboard::tabBox(
            id = "coupledModelBox", title = "Coupled Model", width = "100%",
            tabPanel(
              "Components",
              rhandsontable::rHandsontableOutput("componentIds",
                                                 height = tableHeight,
                                                 width = tableWidth)
            ),
            tabPanel(
              "Connections",
              rhandsontable::rHandsontableOutput("connections",
                                                 height = tableHeight,
                                                 width = tableWidth)
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
      h5(strong("Programming Language")),
      h5("Everything created in this application must be written using the
         programming language R."),
      br(),
      h5(strong("Model")),
      h5(
        "A system dynamics model defined by the sdsim package consists of 
        equations that, together with a scenario, are used to simulate the 
        values of state variables in accordance to the ellapsed simulation time.
        The simulation works by integrating the results of the differential 
        equations systems to the state variables value at each step of the 
        simulation."
      ),
      h5("The functions that compose the model are:"),
      tags$ul(
        tags$li("Differential Equations function that contains the differential 
                  equations system of the model, and will be used to calculate 
                  the derivatives of each state variable which will be
                  integrated by the chosen simulation method."),
        tags$li("Optional variable intialization function that is executed 
                  before each simulation, and is used to initialize and/or 
                  change values of state or parameter variables."),
        tags$li("Optional trigger (root) function. This function check the 
                  conditions to trigger an event. When conditions are matched 
                  it should return 0 (zero). If no event function is defined the
                  simulation stops when it returns 0 (zero)."),
        tags$li("Optional event function that is triggered by the root 
                  function. It can be used to change the state variables values
                  during the simulation."),
        tags$li("Auxiliary variables that are be used to calculate auxiliary 
                  variables at each step of the simulation. These variables are 
                  calculated from an equation using the other variables of the 
                  model. These variables contain intermediary values used to 
                  compute the derivatives of the differential equations."),
        tags$li("Optional global functions that can be executed in the scope
                  of other functions defined in the model.")
      ),
      h5(
        "The model can be defined using the \"Functions\" button in the sidebar 
        menu"
      ),
      br(),
      h5(strong("Scenario")),
      h5("A scenario includes the initial values of state variables and all 
         the other variables values not calculated by the model equations."),
      h5("A scenario is composed of:"),
      tags$ul(
        tags$li("The initial value of state variables, which will change over 
                time during the simulation according to the values of their 
                respective differential equations. State variables are the main 
                variables of the system and represent the simulation result 
                trajectory. Every dynamic model must have at least one state 
                variable."),
        tags$li("Constants are imutable across simulations and inaccessible to 
                statistical calibration."),
        tags$li("Parameters are constant during a simulation, but are acessible 
                for statistical calibration."),
        tags$li("Switches are discrete values used as conditional selectors in 
                the model. Swiches can be used, for instance to change parameter
                initialization values or to change the control flow of the model
                (i.e. to change the parts of the code which will be used by the
                model)."),
        tags$li("Inputs are exogenous variables(also called drivers). These 
                variables can be numeric values, time series or \"forcing 
                functions (i.e. functions that only depends on time)\". 
                Time series should be \"csv\" files with time in the first 
                column and the respective variable value in the second column. 
                They also must have the headers time and variable name.")
      ),
      h5("The model's scenario can be defined using the \"Scenario\" 
                button in the sidebar menu."),
      br(),
      h5(strong("Declaring a variable")),
      h5(
        'Variables are defined using tables in the menu "Scenario". 
        Each line represents one variable, and the columns represent:'
      ),
      tags$ul(
        tags$li("Variable: The name of the variable."),
        tags$li("Value: The value of the variable. In the case of time series, 
                the name of the file with the data should be informed 
                instead."),
        tags$li("Unit: The unit of measurement of the variable. (optional)"),
        tags$li("Description: A short description of the variable. (optional)"
        )),
      br(),
      h5(strong("Model Description")),
      h5(
        "An optional description of the model can be added using the
        \"Description\" menu. This description helps to document any details
        such as how the model works, who created it and what it is intended
        for."
      ),
      br(),
      h5(strong("Saving")),
      h5(
        'A model can be saved using the button "Save Model". A notification
        window will open and the model can be named and downloaded as a XML 
        file. This file can also be loaded in R using the sdsim package'
      ),
      br(),
      h5(strong("Loading")),
      h5(
        "To load a previously saved XML file click the button 'Browse'
        in 'Load Model', this will open a menu in which a xml file can be chosen
        to load."
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
      h5("This application was designed to assist the implementation and simulation of System
         Dynamics models. It makes use of the R language package sdsim and shares its 
         standards for Differential Equations and file formats. It also relies on the  
         simulation algorithms and other functionalities of the sdsim package."),
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
