
shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Reactive Sample Size" , titleWidth = "350px",
                                  shinydashboard::dropdownMenuOutput("review_status_ui"),
                  shiny::uiOutput("Online_users_UI"),
                  tags$li(shiny::plotOutput("Online_users", height = "30px", inline = TRUE), class = "dropdown")),
  
  
  # SIDEBAR -----------------------------------------------------------------
  
  shinydashboard::dashboardSidebar(
    # Custom CSS to hide the default logout panel
    tags$head(
      # Custom CSS to hide the default logout panel
      tags$style(shiny::HTML('.shiny-server-account { display: none; }')),
      # Custom JavaScript -- just to log in when a user hits "enter".
      shiny::includeScript("www/sendOnEnter.js"), shiny::includeCSS("www/comment.css"),
      
      tags$script('
                            Shiny.addCustomMessageHandler("unbinding_table_elements", function(x) {
                            Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                            });'
      )
    ), shinyalert::useShinyalert(),shinyjs::useShinyjs(),
    # The dynamically-generated user panel
    shiny::uiOutput("userpanel"),
    
    
    
    shinydashboard::sidebarMenu(id = "sideMenu",
                
                hr(),
                
                shinydashboard::menuItem("Select trial", tabName = "select_trial", selected = TRUE,  icon = icon("binoculars")),
                shinydashboard::menuItem("Setup trial",  tabName = "setup_trial", selected = FALSE, icon = icon("cogs"), expandedName = "setup_trial_exp"
                ),
                shinydashboard::menuItem("Online user", tabName = "review_overview",selected = FALSE, icon = icon("globe")),
                hr(),
                
                
                shinydashboard::menuItemOutput("out_tab"),
                shinydashboard::menuItemOutput("out_tab2"),
                shinydashboard::menuItemOutput("out_tab3"),
                shinydashboard::menuItemOutput("out_tab4")
                
  
                
    ),
    width = "350px"
  ),
  
  
  # BODY --------------------------------------------------------------------
  
  
  shinydashboard::dashboardBody(
    
    shinydashboard::tabItems(
      
      
      # SELECT TRIAL ------------------------------------------------------------
      
      
      shinydashboard::tabItem(tabName = "select_trial",
                              shiny::fluidRow( 
                                              shinydashboard::box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                                  title = tagList("Trial type:"), 
                                                                  status = "primary",radioButtons("radio", h3("Select trial type:"),
                                                                  choices = list("Outcomes trial" = 1,
                                                                                 "Fixed design trial" = 2,
                                                                                  "Bioequivalence trial" = 3), selected = character(0)))),
                              shiny::fluidRow(
                                            shinydashboard::box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                                            title = tagList("Trials - only ongoing", 
                                            shinyWidgets::prettyCheckbox(inputId = "only_ongoing",
                                                                         label = "Yes", value = TRUE, inline = TRUE)), 
                                                                         status = "primary",
                    
                    DT::dataTableOutput("setup_all_available") %>% shinycssloaders::withSpinner(color = "#0dc5c1",size = 0.5),
                    shiny::uiOutput("setup_reload_data_ui")
                )
              ),
              shiny::uiOutput("time_range_x")
              
      ),
      
      # TRIAL INFO --------------------------------------------------------------
      
      
      shinydashboard::tabItem(tabName = "setup_trial",
                              shiny::fluidRow(
                                              shinydashboard::box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                                                                  title = "Settings of trial", status = "primary",
                                                                  h3("Trial info"),
                                                                  shiny::splitLayout(cellWidths = c("10%","10%","80%"),
                                                                                     shiny::textInput("new_trial_project", "Project - 4 digits"),
                                                                                     shiny::textInput("new_trial_trial", "Trial"),
                                                                                     shiny::textInput("new_trial_title", "Protocol Title")
                    ),
                    
                    h3("Users"),
                    shiny::splitLayout(
                      shiny::selectizeInput("new_trial_superusers", "Superusers", choices = list(""), multiple = TRUE,
                                     options = list(create = TRUE)),
                      shiny::selectizeInput("new_trial_users", "Users", choices = list(""), multiple = TRUE,
                                     options = list(create = TRUE))
                    ),
                    h3("Ongoing"),
                    shiny::checkboxInput("new_trial_ongoing", "ongoing", value = TRUE),
                    
                    #column(width = 6,
                    tags$span(style = "float:right",
                              shiny::splitLayout(cellWidths = c(150, 150),
                                                 shiny::uiOutput("new_trial_update_ui"),
                                                 shiny::uiOutput("new_trial_save_ui")
                              )
                    )
                    #)
                ),
                shinydashboard::box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                                    title = "Upload documentation", status = "primary",
                                    tags$strong("Guide to uploading sample size documentation"),
                                    br(),
                                    paste("In order to generate a sample size report using this Shiny App, 
                                          perform the following steps:"), 
                                    tags$ol(
                                      tags$li("download and complete the rmarkdown 
                                              document"),
                                      tags$li("upload the completed rmarkdown")
                                    ), 
                                    br(),
                                    tags$strong("STEP 1 - Download Rmarkdown file"),
                                    br(),
                                    shiny::downloadButton(outputId = "downloadNNSampleSizeDoc", 
                                                                 label = "Download Rmarkdown file",
                                                                 icon_ = icon("file")),
                                    br(),
                                    br(),
                                    shiny::fileInput('file_input', 'STEP 2 - Upload file ( . rmd format only)', accept = c('.rmd'))
                                    ),
                #shiny::uiOutput("pdfview"),
                
                shiny::uiOutput("new_trial_save_success")
              ),
              shiny::fluidRow(
                shiny::uiOutput("new_trial_upload_file")
              ),
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              )
              
              
      ), 
      
      
      
      # Outcomes ----------------------------------------------------------------
      

      
      shinydashboard::tabItem(tabName = "out_tab",
              h1("Outcomes overview"),
              shiny::uiOutput('essentials'),
              
              shinydashboard::box(plotOutput('events_vs_hr')),
              
              
              shiny::uiOutput('hypotheses'),
              
              shiny::uiOutput('explanatory_text'),
              
              shinydashboard::box(
                h4("All assumptions:"),
                
                div(style = 'overflow-x: scroll', shiny::tableOutput("txt"))
              ),

              h1("Documentation"),
              br() #,
              # myDownloadBttn(outputId = "downloadDoc", 
              #                label = "Download word documentation",
              #                icon_ = icon("file-word")),
              # myDownloadBttn(outputId = "downloadSAS", 
              #                label = "Download SAS program",
              #                icon_ = icon("file-code")),
              # myDownloadBttn(outputId = "downloadProtocolTxt", 
              #                label = "Download protocol text")
      ),
      
      
      # Fixed design ------------------------------------------------------------
      
      
     
      
      shinydashboard::tabItem(tabName = "fix_tab",
                              shiny::conditionalPanel(
                condition = "input.ep_determine == 'n'",
                h1("Fixed Design Overview - Sample Size"),
                # Boxes containing the number of events and subjects needed
                shiny::uiOutput('epSampleSizeResult')
                
              ),
              
              shiny::conditionalPanel(
                condition = "input.ep_determine == 'power'",
                h1("Fixed Design Overview - Power"),
                shinydashboard::box(
                  # box args
                  title = "Results of the Hierarchical Testing Procedure",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  paste("Assumptions, marginal power, and effective power for each confirmatory endpoint in the hierarchical
                    testing procedure. The effectice powers is the cumulative product corresponding to an assumption of 
                    independent endpoints."),
                  div(shiny::tableOutput("effPower"), hr(), style = 'overflow-x: scroll'), 
                  br(),
                  tags$em("No.: Number of endpoint, Endpoint: Name of endpoint, Test type: Endpoint test type, N: Number of subjects, 
                    Determine: Whether sample size or power are to be calculated")
                ),
                shinydashboard::box(
                  title = "All Assumptions of the Current Endpoint (STEP 1)",
                  status = "primary",
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  width = 6,
                  paste("Specify the parameters of an endpoint using the left-side tab 'Endpoint Inputs'. 
                    Subsequently, add the endpoint to the table of confirmatory endpoints in the 
                    hierarchical testing procedure."),
                  br(),
                  hr(),
                  div(shiny::tableOutput("assumptions"), hr(), style = 'overflow-x: scroll'),
                  br(),
                  tags$em("No.: Number of endpoint, Endpoint: Name of endpoint, Test type: Endpoint test type, N: Number of subjects, 
                    Determine: Whether sample size or power are to be calculated"),
                  hr(),
                  shiny::actionButton("addButton", "Add", style="background-color: #3c8dbc; color: #fff")
                ),
                
                
                shinydashboard::box(
                  # box args
                  title = "All Assumptions of the Confirmatory Endpoints (STEP 2)", 
                  status = "primary",
                  solidHeader = TRUE, 
                  collapsible = TRUE, 
                  width = 6,
                  paste("Table containing assumptions for each confirmatory endpoint in the hierarchical,
                    testing procedure. When all endpoints have been added, the effective power or sample size can be calculated. 
                    The effectice powers is the cumulative product corresponding to an assumption of independent endpoints."),
                  br(),
                  hr(),
                  div(shiny::tableOutput("confirmatoryEndpoints"), hr(), style = 'overflow-x: scroll'),
                  br(),
                  tags$em("No.: Number of endpoint, Endpoint: Name of endpoint, Test type: Endpoint test type, N: Number of subjects, 
                    Determine: Whether sample size or power are to be calculated"),
                  hr(),
                  shiny::actionButton("calcPowerSSizeButton", "Calculate Effective Power", style="background-color: #3c8dbc; color: #fff"),
                  shiny::actionButton("clearAllButton", "Clear All", style="background-color: #3c8dbc; color: #fff")
                )
                
              )
              

      
      
             
              
              
      ),
      



      
      # Bioequivalence ----------------------------------------------------------
      
      

      
      shinydashboard::tabItem(tabName = "bio_tab",
              
              h1("Bioequivalence Overview"),
              shiny::uiOutput('essentialsBE'),
              shiny::uiOutput('hypotheses_bioequivalence'),
              
              shinydashboard::box(
                h4("All assumptions:"),
                
                div(style = 'overflow-x: scroll', shiny::tableOutput("txtBE"))
              ),
              h1("Documentation"),
              br()
              
      ),
      
     
      
      # First tab ---------------------------------------------------------------
      
      
      
      shinydashboard::tabItem(tabName = "review_overview",
                              shiny::fluidRow(
                shinydashboard::box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                    title = "Online users", status = "primary",
                    DT::dataTableOutput("review_overview")
                )
                
              )
      )
    )
  )
)
