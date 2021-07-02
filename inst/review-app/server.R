

# Global info shared ------------------------------------------------------

  
  options(warn = 0)
  options(shiny.maxRequestSize = 30*1024^2) 
  
  # update ongoing trials every 10 minutes
  checkReviewAccess <- function(path = file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "trial_setup")) {
    list.files(path, pattern = ".rds", full.names = TRUE, recursive = TRUE)
  }


  readReviewAccess <- function(path = file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "trial_setup/")){
    checkfile <- list.dirs(path)
  
    files <- lapply(checkfile, list.files,  pattern = ".rds", full.names = TRUE)
  
    newest_files <- lapply(files, function(x) x[length(x)])
  
    avail_files <- newest_files[seq_along(newest_files)[unlist(lapply(newest_files, length)) > 0]]
  
    l <- lapply(avail_files, readRDS)
  
    dplyr::bind_rows(l)
  }
  
  GlobalRV <- shiny::reactiveValues(data       = list(),
                             #comments   = list(),
                             online     = list(),
                             review_access = readReviewAccess(),
                             avatar_col = tibble::tibble())
  
  # Function for reading avatar colour files
  readAvatars <- function(path = file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "avatars"),
                          current = NULL){
    
    files <- files_read <- list.files(path, pattern = ".rds", full.names = TRUE)
    
    if (!is.null(current))
      files_read <- dplyr::setdiff(files, attr(current, "file_track"))
    
    if (length(files_read)) { 
      l <- lapply(files, readRDS)
      avatar_col <- dplyr::bind_rows(l)
    } else {
      avatar_col <- tibble::tibble(user = character(0), col = character(0))
    }
    
    if (!is.null(current))
      avatar_col <- dplyr::bind_rows(current, avatar_col)
    
    attr(avatar_col, "file_track") <- files
    avatar_col
  }
  
  
  
  
  # when the server stops this is run
  shiny::onStop(fun = function() {
    message("The server has shut down")
  
    NNSampleSize::SampleSizeEnv(c("online_user", "trial", "username", 
              "workspace", "verbose", "libref",
              "access_data_path", "runApp", "admins"), remove = TRUE)
  })
  
  shiny::shinyServer(function(input, output, session) {
  
    session.id <- session$token
    
    
    review_access <- shiny::reactive({
      GlobalRV$review_access
    })
    
    
    # ongoing review
    on_going_review <- shiny::reactive({
      if (NNSampleSize::SampleSizeEnv("verbose")) cat("on_going_review\n")
      all <- review_access()
      all[all$ongoing, ]$Trial
    })
    
    # all user that may log in to the app
    app_users <- shiny::reactive({
      if (NNSampleSize::SampleSizeEnv("verbose")) cat("app_users\n")
      all <- review_access()
      
      if (nrow(all)) {
        app_users <- toupper(unique(c(unlist(all$superusers), unlist(all$users))))
      } else {
        app_users <- c()
      }
      
      unique(c(app_users, toupper(NNSampleSize::SampleSizeEnv("admins"))))
    })
    
    # colour for the user avatars
    avatar_colors <- reactive({
      if (NNSampleSize::SampleSizeEnv("verbose")) cat("avatar_colors\n")
      user <- app_users()
      sample(nncol$company[-1], length(user), replace = length(user) >= length(nncol$company))
      
      shiny::isolate({ 
        GlobalRV$avatar_col <<- 
          avatar_colors <- readAvatars(current = GlobalRV$avatar_col)
      })
      
      new_user <- dplyr::setdiff(c(user), avatar_colors$user)
      
      if (length(new_user)) {
        
        new_colors <-
          tibble::tibble(user = new_user,
                         col  = sample(nncol$company[-1], length(user),
                                       replace = length(user) >= length(nncol$company)))
        
        lapply(seq_len(nrow(new_colors)), function(i) { 
          NNSampleSize::writeTable(new_colors[i,], file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "avatars", paste0(new_user[i], ".csv")))
        })
        
        avatar_colors <- dplyr::bind_rows(avatar_colors, new_colors)
      }
      
      return(avatar_colors)
    })


# Log-in  -----------------------------------------------------------------

  setupRV <- shiny::reactiveValues(user = "", timeline = TRUE)

  if (NNSampleSize::SampleSizeEnv("workspace") == "server") {

    shiny::showModal(modalDialog(footer = "",
      title = "Login",

      shiny::splitLayout(cellWidths = c("80%", "20%"),
                  shiny::textInput(inputId = "username", "Initials:"),
                  div(br(),
                      shiny::actionButton("login", "Login", style = paste0("color: white; background-color: ",
                                                                    "#3F9C35",";border-color: ","#3F9C35"))
                      ))
    ))
  } else if (NNSampleSize::SampleSizeEnv("workspace") == "shinyServer") {
    shiny::reactive(app_users())
    setupRV$user <- toupper(session$user)
    NNSampleSize::updateLoginInfo(session.id, toupper(session$user), login = TRUE)
  } else {
    shiny::reactive(app_users())
    setupRV$user <- toupper(NNSampleSize::SampleSizeEnv("username"))
    NNSampleSize::updateLoginInfo(session.id, NNSampleSize::SampleSizeEnv("username"), login = TRUE)
  }

  
  shiny::observeEvent(input$login, {
    if (NNSampleSize::SampleSizeEnv("verbose")) cat(paste("login-started", input$username, "\n"))
    if (input$username == "") {
      shinyalert::shinyalert("Oops!","Initials not filled in",type = "warning")
    }

    else if (!(toupper(input$username) %in% app_users())) {
      setupRV$user <- ""
      shinyalert::shinyalert("Oops!","You do not have permission to use this app",type = "warning")
    }
    else {
      shiny::removeModal()
      # Assign the username
      setupRV$user <- toupper(input$username)

      # updateLoginInfo
      NNSampleSize::updateLoginInfo(session.id, toupper(input$username), login = TRUE)

      # Reset the textInput
      shiny::updateTextInput(session, "username", "")
    }
  })


# Avatar ------------------------------------------------------------------



  avatar_link <- reactive({
    if (NNSampleSize::SampleSizeEnv("verbose")) cat("avatar_link\n")
    shiny::req(avatar_colors())
    avatar_colors <- avatar_colors()

    if (!is.null(setupRV$user) && setupRV$user != "") {

      if (setupRV$user %in% avatar_colors$user) {
        color <- avatar_colors[avatar_colors$user == setupRV$user, ]$col
      } else {
        color <- sample(nncol$company[-1], 1)
      }

      file <- file.path("www", "avatars", paste0(tolower(setupRV$user), ".png"))
      dir.create(file.path("www", "avatars"), showWarnings = FALSE)
      NNSampleSize::create_avatar(setupRV$user, save_png = TRUE, file = file, type = "square",
                    col = color)

      return(file.path("avatars", paste0(tolower(setupRV$user), ".png")))
    }
  })

  output$userpanel <- renderUI({
    if (NNSampleSize::SampleSizeEnv("verbose")) cat("userpanel\n")
    shiny::req(avatar_link())

    text <- "Logged in"

    if (!is.null(setupRV$trial_select) && setupRV$trial_select != "")
      text <- paste(text, "to", setupRV$trial_select)

    if (!is.null(setupRV$user_type) && setupRV$user_type != "")
      text <- paste(text, "as", setupRV$user_type)

    if (NNSampleSize::SampleSizeEnv("verbose")) cat(paste("userpanel:", text, "\n"))
    if (!is.null(setupRV$user) && setupRV$user != "") {

      if (NNSampleSize::SampleSizeEnv("workspace") == "shinyServer") {
        subtitle <- a(icon("sign-out"), "Logout", href = "__logout__")
      } else {
        subtitle <- shiny::actionLink("signout_link", "Logout", icon = icon("sign-out"))
      }

      shinydashboard::sidebarUserPanel(
        name     = shiny::span(text),
        subtitle = subtitle,
        image    = avatar_link()
      )
    }
  })


# Signout -----------------------------------------------------------------
  shiny::observeEvent(input$signout_link, {
    message(setupRV$user, " signing out")
    shinydashboard::updateTabItems(session, "sideMenu", selected = "select_trial")

    # update session_info
    NNSampleSize::updateLoginInfo(session.id, setupRV$user, logout = TRUE)

    # set user info
    setupRV$user <- ""
    setupRV$trial_select <- ""
    setupRV$user_type <- ""
    pkplotRV$highlighted <- NULL
    setupRV$trial_id <- NULL

    shiny::showModal(shiny::modalDialog(footer = "",
                          title = "Login",

                          shiny::splitLayout(cellWidths = c("80%", "20%"),
                                      shiny::textInput(inputId = "username", "Initials:"),
                                      div(br(),
                                          shiny::actionButton("login", "Login", style = paste0("color: white; background-color: ",
                                                                                        "#3F9C35",";
                                                             border-color: ","#3F9C35"))
                                      ))
    ))
  })


# On stop -----------------------------------------------------------------
  shiny::onStop(fun = function(env_id = session.id) {

    message("exit by ", session.id)

    # Update session_info
    NNSampleSize::updateLoginInfo(session.id, logout = TRUE)
  })

 

# Setup trials ------------------------------------------------------------
  all_trial_data <- shiny::reactive({

    shiny::req(setupRV$user,  !is.null(input$only_ongoing))
    if (NNSampleSize::SampleSizeEnv("verbose")) cat("all_trial_data\n")

    # All trials
    all <- GlobalRV$review_access

    if (nrow(all) == 0) return(NULL)

    # User specific trials
    trial_data_store <-
      all[sapply(all$users, function(x) tolower(setupRV$user) %in% tolower(x)) |
          sapply(all$superusers, function(x) tolower(setupRV$user) %in% tolower(x)), ]

    # Only include ongoing
    if (input$only_ongoing)
      trial_data_store <-
      trial_data_store %>% dplyr::filter(Trial %in% on_going_review())

    # If possible preselect trial
    if (!is.null(NNSampleSize::SampleSizeEnv("trial"))) {
      setupRV$trial_select <- NNSampleSize::SampleSizeEnv("trial")
      if (NNSampleSize::SampleSizeEnv("verbose")) cat(paste("forced trial:",  NNSampleSize::SampleSizeEnv("trial")))
      shinydashboard::updateTabItems(session, "sideMenu", selected = "plot_profile")
      NNSampleSize::SampleSizeEnv("trial", NULL)
      if (NNSampleSize::SampleSizeEnv("verbose")) cat(paste("reset to:", NNSampleSize::SampleSizeEnv("trial", NULL), "\n"))
    }

    if (nrow(trial_data_store) == 1 & is.null(setupRV$trial_select_id)) {
      setupRV$trial_select <- trial_data_store$Trial
      setupRV$trial_select_id <- trial_data_store$trial_id
      if (length(dir(file.path(trial_data_store$app_data), "data"))) {
       shinydashboard::updateTabItems(session, "sideMenu", selected = "plot_profile")
      } else {
        shinydashboard::updateTabItems(session, "sideMenu", selected = "setup_trial")
      }
    }
    trial_data_store
  })
  
  shiny::observe({
    shiny::req(input$file_input)
    
    file.copy(input$file_input$datapath,"documentationOutput", overwrite = T)
    
 })
  
  output$downloadNNSampleSizeDoc <- shiny::downloadHandler(
    filename = "NNSampleSizeDocumentation.Rmd",
    content = function(file) {
      file.copy("documentationOutput//NNSampleSizeDocumentation.Rmd", file)
    }
  )
  

# Outcomes ----------------------------------------------------------------
  # Parse the "allocation ratio" input field
  alloc_ratio <- shiny::reactive({
    props <- as.numeric(unlist(strsplit(input$alloc_ratio, ":")))
    return(props[1]/props[2])
  })
  
  # Parse the "sided" input field
  sided <- shiny::reactive({
    ifelse(input$two_sided, 2, 1)
  })
  
  
  # Parse the "alpha" input field
  alpha <- shiny::reactive({
    input$alpha/100
  })
  
  # Parse the "beta" input field
  beta <- shiny::reactive({
    1 - input$power/100
  })
  
  # Calculate minimum follow-up period by parsing "max_study_duration" and "accrual_druation" input field
  minfup <- shiny::reactive({
    (input$max_study_duration - input$accrual_druation)/12
  }) 
  
  # Parse the "lambda2" input field
  lambdaC <- shiny::reactive({
    ((input$lambda2)/100)
  })
  
  # Parse the "max_study_duration" input field
  max_study_duration <- shiny::reactive({
    (input$max_study_duration)/12 
  })  
  
  # Parse rr input field and calculate hazard ratio:
  hazardRatio <-  shiny::reactive({
    if (input$multiPopulation){
      rrMix<-exp((input$mixRatio/100)*log(input$rrPopA)+(1-input$mixRatio/100)*log(input$rrPopB))
      1-(rrMix/100)
    }
    else{
      ((100-input$rr)/100)
    }
  })
  
  
  
  # Parse the 
  popAprop <- shiny::reactive({
    (input$mixRatio)/100
  })
  
  popBprop <- shiny::reactive({
    (100-input$mixRatio)/100
  })
  
  output$popAname <- shiny::renderText({
    input$namePopA
  })
  
  output$popBname <- shiny::renderText({
    input$namePopB
  })
  
  
  
  output$mixture <- shiny::renderUI({
    box(   title = tags$h5(div(style="display: inline-block;vertical-align:top; width: 150px;", input$mixRatio, ":", 100-input$mixRatio, "(Population A : Population B)")),
           background = "navy",
           solidHeader = FALSE,
           collapsible = FALSE, 
           collapsed = FALSE,
           width = 1
    )
  })
  
  # Compute number of events and samples needed for given input
  power_calc <-  shiny::reactive({
    out <-NNSampleSize::get_ss_estimates(lambdaC  = lambdaC(), 
                            T        = max_study_duration(),
                            minfup   = minfup(),
                            alpha    = alpha(),
                            beta     = beta(),
                            sided    = sided(),
                            ratio    = alloc_ratio(),
                            hr       = hazardRatio(),  
                            eta      = input$eta,
                            interim  = input$interimAnalysis,
                            timing   = input$timingInterim,
                            sfu      = input$sfuInterim
    )
  })
  
  
  
  # Boxes containing the number of events and subjects needed
  output$essentials <- shiny::renderUI({
    out <- power_calc()
    if (input$interimAnalysis == 'no_interim'){
      list(
        shinydashboard::valueBox(
          value = ceiling(out$n),
          subtitle = toupper("randomized subjects needed"),
          icon = icon("users")
        ),
        shinydashboard::valueBox(
          value = ceiling(out$d), 
          subtitle = toupper("events needed"),
          icon = icon("heartbeat")
        )
      )}
    else{
      list(
        shinydashboard::valueBox(
          value = ceiling(out$eNC1+out$eNC2),
          subtitle = toupper("randomized subjects needed"),
          icon = icon("users")
        ), 
        shinydashboard::valueBox(
          value = ceiling(out$n.I1), 
          subtitle = toupper("events needed at scheduled termination"),
          icon = icon("heartbeat")
        ),
        shinydashboard::valueBox( 
          value = ceiling(out$n.I2), 
          subtitle = toupper("events needed at interim"),
          icon = icon("heartbeat")
        )
      )
    }
  })
  
  # Custom hypotheses text 
  output$hypotheses <- shiny::renderUI({
    shinydashboard::box(
      shiny::withMathJax(
        h4("Hypotheses:"),
        p(
          "The null and alternative hypotheses tested are:", 
          paste0("$$H_0: HR = \\lambda_E/\\lambda_C = 1$$"),
          paste0("$$H_A: HR = \\lambda_E/\\lambda_C = ", hazardRatio(), "$$"),
          "where \\(\\lambda_E\\) and \\(\\lambda_C\\) are the hazards of the experimental and control group, respectively.",
          paste0("This alternative corresponds to a ", 100*round(1 - hazardRatio(), 3), "% reduction of risk.")
        )
      )
    )
  })
  
  # Table of results / inputs
  output$txt <- shiny::renderTable(power_calc(),
                            align = "c",
                            spacing = "xs")
  
  
  # Plot of EVENTS VS HR
  output$events_vs_hr <- shiny::renderPlot({
    # Initializing vector containing values of hazard ratio
    hr_min <- 0
    hr_max <- 1
    hr_x <- numeric()
    hr_x <- sort(c(seq(hr_min, hr_max, length.out = 300), hazardRatio()))
    hr_x <- hr_x[2:(length(hr_x)-1)]
    
    # Initializing/clearing vectors containing the events with regards to hazard ratio 
    eventsHR <- numeric()
    eventsHRTotal <- numeric()
    eventsHRInterim <- numeric()
    
    # Calculating sample size with regards to hazard ratio  
    for (i in seq_along(hr_x)){
      tempEvents <- suppressWarnings(NNSampleSize::get_ss_estimates(lambdaC  = lambdaC(), 
                                                      T = max_study_duration(),
                                                      minfup   = minfup(),
                                                      alpha    = alpha(),
                                                      beta     = beta(),
                                                      sided    = sided(),
                                                      ratio    = alloc_ratio(),
                                                      hr       = hr_x[i],  
                                                      eta      = input$eta,
                                                      interim  = input$interimAnalysis,
                                                      timing   = input$timingInterim,
                                                      sfu      = input$sfuInterim))
      
      if (input$interimAnalysis =='no_interim'){
        eventsHR[i] <- tempEvents[["d"]] 
      }
      else{
        eventsHRTotal[i]   <- tempEvents[["n.I1"]]
        eventsHRInterim[i] <- tempEvents[["n.I2"]]
      }
    }
    if (input$interimAnalysis =='no_interim'){
      events_vs_hr_data <- tibble::tibble(hr_x, eventsHR)
      
      # Extract the events corresponding to the input hazard ratio
      events_vs_hr_data_sub <-  
        events_vs_hr_data %>% 
        dplyr::filter(hr_x == hazardRatio()) %>% 
        dplyr::select(eventsHR, hr_x)
      
      n_selected <- events_vs_hr_data_sub$eventsHR
      
      # Plot of the events as a function of hazard ratio
      events_vs_hr <- 
        ggplot2::ggplot(events_vs_hr_data, ggplot2::aes(x = hr_x, y = eventsHR)) +
        ggplot2::geom_line(size = 1.1) +
        ggplot2::xlab("Hazard ratio") +
        ggplot2::ylab("Number of events") +
        ggplot2::coord_cartesian(ylim = c(0,4000)) + 
        ggplot2::geom_segment(ggplot2::aes(x = hazardRatio(), y = 0, 
                         xend = hazardRatio(), yend = n_selected), 
                     colour = "steelblue", size = 1.1) +
        ggplot2::geom_segment(ggplot2::aes(x = hazardRatio(), y = n_selected,
                         xend = 0, yend = n_selected), 
                     colour = "steelblue", size = 1.1, 
                     arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm"), type = "closed")) +
        ggplot2::geom_text(x = 0, y = n_selected, label = paste(round(n_selected), "events"),
                  vjust = "bottom", hjust = "left") +
        ggplot2::geom_text(x = hazardRatio(), y = 0, label = hazardRatio(), 
                  vjust = "top", hjust = "center")
      
      return(events_vs_hr)
    }
    
    else{
      events_vs_hr_data <- tibble::tibble(hr_x, eventsHRTotal, eventsHRInterim)
      
      # Extract the events corresponding to the input hazard ratio
      events_vs_hr_data_sub <-  
        events_vs_hr_data %>% 
        dplyr::filter(hr_x == hazardRatio()) %>% 
        dplyr::select(eventsHRTotal, eventsHRInterim, hr_x)
      
      n_selectedInterim <- events_vs_hr_data_sub$eventsHRInterim
      n_selectedTotal <- events_vs_hr_data_sub$eventsHRTotal
      
      # Group by events needed at scheduled termination and at interim  
      group <- character() 
      group[1:299] = "eventsHRTotal"
      group[300:598] = "eventsHRInterim"
      
      plotData <- tibble::tibble(
        hr_x = rep(hr_x,2),
        events = c(eventsHRTotal,eventsHRInterim),
        group = group
      )
      
      # Plot of the events as a function of hazard ratio and grouped by events needed at scheduled termination and at interim
      events_vs_hr <- 
        ggplot2::ggplot(plotData, ggplot2::aes(x = hr_x, y = events, group = group))+
        ggplot2::geom_line(ggplot2::aes(colour = group), size = 1.1) +
        ggplot2::xlab("Hazard ratio") +
        ggplot2::ylab("Number of events") +
        ggplot2::coord_cartesian(ylim = c(0,4000)) + 
        ggplot2::geom_segment(ggplot2::aes(x = hazardRatio(), y = 0, 
                         xend = hazardRatio(), yend = n_selectedTotal), 
                     colour = "steelblue", size = 1.1) +
        ggplot2::geom_segment(ggplot2::aes(x = hazardRatio(), y = n_selectedTotal,
                         xend = 0, yend = n_selectedTotal), 
                     colour = "steelblue", size = 1.1, 
                     arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm"), type = "closed")) +
        ggplot2::geom_text(x = 0, y = n_selectedTotal, label = paste(round(n_selectedTotal), "events (at sceduled termination)"),
                  vjust = "bottom", hjust = "left") +
        ggplot2::geom_text(x = hazardRatio(), y = 0, label = hazardRatio(), 
                  vjust = "top", hjust = "center") + 
        ggplot2::geom_segment(ggplot2::aes(x = hazardRatio(), y = 0, 
                         xend = hazardRatio(), yend = n_selectedInterim), 
                     colour = "steelblue", size = 1.1) +
        ggplot2::geom_segment(ggplot2::aes(x = hazardRatio(), y = n_selectedInterim,
                         xend = 0, yend = n_selectedInterim), 
                     colour = "steelblue", size = 1.1, 
                     arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm"), type = "closed")) +
        ggplot2::geom_text(x = 0, y = n_selectedInterim, label = paste(round(n_selectedInterim), "events (at interim)"),
                  vjust = "bottom", hjust = "left") +
        ggplot2::geom_text(x = hazardRatio(), y = 0, label = hazardRatio(), 
                  vjust = "top", hjust = "center")
      
      return(events_vs_hr)
    }
  })
  
  
  
  output$click_info <- shiny::renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- shiny::renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- shiny::renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- shiny::renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
  
  # # Download documentation
  # output$downloadDoc <- downloadHandler(
  #   filename = function() {
  #     paste('data-', Sys.Date(), '.docx', sep='')
  #   },
  #   content = function(con) {
  #     write.csv(mtcars, con)
  #   }
  # )
  # 

  
  
  
  

# Fixed design ------------------------------------------------------------
  # Core objects
  # * table power_in is the core table to be edited
  # * table power_out is the output table computed
  # * table power_in_base and power_in_empty are example of power_in
  power_in_base <- tibble::tibble(no = seq_len(2),
                          desc = paste("Endpoint", no),
                          end_type = c("diff", "prop"),
                          n = c(NA, 35),
                          sig.level = 0.05,
                          power = c(0.9, NA),
                          trt = c(1, 0.25),
                          ctrl = c(2, 0.75),
                          sd = c(1, NA),
                          alternative = "two.sided"#,
                          #determine = c("n", "power")
                          )
  
  power_in_empty <- power_in_base %>% dplyr::filter(no == 0)
  
  named_colnames <- c(
    "No." = "no",
    "Endpoint" = "desc",
    "Test type" = "end_type",
    "N" = "n",
    "Significance level" = "sig.level",
    "Marginal power" = "power",
    "Treatment mean/prop." = "trt",
    "Control mean/prop." = "ctrl",
    "SD" = "sd",
    "Alternative hypothesis" = "alternative"#,
    #"Determine" = "determine"#,
    #"Method" = "method"#,
    #"Difference" = "diff",
    #"Effective power" = "tot_power"
  )
  
  power_in <- shiny::reactiveVal()
  power_in2 <- shiny::reactiveVal()
  proxy_in <- DT::dataTableProxy("power_in_table")
  proxy_out <- DT::dataTableProxy("power_out_table")
  
  sig.level_parse <- shiny::reactive({
    (input$ep_sig.level)/100
  })
  
  power_parse <- shiny::reactive({
    (input$ep_power)/100
  })
  
  ep_table_power <- tibble::tibble(
    "no"          = '',
    "desc"        = '',
    "end_type"    = '',
    "n"           = '',
    "sig.level"   = '',
    "power"       = '',
    "trt"         = '',
    "ctrl"        = '',
    "sd"          = '',
    "alternative" = ''
    #"determine"  = '',
    #"method"     = ''#,
    #"diff"       = '',
    #"tot_power"  = ''#,
    #"delta"      = '',
    #"note"       = '' 
  )
  
  ep_table_temp <- ep_table_power%>% dplyr::filter(no == 0)
  ep_table_confirmatory <- ep_table_power%>% dplyr::filter(no == 0)
  ep_table_effPower <- ep_table_power%>% dplyr::filter(no == 0)
  dplyr::rename(ep_table_effPower, named_colnames)
  
  ep_calc <-  shiny::reactive({
    if (input$ep_determine == "power"){
      out <- NNSampleSize::pwr(end_type   = input$ep_type,
                 n           = input$ep_n,
                 sig.level   = sig.level_parse(),
                 power       = NA,
                 trt         = input$ep_trt_mean,
                 ctrl        = input$ep_ctrl_mean,
                 sd          = input$ep_sd,
                 alternative = input$ep_alternative,
                 determine   = "power")
      ep_table_temp <- ep_table_temp %>% dplyr::add_row(no = as.character(input$"ep_no"), desc = as.character(input$ep_desc), end_type = as.character(input$ep_type), 
                                                 n = as.character(input$ep_n), sig.level = as.character(sig.level_parse()*100), power = as.character(round(out[["power"]]*100,2)),
                                                 trt = as.character(input$ep_trt_mean), ctrl = as.character(input$ep_ctrl_mean), sd = as.character(input$ep_sd),
                                                 alternative = as.character(input$ep_alternative)#, 
                                                 #method = as.character(out[["method"]])
                                                 )
      
      power_in(dplyr::rename(ep_table_temp,named_colnames))
      return(power_in())
    }
    else if (input$ep_determine == "n"){
      out <- NNSampleSize::pwr(end_type    = input$ep_type,
                 n           = NA,
                 sig.level   = sig.level_parse(),
                 power       = power_parse(),
                 trt         = input$ep_trt_mean,
                 ctrl        = input$ep_ctrl_mean,
                 sd          = input$ep_sd,
                 alternative = input$ep_alternative,
                 determine   = "n")
      ep_table_temp <- ep_table_temp %>% dplyr::add_row(no = as.character(input$"ep_no"), end_type = as.character(input$ep_type),
                                                 n = as.character(out[["n"]]), sig.level = as.character(sig.level_parse()), power = as.character(power_parse()),
                                                 trt = as.character(input$ep_trt_mean), ctrl = as.character(input$ep_ctrl_mean), sd = as.character(input$ep_sd),
                                                 alternative = as.character(input$ep_alternative)#, 
                                                 #method = as.character(out[["method"]])
                                                 )
      power_in(dplyr::rename(ep_table_temp,named_colnames))
      #power_in(power_in() %>% mutate(Endpoint = NULL))
      return(power_in())
    }
  })
  

  # Table of results / inputs
  output$assumptions <- shiny::renderTable(ep_calc(),
                                    align = "c",
                                    spacing = "xs")
  
  
  output$epSampleSizeResult <-shiny::renderUI({
    outSampleSize <- ep_calc()
    outSampleSize <- outSampleSize %>%  shiny::mutate(Endpoint = NULL, No. = NULL)
    outNumSubjRecruit <- NNSampleSize::getNumSubjRecruit(scr_fail_risk     = (input$fixed_scr_fail_risk)/100, 
                                           dropout_risk_trt  = (input$fixed_dropout_risk_trt)/100,
                                           dropout_risk_ctrl = (input$fixed_dropout_risk_ctrl)/100,
                                           cal_sample_size   = as.numeric(outSampleSize$N)
    )
    outSampleSize <- outSampleSize %>% tibble::add_column('Screening failure risk' = (input$fixed_scr_fail_risk)/100,
                                                  'Treatment dropout risk' = (input$fixed_dropout_risk_trt)/100,
                                                  'Control dropout risk'   = (input$fixed_dropout_risk_ctrl)/100,
                                                  'Recruited N'            = outNumSubjRecruit)
    
    list(
      shinydashboard::valueBox(
        value = ceiling(as.numeric(outSampleSize$N)),
        subtitle = toupper("randomized subjects needed (in each group)"),
        icon = icon("users")
      ),
      shinydashboard::valueBox(
        value = ceiling(as.numeric(outNumSubjRecruit)),
        subtitle = toupper("randomized subjects to be recruited (total)"),
        icon = icon("users")
      ),
      
      shinydashboard::box(
        h4("All assumptions:"),
        width = 8,
        br(),
        
        div(style = 'overflow-x: scroll', shiny::renderTable(outSampleSize,
                                                     align = "c",
                                                     spacing = "xs")),
        br(),
        tags$em("Test type: Endpoint test type, N: Number of subjects, Recruited N: Number 
              of subjects to be recruited, when considering screening failure risk and dropout 
              risk.")
      )
    )
  })
  
  
  # Add the current endpoint to the table of confirmatory endpoints using the "Add"-button: 
  shiny::observeEvent(input$addButton,{
    ep_table_confirmatory <- dplyr::bind_rows(power_in2(),power_in())
    power_in2(ep_table_confirmatory)
    output$confirmatoryEndpoints <- shiny::renderTable(power_in2(),
                                                align = "c",
                                                spacing = "xs")                                                                        
  })
  
  shiny::observeEvent(input$calcPowerSSizeButton,{
    ep_table_effPower <- power_in2()
    ep_table_effPower <- ep_table_effPower %>% tibble::add_column("Effective power" = "")
    for(i in seq(ep_table_effPower[["No."]])){
      if (i == 1){
        ep_table_effPower[["Effective power"]][i] = ep_table_effPower[["Marginal power"]][i]
      }
      else{
        tempPriorEffPower <- as.numeric(ep_table_effPower[["Effective power"]][i-1])/100
        tempEffPower <- as.numeric(ep_table_effPower[["Marginal power"]][i])/100
        ep_table_effPower[["Effective power"]][i] = round((as.numeric(ep_table_effPower[["Effective power"]][i-1])/100*as.numeric(ep_table_effPower[["Marginal power"]][i])/100)*100,2)
      }
    }
    output$effPower <- shiny::renderTable(ep_table_effPower,
                                   align = "c",
                                   spacing = "xs") 
  })
  
  # Remove all added confirmatory endpoints added to 
  shiny::observeEvent(input$clearAllButton,{
    ep_table_confirmatory <- ep_table_confirmatory %>% dplyr::filter(no == 0)
    power_in2(dplyr::rename(ep_table_confirmatory,named_colnames))
    output$confirmatoryEndpoints <- shiny::renderTable(power_in2(),
                                                align = "c",
                                                spacing = "xs")
    
    output$effPower <- shiny::renderTable(dplyr::rename(ep_table_effPower, named_colnames),
                                   align = "c",
                                   spacing = "xs")
  })
  
  # Download documentation
  output$downloadDocWordFixed <- shiny::downloadHandler(
    filename = function() {
      paste('NNXXXX-XXXX_Sample size calculation_', Sys.Date(), '.docx', sep='')
    },
    #filename = "report.docx",
    content = function(file) {
      if(file.exists("documentationOutput//autogeneratedDocumentation.Rmd")){
        file.remove("documentationOutput//autogeneratedDocumentation.Rmd") 
      }
      file.create("documentationOutput//autogeneratedDocumentation.Rmd")
      tempDoc <- readLines("documentationOutput//0.Rmd")
      tempDocCodeOutput <- readLines("documentationOutput//fixedDesignWordTemplate.Rmd")
      write(tempDoc, "documentationOutput//autogeneratedDocumentation.Rmd", append=TRUE)
      write(tempDocCodeOutput, "documentationOutput//autogeneratedDocumentation.Rmd", append=TRUE)
      file.copy("documentationOutput//autogeneratedDocumentation.Rmd", file)
      #tempReport <- file.path("documentationOutput//fixedDesignProgCode.Rmd")
      #tempReport <- file.path(tempdir(), "fixedDesignTemplate.Rmd")
      #file.copy("fixedDesignProgCode.Rmd", tempReport, overwrite = TRUE)
      #rmarkdown::render(input = tempReport, output_file = file, params = list(tableSampleSize = power_in2()), envir = new.env(parent = globalenv()))
      # params <- list(
      #   endType = input$ep_type
      # )
      #rmarkdown::render(input = "documentationOutput//autogeneratedDocumentation.Rmd", output_file = file, params = params, envir = new.env(parent = globalenv()))
      rmarkdown::render(input = "documentationOutput//autogeneratedDocumentation.Rmd", output_file = file)
      
    }
  )

  output$downloadDocRmdFixed <- shiny::downloadHandler(
      #filename = "your-pdf-name.Rmd",
      filename = function() {
        paste('NNXXXX-XXXX_Sample size calculation_', Sys.Date(), '.Rmd', sep='')
      },
      content = function(file) {
        if(file.exists("documentationOutput//autogeneratedDocumentation.Rmd")){
          file.remove("documentationOutput//autogeneratedDocumentation.Rmd") 
        }
        file.create("documentationOutput//autogeneratedDocumentation.Rmd")
        tempDoc <- readLines("documentationOutput//0.Rmd")
        tempDocCodeOutput <- readLines("documentationOutput//fixedDesignRmdTemplate.Rmd")
        write(tempDoc, "documentationOutput//autogeneratedDocumentation.Rmd", append=TRUE)
        write(tempDocCodeOutput, "documentationOutput//autogeneratedDocumentation.Rmd", append=TRUE)
        file.copy("documentationOutput//autogeneratedDocumentation.Rmd", file)
      }
    )
  
  shiny::observeEvent(input$downloadDocRmdFixed,{
    shinyFiles::shinyFileSave("documentationOutput//0.Rmd")
      
    })
        
  
  # Power calculations
  power_out <- shiny::reactive({
    shiny::req(power_in())
    power_in() %>% 
      tidyr::nest(-c(no, desc)) %>% 
      dplyr::mutate(out = invoke_map(pwr, data) %>% map(as_tibble)) %>%  # Apply pwr to each row
      tidyr::unnest() %>% 
      dplyr::mutate(n = dplyr::coalesce(n, n1),
             sig.level = dplyr::coalesce(sig.level, sig.level1),
             power = dplyr::coalesce(power, power1)) %>% 
      dplyr::select(-c(dplyr::ends_with("1"), p2)) %>% 
      dplyr::arrange(no) %>% 
      dplyr:mutate(tot_power = cumprod(power)) %>% 

      dplyr::select(no, desc, n, power, tot_power, note, method)
    
  })

# Bioequivalence ----------------------------------------------------------
  
  # Parse the "study design" input field
  power_calc_BE <-  shiny::reactive({
    out_BE <- NNSampleSize::getSampleSizeBioequi(input$powerBE/100, input$theta0AUC, input$theta0CMAX, input$CV_AUC/100, input$CV_CMAX/100, input$sDesign)
  })
  
  output$essentialsBE <- shiny::renderUI({
    out_BE <- power_calc_BE()
    list(
      shinydashboard::valueBox(
        value = ceiling(out_BE$"Sample size"),
        subtitle = toupper("randomized subjects needed"),
        icon = icon("users")
      )
      
    )
    
  })
  
  
  
  # ------ Output calculated values -------------
  # Custom hypotheses text 
  output$hypotheses_bioequivalence <- shiny::renderUI({
    shinydashboard::box(
      shiny::withMathJax(
        h4("Hypotheses:"),
        p(
          "Two one-sided t-tests are performed for both AUC and CMAX.",
          "The tested null and alternative hypotheses of the two one-sided tests are:", 
          paste0("$$H_0: R <= 0.8"),
          paste0("$$H_A: R <  0.8"),
          "and",
          paste0("$$H_0: R =>  1.25"),
          paste0("$$H_A: R >   1.25"),
          "where R is the ratio of either AUC or CMAX."
        )
      )
    )
  })
  
  # Table of results / inputs
  output$txtBE <- shiny::renderTable(power_calc_BE(),
                              align = "c",
                              spacing = "xs")
  




# Select trial ------------------------------------------------------------
shiny::observeEvent(input$setup_trial_sel_button, {
  if (NNSampleSize::SampleSizeEnv("verbose")) cat("setup_trial_sel_button\n")
  selectedRow <- as.numeric(strsplit(input$setup_trial_sel_button, "_")[[1]][5])
  all_trials <- all_trial_data()

  trial <- all_trials[rownames(all_trials) == selectedRow, ]

  setupRV$trial_select_id <- NULL
  setupRV$trial_select <- trial$Trial
  setupRV$trial_select_id <- trial$trial_id
})


output$out_tab <- shinydashboard::renderMenu({
  if(input$radio == 1){
    shinydashboard::menuItem("Outcomes trial", tabName = "out_tab", icon = icon("table")
    )
    
      
  } else if (input$radio == 2){
    shinydashboard::menuItem("Fixed design trial", tabName = "fix_tab", icon = icon("table")         
    )
    
 
  } else if (input$radio == 3){
    shinydashboard::menuItem("Bioequivalence trial", tabName = "bio_tab", icon = icon("table")
         
    )
  }
})

output$out_tab2 <- shinydashboard::renderMenu({
  if(input$radio == 1){
    shinydashboard::box(
      # Args
      title = "Statistical assumptions",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE, 
      collapsed = TRUE,
      width = 12,
      
      # Content when expanded
      
      
      shiny::sliderInput(inputId = "power",
                         label = div(id = "power_hov", "Power:", icon("question-circle")),
                         post = "%",
                         min = 0,
                         max = 100,
                         step = 0.5,
                         value = 90),
      
      shinyBS::bsPopover(id = "power_hov",       #bsTooltip
                         title = "Statistical power",
                         content = "The power is the ability to correctly reject false null hypothesis.",
                         placement = "right", 
                         trigger = "hover",
                         options = list(container = "body")),
      
      shiny::sliderInput(inputId = "alpha",
                         label = "Alpha level:",
                         post = "%",
                         min = 0,
                         max = 100,
                         step = 0.5,
                         value = 2.5),
      
      shiny::checkboxInput(inputId = "two_sided",
                           label = "Two sided hypothesis",
                           value = TRUE),
      
      tags$strong("Assumptions under the alternative:"),
      
      shiny::conditionalPanel(condition = "input.multiPopulation==0",
                              # numericInput(inputId = "hr",
                              #              label = "Hazard ratio:",
                              #              value = 0.85,
                              #              step = 0.01),
                              shiny::numericInput(inputId = "rr",
                                                  label = "Risk reduction (%)",
                                                  value = 15,
                                                  step = 0.5),
                              
                              shiny::numericInput(inputId = "lambda2",
                                                  label = "Yearly placebo event rate (%) for patients (Hazard rate C):",
                                                  value = 2.17,
                                                  step = 0.01)
      ),
      
      shiny::conditionalPanel(condition = "input.multiPopulation==1",
                              shiny::uiOutput('popAname'),
                              shiny::splitLayout(
                                shiny::numericInput(inputId = "rrPopA",
                                                    label = div(style="display: inline-block;vertical-align:top; width: 150px;", tags$br(), "Risk reduction (%)"),
                                                    value = 15,
                                                    step = 0.5),
                                shiny::numericInput(inputId = "lambda2PopA",
                                                    label = div(style="display: inline-block;vertical-align:top; width: 150px;", "Yearly placebo event rate (%) for patients:"),
                                                    value = 2.17,
                                                    step = 0.01)
                              ),
                              shiny::uiOutput('popBname'),
                              shiny::splitLayout( 
                                shiny::numericInput(inputId = "rrPopB",
                                                    label = div(style="display: inline-block;vertical-align:top; width: 150px;", tags$br(),"Risk reduction (%)"),
                                                    value = 15,
                                                    step = 0.5),
                                shiny::numericInput(inputId = "lambda2PopB",
                                                    label = div(style="display: inline-block;vertical-align:top; width: 150px;", "Yearly placebo event rate (%) for patients:"),
                                                    value = 2.17,
                                                    step = 0.01)
                              )                 
      )
    )
  } else if(input$radio == 2){
    shinydashboard::box(
      # Args
      title = "Trial-wide assumptions",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE, 
      collapsed = TRUE,
      width = 12,
      
      # Content when expanded
      # Dropout risk
      shiny::sliderInput(inputId = "fixed_scr_fail_risk", 
                         label = span("Screening failure risk"),
                         min = 0, max = 100, value = 15, step = 0.5,
                         post = "%"),
      
      # Dropout risk
      shiny::sliderInput(inputId = "fixed_dropout_risk_trt", 
                         label = shiny::span("Dropout risk in treatment group"),
                         min = 0, max = 100, value = 10, step = 0.1,
                         post = "%"),
      shiny::sliderInput(inputId = "fixed_dropout_risk_ctrl", 
                         label = "Dropout risk in control group",
                         min = 0, max = 100, value = 10, step = NULL,
                         post = "%") #,
      
      
    )
  } else if(input$radio == 3){
    shinydashboard::box(
      # Args
      title = "Trial design parameters",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE, 
      collapsed = TRUE,
      width = 12,
      
      
      shiny::radioButtons(inputId = "sDesign",
                          label = "Study design:",
                          choices = c("Parallel" = "parallel",
                                      "Cross-over" = "2x2"),
                          selected = c("parallel"))
    )
  }
  
})


output$out_tab3 <- shinydashboard::renderMenu({
  if(input$radio == 1){
    shinydashboard::box(
      # Args
      title = "Trial design parameters",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      
      
      
      shiny::radioButtons(inputId = "interimAnalysis",
                          label = "Interim analysis",
                          choices = c("Interim" = "interim",
                                      "No interim" = "no_interim"),
                          selected = c("no_interim"),
                          inline = TRUE),
      
      shiny::conditionalPanel(condition = "input.interimAnalysis=='interim'",
                              shiny::numericInput(inputId = "timingInterim",
                                                  label = "Interim timing",
                                                  value = 2/3,
                                                  min   = 0,
                                                  max   = 1,
                                                  step  = 0.01)),
      
      shiny::conditionalPanel(condition = "input.interimAnalysis=='interim'",
                              shiny::radioButtons(inputId = "sfuInterim",
                                                  label = "Spending function:",
                                                  choices = c("O'Brien-Flemming bounds" = "OF",
                                                              "Pocock bounds" = "Pocock"),
                                                  selected = c("OF"))),
      
      
      
      shiny::sliderInput(inputId = "max_study_duration",
                         label = "Maximum study duration (in months):",
                         min = 0,
                         max = 120,
                         step = 1,
                         value = 59),
      
      shiny::sliderInput(inputId = "accrual_druation",
                         label = "Accrual (recuitment) duration (in months):",
                         min = 0,
                         max = 120,
                         step = 1,
                         value = 28),
      
      tags$strong("Description of Population:"),
      
      # Content when expanded
      shiny::textInput(inputId = "alloc_ratio",
                       label = "Allocation ratio (Experimental : Control):",
                       value = "1 : 1"),
      
      shiny::numericInput(inputId = "eta",
                          label = "Dropout rate (same in both groups):",
                          value = 0.01,
                          step = 0.01),
      
      shiny::checkboxInput(inputId = "multiPopulation",
                           label = "Define more than one population",
                           value = FALSE),
      
      # Content when expanded
      shiny::conditionalPanel(condition = "input.multiPopulation==1",
                              shiny::textInput(inputId = "namePopA",
                                               label = "Name of population A:",
                                               value = "Population A"),
                              
                              shiny::textInput(inputId = "namePopB",
                                               label = "Name of population B:",
                                               value = "Population B"),
                              
                              
                              shiny::sliderInput(inputId = "mixRatio",
                                                 label = "Mixing ratio:",
                                                 min = 0,
                                                 max = 100,
                                                 step = 5,
                                                 value = 50),
                              
                              shiny::uiOutput('mixture'))
      
    )
  } else if(input$radio == 2){
    shinydashboard::box(
      # Args
      title = "Endpoint input",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE, 
      collapsed = TRUE,
      width = 12,
      
      # Content when expanded
      tags$strong("Determine the type of calculation"),
      shinyWidgets::radioGroupButtons(inputId = "ep_determine", label = "",
                                      choices = c("Power" = "power", 
                                                  "Number of subjects" = "n"),
                                      selected = "power"),
      hr(),
      
      
      shiny::conditionalPanel(
        condition = "input.ep_determine == 'n'",
        tags$strong("Specify trial design and statistical assumptions"),
        sliderInput(inputId = "ep_power", label = "Power",
                    min = 0, max = 100, step = 1,
                    value = 80, post = "%")
      ),
      
      shiny::conditionalPanel(
        condition = "input.ep_determine == 'power'",
        tags$strong("Specify number of subjects"),
        shiny::numericInput(inputId = "ep_n", label = "Number of subjects in each group (NOTE: Should be the same for all endpoints)", 
                            value = 90),
        hr(),
        tags$strong("Specify the parameters of each endpoint"),
        shiny::numericInput(inputId = "ep_no", label = "Select endpoint number", min = 1,
                            value = 1, step = 1),
        shiny::textInput(inputId = "ep_desc", label = "Endpoint name")
        
      ),
      
      shiny::sliderInput(inputId = "ep_sig.level", label = "Significance level",
                         min = 0, max = 100, step = 0.5,
                         value = 5, post = "%"),
      
      shinyWidgets::radioGroupButtons(inputId = "ep_type", label = "Endpoint test type",
                                      choices = c("Difference" = "diff", 
                                                  "Proportions" = "prop")),
      shiny::conditionalPanel(
        condition = "input.ep_type == 'diff'",
        shiny::numericInput(inputId = "ep_trt_mean",  label = "Mean in treatment group",
                            value = 1.5),
        shiny::numericInput(inputId = "ep_ctrl_mean", label = "Mean in control group",
                            value = 0.5),
        shiny::numericInput(inputId = "ep_sd", label = "Standard deviation in both groups",
                            value = 1, min = 0)
      ),
      shiny::conditionalPanel(
        condition = "input.ep_type == 'prop'",
        shiny::sliderInput(inputId = "ep_trt_prop",  label = "Proportion in treatment group",
                           min = 0, max = 1, step = 0.01, value = 0.85),
        shiny::sliderInput(inputId = "ep_ctrl_prop", label = "Proportion in control group",
                           min = 0, max = 1, step = 0.01, value = 0.75)
      ),
      shinyWidgets::radioGroupButtons(inputId = "ep_alternative", label = "Alternative hypothesise",
                                      choices = c("One sided" = "one.sided", 
                                                  "Two sided" = "two.sided"),
                                      selected = "two.sided")
      
    )
  } else if(input$radio == 3){
    shinydashboard::box(
      # Args
      title = "Statistical assumptions",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE, 
      collapsed = TRUE,
      width = 12,
      
      # Content when expanded
      shiny::sliderInput(inputId = "powerBE",
                         label = div(id = "power_hov", "Power:", icon("question-circle")),
                         post = "%",
                         min = 0,
                         max = 100,
                         step = 0.5,
                         value = 90),
      
      shinyBS::bsPopover(id = "power_hov",       #bsTooltip
                         title = "Statistical power",
                         content = "The power is the ability to correctly reject false null hypothesis.",
                         placement = "right", 
                         trigger = "hover",
                         options = list(container = "body")),
      
      shiny::sliderInput(inputId = "theta0AUC",
                         label = "Assumed AUC ratio:",
                         min = 0.5,
                         max = 1.5,
                         step = 0.01,
                         value = 1.05),
      
      shiny::sliderInput(inputId = "theta0CMAX",
                         label = "Assumed CMAX ratio:",
                         min = 0.5,
                         max = 1.5,
                         step = 0.01,
                         value = 1.05),
      
      sliderInput(inputId = "CV_AUC",
                  label = "Coefficent of variations (%) of the AUC ratio",
                  min = 0,
                  max = 100,
                  step = 0.1,
                  value = 17.5),
      
      shiny::sliderInput(inputId = "CV_CMAX",
                         label = "Coefficent of variations (%) of the CMAX ratio",
                         min = 0,
                         max = 100,
                         step = 0.1,
                         value = 17.5)
   )
   }
  
})

output$out_tab4 <- shinydashboard::renderMenu({
  if(input$radio == 1){
    shinydashboard::box(
      title = "Download documentation",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      NNSampleSize::myDownloadBttn(outputId = "downloadDocWordOut", 
                                   label = "Word document",
                                   icon = icon("file-word")
      ),
      
      NNSampleSize::myDownloadBttn(outputId = "downloadDocRmdOut", 
                                   label = " Rmarkdown document",
                                   icon = icon("file")
      
    ))
  } else if(input$radio == 2){
    shinydashboard::box(
      # Args
      title = "Download documentation",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE, 
      collapsed = TRUE,
      width = 12,
      NNSampleSize::myDownloadBttn(outputId = "downloadDocWordFixed", 
                                   label = "Word document",
                                   icon = icon("file-word")
                                  ),
      
      NNSampleSize::myDownloadBttn(outputId = "downloadDocRmdFixed", 
                                   label = " Rmarkdown document",
                                   icon = icon("file")
                                  )
    )
  } else if(input$radio == 3){
    shinydashboard::box(
      # Args
      title = "Download documentation",
      status = "info",
      background = "navy",
      solidHeader = FALSE,
      collapsible = TRUE, 
      collapsed = TRUE,
      width = 12,
      NNSampleSize::myDownloadBttn(outputId = "downloadDocWordBioEq", 
                                   label = "Word document",
                                   icon = icon("file-word")
      ),
      
      NNSampleSize::myDownloadBttn(outputId = "downloadDocRmdBioEq", 
                                   label = " Rmarkdown document",
                                   icon = icon("file")
      )
      
    )
  }
  
})





trial_info <- shiny::reactive({

  shiny::req(setupRV$user, setupRV$trial_select_id, all_trial_data())
  if (NNSampleSize::SampleSizeEnv("verbose")) cat("trial_info\n")

  # all trials info
  review_access_all <- all_trial_data()
  review_access <- review_access_keep <- review_access_all[review_access_all$trial_id == setupRV$trial_select_id, ]

  # user info
  if (NNSampleSize::SampleSizeEnv("verbose")) cat(paste("selected_trial", setupRV$trial_select, "\n"))

  if (setupRV$user %in% unlist(review_access$superusers)) {
    setupRV$user_type <- "superuser"
  } else if (setupRV$user %in% unlist(review_access$users)) {
    setupRV$user_type <- "reviewer"
  } else {
    setupRV$user_type <- "Visitor"
  }

  # Get all data directories
  dirs <- list.dirs(file.path(review_access$app_data, "data"))

  # subset to actual data directories
  dirs <- dirs[grep("review_round_", dirs)]
  setupRV$trial_data_dir <- file.path(review_access$app_data, "data", paste0("review_round_", length(dirs)))

  #print(setupRV$trial_data_dir)
  if (NNSampleSize::SampleSizeEnv("verbose")) cat(paste("user type:", setupRV$user_type, "\n"))

  # Get the reviewers
  setupRV$reviewers <-
    unique(c(unlist(review_access[review_access$Trial == setupRV$trial_select, "superusers"]),
             unlist(review_access[review_access$Trial == setupRV$trial_select, "users"])))

  out <- list(
    user           = setupRV$user,
    user_type      = setupRV$user_type,
    reviewers      = setupRV$reviewers,
    trial_id       = setupRV$trial_select_id,
    trial          = setupRV$trial_select,
    trial_app_dir  = review_access$app_data,
    trial_data_dir = setupRV$trial_data_dir
  )

  return(out)
})



# Trial create/update -----------------------------------------------------
  # update the selections in new/create trial section
  shiny::observeEvent(setupRV$trial_select_id, {

    shiny::req(GlobalRV$review_access, nrow(GlobalRV$review_access))
    if (NNSampleSize::SampleSizeEnv("verbose")) cat("update_trial_edit_parms\n")

    review_access_all <- GlobalRV$review_access
    review_access <- review_access_keep <- review_access_all[review_access_all$trial_id == setupRV$trial_select_id, ]

    shiny::updateTextInput(session, "new_trial_project", value = review_access$Project)
    shiny::updateTextInput(session, "new_trial_trial", value = review_access$Trial)
    shiny::updateTextInput(session, "new_trial_title", value = review_access$Title)

    if (NNSampleSize::SampleSizeEnv("data_storage") == "trial_specific") {
      NNSampleSize::updatePathInput(session, "new_trial_datapath", value = review_access$data_path)
      NNSampleSize::updatePathInput(session, "new_trial_appdata", value = review_access$app_data)
    }

    shiny::updateSelectizeInput(session, "new_trial_superusers", selected = as.list(unlist(review_access$superusers)),
                         choices = as.list(unlist(review_access$superusers)))

    shiny::updateSelectizeInput(session, "new_trial_users", selected = as.list(unlist(review_access$users)),
                         choices = as.list(unlist(review_access$users)))

    shiny::updateCheckboxInput(session, "new_trial_ongoing", value = review_access$ongoing)
  })


  # when all info fields are specified and saved it is possible to upload data
  output$new_trial_save_ui <- shiny::renderUI({

    shiny::validate(
      shiny::need(input$new_trial_project, "Project needs to be defined"),
      shiny::need(input$new_trial_trial, "Trial needs to be four digits"),
      shiny::need(input$new_trial_title, "Protocol title needs to be added"),
      shiny::need(input$new_trial_superusers, "Some superuser need to be assigned")
    )

    if (NNSampleSize::SampleSizeEnv("data_storage") == "trial_specific") {
      if (NNSampleSize::SampleSizeEnv("verbose")) cat("checking dir exist\n")
      shiny::validate(
        #need(dir.exists(input$new_trial_datapath), "The data path needs to exist"),
        shiny::need(dir.exists(input$new_trial_appdata), "The app data path needs to exist")
      )
    }
    shiny::actionButton("new_trial_save",   "Save as new trial", width = "150px")
  })

  # when editing an existing trial it is possible to update
  output$new_trial_update_ui <- shiny::renderUI({
    shiny::req(input$new_trial_project, input$new_trial_trial,
        input$new_trial_title, input$new_trial_superusers)

    trial_data_store <- all_trial_data()
    trial_id <- setupRV$trial_select_id
    
    
    
    if (NNSampleSize::SampleSizeEnv("data_storage") == "trial_specific") {
      if (NNSampleSize::SampleSizeEnv("verbose")) cat("checking dir exist\n")
      shiny::req(dir.exists(input$new_trial_appdata))
    }

    shiny::req(all_trial_data(), setupRV$trial_select_id %in% all_trial_data()$trial_id)

    
    shiny::req(setupRV$user %in% 
          unlist(trial_data_store[trial_data_store$trial_id == trial_id,]$superusers))
    
    shiny::actionButton("new_trial_update", "Update", width = "150px")
  })

  # When pushing the trial info save button
  shiny::observeEvent(input$new_trial_save, {


    trial_id <- paste0("SampleSize_review_",
                       gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))),
                       "_", sample(100:999, 1))

    setupRV$trial_select_id <- trial_id
    setupRV$trial_select <- input$new_trial_trial

    trial_info <-
      tibble::tibble(Project    = input$new_trial_project,
                     Trial      = input$new_trial_trial,
                     Title      = input$new_trial_title,
                     superusers = list(toupper(input$new_trial_superusers)),
                     users      = list(toupper(input$new_trial_users)),
                     ongoing    = input$new_trial_ongoing,
                     data_path  = "",
                     app_data   = file.path(NNSampleSize::SampleSizeEnv("data_storage"), paste0("data_", trial_id)), # the place to store data, comments
                     trial_id   = trial_id)

    if (NNSampleSize::SampleSizeEnv("data_storage") == "trial_specific") {
      trial_info$data_path <- input$new_trial_datapath
      trial_info$app_data  <- input$new_trial_appdata
    } else {
      dir.create(trial_info$app_data, showWarnings = FALSE, recursive = TRUE)
    }

    file <- trial_id

    dir.create(file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "trial_setup", paste0("access_", trial_id)),
               showWarnings = FALSE, recursive = TRUE)

    time <- gsub("[:-]", "", gsub(" ", "_", as.character(Sys.time())))

    
    GlobalRV$review_access <<- dplyr::bind_rows(GlobalRV$review_access, trial_info)
    
    setupRV$just_added <- TRUE
    
    saveRDS(trial_info, file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "trial_setup", paste0("access_", trial_id),
                                  paste0(time, "_", "trial_info", ".rds")))
  })

  # when clicking update
  shiny::observeEvent(input$new_trial_update, {

    trial_id <- setupRV$trial_select_id

    trial_info <-
      tibble::tibble(Project    = input$new_trial_project,
                     Trial      = input$new_trial_trial,
                     Title      = input$new_trial_title,
                     superusers = list(toupper(input$new_trial_superusers)),
                     users      = list(toupper(input$new_trial_users)),
                     ongoing    = input$new_trial_ongoing,
                     data_path  = "",
                     app_data   = file.path(NNSampleSize::SampleSizeEnv("data_storage"), paste0("data_", trial_id)), # the place to store data, comments
                     trial_id   = trial_id)

    if (NNSampleSize::SampleSizeEnv("data_storage") == "trial_specific") {
      trial_info$data_path <- input$new_trial_datapath
      trial_info$app_data  <- input$new_trial_appdata
    } else {
      dir.create(trial_info$app_data, showWarnings = FALSE, recursive = TRUE)
    }

    file <- trial_id

    dir.create(file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "trial_setup",  paste0("access_", trial_id)),
               showWarnings = FALSE, recursive = TRUE)

    time <- gsub("[:-]", "", gsub(" ", "_", as.character(Sys.time())))

    c_access <- GlobalRV$review_access
    
    c_access[c_access$trial_id == trial_id, ] <- trial_info
    
    GlobalRV$review_access <<- c_access
    
    setupRV$just_added <- TRUE
    
    saveRDS(trial_info, file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "trial_setup", paste0("access_", trial_id),
                                  paste0(time, "_", "trial_info", ".rds")))
  })
  
  output$new_trial_save_success <- shiny::renderUI({
    req(input$new_trial_project, input$new_trial_trial,
        input$new_trial_title, input$new_trial_superusers)
    
    trial_data_store <- all_trial_data()
    trial_id <- setupRV$trial_select_id
    
    
    
    if (NNSampleSize::SampleSizeEnv("data_storage") == "trial_specific") {
      if (NNSampleSize::SampleSizeEnv("verbose")) cat("checking dir exist\n")
      shiny::req(dir.exists(input$new_trial_appdata))
    }
    
    shiny::req(all_trial_data(), setupRV$trial_select_id %in% all_trial_data()$trial_id)
    
    
    shiny::req(setupRV$user %in% 
          unlist(trial_data_store[trial_data_store$trial_id == trial_id,]$superusers))
    
  })


# Usage information -------------------------------------------------------
  login_info <-
    shiny::reactivePoll(500, NULL, checkFunc = function() NNSampleSize::loginInfo(TRUE),
               valueFunc = loginInfo)

  output$review_overview <- DT::renderDataTable({
    shiny::req(login_info())
    if (NNSampleSize::SampleSizeEnv("verbose")) cat("initialising review table\n")

    return(login_info())


    setupRV$user

    session_info <- session_info_re()

    session_info[grep(" ", names(session_info), invert = TRUE)] <- NULL

    data <- dplyr::bind_rows(lapply(session_info, tibble::as_tibble))

    data[data$online, "logout_time"] <- Sys.time()

    data
  })
})
