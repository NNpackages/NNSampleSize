
#' Create multiple similar shiny inputs for a table
#'
#' @param FUN \code{function} The shiny input function to use
#' @param n \code{numeric} giving The number of shiny inputs needed
#' @param id Id \code{character} with an id
#' @param ... Additional arguments passed on to function \code{FUN}
#'
#' @return \code{character} of length n containing the shiny inputs with id
#'   given as  paste0(id, i) where i is the row number
#' @export
#'
#' @examples
#' if (require("shiny"))
#'   shinyInput(actionButton, 4, "a_good_id_", "go")
shinyInput <- function(FUN, n, id, ...) {
  inputs <- character(n)
  for (i in seq_len(n)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}


coerceToFunc <- function(x) {
  force(x)
  if (is.function(x))
    return(x)
  else return(function() x)
}

#' Return objects from SampleSizeEnv environment
#'
#' @param intervalMillis Approximate number of milliseconds to wait between
#'   checking the variable check.
#' @param session The user session to associate this file reader with, or NULL
#'   if none. If non-null, the reader will automatically stop when the session
#'   ends.
#' @param check character indicating what to check. If this has changed the
#'   object referred to by check is fectched
#' @param name The object to fetch from the SampleSizeEnv
#'
#' @return reactive function with the object stored in SampleSizeEnv with name: name
#' @export
#' @importFrom shiny reactiveValues isolate observe invalidateLater reactive
checkSampleSizeEnv <- function(intervalMillis, session = NULL, check, name){
  
  intervalMillis <- coerceToFunc(intervalMillis)
  rv <- shiny::reactiveValues(cookie = shiny::isolate(SampleSizeEnv(check)))
  
  shiny::observe({
    rv$cookie <- SampleSizeEnv(check)
    shiny::invalidateLater(intervalMillis(), session)
  })
  
  re <- shiny::reactive({
    rv$cookie
    SampleSizeEnv(name)
  }, label = NULL)
  return(re)
}

#' Update log in info
#'
#' @param session.id A unique session id
#' @param user the user 
#' @param login time of login
#' @param logout time of logout
#'
#' @return The function is used for the side effects. It returns TRUE
#' @export
updateLoginInfo <- function(session.id, user, login = FALSE, logout = FALSE) {
  
  # Get current session info
  session_info <- SampleSizeEnv("session_info")
  
  if (!missing(login) && login) {
    if (missing(user)) {
      message("You need to specify user for login")
    }
    # add user to online users
    SampleSizeEnv("online_user", unique(c(SampleSizeEnv("online_user"), toupper(user))))
    
    # Add 1 to the number of logins
    if (is.null(session_info[[session.id]][["login_count"]])) {
      session_info[[session.id]][["login_count"]] <- 0
    }
    n_logins <- session_info[[session.id]][["login_count"]] + 1
    session_info[[session.id]][["login_count"]] <- n_logins
    
    # Add the new user to session info
    session_info[[paste(session.id, n_logins)]] <-
      list(user        = toupper(user),
           login_time  = Sys.time(),
           logout_time = Sys.time(),
           online      = TRUE)
  }
  
  if (!missing(logout) && logout) {
    #remove from online users
    if (!missing(user))
      SampleSizeEnv("online_user", setdiff(SampleSizeEnv("online_user"), toupper(user)))
    
    
    # Get number of logins
    n_logins <- session_info[[session.id]][["login_count"]]
    
    # update session_info
    session_info[[paste(session.id, n_logins)]][["logout_time"]] <- Sys.time()
    session_info[[paste(session.id, n_logins)]][["online"]] <- FALSE
  }
  
  SampleSizeEnv("session_info", session_info)
  
  SampleSizeEnv("session_info_last_change", Sys.time())
  
  return(TRUE)
}



#' Login info for a current run
#'
#' @param check Should the function return last update time or the actual login info
#'
#' @return An object of class \code{tibble} with info on user logins
#' @export
loginInfo <- function(check = FALSE) {
  
  if (check) return(SampleSizeEnv("session_info_last_change"))
  
  # Get the login info
  session_info <- SampleSizeEnv("session_info")
  
  # remove unwanted data
  session_info[grep(" ", names(session_info), invert = TRUE)] <- NULL
  
  # convert to tibble
  data <- dplyr::bind_rows(lapply(session_info, tibble::as_tibble))
  
  # Update users logout time to now for online users
  data[data$online, "logout_time"] <- Sys.time()
  
  return(data)
}
