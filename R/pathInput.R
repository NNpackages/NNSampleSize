#' This function generates the client-side HTML for a path input
#'
#' @param inputId The id of the input
#' @param label The label
#' @param value The initiating value
#' @param width The width of the input
#'
#' @export
#' @importFrom shiny tagList singleton tags
pathInput <- function(inputId, label, value = "", width = NULL) {
  "%AND%" <- function(x, y)
  {
    if (!is.null(x) && !is.na(x))
      if (!is.null(y) && !is.na(y))
        return(y)
    return(NULL)
  }

  shiny::tagList(
    # This makes web page load the JS file in the HTML head.
    # The call to singleton ensures it's only included once
    # in a page.
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = "path-input-binding.js")
      )
    ),


    shiny::tags$div(class = "form-group shiny-input-container", style = if (!is.null(width))
      paste0("width: ", shiny::validateCssUnit(width), ";"), label %AND%
        shiny::tags$label(label, `for` = inputId),
      shiny::tags$input(id = inputId, class = "form-control",
                        type = "path",
                        value = value))
  )
}


#' Send an update message to a path input on the client.
#'
#' This update message can change the value and/or label.
#'
#' @param session The session to update
#' @param inputId The id of the path
#' @param label The label
#' @param value The initiating path
#'
#' @export
updatePathInput <- function(session, inputId,
                            label = NULL, value = NULL) {

  message <- dropNulls(list(label = label, value = value))
  session$sendInputMessage(inputId, message)
}



#' Time input
#'
#' This function generates the client-side HTML for a URL input
#'
#' @param inputId The id of the input
#' @param label The label
#' @param value The initiating value
#'
#' @export
timeInput <- function(inputId, label, value = "") {
  tagList(
    # This makes web page load the JS file in the HTML head.
    # The call to singleton ensures it's only included once
    # in a page.
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = "time-input-binding.js")
      )
    ),
    shiny::tags$label(label, `for` = inputId),
    shiny::tags$input(id = inputId,  class = "form-control", type = "time", value = value)
  )
}

#' Update time input
#'
#' Send an update message to a path input on the client.
#' This update message can change the value and/or label.
#'
#'
#' @param session The session to update
#' @param inputId The id of the path
#' @param label The label
#' @param value The initiating path
#' @export
updateTimeInput <- function(session, inputId,
                            label = NULL, value = NULL) {

  message <- dropNulls(list(label = label, value = value))
  session$sendInputMessage(inputId, message)
}


# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

