#' Run the pk pd review tool
#'
#' @param trial The four digit trial to review
#' @param username Username to initialise the app without logging in
#' @param workspace is the app run locally or at server
#' @param verbose Should text be written in the console to describe what is
#'   currently running. Used for debugging.
#' @param port The to initialise the app on.
#' @param ... Additional arguments passed on to \cite{shiny}{runApp}
#' @param access_data_path Path to store application data
#' @param admins \code{character} indicating user that admins
#' @param data_storage \code{character} with either path to folder for data
#'   storage or \code{"trial_specific"} for path to be defined in the UI.
#'
#' @return Runs the server app with the specified settings
#' @export
#'
#' @examples
#' \dontrun{
#' runSampleSizereview()
#' }
#' @importFrom shiny runApp
#' @importFrom magrittr '%>%'
#' @importFrom shiny actionLink
#' @importFrom shinyjs addClass
#' @importFrom shinydashboard box
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyalert runExample
#' @importFrom DT addRow
#' @importFrom haven read_sas
#' @importFrom readxl anchored
#' @importFrom zip unzip
#' @importFrom shinyBS addPopover
#' @importFrom kableExtra add_footnote
#' @importFrom gridExtra arrangeGrob
#' @importFrom rhandsontable editAddin
#' @importFrom stringr boundary
runSampleSizereview <- function(trial = NULL, username = unname(Sys.info()["user"]),
                          workspace = c("local", "server", "shinyServer"),
                          verbose = FALSE,
                          port = trial,
                          access_data_path = file.path(getwd(), "persistent_data"),
                          data_storage = file.path(access_data_path, "trial_data"),
                          admins = username,
                          ...) {

  workspace <- match.arg(workspace)

  appDir <- system.file("review-app", package = "NNSampleSize")

  if (appDir == "") {
    stop("Could not find the review app directory. Try re-installing 'NNSampleSize'.", call. = FALSE)
  }

  SampleSizeEnv("admins", admins)
  SampleSizeEnv("online_user", character(0))
  SampleSizeEnv("trial", trial)
  SampleSizeEnv("username", username)
  SampleSizeEnv("workspace", workspace)
  SampleSizeEnv("verbose", verbose)

  SampleSizeEnv("access_data_path", access_data_path)
  SampleSizeEnv("data_storage", data_storage)
  SampleSizeEnv("session_info", list())

  SampleSizeEnv("runApp", TRUE)

  if (workspace == "shinyServer") {
    return(appDir)
  } else {
    shiny::runApp(appDir, display.mode = "normal", port = port)
  }
}

#' Read a file into R
#'
#' @param path path to the file
#' @param sep The separator between columns. Defaults to the character in the
#'   set \code{[,\\t |;:]} that separates the sample of rows into the most
#'   number of lines with the same number of fields. Use \code{NULL} or
#'   \code{""} to specify no separator; i.e. each line a single character column
#'   like \code{base::readLines} does.
#' @param sep2 separator to be used within columns
#' @param which_sep2 The columns to use sep2 on, either as integer with column
#'   numbers or character with column names. If Null all character columns are
#'   used.
#' @param sep3 Third level separator
#' @param which_sep3 columns to use the third level sep on.
#' @param ... Arguments passed on to fread
#'
#' @return the loaded data as tibble
#' @export
#'
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
readTable <- function(path, sep="auto", sep2 = "\\|", which_sep2 = NULL, sep3 = "\032", which_sep3 = NULL, ...) {

  data <- tibble::as_tibble(data.table::fread(input = path, sep = sep, ...))

  if (nrow(data)) {
    if (!is.null(sep2)) {
      if (is.null(which_sep2)) {
        chars <- which(sapply(data, class) == "character")
        which_sep2 <- chars[which(sapply(data[chars], function(x) any(grepl(sep2, x))))]
      } else {
        if (class(which_sep2) == "character") {
          which_sep2 <- match(which_sep2, colnames(data))
        }
      }

      data[which_sep2] <-
        lapply(lapply(data[which_sep2], as.character), base::strsplit, split = sep2)
    }

    if (!is.null(which_sep3)) {
      if (class(which_sep3) == "character") {
        which_sep3 <- match(which_sep3, colnames(data))
      }

      data[which_sep3] <-
        lapply(data[which_sep3], lapply, base::strsplit, split = sep3)
    }
  }

  return(data)
}


#' Write a data table to a file
#'
#' @param x The tibble to save
#' @param path The path to the file
#' @param sep The separator between columns
#' @param sep2 The separator within columns
#' @param sep3 Third level separator
#' @param which_sep3 columns to use the third level sep on.
#' @param ... Arguments passed on to fwrite
#'
#' @return Saves x to the file supplied in path
#' @export
writeTable <- function(x, path, sep = ";", sep2 = c("","|",""), sep3 = "\032", which_sep3 = NULL, ...) {

  if (!is.null(which_sep3)) {
    if (class(which_sep3) == "character") {
      which_sep3 <- match(which_sep3, colnames(x))
    }

    x[which_sep3] <-
      lapply(x[which_sep3], lapply,  function(x) sapply(x,
            function(x) {paste(x, collapse = sep3, sep = "")}))

    x[which_sep3] <- lapply(x[which_sep3], lapply, function(x) if (length(x)) {return(x)} else {return("")})
  }

  data.table::fwrite(x = x, file = path, sep = sep, sep2 = sep2)
}


#' Returns the path to the project drive
#'
#' Based on the OS, this function returns the path on the project drive.
#'
#' @inherit hDrive
#' @return Windows systems returns \code{"//whale.novo.dk/projstat/"} and
#'   non-windows systems returns \code{"/projstat/"} followed by any giving
#'   paths.
#' @export
pDrive <- function(path = "", ...) {
  if (.Platform$OS.type == "windows") {
    p_drive <- "//whale.novo.dk/projstat"
  } else {
    p_drive <- "/projstat"
  }
  return(paste(p_drive, path, ..., sep = .Platform$file.sep))
}

#' Returns the path to the h drive
#'
#' Based on the OS, this function returns the path on the user specific ("H")
#' drive.
#'
#' @param path A \code{character} specifying a path to a subfolder.
#' @param ... More paths (passed to \code{\link{paste}}).
#' @return Windows systems returns \code{"//whale.novo.dk/<initial>/"} whereas
#'   non-windows systems returns \code{file.path("/z/falcon/home/<initial>/"}
#'   followed by any given paths.
#' @export
hDrive <- function(path = "", ...) {
  if (.Platform$OS.type == "windows") {
    h_drive <- file.path("//whale.novo.dk", unname(Sys.info()["user"]))
  } else {
    h_drive <- file.path("/z/falcon/home", unname(Sys.info()["user"]))
  }
  return(paste(h_drive, path, ..., sep = .Platform$file.sep))
}
