#' Download button
#'
#' @param outputId    The id of the output
#' @param label       To be downloaded
#' @param style       The style
#' @param color       The color
#' @param size        The size
#' @param block       True or false
#' @param no_outline  True or false
#' @param icon_       The icon for the button
#'
#' @return Downloads the output
#' @export
#' 
#' @importFrom shiny icon
myDownloadBttn <-
function(outputId,
                           label = "Download",
                           style = "bordered",
                           color = "default", 
                           size = "md",  
                           block = FALSE,
                           no_outline = FALSE,
                           icon_ = icon("download")){
  bttn <- shinyWidgets::actionBttn(inputId = paste0(outputId, "_bttn"),
                     label = tags$a(id = outputId, 
                                    class = "shiny-download-link",
                                    href = "", target = "_blank", 
                                    download = NA, label),
                     color = color, 
                     style = style, 
                     size = size, 
                     block = block, 
                     no_outline = no_outline, 
                     icon = icon_)
  bttn
}

