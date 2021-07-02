#' Get proportion color
#'
#' @param prop The proportion
#'
#' @return Returns the correct proportion color
#' @export
#'
#' @examples
#' 0.8

get_prop_col <-
function(prop) {
  c("red", "yellow", "green")[findInterval(prop, c(0, 0.6, 0.8, 1))]
}
