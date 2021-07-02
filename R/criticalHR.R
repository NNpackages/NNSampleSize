#' Calculate the critical hazard ratio
#'
#' @param n_events Number of events 
#' @param z_alpha One sided significance level
#'
#' @return Returns the critical hazard ratio
#' @export
#'
#' @examples
#' exp(-2*1*sqrt(1/40))

criticalHR <-
function(n_events, z_alpha) {
  return(exp(-2*z_alpha*sqrt(1/n_events)))
}
