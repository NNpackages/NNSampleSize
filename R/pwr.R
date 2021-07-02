#' Calculate the power
#'
#' @param end_type The endpoint test type
#' @param n Number of observations
#' @param sig.level Significance level
#' @param power Power of test
#' @param trt Mean in treatment group
#' @param ctrl Men in control group
#' @param sd Standard deviation in both groups
#' @param alternative Alternative hypothesis
#' @param determine Set to NA
#' 
#' @importFrom stats power.prop.test power.t.test qnorm  
#'
#' @return Returns the power calculation
#' @export
#'

pwr <-
function(end_type, 
                n, 
                sig.level,
                power,
                trt,
                ctrl, 
                sd, 
                alternative,
                determine) {
  assign(determine, NA)
  if (anyNA(n)) {n <- NULL}
  if (anyNA(power)) {power <- NULL}

  if (end_type == "diff") {
    delta <- trt - ctrl
    res <- power.t.test(
      n = n,
      delta = delta,
      sd = sd,
      power = power,
      sig.level = sig.level,
      alternative = alternative
    )
  } else if (end_type == "prop") {
    res <- power.prop.test(
      n = n, 
      p1 = trt, 
      p2 = ctrl, 
      power = power,
      sig.level = sig.level,
      alternative = alternative,
      strict = FALSE,
      tol = .Machine$double.eps^0.25
    )
  } else {
    stop("end_type not supported")
  }
  
  exclude <- c("alternative")
  out <- unclass(res)
  return(subset(out, !(names(out) %in% exclude)))
}
