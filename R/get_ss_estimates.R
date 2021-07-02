#' Get sample size estimates
#'
#' Core function for computing number of events and subjects
#'
#' @param lambdaC Yearly placebo event rate (percent) for patients (Hazard rate C)
#' @param eta Equal dropout hazard rate for both groups
#' @param minfup Minimum follow up
#' @param T_max Maximum study duration
#' @param sided One or two sided test
#' @param ratio Randomization ratio between placebo and treatment group
#' @param hr Hazard rate
#' @param alpha The significance level
#' @param beta 1-beta is the power
#' @param interim Should the analysis include interim or not
#' @param timing The timing of interim
#' @param sfu Spending function when including interim
#'
#' @return Returns the calculated sample size estimates
#' @export
#' 
#' @importFrom tibble as_tibble
get_ss_estimates <- function(lambdaC, eta, minfup, T_max, sided, ratio, hr, alpha, beta, interim, timing, sfu) {
  
  if (interim =='no_interim'){  
    # Calculation of sample size (no interim):
    res <- mapply(gsDesign::nSurv, 
                  lambdaC  = lambdaC, 
                  eta      = eta,
                  minfup   = minfup,
                  T        = T_max,
                  sided    = sided,
                  ratio    = ratio,
                  hr       = hr, 
                  beta     = beta,
                  alpha    = alpha,
                  gamma    = 1,
                  SIMPLIFY = FALSE)
    # Removing potential null values: 
    resNA <- lapply(res, tidyr::replace_na)
    resNAFlat <- purrr::flatten(purrr::flatten(resNA))
     resNA[[1]] <- resNAFlat
     out <- tibble::as_tibble(resNA[[1]])
  } 
   else{
   # Calculation of sample size (with interim):
    res <- mapply(gsDesign::gsSurv,
                  lambdaC  = lambdaC,
                  eta      = eta,
                  minfup   = minfup,
                  T        = T_max,
                  sided    = sided,
                  ratio    = ratio,
                  hr       = hr,
                  beta     = beta,
                  alpha    = alpha,
                  gamma    = 1,
                  timing   = timing,
                  k        = 2,
                  test.type= 1,
                  sfu      = sfu,
                  SIMPLIFY = FALSE)

  #
  #   # Removing potential null values:
  resNA <- lapply(res, tidyr::replace_na)
  #
  #   # Adding the correct value names:
    resNA[[1]]$lower <- NULL
    resNA[[1]]$upper <- NULL
  #
    objNames <- character()
    idx = 1
    for (i in seq_along(resNA[[1]])){
      if (length(resNA[[1]][[i]])>1){
        for (j in seq_along(resNA[[1]][[i]])){
          objNames[idx] <- paste(names(resNA[[1]][i]), j, sep="")
          idx <- idx+1
        }
      }
      else{
        objNames[idx] <- names(resNA[[1]][i])
        idx <- idx+1
      }
    }

    resNAFlat <- purrr::flatten(purrr::flatten(resNA))
    resNA[[1]] <- resNAFlat
    names(resNA[[1]]) <- objNames


 # Converting the list containing sample size results to a tibble:

   #out <- data.table::rbindlist(resNA, fill=TRUE)
   out <- tibble::as_tibble(resNA[[1]])
   }
   
  # Enrich
  out <- out %>%
    dplyr::mutate(pct_reduction = 100*(1 - hr),
           z_alpha       = stats::qnorm(1 - alpha/sided),
           z_beta        = stats::qnorm(1 - beta),
           # nEvents_freedman = ((1 + hr)/(1 - hr))^2*(z_alpha + z_beta)^2,
           nEvents_schoenfeld = 4/log(hr)^2*(.data$z_alpha + .data$z_beta)^2
    )
  
  return(out)
}

# To avoid NOTEs the R CMD check
#' @importFrom utils globalVariables
utils::globalVariables(".data")


