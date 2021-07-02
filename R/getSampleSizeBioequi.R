#' Get sample size estimate for bioequivalence trials
#'
#' Core function for computing number of events and subjects
#' 
#' @param  targetpower  The minimum power to be obtained
#' @param  theta0_AUC   Assumed AUC ratio
#' @param  theta0_CMAX  Assumed CMAX ratio
#' @param  CV_AUC       Coefficient of variations (percent) of the AUC ratio
#' @param  CV_CMAX      Coefficient of variations (percent) of the CMAX ratio
#' @param  studyDesign  Study design, eg. parallel and cross-over
#' 
#' @return Returns the calculated sample size estimates
#' @export
getSampleSizeBioequi <- function(targetpower, theta0_AUC, theta0_CMAX , CV_AUC, CV_CMAX, studyDesign) {
  out_BE <- PowerTOST::sampleN.2TOST(alpha = c(0.05, 0.05), 
                targetpower = targetpower, 
                theta0 = c(theta0_AUC, theta0_CMAX), 
                theta1 = c(0.8,0.8),
                theta2 = c(1.25, 1.25),
                CV = c(PowerTOST::se2CV(CV_AUC), PowerTOST::se2CV(CV_CMAX)),
                rho = 0, 
                design = studyDesign,
                print  = FALSE)
  
  varType <- c(" AUC", " CMAX")
  objNames <- character()
  idx = 1
  for (i in seq_along(out_BE)){
    if (length(out_BE[[i]])>1){
      for (j in seq_along(out_BE[[i]])){
        objNames[idx] <- paste(names(out_BE[i]), varType[j], sep="")
        idx <- idx+1
      }
    }
    else{
      objNames[idx] <- names(out_BE[i])
      idx <- idx+1
    }
  }
  
  out_BE <- purrr::flatten(out_BE)
  names(out_BE) <- objNames
  return(out_BE)
  
}