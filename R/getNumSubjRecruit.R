#' Get number of subjects to be recruited, when considering screening failure risk and 
#' dropout risk.  
#'
#' @param scr_fail_risk       Screening failure risk 
#' @param dropout_risk_trt    Dropout risk in treatment group
#' @param dropout_risk_ctrl   Dropout risk in control group
#' @param cal_sample_size     Calculated sample size
#' @return Returns the number of subjects to be recruited to obtain the calculated sample size
#' @export
#'
#' @examples
#' list

# Core function for computing the number of subjects to be recruited to obtain the calculated sample size
getNumSubjRecruit <- function(scr_fail_risk, dropout_risk_trt, dropout_risk_ctrl, cal_sample_size){
  
  cal_sample_size <- ceiling(cal_sample_size)
  
  # Calculating the number of subjects to be recruited in each group:
  tempTrt <- ceiling(cal_sample_size * (1+dropout_risk_trt))
  tempCtrl <- ceiling(cal_sample_size * (1+dropout_risk_ctrl))
  
  # Calculating the total subjects needed to be recruited:
  numSubjRecruit <- ceiling((tempTrt + tempCtrl)*(1+scr_fail_risk))
  
  res <- list('numSubjRecruit' = numSubjRecruit, 
              'scr_fail_risk' = scr_fail_risk,
              'dropout_risk_trt' = dropout_risk_trt,
              'dropout_risk_ctrl' = dropout_risk_ctrl,
              'cal_sample_size' = cal_sample_size)
  
  return(res)
}
