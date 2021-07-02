#---------------------------------#
######        Packages       ######
#---------------------------------#

library(NNSampleSize)
library("magrittr")
utils::globalVariables(c("nSurvival"))

#-------------------------------------#
######    Initiation variables  ######
#-------------------------------------#

if (identical(Sys.getenv("ON_DEVOPS"), "TRUE")) {
  NNSampleSize::runSampleSizereview(workspace = "shinyServer", username = NULL, admins = c("MLQJ", "NYHC", "CDN", "SFFL"),
                        verbose = TRUE)
} else {

  # only run when when not started via runSampleSizereview
  if (is.null(NNSampleSize::SampleSizeEnv("runApp"))) {
  
    NNSampleSize::SampleSizeEnv("online_user", character(0))
    NNSampleSize::SampleSizeEnv("trial", NULL)
    NNSampleSize::SampleSizeEnv("username", NULL)
    NNSampleSize::SampleSizeEnv("workspace", "server")
    NNSampleSize::SampleSizeEnv("verbose", TRUE)
    NNSampleSize::SampleSizeEnv("session_info", list())
    NNSampleSize::SampleSizeEnv("admins", "MLQJ")
    NNSampleSize::SampleSizeEnv("access_data_path", NNSampleSize::pDrive("general", "toolpool", "rug", "nntools",
                                       "NNSampleSize", "appData_test"))
    
    NNSampleSize::SampleSizeEnv("data_storage", file.path(NNSampleSize::SampleSizeEnv("access_data_path"), "trial_data"))
    NNSampleSize::SampleSizeEnv("workspace", "local")
    NNSampleSize::SampleSizeEnv("username", unname(Sys.info()["user"]))

  }
}


#------------------------------------------#
######   Initiations before app run   ######
#------------------------------------------#

# create directories
lapply(file.path(NNSampleSize::SampleSizeEnv("access_data_path"), c("trial_setup", "trial_data", "avatars")), dir.create,
           showWarnings = FALSE, recursive = TRUE)


