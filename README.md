# Introduction 
The NNSampleSize shiny application is a "proof-of-concept" work in progress to try to simplify the process for determining sample size in clinical trials. All features may not work in other environments, or have not been implemented, yet.

# Getting Started
The application is designed as an R package where the shiny app is included in the 'inst' subfolder. The steps to run the app is 

1.	Clone the repository
2.	In Rstudio, build tab, click "Install and Restart"
3.	In the R prompt, write 
```
    library("NNSampleSize")
    runSampleSizereview()
```
4. Pick a trial type (e.g. Outcomes trial) and expand the window so you can see the lefthand pane

5. Click the 'outcomes trial' button in the pane to show the results window

6. Expand Statistical assumptions and adjust parameters (and similar with Trial design parameters)
