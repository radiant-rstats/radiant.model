## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["Linear regression (OLS)"]] <-
  list("tabs_regress" = list("Summary" = "model/regress/",
                             "Predict" = "model/regress/predict/",
                             "Plot"    = "model/regress/plot/"))
r_url_list[["Logistic regression (GLM)"]] <-
  list("tabs_logistic" = list("Summary" = "model/logistic/",
                              "Predict" = "model/logistic/predict/",
                              "Plot"    = "model/logistic/plot/"))
r_url_list[["Neural Network (ANN)"]] <-
  list("tabs_ann" = list("Summary" = "model/ann/", "Plot" = "model/ann/plot/"))
# r_url_list[["Collaborative Filtering"]] <-
  # list("tabs_crs" = list("Summary" = "model/crs/", "Plot" = "model/crs/plot/"))
r_url_list[["Evaluate regression"]] <-
  list("tabs_evalreg" = list("Summary" = "model/evalreg/"))
r_url_list[["Evaluate classification"]] <-
  list("tabs_evalbin" = list("Summary" = "model/evalbin/", "Plot" = "model/evalbin/plot/"))
r_url_list[["Decision analysis"]] <-
  list("tabs_dtree" = list("Model" = "model/dtree/", "Plot"  = "model/dtree/plot/",
                           "Sensitivity" = "model/dtree/sensitivity"))
r_url_list[["Simulate"]] <-
  list("tabs_simulate" = list("Simulate" = "model/simulate/", "Repeat" = "model/simulate/repeat/"))
options(radiant.url.list = r_url_list); rm(r_url_list)

## model menu
options(radiant.model_ui =
	tagList(
    navbarMenu("Model",
      "Estimate",
      tabPanel("Linear regression (OLS)", uiOutput("regress")),
      tabPanel("Logistic regression (GLM)", uiOutput("logistic")),
      tabPanel("Neural Network (ANN)", uiOutput("ann")),
      # "----", "Recommend",
      # tabPanel("Collaborative Filtering", uiOutput("crs")),
      "----", "Evaluate",
      tabPanel("Evaluate regression", uiOutput("evalreg")),
      tabPanel("Evaluate classification", uiOutput("evalbin")),
      "----", "Decide",
      tabPanel("Decision analysis", uiOutput("dtree")),
      tabPanel("Simulate", uiOutput("simulater")),
      includeCSS(file.path(getOption("radiant.path.model"),"app/www/style.css"))
    )
  )
)
