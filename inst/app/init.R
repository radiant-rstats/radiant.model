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
r_url_list[["Collaborative Filtering"]] <-
  list("tabs_crs" = list("Summary" = "model/crs/", "Plot" = "model/crs/plot/"))
r_url_list[["Regression"]] <-
  list("tabs_evalreg" = list("Summary" = "model/evalreg/", "Plot" = "model/evalreg/plot/"))
r_url_list[["Classification"]] <-
  list("tabs_evalbin" = list("Summary" = "model/evalbin/", "Plot" = "model/evalbin/plot/"))
r_url_list[["Decision tree"]] <-
  list("tabs_dtree" = list("Model" = "model/dtree/", "Plot"  = "model/dtree/plot/"))
r_url_list[["Simulate"]] <-
  list("tabs_simulate" = list("Simulate" = "model/simulate/", "Repeat" = "model/simulate/repeat/"))
options(radiant.url.list = r_url_list); rm(r_url_list)

## design menu
model_ui <-
	tagList(
    includeCSS(file.path(getOption("radiant.path.model"),"app/www/style.css")),
    navbarMenu("Model",
      "Estimate",
      tabPanel("Linear regression (OLS)", uiOutput("regress")),
      tabPanel("Logistic regression (GLM)", uiOutput("logistic")),
      tabPanel("Neural Network (ANN)", uiOutput("ann")),
      "----", "Recommend",
      tabPanel("Collaborative Filtering", uiOutput("crs")),
      "----", "Evaluate",
      tabPanel("Regression", uiOutput("evalreg")),
      tabPanel("Classification", uiOutput("evalbin")),
      "----", "Decide",
      tabPanel("Decision tree", uiOutput("dtree")),
      tabPanel("Simulate", uiOutput("simulater"))
    )
  )
