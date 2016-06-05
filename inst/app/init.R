## urls for menu
r_url_list <- getOption("radiant.url.list")

r_url_list[["Linear regression (OLS)"]] <-
  list("tabs_regress" = list("Summary" = "regress/linear/",
                             "Predict" = "regress/linear/predict/",
                             "Plot"    = "regress/linear/plot/"))
r_url_list[["Logistic regression (GLM)"]] <-
  list("tabs_logistic" = list("Summary" = "classify/logistic/",
                             "Predict" = "classify/logistic/predict/",
                             "Plot"    = "classify/logistic/plot/"))
r_url_list[["Neural Network (ANN)"]] <-
  list("tabs_ann" = list("Summary" = "model/ann/", "Plot" = "model/ann/plot/"))
r_url_list[["Collaborative Filtering"]] <-
  list("tabs_crs" = list("Summary" = "model/crs/", "Plot" = "model/crs/plot/"))
r_url_list[["Evaluate classification"]] <-
  list("tabs_evalbin" = list("Summary" = "model/evalbin/", "Plot" = "model/evalbin/plot/"))
options(radiant.url.list = r_url_list); rm(r_url_list)

## design menu
model_ui <-
	tagList(
    navbarMenu("Model",
      "Estimate",
      tabPanel("Linear regression (OLS)", uiOutput("regress")),
      tabPanel("Logistic regression (GLM)", uiOutput("logistic")),
      tabPanel("Neural Network (ANN)", uiOutput("ann")),
      "----", "Performance",
      tabPanel("Evaluate classification", uiOutput("evalbin")),
      "----", "Recommend",
      tabPanel("Collaborative Filtering", uiOutput("crs"))
    )
  )
