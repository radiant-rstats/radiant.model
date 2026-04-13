help_model <- c(
  "Linear regression (OLS)" = "regress.Rmd",
  "Logistic regression (GLM)" = "logistic.Rmd",
  "Multinomial logistic regression (MNL)" = "mnl.Rmd",
  "Naive Bayes" = "nb.md",
  "Neural Network" = "nn.md",
  "Classification and regression trees" = "crtree.md",
  "Random Forest" = "rf.md",
  "Gradient Boosted Trees" = "gbt.md",
  "Evaluate regression" = "evalreg.md",
  "Evaluate classification" = "evalbin.md",
  "Collaborative filtering" = "crs.md",
  "Decision analysis" = "dtree.Rmd",
  "Simulate" = "simulater.md"
)
output$help_model <- reactive(append_help("help_model", file.path(getOption("radiant.path.model"), "app/tools/help/"), Rmd = TRUE))
observeEvent(input$help_model_all, {
  help_switch(input$help_model_all, "help_model")
})
observeEvent(input$help_model_none, {
  help_switch(input$help_model_none, "help_model", help_on = FALSE)
})

help_model_panel <- tagList(
  wellPanel(
    HTML("<label>Model menu: <i id='help_model_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_model_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput(
      "help_model", NULL, help_model,
      selected = state_group("help_model"), inline = TRUE
    )
  )
)
