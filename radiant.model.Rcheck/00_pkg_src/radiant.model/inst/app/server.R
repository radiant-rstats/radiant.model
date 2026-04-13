if (isTRUE(getOption("radiant.from.package"))) {
  library(radiant.model)
}

shinyServer(function(input, output, session) {

  ## source shared functions
  source(file.path(getOption("radiant.path.data"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  source(file.path(getOption("radiant.path.data"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  source("help.R", encoding = getOption("radiant.encoding"), local = TRUE)
  source("radiant.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## help ui
  output$help_model_ui <- renderUI({
    sidebarLayout(
      sidebarPanel(
        help_data_panel,
        help_model_panel,
        uiOutput("help_text"),
        width = 3
      ),
      mainPanel(
        HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
        htmlOutput("help_data"),
        htmlOutput("help_model")
      )
    )
  })

  ## packages to use for example data
  options(radiant.example.data = c("radiant.data", "radiant.model"))

  ## source data & app tools from radiant.data
  for (file in list.files(
    c(
      file.path(getOption("radiant.path.data"), "app/tools/app"),
      file.path(getOption("radiant.path.data"), "app/tools/data")
    ),
    pattern = "\\.(r|R)$", full.names = TRUE
  ))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## 'sourcing' package functions in the server.R environment for development
  if (!isTRUE(getOption("radiant.from.package"))) {
    for (file in list.files("../../R", pattern = "\\.(r|R)$", full.names = TRUE)) {
      source(file, encoding = getOption("radiant.encoding"), local = TRUE)
    }
    cat("\nGetting radiant.model from source ...\n")
  }

  ## source analysis tools for model menu
  for (file in list.files(c("tools/analysis"), pattern = "\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})
