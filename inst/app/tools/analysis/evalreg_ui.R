ereg_train <- list("All" = "All", "Training" = "Training", "Validation" = "Validation", "Both" = "Both")

## list of function arguments
ereg_args <- as.list(formals(evalreg))

ereg_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  ereg_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ereg_args$dataset <- input$dataset
  for (i in r_drop(names(ereg_args)))
    ereg_args[[i]] <- input[[paste0("ereg_", i)]]
  ereg_args
})

###############################################################
# Evaluate model evalreg
###############################################################
output$ui_ereg_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    isNum <- .get_class() %in% c("integer", "numeric")
    vars <- varnames()[isNum]
  })
  selectInput(
    inputId = "ereg_rvar", label = "Response variable:", choices = vars,
    selected = state_single("ereg_rvar", vars), multiple = FALSE
  )
})

output$ui_ereg_pred <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric")
  vars <- varnames()[isNum]

  req(available(input$ereg_rvar))
  ## don't use setdiff, removes names
  if (length(vars) > 0 && input$ereg_rvar %in% vars) {
    vars <- vars[-which(vars == input$ereg_rvar)]
  }

  selectInput(
    inputId = "ereg_pred", label = "Select stored predictions:", choices = vars,
    selected = state_multiple("ereg_pred", vars, isolate(input$ereg_pred)),
    multiple = TRUE, size = min(4, length(vars)), selectize = FALSE
  )
})

output$ui_ereg_train <- renderUI({
  selectInput(
    "ereg_train", label = "Show results for:", ereg_train,
    selected = state_single("ereg_train", "All")
  )
})

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  sapply(r_drop(names(ereg_args)), function(x) input[[paste0("ereg_", x)]])

  ## notify user when the regression needs to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$ereg_run) && !is.null(input$ereg_pred)) {
    if (isTRUE(attr(ereg_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "ereg_run", "Re-evaluate models", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "ereg_run", "Evaluate models", icon = icon("play"))
    }
  }
})

output$ui_evalreg <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ereg_run", "Evaluate models", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      uiOutput("ui_ereg_rvar"),
      uiOutput("ui_ereg_pred"),
      uiOutput("ui_ereg_train"),
      checkboxInput("ereg_show_plots", "Show plots", state_init("ereg_show_plots", FALSE))
    ),
    help_and_report(
      modal_title = "Evaluate regressions",
      fun_name = "evalreg",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/evalreg.md"))
    )
  )
})

ereg_plot_width <- function() 650
ereg_plot_height <- function() 650

## output is called from the main radiant ui.R
output$evalreg <- renderUI({
  register_print_output("summary_evalreg", ".summary_evalreg")
  register_plot_output(
    "plot_evalreg", ".plot_evalreg",
    width_fun = "ereg_plot_width",
    height_fun = "ereg_plot_height"
  )

  ## one output with components stacked
  ereg_output_panels <- tagList(
    download_link("dl_ereg_tab"), br(),
    verbatimTextOutput("summary_evalreg"),
    conditionalPanel(
      condition = "input.ereg_show_plots == true",
      download_link("dlp_evalreg"),
      plotOutput("plot_evalreg", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Evaluate",
    tool = "Evaluate Regression",
    tool_ui = "ui_evalreg",
    output_panels = ereg_output_panels
  )
})

.evalreg <- eventReactive(input$ereg_run, {
  do.call(evalreg, ereg_inputs())
})

.summary_evalreg <- reactive({
  if (not_pressed(input$ereg_run)) return("** Press the Evaluate button to evaluate models **")
  if (not_available(input$ereg_rvar) || not_available(input$ereg_pred)) {
    return("This analysis requires a numeric response variable and one or more\nnumeric predictors. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("diamonds"))
  }
  summary(.evalreg())
})

.plot_evalreg <- eventReactive(input$ereg_run, {
  req(input$ereg_train)
  plot(.evalreg())
})

observeEvent(input$evalreg_report, {
  if (is_empty(input$ereg_pred)) return(invisible())

  inp_out <- list("", "")
  outputs <- "summary"
  figs <- FALSE
  if (isTRUE(input$ereg_show_plots)) {
    inp_out[[2]] <- list(custom = FALSE)
    outputs <- c("summary", "plot")
    figs <- TRUE
  }

  update_report(
    inp_main = clean_args(ereg_inputs(), ereg_args),
    fun_name = "evalreg",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = ereg_plot_width(),
    fig.height = ereg_plot_height()
  )
})

dl_ereg_tab <- function(path) {
  .evalreg() %>%
    {if (!is_empty(.$dat)) write.csv(.$dat, file = path, row.names = FALSE)}
}

download_handler(
  id = "dl_ereg_tab", 
  fun = dl_ereg_tab, 
  fn = paste0(input$dataset, "_evalreg.csv"),
  caption = "Download model evaluations"
)

download_handler(
  id = "dlp_evalreg", 
  fun = download_handler_plot, 
  fn = paste0(input$dataset, "_evalreg.png"),
  caption = "Download model evaluation plot",
  plot = .plot_evalreg,
  width = ereg_plot_width,
  height = ereg_plot_height
)
