ereg_train <- list("All" = "All", "Training" = "Training", "Test" = "Test", "Both" = "Both")

## list of function arguments
ereg_args <- as.list(formals(evalreg))

ereg_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  ereg_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ereg_args$rows <- if (input$show_filter) input$data_rows else ""
  ereg_args$dataset <- input$dataset
  for (i in r_drop(names(ereg_args))) {
    ereg_args[[i]] <- input[[paste0("ereg_", i)]]
  }
  ereg_args
})

###############################################################
# Evaluate model evalreg
###############################################################
output$ui_ereg_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    isNum <- .get_class() %in% c("integer", "numeric", "ts")
    vars <- varnames()[isNum]
  })
  selectInput(
    inputId = "ereg_rvar", label = "Response variable:", choices = vars,
    selected = state_single("ereg_rvar", vars), multiple = FALSE
  )
})

output$ui_ereg_pred <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
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
    "ereg_train",
    label = "Show results for:", ereg_train,
    selected = state_single("ereg_train", ereg_train, "All")
  )
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(ereg_args, "ereg", init = "pred", label = "Evaluate models", relabel = "Re-evaluate models")

output$ui_evalreg <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ereg_run", "Evaluate models", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
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
  eri <- ereg_inputs()
  eri$envir <- r_data
  do.call(evalreg, eri)
})

.summary_evalreg <- reactive({
  if (not_pressed(input$ereg_run)) {
    return("** Press the Evaluate button to evaluate models **")
  }
  if (not_available(input$ereg_rvar) || not_available(input$ereg_pred)) {
    return("This analysis requires a numeric response variable and one or more\nnumeric predictors. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("diamonds"))
  }
  summary(.evalreg())
})

.plot_evalreg <- eventReactive(input$ereg_run, {
  req(input$ereg_train)
  plot(.evalreg())
})

evalreg_report <- function() {
  if (is.empty(input$ereg_pred)) {
    return(invisible())
  }

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
}

dl_ereg_tab <- function(path) {
  .evalreg() %>%
    (function(x) if (!is.empty(x$dat)) write.csv(x$dat, file = path, row.names = FALSE))
}

download_handler(
  id = "dl_ereg_tab",
  fun = dl_ereg_tab,
  fn = function() paste0(input$dataset, "_evalreg"),
  type = "csv",
  caption = "Save model evaluations"
)

download_handler(
  id = "dlp_evalreg",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_evalreg"),
  type = "png",
  caption = "Save model evaluation plot",
  plot = .plot_evalreg,
  width = ereg_plot_width,
  height = ereg_plot_height
)

observeEvent(input$evalreg_report, {
  r_info[["latest_screenshot"]] <- NULL
  evalreg_report()
})

observeEvent(input$evalreg_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_evalreg_screenshot")
})

observeEvent(input$modal_evalreg_screenshot, {
  evalreg_report()
  removeModal() ## remove shiny modal after save
})