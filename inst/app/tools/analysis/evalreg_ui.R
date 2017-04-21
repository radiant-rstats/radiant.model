ereg_train <- list("All" = "All", "Training" = "Training", "Validation" = "Validation", "Both" = "Both")

## list of function arguments
ereg_args <- as.list(formals(evalreg))

ereg_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  ereg_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ereg_args$dataset <- input$dataset
  for (i in r_drop(names(ereg_args)))
    ereg_args[[i]] <- input[[paste0("ereg_",i)]]
  ereg_args
})

###############################################################
# Evaluate model evalreg
###############################################################
output$ui_ereg_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    isNum <- .getclass() %in% c("integer","numeric")
    vars <- varnames()[isNum]
  })
  selectInput(inputId = "ereg_rvar", label = "Response variable:", choices = vars,
    selected = state_single("ereg_rvar", vars), multiple = FALSE)
})

output$ui_ereg_pred <- renderUI({
  isNum <- .getclass() %in% c("integer","numeric")
  vars <- varnames()[isNum]

  req(available(input$ereg_rvar))
  ## don't use setdiff, removes names
  if (length(vars) > 0 && input$ereg_rvar %in% vars)
    vars <- vars[-which(vars == input$ereg_rvar)]

  selectInput(inputId = "ereg_pred", label = "Predictor:", choices = vars,
    selected = state_multiple("ereg_pred", vars),
    multiple = TRUE, size = min(4, length(vars)), selectize = FALSE)
})

output$ui_ereg_train <- renderUI({
  radioButtons("ereg_train", label = "Show results for:", ereg_train,
    selected = state_init("ereg_train", "All"),
    inline = TRUE)
})

output$ui_evalreg <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ereg_run", "Evaluate", width = "100%")
    ),
  	wellPanel(
	    uiOutput("ui_ereg_rvar"),
      uiOutput("ui_ereg_pred"),
      uiOutput("ui_ereg_train")
  	),
  	help_and_report(modal_title = "Evaluate regressions",
  	                fun_name = "evalreg",
  	                help_file = inclMD(file.path(getOption("radiant.path.model"),"app/tools/help/evalreg.md")))
	)
})

ereg_plot_width <- function() 650
ereg_plot_height <- function() 650

## output is called from the main radiant ui.R
output$evalreg <- renderUI({
	register_print_output("summary_evalreg", ".summary_evalreg")
	register_plot_output("plot_evalreg", ".plot_evalreg",
                       	width_fun = "ereg_plot_width",
                       	height_fun = "ereg_plot_height")

	## one output with components stacked
	ereg_output_panels <- tagList(
      downloadLink("dl_ereg_tab", "", class = "fa fa-download alignright"), br(),
      verbatimTextOutput("summary_evalreg"),
      plot_downloader("evalreg", height = ereg_plot_height()),
      plotOutput("plot_evalreg", height = "100%")
  )

	stat_tab_panel(menu = "Model > Evaluate",
	              tool = "Evaluate Regression",
	              tool_ui = "ui_evalreg",
	             	output_panels = ereg_output_panels)
})

.evalreg <- eventReactive(input$ereg_run, {
	do.call(evalreg, ereg_inputs())
})

.summary_evalreg <- reactive({
  if (not_available(input$ereg_rvar) || not_available(input$ereg_pred))
    return("This analysis requires a numeric response variable and one or more\nnumeric predictors. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("diamonds"))
  if (not_pressed(input$ereg_run)) return("** Press the Evaluate button to evaluate models **")
  summary(.evalreg())
})

.plot_evalreg <- reactive({
  if (not_pressed(input$ereg_run)) return(invisible())
  if (not_available(input$ereg_rvar) || not_available(input$ereg_pred)) return(" ")
  req(input$ereg_train)
  plot(.evalreg())
})

observeEvent(input$evalreg_report, {
  if (is_empty(input$ereg_pred)) return(invisible())

  outputs <- c("summary","plot")
  update_report(inp_main = clean_args(ereg_inputs(), ereg_args),
                fun_name = "evalreg",
                inp_out = list("",""),
                outputs = outputs,
                figs = TRUE,
                fig.width = ereg_plot_width(),
                fig.height = ereg_plot_height())
})

output$dl_ereg_tab <- downloadHandler(
  filename = function() { "evalreg.csv" },
  content = function(file) {
    do.call(summary, c(list(object = .evalreg()), ereg_inputs(),
            list(prn = FALSE))) %>%
      write.csv(., file = file, row.names = FALSE)
  }
)
