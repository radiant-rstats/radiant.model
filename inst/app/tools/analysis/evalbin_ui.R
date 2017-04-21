# ebin_method <- list("xtile" = "xtile", "ntile" = "ntile")
ebin_plots <- list("Lift" = "lift", "Gains" = "gains", "Profit" = "profit", "ROME" = "rome")
ebin_train <- list("All" = "All", "Training" = "Training", "Validation" = "Validation", "Both" = "Both")

# list of function arguments
ebin_args <- as.list(formals(evalbin))

ebin_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  ebin_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ebin_args$dataset <- input$dataset
  for (i in r_drop(names(ebin_args)))
    ebin_args[[i]] <- input[[paste0("ebin_",i)]]
  ebin_args
})

###############################################################
# Evaluate model evalbin
###############################################################
output$ui_ebin_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    vars <- two_level_vars()
  })
  selectInput(inputId = "ebin_rvar", label = "Response variable:", choices = vars,
    selected = state_single("ebin_rvar", vars), multiple = FALSE)
})

output$ui_ebin_lev <- renderUI({
  if (is_empty(input$ebin_rvar)) return()
  if (available(input$ebin_rvar))
    levs <- .getdata()[[input$ebin_rvar]] %>% as.factor %>% levels
  else
    levs <- c()
  selectInput(inputId = "ebin_lev", label = "Choose level:",
              choices = levs,
              selected = state_init("ebin_lev"))
})

output$ui_ebin_pred <- renderUI({
  isNum <- .getclass() %in% c("integer","numeric")
  vars <- varnames()[isNum]
  selectInput(inputId = "ebin_pred", label = "Predictor:", choices = vars,
    selected = state_multiple("ebin_pred", vars),
    multiple = TRUE, size = min(4, length(vars)), selectize = FALSE)
})

output$ui_ebin_train <- renderUI({
  # if (is.null(input$show_filter) || input$show_filter == "FALSE" ||
  #     is_empty(input$data_filter)) {
  #   ebin_train <- ebin_train[1]
  #   r_state$ebin_train <<- ebin_train
  # }

  radioButtons("ebin_train", label = "Show results for:", ebin_train,
    selected = state_init("ebin_train", "All"),
    inline = TRUE)
})

output$ui_evalbin <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ebin_run", "Evaluate", width = "100%")
    ),
  	wellPanel(
	    uiOutput("ui_ebin_rvar"),
      uiOutput("ui_ebin_lev"),
      uiOutput("ui_ebin_pred"),
      conditionalPanel("input.tabs_evalbin != 'Confusion'",
        numericInput("ebin_qnt", label = "# quantiles:",
          value = state_init("ebin_qnt", 10), min = 2)
      ),
      tags$table(
        tags$td(numericInput("ebin_cost", label = "Cost:",
          value = state_init("ebin_cost",1))),
        tags$td(numericInput("ebin_margin", label = "Margin:",
          value = state_init("ebin_margin",2), width = "117px"))
      ),
      uiOutput("ui_ebin_train"),
      conditionalPanel("input.tabs_evalbin == 'Plot'",
        checkboxGroupInput("ebin_plots", "Plots:", ebin_plots,
          selected = state_group("ebin_plots", ""),
          inline = TRUE)
      ),
      conditionalPanel("input.tabs_evalbin == 'Confusion'",
        checkboxInput("ebin_scale_y", "Scale free Y-axis", state_init("ebin_scale_y", TRUE))
      )
  	),
    conditionalPanel("input.tabs_evalbin != 'Confusion'",
      help_and_report(modal_title = "Evaluate classification",
                      fun_name = "evalbin",
                      help_file = inclMD(file.path(getOption("radiant.path.model"),"app/tools/help/evalbin.md")))
    ),
    conditionalPanel("input.tabs_evalbin == 'Confusion'",
      help_and_report(modal_title = "Model evaluate confusion matrix",
                      fun_name = "confusion",
                      help_file = inclMD(file.path(getOption("radiant.path.model"),"app/tools/help/evalbin.md")))
    )
	)
})

ebin_plot_width <- function() {
  # if (length(input$ebin_pred) > 1 || (!is.null(input$ebin_train) && input$ebin_train == "Both")) 700 else 500
  700
}
ebin_plot_height <- function() {
  if (is_empty(input$ebin_plots)) 200 else length(input$ebin_plots) * 500
}

confusion_plot_width <- function() 650
confusion_plot_height <- function() 800

# output is called from the main radiant ui.R
output$evalbin <- renderUI({
	register_print_output("summary_evalbin", ".summary_evalbin")
	register_plot_output("plot_evalbin", ".plot_evalbin",
                       	width_fun = "ebin_plot_width",
                       	height_fun = "ebin_plot_height")
  register_print_output("summary_confusion", ".summary_confusion")
  register_plot_output("plot_confusion", ".plot_confusion",
                        width_fun = "confusion_plot_width",
                        height_fun = "confusion_plot_height")
  register_print_output("summary_performance", ".summary_performance")
  # register_plot_output("plot_performance", ".plot_performance",
  #                       width_fun = "confusion_plot_width",
  #                       height_fun = "confusion_plot_height")


	# one output with components stacked
	# ebin_output_panels <- tagList(
  ebin_output_panels <- tabsetPanel(
    id = "tabs_evalbin",
    tabPanel("Summary",
      downloadLink("dl_ebin_tab", "", class = "fa fa-download alignright"), br(),
      verbatimTextOutput("summary_evalbin")
    ),
    tabPanel("Plot",
      plot_downloader("evalbin", height = ebin_plot_height()),
      plotOutput("plot_evalbin", height = "100%")
    ),
    # tabPanel("Performance",
    #    downloadLink("dl_performance_tab", "", class = "fa fa-download alignright"), br(),
    #    verbatimTextOutput("summary_performance"),
    #    plot_downloader("performance", height = ebin_plot_height()),
    #    plotOutput("plot_performance", height = "100%")
    # ),
    tabPanel("Confusion",
       downloadLink("dl_confusion_tab", "", class = "fa fa-download alignright"), br(),
       verbatimTextOutput("summary_confusion"),
       plot_downloader("confusion", height = ebin_plot_height()),
       plotOutput("plot_confusion", height = "100%")
    )
  )

	stat_tab_panel(menu = "Model > Evaluate",
	              tool = "Evaluate classification",
	              tool_ui = "ui_evalbin",
	             	output_panels = ebin_output_panels)
})

.evalbin <- eventReactive(input$ebin_run, {
	do.call(evalbin, ebin_inputs())
})

.summary_evalbin <- reactive({
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
      is_empty(input$ebin_lev))
    return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
  if (not_pressed(input$ebin_run)) return("** Press the Evaluate button to evaluate models **")
  summary(.evalbin())
})

.plot_evalbin <- reactive({
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
      is_empty(input$ebin_lev)) {
    return(" ")
  }
  if (not_pressed(input$ebin_run)) return("** Press the Evaluate button to evaluate models **")
  if (all(is_empty(input$ebin_plots))) return("** Select a plot to display **")
  plot(.evalbin(), plots = input$ebin_plots, shiny = TRUE)
})

.confusion <- reactive({
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
      is_empty(input$ebin_lev))
    return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
  if (not_pressed(input$ebin_run)) return("** Press the Evaluate button to evaluate models **")
  do.call(confusion, ebin_inputs())
})

.summary_confusion <- reactive({
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
      is_empty(input$ebin_lev))
    return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
  if (not_pressed(input$ebin_run)) return("** Press the Evaluate button to evaluate models **")
  summary(.confusion())
})

.plot_confusion <- reactive({
  if (not_pressed(input$ebin_run)) return(invisible())
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred)) return(" ")
  req(input$ebin_train, !is_not(input$ebin_scale_y))
  plot(.confusion(), scale_y = input$ebin_scale_y)
})

observeEvent(input$evalbin_report, {
  if (is_empty(input$ebin_rvar) || is_empty(input$ebin_pred)) return(invisible())
  
  if (length(input$ebin_plots) > 0) {
    inp_out <- list("", list(plots = input$ebin_plots, custom = FALSE))
    outputs <- c("summary","plot")
    figs <- TRUE
  } else {
    outputs <- c("summary")
    inp_out <- list("","")
    figs <- FALSE
  }
  update_report(inp_main = clean_args(ebin_inputs(), ebin_args),
                fun_name = "evalbin",
                inp_out = inp_out,
                outputs = outputs,
                figs = figs,
                fig.width = ebin_plot_width(),
                fig.height = ebin_plot_height())
})

observeEvent(input$confusion_report, {
  if (is_empty(input$ebin_rvar) || is_empty(input$ebin_pred)) return(invisible())

  inp_out <- list("","")
  if (!input$ebin_scale_y)
    inp_out[[2]] <- list(scale_y = input$ebin_scale_y)
  outputs <- c("summary","plot")
  update_report(inp_main = clean_args(ebin_inputs(), ebin_args),
                fun_name = "confusion",
                inp_out = inp_out,
                outputs = outputs,
                figs = TRUE,
                fig.width = confusion_plot_width(),
                fig.height = 1.5 * confusion_plot_height())
})


output$dl_ebin_tab <- downloadHandler(
  filename = function() { "evalbin.csv" },
  content = function(file) {
    eb <- .evalbin()
    if (!is_empty(eb$dat)) write.csv(eb$dat, file = file, row.names = FALSE)
  }
)

output$dl_confusion_tab <- downloadHandler(
  filename = function() { "confusion.csv" },
  content = function(file) {
    .confusion()$dat %>%
    write.csv(., file = file, row.names = FALSE)
  }
)
