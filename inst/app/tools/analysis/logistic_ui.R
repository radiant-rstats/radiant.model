# logit_link <- c("Logit" = "logit", "Probit" = "probit")
logit_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
logit_predict <- c("None" = "none", "Data" = "data","Command" = "cmd", "Data & Command" = "datacmd")
logit_check <- c("Standardize" = "standardize", "Center" = "center",
               "Stepwise" = "stepwise")
logit_sum_check <- c("VIF" = "vif", "Confidence intervals" = "confint",
                   "Odds" = "odds")
logit_plots <- c("None" = "", "Histograms" = "hist",
                 "Correlations" = "correlations", "Scatter" = "scatter",
                 "Model fit" = "fit", "Coefficient plot" = "coef")

## list of function arguments
logit_args <- as.list(formals(logistic))

## list of function inputs selected by user
logit_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  logit_args$data_filter <- if (input$show_filter) input$data_filter else ""
  logit_args$dataset <- input$dataset
  for (i in r_drop(names(logit_args)))
    logit_args[[i]] <- input[[paste0("logit_",i)]]

  # isolate(cat(paste0(names(logit_args), " ", logit_args, collapse = ", "), file = stderr(), "\n"))
  logit_args
})

logit_sum_args <- as.list(if (exists("summary.logistic")) formals(summary.logistic)
                        else formals(radiant.model:::summary.logistic))

## list of function inputs selected by user
logit_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(logit_sum_args))
    logit_sum_args[[i]] <- input[[paste0("logit_",i)]]
  logit_sum_args
})

logit_plot_args <- as.list(if (exists("plot.logistic")) formals(plot.logistic)
                         else formals(radiant.model:::plot.logistic))

## list of function inputs selected by user
logit_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(logit_plot_args))
    logit_plot_args[[i]] <- input[[paste0("logit_",i)]]

  # cat(paste0(names(logit_plot_args), " ", logit_plot_args, collapse = ", "), file = stderr(), "\n")
  logit_plot_args
})

logit_pred_args <- as.list(if (exists("predict.logistic")) formals(predict.logistic)
                         else formals(radiant.model:::predict.logistic))

# list of function inputs selected by user
logit_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(logit_pred_args))
    logit_pred_args[[i]] <- input[[paste0("logit_",i)]]

  logit_pred_args$pred_cmd <- logit_pred_args$pred_data <- ""
  if (input$logit_predict == "cmd") {
    logit_pred_args$pred_cmd <- gsub("\\s", "", input$logit_pred_cmd) %>% gsub("\"","\'",.)
  } else if (input$logit_predict == "data") {
    logit_pred_args$pred_data <- input$logit_pred_data
  } else if (input$logit_predict == "datacmd") {
    logit_pred_args$pred_cmd <- gsub("\\s", "", input$logit_pred_cmd) %>% gsub("\"","\'",.)
    logit_pred_args$pred_data <- input$logit_pred_data
  }
  logit_pred_args
})

# logit_pred_plot_args <- as.list(if (exists("plot.logistic.predict")) formals(plot.logistic.predict)
#                                 else formals(radiant.model:::plot.logistic.predict))

logit_pred_plot_args <- as.list(if (exists("plot.model.predict")) formals(plot.model.predict)
                                else formals(radiant.model:::plot.model.predict))


# list of function inputs selected by user
logit_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(logit_pred_plot_args))
    logit_pred_plot_args[[i]] <- input[[paste0("logit_",i)]]
  logit_pred_plot_args
})

output$ui_logit_rvar <- renderUI({
  # req(input$dataset)
 	vars <- two_level_vars()
  selectInput(inputId = "logit_rvar", label = "Response variable:", choices = vars,
  	selected = state_single("logit_rvar",vars), multiple = FALSE)
})

output$ui_logit_lev <- renderUI({
  req(input$logit_rvar)
  if (available(input$logit_rvar))
    levs <- .getdata()[[input$logit_rvar]] %>% as.factor %>% levels
  else
    levs <- c()
  selectInput(inputId = "logit_lev", label = "Choose level:",
    choices = levs, selected = state_init("logit_lev"))
})

output$ui_logit_evar <- renderUI({
  req(available(input$logit_rvar))
	notChar <- "character" != .getclass()
  vars <- varnames()[notChar]
  if (length(vars) > 0 && input$logit_rvar %in% vars)
    vars <- vars[-which(vars == input$logit_rvar)]

  selectInput(inputId = "logit_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("logit_evar", vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_logit_wts <- renderUI({
  req(available(input$logit_rvar), available(input$logit_evar))
  isNum <- .getclass() %in% c("numeric","integer")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$logit_evar)) {
    vars <- setdiff(vars, input$logit_evar)
    names(vars) <- varnames() %>% {.[match(vars, .)]} %>% names
  }
  vars <- c("None", vars)

  selectInput(inputId = "logit_wts", label = "Weights:", choices = vars,
    selected = state_single("logit_wts", vars),
    multiple = FALSE)
})

output$ui_logit_test_var <- renderUI({
  req(available(input$logit_evar))
 	vars <- input$logit_evar
	if (!is.null(input$logit_int)) vars <- c(vars,input$logit_int)

  selectizeInput(inputId = "logit_test_var", label = "Variables to test:",
    choices = vars,
    selected = state_multiple("logit_test_var", vars),
    multiple = TRUE,
    options = list(placeholder = "None", plugins = list("remove_button"))
  )
})

output$ui_logit_show_interactions <- renderUI({
  # req(input$logit_evar)
  choices <- logit_show_interactions[1:max(min(3,length(input$logit_evar)),1)]
  radioButtons(inputId = "logit_show_interactions", label = "Interactions:",
    choices = choices,
    selected = state_init("logit_show_interactions"),
    inline = TRUE)
})

output$ui_logit_int <- renderUI({
  if (isolate("logit_show_interactions" %in% names(input)) &&
      is_empty(input$logit_show_interactions)) {
    choices <- character(0)
  } else if (is_empty(input$logit_show_interactions)) {
    return()
  } else {
    vars <- input$logit_evar
    if (not_available(vars) || length(vars) < 2) return()
    ## list of interaction terms to list
    choices <- iterms(vars, input$logit_show_interactions)
  }

	selectInput("logit_int", label = NULL, choices = choices,
    selected = state_init("logit_int"),
  	multiple = TRUE, size = min(4,length(choices)), selectize = FALSE)
})

output$ui_logit_predict_plot <- renderUI({
  predict_plot_controls("logit")
})

output$ui_logistic <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("logit_run", "Estimate", width = "100%")
    ),
    conditionalPanel(condition = "input.tabs_logistic == 'Predict'",
      wellPanel(

        selectInput("logit_predict", label = "Prediction input:", logit_predict,
          selected = state_single("logit_predict", logit_predict, "none")),
        conditionalPanel("input.logit_predict == 'data' | input.logit_predict == 'datacmd'",
          selectizeInput(inputId = "logit_pred_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_init("logit_pred_data"), multiple = FALSE)
        ),
        conditionalPanel("input.logit_predict == 'cmd' | input.logit_predict == 'datacmd'",
          returnTextAreaInput("logit_pred_cmd", "Prediction command:",
            value = state_init("logit_pred_cmd",""))
        ),
        conditionalPanel(condition = "input.logit_predict != 'none'",
          checkboxInput("logit_pred_plot", "Plot predictions", state_init("logit_pred_plot", FALSE)),
          conditionalPanel("input.logit_pred_plot == true",
            uiOutput("ui_logit_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel("input.logit_predict == 'data' | input.logit_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("logit_store_pred_name", "Store predictions:", state_init("logit_store_pred_name","predict_logit"))),
            tags$td(actionButton("logit_store_pred", "Store"), style="padding-top:30px;")
          )
        )
        # conditionalPanel("input.logit_predict == 'cmd'",
        #   tags$table(
        #     tags$td(textInput("logit_store_pred_dat_name", "Store predicted dataset:", state_init("logit_store_pred_dat_name",paste0(input$dataset,"_pred")))),
        #     tags$td(actionButton("logit_store_pred", "Store"), style="padding-top:30px;")
        #   )
        # )
      )
    ),
    conditionalPanel(condition = "input.tabs_logistic == 'Plot'",
      wellPanel(
        selectInput("logit_plots", "Plots:", choices = logit_plots,
          selected = state_single("logit_plots", logit_plots)),
        conditionalPanel(condition = "input.logit_plots == 'coef'",
          checkboxInput("logit_intercept", "Include intercept", state_init("logit_intercept", FALSE)))
      )
    ),
    wellPanel(
      # checkboxInput("logit_pause", "Pause estimation", state_init("logit_pause", FALSE)),
    	# radioButtons(inputId = "logit_link", label = NULL, logit_link,
    	# 	selected = state_init("logit_link","logit"), inline = TRUE),
	    uiOutput("ui_logit_rvar"),
      uiOutput("ui_logit_lev"),
	    uiOutput("ui_logit_evar"),
      uiOutput("ui_logit_wts"),
      conditionalPanel(condition = "input.logit_evar != null",

  			uiOutput("ui_logit_show_interactions"),
  		  conditionalPanel(condition = "input.logit_show_interactions != ''",
  				uiOutput("ui_logit_int")
  			),
        conditionalPanel(condition = "input.tabs_logistic == 'Summary'",
  		    uiOutput("ui_logit_test_var"),
          checkboxGroupInput("logit_check", NULL, logit_check,
            selected = state_group("logit_check"), inline = TRUE),
          checkboxGroupInput("logit_sum_check", NULL, logit_sum_check,
            selected = state_group("logit_sum_check", ""), inline = TRUE)
  			),
        ## Using && to check that input.logit_sum_check is not null (must be &&)
  	    conditionalPanel(condition = "(input.logit_sum_check && (input.logit_sum_check.indexOf('odds') >= 0 |
                         input.logit_sum_check.indexOf('confint') >= 0)) |
  	                     input.logit_plots == 'coef' |
                         input.tabs_logistic == 'Predict'",
   					 sliderInput("logit_conf_lev", "Confidence level:", min = 0.80,
   					             max = 0.99, value = state_init("logit_conf_lev",.95),
   					             step = 0.01)
  		  ),
        conditionalPanel(condition = "input.tabs_logistic == 'Summary'",
          tags$table(
            tags$td(textInput("logit_store_res_name", "Store residuals:", state_init("logit_store_res_name","residuals_logit"))),
            tags$td(actionButton("logit_store_res", "Store"), style="padding-top:30px;")
          )
        )
      )
	  ),
  	help_and_report(modal_title = "Logist regression (GLM)", fun_name = "logistic",
  	                help_file = inclRmd(file.path(getOption("radiant.path.model"),"app/tools/help/logistic.Rmd")))
	)
})

logit_plot <- reactive({

  if (logit_available() != "available") return()
  if (is_empty(input$logit_plots)) return()

  plot_height <- 500
  plot_width <- 650
  nrVars <- length(input$logit_evar) + 1

  if (input$logit_plots == 'hist') plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
  if (input$logit_plots == 'fit') plot_width <- 1.5 * plot_width
  if (input$logit_plots == "correlations") { plot_height <- 150 * nrVars; plot_width <- 150 * nrVars }
  if (input$logit_plots == 'scatter') plot_height <- 300 * nrVars
  if (input$logit_plots == 'coef') plot_height <- 300 + 20 * length(.logistic()$model$coefficients)

  list(plot_width = plot_width, plot_height = plot_height)
})

logit_plot_width <- function()
  logit_plot() %>% { if (is.list(.)) .$plot_width else 650 }

logit_plot_height <- function()
  logit_plot() %>% { if (is.list(.)) .$plot_height else 500 }

logit_pred_plot_height <- function()
  if (input$logit_pred_plot) 500 else 0

## output is called from the main radiant ui.R
output$logistic <- renderUI({

		register_print_output("summary_logistic", ".summary_logistic")
    register_print_output("predict_logistic", ".predict_print_logistic")
    register_plot_output("predict_plot_logistic", ".predict_plot_logistic",
                          height_fun = "logit_pred_plot_height")
		register_plot_output("plot_logistic", ".plot_logistic",
                          height_fun = "logit_plot_height",
                          width_fun = "logit_plot_width")

		## two separate tabs
		logit_output_panels <- tabsetPanel(
	    id = "tabs_logistic",
	    tabPanel("Summary",
        downloadLink("dl_logit_coef", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("summary_logistic")
      ),
      tabPanel("Predict",
        conditionalPanel("input.logit_pred_plot == true",
          plot_downloader("logistic", height = logit_pred_plot_height(), po = "dlp_", pre = ".predict_plot_"),
          plotOutput("predict_plot_logistic", width = "100%", height = "100%")
        ),
        downloadLink("dl_logit_pred", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("predict_logistic")
      ),
	    tabPanel("Plot",
               plot_downloader("logistic", height = logit_plot_height()),
               plotOutput("plot_logistic", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Model > Estimate", tool = "Logistic regression (GLM)",
                   tool_ui = "ui_logistic", output_panels = logit_output_panels)
})

logit_available <- reactive({
  if (not_available(input$logit_rvar))
    return("This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))

  if (not_available(input$logit_evar))
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("titanic"))

  "available"
})

.logistic <- eventReactive(input$logit_run, {

  req(available(input$logit_rvar), available(input$logit_evar))
  req(input$logit_lev)
  req(input$logit_wts == "None" || available(input$logit_wts))

  withProgress(message = 'Estimating model', value = 1,
    do.call(logistic, logit_inputs())
  )
})

.summary_logistic <- reactive({
  if (logit_available() != "available") return(logit_available())
  if (not_pressed(input$logit_run)) return("** Press the Estimate button to estimate the model **")
  do.call(summary, c(list(object = .logistic()), logit_sum_inputs()))
})

.plot_logistic <- reactive({
  if (logit_available() != "available")
    return(logit_available())
  if (is_empty(input$logit_plots))
    return("Please select a logistic regression plot from the drop-down menu")
  if (not_pressed(input$logit_run))
    return("** Press the Estimate button to estimate the model **")

  pinp <- logit_plot_inputs()
  pinp$shiny <- TRUE

  if (input$logit_plots == "correlations") {
    capture_plot(do.call(plot, c(list(x = .logistic()), pinp)))
  } else {
    do.call(plot, c(list(x = .logistic()), pinp))
  }
})

.predict_logistic <- reactive({
  if (logit_available() != "available") return(logit_available())
  if (not_pressed(input$logit_run)) return("** Press the Estimate button to estimate the model **")
  if (is_empty(input$logit_predict, "none")) return("** Select prediction input **")
  if((input$logit_predict == "data" || input$logit_predict == "datacmd") && is_empty(input$logit_pred_data))
    return("** Select data for prediction **")
  if(input$logit_predict == "cmd" && is_empty(input$logit_pred_cmd))
    return("** Enter prediction commands **")

  withProgress(message = "Generating predictions", value = 1, {
    do.call(predict, c(list(object = .logistic()), logit_pred_inputs()))
  })
})

.predict_print_logistic <- reactive({
  .predict_logistic() %>% {if (is.character(.)) cat(.,"\n") else print(.)}
})

.predict_plot_logistic <- reactive({
  if (logit_available() != "available") return(logit_available())
  req(input$logit_pred_plot, available(input$logit_xvar))
  if (not_pressed(input$logit_run)) return(invisible())
  if (is_empty(input$logit_predict, "none")) return(invisible())
  if((input$logit_predict == "data" || input$logit_predict == "datacmd") && is_empty(input$logit_pred_data))
    return(invisible())
  if(input$logit_predict == "cmd" && is_empty(input$logit_pred_cmd))
    return(invisible())
  do.call(plot, c(list(x = .predict_logistic()), logit_pred_plot_inputs()))
})

observeEvent(input$logistic_report, {
  outputs <- c("summary")
  inp_out <- list("","")
  inp_out[[1]] <- clean_args(logit_sum_inputs(), logit_sum_args[-1])
  figs <- FALSE
  if (!is_empty(input$logit_plots)) {
    inp_out[[2]] <- clean_args(logit_plot_inputs(), logit_plot_args[-1])
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }
  xcmd <- ""
  if (!is_empty(input$logit_predict, "none") &&
      (!is_empty(input$logit_pred_data) || !is_empty(input$logit_pred_cmd))) {

    pred_args <- clean_args(logit_pred_inputs(), logit_pred_args[-1])
    inp_out[[2 + figs]] <- pred_args

    outputs <- c(outputs,"pred <- predict")
    dataset <- if (input$logit_predict %in% c("data","datacmd")) input$logit_pred_data else input$dataset
    xcmd <-
      paste0("print(pred, n = 10)\nstore(pred, data = '", dataset, "', name = '", input$logit_store_pred_name,"')\n") %>%
      paste0("# write.csv(pred, file = '~/logit_predictions.csv', row.names = FALSE)")

    if (input$logit_predict == "cmd") xcmd <- ""

    if (input$logit_pred_plot && !is_empty(input$logit_xvar)) {
      inp_out[[3 + figs]] <- clean_args(logit_pred_plot_inputs(), logit_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  update_report(inp_main = clean_args(logit_inputs(), logit_args),
                fun_name = "logistic",
                inp_out = inp_out,
                outputs = outputs,
                figs = figs,
                fig.width = logit_plot_width(),
                fig.height = logit_plot_height(),
                xcmd = xcmd)
})

observeEvent(input$logit_store_res, {
  req(pressed(input$logit_run))
  robj <- .logistic()
  if (!is.list(robj)) return()
  withProgress(message = "Storing residuals", value = 1,
    store(robj, name = input$logit_store_res_name)
  )
})

observeEvent(input$logit_store_pred, {
  req(!is_empty(input$logit_pred_data), pressed(input$logit_run))
  pred <- .predict_logistic()
  if (is.null(pred)) return()
  withProgress(message = "Storing predictions", value = 1,
    store(pred, data = input$logit_pred_data, name = input$logit_store_pred_name)
  )
})

output$dl_logit_coef <- downloadHandler(
  filename = function() { "logit_coefficients.csv" },
  content = function(file) {
    if (pressed(input$logit_run)) {
      ret <- .logistic()[["coeff"]][-1,]
      if ("standardize" %in% input$logit_check) {
        ret$importance <- pmax(ret$OR, 1/ret$OR)
        cat("Standardized coefficients selected\n\n", file = file)
        sshhr(write.table(ret, sep = ",", append = TRUE, file = file, row.names = FALSE))
      } else {
        cat("Standardized coefficients not selected\n\n", file = file)
        sshhr(write.table(ret, sep = ",", append = TRUE, file = file, row.names = FALSE))
      }
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)

output$dl_logit_pred <- downloadHandler(
  filename = function() { "logit_predictions.csv" },
  content = function(file) {
    if (pressed(input$logit_run)) {
      .predict_logistic() %>%
        write.csv(file = file, row.names = FALSE)
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)
