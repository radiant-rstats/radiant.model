################################################################
# Regression - UI
################################################################
reg_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
# reg_predict <- c("None" = "none", "Variable" = "vars", "Data" = "data","Command" = "cmd")
reg_predict <- c("None" = "none", "Data" = "data","Command" = "cmd", "Data & Command" = "datacmd")
reg_check <- c("Standardize" = "standardize", "Center" = "center",
               "Stepwise" = "stepwise")
reg_sum_check <- c("RMSE" = "rmse", "Sum of squares" = "sumsquares",
                   "VIF" = "vif", "Confidence intervals" = "confint")
reg_lines <- c("Line" = "line", "Loess" = "loess", "Jitter" = "jitter")
reg_plots <- c("None" = "", "Histograms" = "hist",
               "Correlations" = "correlations", "Scatter" = "scatter",
               "Dashboard" = "dashboard",
               "Residual vs explanatory" = "resid_pred",
               "Coefficient plot" = "coef")

reg_args <- as.list(formals(regress))

## list of function inputs selected by user
reg_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  reg_args$data_filter <- if (input$show_filter) input$data_filter else ""
  reg_args$dataset <- input$dataset
  for (i in r_drop(names(reg_args)))
    reg_args[[i]] <- input[[paste0("reg_",i)]]
  reg_args
})

reg_sum_args <- as.list(if (exists("summary.regress")) formals(summary.regress)
                        else formals(radiant.model:::summary.regress))

## list of function inputs selected by user
reg_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(reg_sum_args))
    reg_sum_args[[i]] <- input[[paste0("reg_",i)]]
  reg_sum_args
})

reg_plot_args <- as.list(if (exists("plot.regress")) formals(plot.regress)
                         else formals(radiant.model:::plot.regress))

## list of function inputs selected by user
reg_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(reg_plot_args))
    reg_plot_args[[i]] <- input[[paste0("reg_",i)]]
  reg_plot_args
})

reg_pred_args <- as.list(if (exists("predict.regress")) formals(predict.regress)
                         else formals(radiant.model:::predict.regress))

## list of function inputs selected by user
reg_pred_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(reg_pred_args))
    reg_pred_args[[i]] <- input[[paste0("reg_",i)]]

  reg_pred_args$pred_cmd <- reg_pred_args$pred_data <- ""
  if (input$reg_predict == "cmd") {
    reg_pred_args$pred_cmd <- gsub("\\s", "", input$reg_pred_cmd) %>% gsub("\"","\'",.)
  } else if (input$reg_predict == "data") {
    reg_pred_args$pred_data <- input$reg_pred_data
  } else if (input$reg_predict == "datacmd") {
    reg_pred_args$pred_cmd <- gsub("\\s", "", input$reg_pred_cmd) %>% gsub("\"","\'",.)
    reg_pred_args$pred_data <- input$reg_pred_data
  }
  reg_pred_args
})

# reg_pred_plot_args <- as.list(if (exists("plot.regress.predict")) formals(plot.regress.predict)
                         # else formals(radiant.model:::plot.regress.predict))

reg_pred_plot_args <- as.list(if (exists("plot.model.predict")) formals(plot.model.predict)
                         else formals(radiant.model:::plot.model.predict))


## list of function inputs selected by user
reg_pred_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(reg_pred_plot_args))
    reg_pred_plot_args[[i]] <- input[[paste0("reg_",i)]]
  reg_pred_plot_args
})

output$ui_reg_rvar <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- varnames()[isNum]
  selectInput(inputId = "reg_rvar", label = "Response variable:", choices = vars,
    selected = state_single("reg_rvar",vars), multiple = FALSE)
})

output$ui_reg_evar <- renderUI({
  req(available(input$reg_rvar))
  notChar <- "character" != .getclass()
  vars <- varnames()[notChar]

  ## don't use setdiff, removes names
  if (length(vars) > 0 && input$reg_rvar %in% vars)
    vars <- vars[-which(vars == input$reg_rvar)]

  selectInput(inputId = "reg_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("reg_evar", vars),
    multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

# adding interaction terms as needed
output$ui_reg_test_var <- renderUI({
  req(available(input$reg_evar))
  vars <- input$reg_evar
  if (!is.null(input$reg_int)) vars <- c(vars, input$reg_int)

  selectizeInput(inputId = "reg_test_var", label = "Variables to test:",
    choices = vars, selected = state_multiple("reg_test_var", vars),
    multiple = TRUE,
    options = list(placeholder = 'None', plugins = list('remove_button'))
  )
})

output$ui_reg_show_interactions <- renderUI({
  choices <- reg_show_interactions[1:max(min(3,length(input$reg_evar)),1)]
  radioButtons(inputId = "reg_show_interactions", label = "Interactions:",
    choices = choices, selected = state_init("reg_show_interactions"),
    inline = TRUE)
 })

output$ui_reg_int <- renderUI({
  if (isolate("reg_show_interactions" %in% names(input)) &&
      is_empty(input$reg_show_interactions)) {
    choices <- character(0)
  } else if (is_empty(input$reg_show_interactions)) {
    return()
  } else {
    vars <- input$reg_evar
    if (not_available(vars) || length(vars) < 2) return()
    ## list of interaction terms to show
    choices <- iterms(vars, input$reg_show_interactions)
  }

  selectInput("reg_int", label = NULL, choices = choices,
    selected = state_init("reg_int"),
    multiple = TRUE, size = min(4,length(choices)), selectize = FALSE)
})

output$ui_reg_predict_plot <- renderUI({
  predict_plot_controls("reg")
})

## data ui and tabs
output$ui_regress <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("reg_run", "Estimate", width = "100%")
    ),
    conditionalPanel(condition = "input.tabs_regress == 'Predict'",
      wellPanel(
        selectInput("reg_predict", label = "Prediction input:", reg_predict,
          selected = state_single("reg_predict", reg_predict, "none")),
        conditionalPanel("input.reg_predict == 'data' | input.reg_predict == 'datacmd'",
          selectizeInput(inputId = "reg_pred_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_single("reg_pred_data", c("None" = "",r_data$datasetlist)), multiple = FALSE)
        ),
        conditionalPanel("input.reg_predict == 'cmd' | input.reg_predict == 'datacmd'",
          returnTextAreaInput("reg_pred_cmd", "Prediction command:",
            value = state_init("reg_pred_cmd", ""))
        ),
        conditionalPanel(condition = "input.reg_predict != 'none'",
          checkboxInput("reg_pred_plot", "Plot predictions", state_init("reg_pred_plot", FALSE)),
          conditionalPanel("input.reg_pred_plot == true",
            uiOutput("ui_reg_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel("input.reg_predict == 'data' | input.reg_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("reg_store_pred_name", "Store predictions:", state_init("reg_store_pred_name","predict_reg"))),
            tags$td(actionButton("reg_store_pred", "Store"), style="padding-top:30px;")
          )
        )
      )
    ),
    conditionalPanel(condition = "input.tabs_regress == 'Plot'",
      wellPanel(
        selectInput("reg_plots", "Regression plots:", choices = reg_plots,
          selected = state_single("reg_plots", reg_plots)),
        conditionalPanel(condition = "input.reg_plots == 'coef'",
          checkboxInput("reg_intercept", "Include intercept", state_init("reg_intercept", FALSE))
        ),
        conditionalPanel(condition = "input.reg_plots == 'scatter' |
                                      input.reg_plots == 'dashboard' |
                                      input.reg_plots == 'resid_pred'",
          checkboxGroupInput("reg_lines", NULL, reg_lines,
            selected = state_group("reg_lines"), inline = TRUE)
        )
      )
    ),

    wellPanel(
      uiOutput("ui_reg_rvar"),
      uiOutput("ui_reg_evar"),

      conditionalPanel(condition = "input.reg_evar != null",

        uiOutput("ui_reg_show_interactions"),
        conditionalPanel(condition = "input.reg_show_interactions != ''",
          uiOutput("ui_reg_int")
        ),
        conditionalPanel(condition = "input.tabs_regress == 'Summary'",
          uiOutput("ui_reg_test_var"),
          checkboxGroupInput("reg_check", NULL, reg_check,
            selected = state_group("reg_check"), inline = TRUE),
          checkboxGroupInput("reg_sum_check", NULL, reg_sum_check,
            selected = state_group("reg_sum_check"), inline = TRUE)
        ),
        conditionalPanel(condition = "input.reg_predict == 'cmd' |
                         input.reg_predict == 'data' |
                         (input.reg_sum_check && input.reg_sum_check.indexOf('confint') >= 0) |
                         input.reg_plots == 'coef'",
             sliderInput("reg_conf_lev", "Confidence level:", min = 0.80,
                         max = 0.99, value = state_init("reg_conf_lev",.95),
                         step = 0.01)
        ),
        ## Only save residuals when filter is off
        conditionalPanel(condition = "input.tabs_regress == 'Summary'",
          tags$table(
            tags$td(textInput("reg_store_res_name", "Store residuals:", state_init("reg_store_res_name","residuals_reg"))),
            tags$td(actionButton("reg_store_res", "Store"), style="padding-top:30px;")
          )
        )
      )
    ),
    help_and_report(modal_title = "Linear regression (OLS)", fun_name = "regress",
                    help_file = inclRmd(file.path(getOption("radiant.path.model"),"app/tools/help/regress.Rmd")))
  )
})

reg_plot <- reactive({

  if (reg_available() != "available") return()
  if (is_empty(input$reg_plots)) return()

  # specifying plot heights
  plot_height <- 500
  plot_width <- 650
  nrVars <- length(input$reg_evar) + 1

  if (input$reg_plots == "hist") plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
  if (input$reg_plots == "dashboard") plot_height <- 1.5 * plot_height
  if (input$reg_plots == "correlations") { plot_height <- 150 * nrVars; plot_width <- 150 * nrVars }
  if (input$reg_plots == "coef") plot_height <- 300 + 20 * length(.regress()$model$coefficients)
  if (input$reg_plots %in% c("scatter","leverage","resid_pred"))
    plot_height <- (plot_height/2) * ceiling((nrVars-1) / 2)

  list(plot_width = plot_width, plot_height = plot_height)
})

reg_plot_width <- function()
  reg_plot() %>% { if (is.list(.)) .$plot_width else 650 }

reg_plot_height <- function()
  reg_plot() %>% { if (is.list(.)) .$plot_height else 500 }

reg_pred_plot_height <- function()
  if (input$reg_pred_plot) 500 else 0

  # if (input$tabs_regress == "Predict" && is.null(r_data$reg_pred)) 0 else 500

# output is called from the main radiant ui.R
output$regress <- renderUI({

    register_print_output("summary_regress", ".summary_regress")
    register_print_output("predict_regress", ".predict_print_regress")
    register_plot_output("predict_plot_regress", ".predict_plot_regress",
                          height_fun = "reg_pred_plot_height")
    register_plot_output("plot_regress", ".plot_regress",
                         height_fun = "reg_plot_height",
                         width_fun = "reg_plot_width")

    # two separate tabs
    reg_output_panels <- tabsetPanel(
      id = "tabs_regress",
      tabPanel("Summary",
        downloadLink("dl_reg_coef", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("summary_regress")),
      tabPanel("Predict",
        conditionalPanel("input.reg_pred_plot == true",
          plot_downloader("regress", height = reg_pred_plot_height(), po = "dlp_", pre = ".predict_plot_"),
          plotOutput("predict_plot_regress", width = "100%", height = "100%")
        ),
        downloadLink("dl_reg_pred", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("predict_regress")
      ),
      tabPanel("Plot", plot_downloader("regress", height = reg_plot_height()),
        plotOutput("plot_regress", width = "100%", height = "100%"))
    )

    stat_tab_panel(menu = "Model > Estimate",
                  tool = "Linear regression (OLS)",
                  tool_ui = "ui_regress",
                  output_panels = reg_output_panels)
})

reg_available <- reactive({

  if (not_available(input$reg_rvar))
    return("This analysis requires a response variable of type integer\nor numeric and one or more explanatory variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("diamonds"))

  if (not_available(input$reg_evar))
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("diamonds"))

  "available"
})

.regress <- eventReactive(input$reg_run, {
  req(available(input$reg_rvar), available(input$reg_evar))

  ## need dependency in reg_int so I can have names(input) in isolate
  # input$reg_int
  # isolate(req("reg_int" %in% names(input)))

  # req(input$reg_pause == FALSE, cancelOutput = TRUE)

  do.call(regress, reg_inputs())
})

.summary_regress <- reactive({
  if (reg_available() != "available") return(reg_available())
  # if (input$reg_rvar %in% input$reg_evar) return()
  if (not_pressed(input$reg_run)) return("** Press the Estimate button to estimate the model **")
  do.call(summary, c(list(object = .regress()), reg_sum_inputs()))
})

.predict_regress <- reactive({
  if (reg_available() != "available") return(reg_available())
  if (not_pressed(input$reg_run)) return("** Press the Estimate button to estimate the model **")
  if (is_empty(input$reg_predict, "none")) return("** Select prediction input **")
  # req(!is_empty(input$reg_predict, "none"))
     # (!is_empty(input$reg_pred_data) || !is_empty(input$reg_pred_cmd)))
  # if (is_empty(input$reg_predict, "none"))

  if((input$reg_predict == "data" || input$reg_predict == "datacmd") && is_empty(input$reg_pred_data))
    return("** Select data for prediction **")
  if(input$reg_predict == "cmd" && is_empty(input$reg_pred_cmd))
    return("** Enter prediction commands **")

  withProgress(message = "Generating predictions", value = 1, {
    do.call(predict, c(list(object = .regress()), reg_pred_inputs()))
  })
})

.predict_print_regress <- reactive({
  .predict_regress() %>% {if (is.character(.)) cat(.,"\n") else print(.)}
})

.predict_plot_regress <- reactive({
  if (reg_available() != "available") return(reg_available())
  # req(input$reg_pred_plot, input$reg_xvar, !is_empty(input$reg_predict, "none"), pressed(input$reg_run))
  req(input$reg_pred_plot, available(input$reg_xvar))
  if (not_pressed(input$reg_run)) return(invisible())
  if (is_empty(input$reg_predict, "none")) return(invisible())
  if((input$reg_predict == "data" || input$reg_predict == "datacmd") && is_empty(input$reg_pred_data))
    return(invisible())
  if(input$reg_predict == "cmd" && is_empty(input$reg_pred_cmd))
    return(invisible())
  do.call(plot, c(list(x = .predict_regress()), reg_pred_plot_inputs()))
})

.plot_regress <- reactive({
  if (reg_available() != "available") return(reg_available())
  if (is_empty(input$reg_plots)) return("Please select a regression plot from the drop-down menu")
  if (not_pressed(input$reg_run)) return("** Press the Estimate button to estimate the model **")

  if (input$reg_plots %in% c("correlations", "leverage"))
    capture_plot( do.call(plot, c(list(x = .regress()), reg_plot_inputs())) )
  else
    reg_plot_inputs() %>% { .$shiny <- TRUE; . } %>% { do.call(plot, c(list(x = .regress()), .)) }
})

observeEvent(input$regress_report, {
  outputs <- c("summary")
  inp_out <- list("","")
  inp_out[[1]] <- clean_args(reg_sum_inputs(), reg_sum_args[-1])
  figs <- FALSE
  if (!is_empty(input$reg_plots)) {
    inp_out[[2]] <- clean_args(reg_plot_inputs(), reg_plot_args[-1])
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }
  xcmd <- ""

  if (!is_empty(input$reg_predict, "none") &&
      (!is_empty(input$reg_pred_data) || !is_empty(input$reg_pred_cmd))) {
    pred_args <- clean_args(reg_pred_inputs(), reg_pred_args[-1])
    # pred_args[["prn"]] <- 10
    inp_out[[2 + figs]] <- pred_args
    outputs <- c(outputs, "pred <- predict")
    dataset <- if (input$reg_predict %in% c("data","datacmd")) input$reg_pred_data else input$dataset
    xcmd <-
      paste0("print(pred, n = 10)\nstore(pred, data = '", dataset, "', name = '", input$reg_store_pred_name,"')\n") %>%
      paste0("# write.csv(pred, file = '~/reg_predictions.csv', row.names = FALSE)")
    if (input$reg_pred_plot && !is_empty(input$reg_xvar)) {
      inp_out[[3 + figs]] <- clean_args(reg_pred_plot_inputs(), reg_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }
  update_report(inp_main = clean_args(reg_inputs(), reg_args),
                fun_name = "regress", inp_out = inp_out,
                outputs = outputs, figs = figs,
                fig.width = reg_plot_width(),
                fig.height = reg_plot_height(),
                xcmd = xcmd)
})

observeEvent(input$reg_store_res, {
  req(pressed(input$reg_run))
  robj <- .regress()
  if (!is.list(robj)) return()
  # if (length(robj$model$residuals) != nrow(getdata(input$dataset, filt = "", na.rm = FALSE))) {
  #   return(message("The number of residuals is not equal to the number of rows in the data. If the data has missing values these will need to be removed."))
  # }
  # store_reg(robj, data = input$dataset, type = "residuals", name = input$reg_store_res_name)
  withProgress(message = 'Storing residuals', value = 1,
    store(robj, name = input$reg_store_res_name)
  )
})

observeEvent(input$reg_store_pred, {
  req(!is_empty(input$reg_pred_data), pressed(input$reg_run))
  pred <- .predict_regress()
  if (is.null(pred)) return()
  # if (nrow(pred) != nrow(getdata(input$reg_pred_data, filt = "", na.rm = FALSE)))
  #   return(message("The number of predicted values is not equal to the number of rows in the data. If the data has missing values these will need to be removed."))
  # store_reg(pred, data = input$reg_pred_data, type = "prediction", name = input$reg_store_pred_name)
  withProgress(message = 'Storing predictions', value = 1,
    store(pred, data = input$reg_pred_data, name = input$reg_store_pred_name)
  )
})

output$dl_reg_coef <- downloadHandler(
  filename = function() { "reg_coefficients.csv" },
  content = function(file) {
    if (pressed(input$reg_run)) {
      ret <- .regress()[["coeff"]][-1,]
      if ("standardize" %in% input$reg_check) {
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

output$dl_reg_pred <- downloadHandler(
  filename = function() { "reg_predictions.csv" },
  content = function(file) {
    if (pressed(input$reg_run)) {
      .predict_regress() %>%
        write.csv(file = file, row.names = FALSE)
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)
