################################################################
# Regression - UI
################################################################
reg_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
reg_predict <- c(
  "None" = "none",
  "Data" = "data",
  "Command" = "cmd",
  "Data & Command" = "datacmd"
)
reg_check <- c(
  "Standardize" = "standardize", "Center" = "center",
  "Stepwise" = "stepwise-backward", "Robust" = "robust"
)
reg_sum_check <- c(
  "RMSE" = "rmse", "Sum of squares" = "sumsquares",
  "VIF" = "vif", "Confidence intervals" = "confint"
)
reg_lines <- c("Line" = "line", "Loess" = "loess", "Jitter" = "jitter")
reg_plots <- c(
  "None" = "none", "Distribution" = "dist",
  "Correlations" = "correlations", "Scatter" = "scatter",
  "Dashboard" = "dashboard",
  "Residual vs explanatory" = "resid_pred",
  "Coefficient plot" = "coef",
  "Influential observations" = "influence"
)

reg_args <- as.list(formals(regress))

## list of function inputs selected by user
reg_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  reg_args$data_filter <- if (input$show_filter) input$data_filter else ""
  reg_args$dataset <- input$dataset
  for (i in r_drop(names(reg_args)))
    reg_args[[i]] <- input[[paste0("reg_", i)]]
  reg_args
})

reg_sum_args <- as.list(if (exists("summary.regress")) {
  formals(summary.regress)
} else {
  formals(radiant.model:::summary.regress)
} )

## list of function inputs selected by user
reg_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(reg_sum_args))
    reg_sum_args[[i]] <- input[[paste0("reg_", i)]]
  reg_sum_args
})

reg_plot_args <- as.list(if (exists("plot.regress")) {
  formals(plot.regress)
} else {
  formals(radiant.model:::plot.regress)
} )

## list of function inputs selected by user
reg_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(reg_plot_args))
    reg_plot_args[[i]] <- input[[paste0("reg_", i)]]
  reg_plot_args
})

reg_pred_args <- as.list(if (exists("predict.regress")) {
  formals(predict.regress)
} else {
  formals(radiant.model:::predict.regress)
} )

## list of function inputs selected by user
reg_pred_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(reg_pred_args))
    reg_pred_args[[i]] <- input[[paste0("reg_", i)]]

  reg_pred_args$pred_cmd <- reg_pred_args$pred_data <- ""
  if (input$reg_predict == "cmd") {
    reg_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$reg_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$reg_predict == "data") {
    reg_pred_args$pred_data <- input$reg_pred_data
  } else if (input$reg_predict == "datacmd") {
    reg_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$reg_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    reg_pred_args$pred_data <- input$reg_pred_data
  }

  ## setting value for prediction interval type
  reg_pred_args$interval <- "confidence"

  reg_pred_args
})

reg_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
  formals(plot.model.predict)
} else {
  formals(radiant.model:::plot.model.predict)
} )

## list of function inputs selected by user
reg_pred_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(reg_pred_plot_args))
    reg_pred_plot_args[[i]] <- input[[paste0("reg_", i)]]
  reg_pred_plot_args
})

output$ui_reg_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    isNum <- .get_class() %in% c("integer", "numeric", "ts")
    vars <- varnames()[isNum]
  })
  selectInput(
    inputId = "reg_rvar", label = "Response variable:", choices = vars,
    selected = state_single("reg_rvar", vars), multiple = FALSE
  )
})

output$ui_reg_evar <- renderUI({
  req(available(input$reg_rvar))
  vars <- varnames()
  ## don't use setdiff, removes names
  if (length(vars) > 0 && input$reg_rvar %in% vars) {
    vars <- vars[-which(vars == input$reg_rvar)]
  }

  selectInput(
    inputId = "reg_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("reg_evar", vars, isolate(input$reg_evar)),
    multiple = TRUE, size = min(10, length(vars)), selectize = FALSE
  )
})

output$ui_reg_incl <- renderUI({
  req(available(input$reg_evar))
  vars <- input$reg_evar
  selectInput(
    inputId = "reg_incl", label = "Explanatory variables to include:", choices = vars,
    selected = state_multiple("reg_incl", vars, vars),
    multiple = TRUE, size = min(10, length(vars)), selectize = FALSE
  )
})

output$ui_reg_test_var <- renderUI({
  req(available(input$reg_evar))
  vars <- input$reg_evar
  if (!is.null(input$reg_int)) vars <- c(vars, input$reg_int)
  selectizeInput(
    inputId = "reg_test_var", label = "Variables to test:",
    choices = vars,
    selected = state_multiple("reg_test_var", vars, isolate(input$reg_test_var)),
    multiple = TRUE,
    options = list(placeholder = "None", plugins = list("remove_button"))
  )
})

## not clear why this is needed because state_multiple should handle this
observeEvent(is.null(input$reg_test_var), {
  if ("reg_test_var" %in% names(input)) r_state$reg_test_var <<- NULL
})

output$ui_reg_show_interactions <- renderUI({
  vars <- input$reg_evar
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  if (any(vars %in% varnames()[isNum])) {
    choices <- reg_show_interactions[1:3]
  } else {
    choices <- reg_show_interactions[1:max(min(3, length(input$reg_evar)), 1)]
  }
  radioButtons(
    inputId = "reg_show_interactions", label = "Interactions:",
    choices = choices, selected = state_init("reg_show_interactions"),
    inline = TRUE
  )
})

output$ui_reg_int <- renderUI({
  choices <- character(0)
  if (isolate("reg_show_interactions" %in% names(input)) &&
    is_empty(input$reg_show_interactions)) {
  } else if (is_empty(input$reg_show_interactions)) {
    return()
  } else {
    vars <- input$reg_evar
    if (not_available(vars)) {
      return()
    } else {
      ## quadratic and qubic terms
      isNum <- .get_class() %in% c("integer", "numeric", "ts")
      isNum <- intersect(vars, varnames()[isNum])
      if (length(isNum) > 0) {
        choices <- qterms(isNum, input$reg_show_interactions)
      }
      ## list of interaction terms to show
      if (length(vars) > 1) {
        choices <- c(choices, iterms(vars, input$reg_show_interactions))
      }
      if (length(choices) == 0) return()
    }
  }

  selectInput(
    "reg_int", label = NULL,
    choices = choices,
    selected = state_init("reg_int"),
    multiple = TRUE,
    size = min(8, length(choices)),
    selectize = FALSE
  )
})

## reset prediction and plot settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "reg_predict", selected = "none")
  updateSelectInput(session = session, inputId = "reg_plots", selected = "none")
})

output$ui_reg_predict_plot <- renderUI({
  predict_plot_controls("reg")
})

output$ui_reg_nrobs <- renderUI({
  nrobs <- nrow(.get_data())
  choices <- c("1,000" = 1000, "5,000" = 5000, "10,000" = 10000, "All" = -1) %>%
    .[. < nrobs]
  selectInput(
    "reg_nrobs", "Number of data points plotted:",
    choices = choices,
    selected = state_single("reg_nrobs", choices, 1000)
  )
})

output$ui_reg_store_res_name <- renderUI({
  req(input$dataset)
  textInput("reg_store_res_name", "Store residuals:", "", placeholder = "Provide variable name")
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(reg_args, "reg", tabs = "tabs_regress", label = "Estimate model", relabel = "Re-estimate model")

## data ui and tabs
output$ui_regress <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(condition = "input.tabs_regress == 'Summary'",
      wellPanel(
        actionButton("reg_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_regress == 'Summary'",
        uiOutput("ui_reg_rvar"),
        uiOutput("ui_reg_evar"),
        conditionalPanel(
          condition = "input.reg_evar != null",
          uiOutput("ui_reg_show_interactions"),
          conditionalPanel(
            condition = "input.reg_show_interactions != ''",
            uiOutput("ui_reg_int")
          ),
          uiOutput("ui_reg_test_var"),
          checkboxGroupInput(
            "reg_check", NULL, reg_check,
            selected = state_group("reg_check"), inline = TRUE
          ),
          checkboxGroupInput(
            "reg_sum_check", NULL, reg_sum_check,
            selected = state_group("reg_sum_check"), inline = TRUE
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_regress == 'Predict'",
        selectInput(
          "reg_predict", label = "Prediction input type:", reg_predict,
          selected = state_single("reg_predict", reg_predict, "none")
        ),
        conditionalPanel(
          "input.reg_predict == 'data' | input.reg_predict == 'datacmd'",
          selectizeInput(
            inputId = "reg_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("reg_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.reg_predict == 'cmd' | input.reg_predict == 'datacmd'",
          returnTextAreaInput(
            "reg_pred_cmd", "Prediction command:",
            value = state_init("reg_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., carat = 1; cut = 'Ideal') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.reg_predict != 'none'",
          checkboxInput("reg_pred_plot", "Plot predictions", state_init("reg_pred_plot", FALSE)),
          conditionalPanel(
            "input.reg_pred_plot == true",
            uiOutput("ui_reg_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.reg_predict == 'data' | input.reg_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("reg_store_pred_name", "Store predictions:", state_init("reg_store_pred_name", "pred_reg"))),
            tags$td(actionButton("reg_store_pred", "Store", icon = icon("plus")), style = "padding-top:30px;")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_regress == 'Plot'",
        selectInput(
          "reg_plots", "Plots:", choices = reg_plots,
          selected = state_single("reg_plots", reg_plots)
        ),
        conditionalPanel(
          condition = "input.reg_plots == 'coef'",
          uiOutput("ui_reg_incl"),
          checkboxInput("reg_intercept", "Include intercept", state_init("reg_intercept", FALSE))
        ),
        conditionalPanel(
          condition = "input.reg_plots == 'correlations' |
                       input.reg_plots == 'scatter' |
                       input.reg_plots == 'dashboard' |
                       input.reg_plots == 'resid_pred'",
          uiOutput("ui_reg_nrobs"),
          conditionalPanel(
            condition = "input.reg_plots != 'correlations'",
            checkboxGroupInput(
              "reg_lines", NULL, reg_lines,
              selected = state_group("reg_lines"), inline = TRUE
            )
          )
        )
      ),
      conditionalPanel(
        condition = "(input.tabs_regress == 'Summary' && input.reg_sum_check != undefined && input.reg_sum_check.indexOf('confint') >= 0) ||
                     (input.tabs_regress == 'Predict' && input.reg_predict != 'none') ||
                     (input.tabs_regress == 'Plot' && input.reg_plots == 'coef')",
        sliderInput(
          "reg_conf_lev", "Confidence level:", min = 0.80,
          max = 0.99, value = state_init("reg_conf_lev", .95),
          step = 0.01
        )
      ),
      conditionalPanel(
        condition = "input.tabs_regress == 'Summary'",
        tags$table(
          tags$td(uiOutput("ui_reg_store_res_name")),
          tags$td(actionButton("reg_store_res", "Store", icon = icon("plus")), style = "padding-top:30px;")
        )
      )
    ),
    help_and_report(
      modal_title = "Linear regression (OLS)", fun_name = "regress",
      help_file = inclRmd(file.path(getOption("radiant.path.model"), "app/tools/help/regress.Rmd"))
    )
  )
})

reg_plot <- reactive({
  if (reg_available() != "available") return()
  if (is_empty(input$reg_plots, "none")) return()

  # specifying plot heights
  plot_height <- 500
  plot_width <- 650
  nrVars <- length(input$reg_evar) + 1

  if (input$reg_plots == "dist") {
    plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
  } else if (input$reg_plots == "dashboard") {
    plot_height <- 1.5 * plot_height
  } else if (input$reg_plots == "correlations") {
    plot_height <- 150 * nrVars
    plot_width <- 150 * nrVars
  } else if (input$reg_plots == "coef") {
    if (input$reg_plots == "coef") {
      incl <- paste0("^(", paste0(input$reg_incl, "[|]*", collapse = "|"), ")")
      nr_coeff <- sum(grepl(incl, .regress()$coeff$label))
      plot_height <- 300 + 20 * nr_coeff
    }
  } else if (input$reg_plots %in% c("scatter", "resid_pred")) {
    plot_height <- (plot_height / 2) * ceiling((nrVars - 1) / 2)
  }

  list(plot_width = plot_width, plot_height = plot_height)
})

reg_plot_width <- function()
  reg_plot() %>% {if (is.list(.)) .$plot_width else 650}

reg_plot_height <- function()
  reg_plot() %>% {if (is.list(.)) .$plot_height else 500}

reg_pred_plot_height <- function()
  if (input$reg_pred_plot) 500 else 1

# output is called from the main radiant ui.R
output$regress <- renderUI({
  register_print_output("summary_regress", ".summary_regress")
  register_print_output("predict_regress", ".predict_print_regress")
  register_plot_output(
    "predict_plot_regress", ".predict_plot_regress",
    height_fun = "reg_pred_plot_height"
  )
  register_plot_output(
    "plot_regress", ".plot_regress",
    height_fun = "reg_plot_height",
    width_fun = "reg_plot_width"
  )

  ## two separate tabs
  reg_output_panels <- tabsetPanel(
    id = "tabs_regress",
    tabPanel(
      "Summary",
      download_link("dl_reg_coef"), br(),
      verbatimTextOutput("summary_regress")
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.reg_pred_plot == true",
        download_link("dlp_reg_pred"),
        plotOutput("predict_plot_regress", width = "100%", height = "100%")
      ),
      download_link("dl_reg_pred"), br(),
      verbatimTextOutput("predict_regress")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_regress"),
      plotOutput("plot_regress", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Estimate",
    tool = "Linear regression (OLS)",
    tool_ui = "ui_regress",
    output_panels = reg_output_panels
  )
})

reg_available <- eventReactive(input$reg_run, {
  if (not_available(input$reg_rvar)) {
    "This analysis requires a response variable of type integer\nor numeric and one or more explanatory variables.\nIf these variables are not available please select another dataset.\n\n" %>%
      suggest_data("diamonds")
  } else if (not_available(input$reg_evar)) {
    "Please select one or more explanatory variables. Then press the Estimate\nbutton to estimate the model.\n\n" %>%
      suggest_data("diamonds")
  } else {
    "available"
  }
})

.regress <- eventReactive(input$reg_run, {
  regi <- reg_inputs()
  regi$envir <- r_data
  withProgress(message = "Estimating model", value = 1, {
    do.call(regress, regi)
  })
})

.summary_regress <- reactive({
  if (not_pressed(input$reg_run)) return("** Press the Estimate button to estimate the model **")
  if (reg_available() != "available") return(reg_available())
  do.call(summary, c(list(object = .regress()), reg_sum_inputs()))
})

.predict_regress <- reactive({
  if (not_pressed(input$reg_run)) return("** Press the Estimate button to estimate the model **")
  if (reg_available() != "available") return(reg_available())
  if (is_empty(input$reg_predict, "none")) return("** Select prediction input **")
  if ((input$reg_predict == "data" || input$reg_predict == "datacmd") && is_empty(input$reg_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$reg_predict == "cmd" && is_empty(input$reg_pred_cmd)) {
    return("** Enter prediction commands **")
  }

  withProgress(message = "Generating predictions", value = 1, {
    rpi <- reg_pred_inputs()
    rpi$object <- .regress()
    rpi$envir <- r_data
    do.call(predict, rpi)
  })
})

.predict_print_regress <- reactive({
  .predict_regress() %>%
    {if (is.character(.)) cat(., "\n") else print(.)}
})

.predict_plot_regress <- reactive({
  req(
    pressed(input$reg_run), input$reg_pred_plot,
    available(input$reg_xvar),
    !is_empty(input$reg_predict, "none")
  )

  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_regress()), reg_pred_plot_inputs()))
  })
})

.plot_regress <- reactive({
  if (not_pressed(input$reg_run)) {
    return("** Press the Estimate button to estimate the model **")
  } else if (is_empty(input$reg_plots, "none")) {
    return("Please select a regression plot from the drop-down menu")
  } else if (reg_available() != "available") {
    return(reg_available())
  }
  if (!input$reg_plots %in% c("coef", "dist", "influence")) req(input$reg_nrobs)
  withProgress(message = "Generating plots", value = 1, {
    if (input$reg_plots == "correlations") {
      capture_plot(do.call(plot, c(list(x = .regress()), reg_plot_inputs())))
    } else {
      do.call(plot, c(list(x = .regress()), reg_plot_inputs(), shiny = TRUE))
    }
  })
})

observeEvent(input$regress_report, {
  if (is_empty(input$reg_evar)) return(invisible())
  outputs <- c("summary")
  inp_out <- list("", "")
  inp_out[[1]] <- clean_args(reg_sum_inputs(), reg_sum_args[-1])
  figs <- FALSE
  if (!is_empty(input$reg_plots, "none")) {
    rpi <- reg_plot_inputs()
    if (!input$reg_plots %in% c("correlations", "scatter", "dashboard", "resid_pred")) {
      rpi$nrobs <- NULL
    } else {
      rpi$nrobs <- as_integer(rpi$nrobs)
    }

    inp_out[[2]] <- clean_args(rpi, reg_plot_args[-1])
    inp_out[[2]]$custom <- FALSE
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }

  if (!is_empty(input$reg_store_res_name)) {
    fixed <- fix_names(input$reg_store_res_name)
    updateTextInput(session, "reg_store_res_name", value = fixed)
    xcmd <- paste0(input$dataset, " <- store(", input$dataset, ", result, name = \"", fixed, "\")\n")
  } else {
    xcmd <- ""
  }

  if (!is_empty(input$reg_predict, "none") &&
     (!is_empty(input$reg_pred_data) || !is_empty(input$reg_pred_cmd))) {
    pred_args <- clean_args(reg_pred_inputs(), reg_pred_args[-1])

    if (!is_empty(pred_args$pred_cmd)) {
      pred_args$pred_cmd <- strsplit(pred_args$pred_cmd, ";")[[1]]
    } else {
      pred_args$pred_cmd <- NULL
    }
    if (!is_empty(pred_args$pred_data)) {
      pred_args$pred_data <- as.symbol(pred_args$pred_data)
    } else {
      pred_args$pred_data <- NULL
    }

    inp_out[[2 + figs]] <- pred_args
    outputs <- c(outputs, "pred <- predict")
    xcmd <- paste0(xcmd, "print(pred, n = 10)")
    if (input$reg_predict %in% c("data", "datacmd")) {
      fixed <- unlist(strsplit(input$reg_store_pred_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
        fix_names() %>%
        deparse(., control = getOption("dctrl"), width.cutoff = 500L)
      xcmd <- paste0(xcmd, "\n", input$reg_pred_data , " <- store(",
        input$reg_pred_data, ", pred, name = ", fixed, ")"
      )
    }

    if (input$reg_pred_plot && !is_empty(input$reg_xvar)) {
      inp_out[[3 + figs]] <- clean_args(reg_pred_plot_inputs(), reg_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  update_report(
    inp_main = clean_args(reg_inputs(), reg_args),
    fun_name = "regress",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = reg_plot_width(),
    fig.height = reg_plot_height(),
    xcmd = xcmd
  )
})

observeEvent(input$reg_store_res, {
  req(pressed(input$reg_run))
  robj <- .regress()
  if (!is.list(robj)) return()
  fixed <- fix_names(input$reg_store_res_name)
  updateTextInput(session, "reg_store_res_name", value = fixed)
  withProgress(
    message = "Storing residuals", value = 1,
    r_data[[input$dataset]] <- store(r_data[[input$dataset]], robj, name = fixed)
  )
})

observeEvent(input$reg_store_pred, {
  req(!is_empty(input$reg_pred_data), pressed(input$reg_run))
  pred <- .predict_regress()
  if (is.null(pred)) return()
  fixed <- unlist(strsplit(input$reg_store_pred_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
    fix_names() %>%
    paste0(collapse = ", ")
  updateTextInput(session, "reg_store_pred_name", value = fixed)
  withProgress(
    message = "storing predictions", value = 1,
    r_data[[input$reg_pred_data]] <- store(
      r_data[[input$reg_pred_data]], pred,
      name = fixed
    )
  )
})

dl_reg_coef <- function(path) {
  if (pressed(input$reg_run)) {
    write.coeff(.regress(), file = path)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_reg_coef",
  fun = dl_reg_coef,
  fn = function() paste0(input$dataset, "_reg_coef"),
  type = "csv",
  caption = "Save coefficients"
)

dl_reg_pred <- function(path) {
  if (pressed(input$reg_run)) {
    write.csv(.predict_regress(), file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_reg_pred",
  fun = dl_reg_pred,
  fn = function() paste0(input$dataset, "_reg_pred"),
  type = "csv",
  caption = "Save regression predictions"
)

download_handler(
  id = "dlp_reg_pred",
  fun = download_handler_plot,
  fn = paste0(input$dataset, "_reg_pred"),
  type = "png",
  caption = "Save regression prediction plot",
  plot = .predict_plot_regress,
  width = plot_width,
  height = reg_pred_plot_height
)

download_handler(
  id = "dlp_regress",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_", input$reg_plots, "_regress"),
  type = "png",
  caption = "Save regression plot",
  plot = .plot_regress,
  width = reg_plot_width,
  height = reg_plot_height
)
