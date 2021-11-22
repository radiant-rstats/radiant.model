mnl_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
mnl_predict <- c(
  "None" = "none",
  "Data" = "data",
  "Command" = "cmd",
  "Data & Command" = "datacmd"
)
mnl_check <- c(
  "Drop intercept" = "no_int", "Standardize" = "standardize",
  "Center" = "center", "Stepwise" = "stepwise-backward"
)
mnl_sum_check <- c(
  "Confidence intervals" = "confint", "RRRs" = "rrr", "Confusion" = "confusion"
)
mnl_plots <- c(
  "None" = "none", "Distribution" = "dist",
  "Correlations" = "correlations", "Coefficient (RRR) plot" = "coef"
)

## list of function arguments
mnl_args <- as.list(formals(mnl))

## list of function inputs selected by user
mnl_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  mnl_args$data_filter <- if (input$show_filter) input$data_filter else ""
  mnl_args$dataset <- input$dataset
  for (i in r_drop(names(mnl_args)))
    mnl_args[[i]] <- input[[paste0("mnl_", i)]]
  mnl_args
})

mnl_sum_args <- as.list(if (exists("summary.mnl")) {
  formals(summary.mnl)
} else {
  formals(radiant.model:::summary.mnl)
} )

## list of function inputs selected by user
mnl_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(mnl_sum_args))
    mnl_sum_args[[i]] <- input[[paste0("mnl_", i)]]
  mnl_sum_args
})

mnl_plot_args <- as.list(if (exists("plot.mnl")) {
  formals(plot.mnl)
} else {
  formals(radiant.model:::plot.mnl)
} )

## list of function inputs selected by user
mnl_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(mnl_plot_args))
    mnl_plot_args[[i]] <- input[[paste0("mnl_", i)]]

  # cat(paste0(names(mnl_plot_args), " ", mnl_plot_args, collapse = ", "), file = stderr(), "\n")
  mnl_plot_args
})

mnl_pred_args <- as.list(if (exists("predict.mnl")) {
  formals(predict.mnl)
} else {
  formals(radiant.model:::predict.mnl)
} )

# list of function inputs selected by user
mnl_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(mnl_pred_args))
    mnl_pred_args[[i]] <- input[[paste0("mnl_", i)]]

  mnl_pred_args$pred_cmd <- mnl_pred_args$pred_data <- ""
  if (input$mnl_predict == "cmd") {
    mnl_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$mnl_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$mnl_predict == "data") {
    mnl_pred_args$pred_data <- input$mnl_pred_data
  } else if (input$mnl_predict == "datacmd") {
    mnl_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$mnl_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    mnl_pred_args$pred_data <- input$mnl_pred_data
  }

  mnl_pred_args
})

mnl_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
  formals(plot.model.predict)
} else {
  formals(radiant.model:::plot.model.predict)
} )


# list of function inputs selected by user
mnl_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(mnl_pred_plot_args))
    mnl_pred_plot_args[[i]] <- input[[paste0("mnl_", i)]]
  mnl_pred_plot_args
})

output$ui_mnl_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    vars <- groupable_vars()
  })
  init <- isolate(input$mnl_rvar)
  selectInput(
    inputId = "mnl_rvar", label = "Response variable:", choices = vars,
    selected = state_single("mnl_rvar", vars, init), multiple = FALSE
  )
})

output$ui_mnl_lev <- renderUI({
  req(available(input$mnl_rvar))
  rvar <- .get_data()[[input$mnl_rvar]]
  levs <- unique(rvar)
  if (length(levs) > 50) {
    HTML("<label>More than 50 levels. Please choose another response variable</label>")
  } else {
    selectInput(
      inputId = "mnl_lev", label = "Choose base level:",
      choices = levs, selected = state_init("mnl_lev")
    )
  }
})

output$ui_mnl_evar <- renderUI({
  req(available(input$mnl_rvar))
  vars <- varnames()
  if (length(vars) > 0 && input$mnl_rvar %in% vars) {
    vars <- vars[-which(vars == input$mnl_rvar)]
  }

  selectInput(
    inputId = "mnl_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("mnl_evar", vars, isolate(input$mnl_evar)),
    multiple = TRUE, size = min(10, length(vars)), selectize = FALSE
  )
})

output$ui_mnl_wts <- renderUI({
  req(available(input$mnl_rvar), available(input$mnl_evar))
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$mnl_evar)) {
    vars <- base::setdiff(vars, input$mnl_evar)
    names(vars) <- varnames() %>%
      {.[match(vars, .)]} %>%
      names()
  }
  vars <- c("None", vars)

  selectInput(
    inputId = "mnl_wts", label = "Weights:", choices = vars,
    selected = state_single("mnl_wts", vars),
    multiple = FALSE
  )
})

output$ui_mnl_test_var <- renderUI({
  req(available(input$mnl_evar))
  vars <- input$mnl_evar
  if (!is.null(input$mnl_int)) vars <- c(vars, input$mnl_int)
  selectizeInput(
    inputId = "mnl_test_var", label = "Variables to test:",
    choices = vars,
    selected = state_multiple("mnl_test_var", vars, isolate(input$mnl_test_var)),
    multiple = TRUE,
    options = list(placeholder = "None", plugins = list("remove_button"))
  )
})

## not clear why this is needed because state_multiple should handle this
observeEvent(is.null(input$mnl_test_var), {
  if ("mnl_test_var" %in% names(input)) r_state$mnl_test_var <<- NULL
})

output$ui_mnl_show_interactions <- renderUI({
  # choices <- mnl_show_interactions[1:max(min(3, length(input$mnl_evar)), 1)]
  vars <- input$mnl_evar
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  if (any(vars %in% varnames()[isNum])) {
    choices <- mnl_show_interactions[1:3]
  } else {
    choices <- mnl_show_interactions[1:max(min(3, length(input$mnl_evar)), 1)]
  }
  radioButtons(
    inputId = "mnl_show_interactions", label = "Interactions:",
    choices = choices, selected = state_init("mnl_show_interactions"),
    inline = TRUE
  )
})

output$ui_mnl_show_interactions <- renderUI({
  vars <- input$mnl_evar
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  if (any(vars %in% varnames()[isNum])) {
    choices <- mnl_show_interactions[1:3]
  } else {
    choices <- mnl_show_interactions[1:max(min(3, length(input$mnl_evar)), 1)]
  }
  radioButtons(
    inputId = "mnl_show_interactions", label = "Interactions:",
    choices = choices, selected = state_init("mnl_show_interactions"),
    inline = TRUE
  )
})

output$ui_mnl_int <- renderUI({
  choices <- character(0)
  if (isolate("mnl_show_interactions" %in% names(input)) &&
   radiant.data::is_empty(input$mnl_show_interactions)) {
  } else if (radiant.data::is_empty(input$mnl_show_interactions)) {
    return()
  } else {
    vars <- input$mnl_evar
    if (not_available(vars)) {
      return()
    } else {
      ## quadratic and qubic terms
      isNum <- .get_class() %in% c("integer", "numeric", "ts")
      isNum <- intersect(vars, varnames()[isNum])
      if (length(isNum) > 0) {
        choices <- qterms(isNum, input$mnl_show_interactions)
      }
      ## list of interaction terms to show
      if (length(vars) > 1) {
        choices <- c(choices, iterms(vars, input$mnl_show_interactions))
      }
      if (length(choices) == 0) return()
    }
  }

  selectInput(
    "mnl_int", label = NULL,
    choices = choices,
    selected = state_init("mnl_int"),
    multiple = TRUE,
    size = min(8, length(choices)),
    selectize = FALSE
  )
})

## reset prediction and plot settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "mnl_predict", selected = "none")
  updateSelectInput(session = session, inputId = "mnl_plots", selected = "none")
})

output$ui_mnl_store_pred_name <- renderUI({
  req(input$mnl_rvar)
  levs <- .get_data()[[input$mnl_rvar]] %>%
    as.factor() %>%
    levels() %>%
    fix_names() %>%
    paste(collapse = ", ")
  textInput(
    "mnl_store_pred_name",
    "Store predictions:",
    state_init("mnl_store_pred_name", levs)
  )
})

output$ui_mnl_predict_plot <- renderUI({
  req(input$mnl_rvar)
  var_colors <- ".class" %>% set_names(input$mnl_rvar)
  predict_plot_controls("mnl", vars_color = var_colors, init_color = ".class")
})

output$ui_mnl_nrobs <- renderUI({
  nrobs <- nrow(.get_data())
  choices <- c("1,000" = 1000, "5,000" = 5000, "10,000" = 10000, "All" = -1) %>%
    .[. < nrobs]
  selectInput(
    "mnl_nrobs", "Number of data points plotted:",
    choices = choices,
    selected = state_single("mnl_nrobs", choices, 1000)
  )
})

output$ui_mnl_store_res_name <- renderUI({
  req(input$dataset)
  textInput("mnl_store_res_name", "Store residuals:", "", placeholder = "Provide variable name")
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(reg_args, "mnl", tabs = "tabs_mnl", label = "Estimate model", relabel = "Re-estimate model")

output$ui_mnl <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(
      condition = "input.tabs_mnl == 'Summary'",
      wellPanel(
        actionButton("mnl_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_mnl == 'Summary'",
        uiOutput("ui_mnl_rvar"),
        uiOutput("ui_mnl_lev"),
        uiOutput("ui_mnl_evar"),
        uiOutput("ui_mnl_wts"),
        conditionalPanel(
          condition = "input.mnl_evar != null",

          uiOutput("ui_mnl_show_interactions"),
          conditionalPanel(
            condition = "input.mnl_show_interactions != ''",
            uiOutput("ui_mnl_int")
          ),
          uiOutput("ui_mnl_test_var"),
          checkboxGroupInput(
            "mnl_check", NULL, mnl_check,
            selected = state_group("mnl_check"), inline = TRUE
          ),
          checkboxGroupInput(
            "mnl_sum_check", NULL, mnl_sum_check,
            selected = state_group("mnl_sum_check", ""), inline = TRUE
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_mnl == 'Predict'",
        selectInput(
          "mnl_predict", label = "Prediction input type:", mnl_predict,
          selected = state_single("mnl_predict", mnl_predict, "none")
        ),
        conditionalPanel(
          "input.mnl_predict == 'data' | input.mnl_predict == 'datacmd'",
          selectizeInput(
            inputId = "mnl_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("mnl_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.mnl_predict == 'cmd' | input.mnl_predict == 'datacmd'",
          returnTextAreaInput(
            "mnl_pred_cmd", "Prediction command:",
            value = state_init("mnl_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., class = '1st'; gender = 'male') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.mnl_predict != 'none'",
          checkboxInput("mnl_pred_plot", "Plot predictions", state_init("mnl_pred_plot", FALSE)),
          conditionalPanel(
            "input.mnl_pred_plot == true",
            uiOutput("ui_mnl_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.mnl_predict == 'data' | input.mnl_predict == 'datacmd'",
          tags$table(
            tags$td(uiOutput("ui_mnl_store_pred_name")),
            tags$td(actionButton("mnl_store_pred", "Store", icon = icon("plus")), class = "top")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_mnl == 'Plot'",
        selectInput(
          "mnl_plots", "Plots:", choices = mnl_plots,
          selected = state_single("mnl_plots", mnl_plots)
        ),
        conditionalPanel(
          condition = "input.mnl_plots == 'coef'",
          checkboxInput("mnl_intercept", "Include intercept", state_init("mnl_intercept", FALSE))
        ),
        conditionalPanel(
          condition = "input.mnl_plots == 'correlations' |
                       input.mnl_plots == 'scatter'",
          uiOutput("ui_mnl_nrobs")
        )
      ),
      # Using && to check that input.mnl_sum_check is not null (must be &&)
      conditionalPanel(
        condition = "(input.tabs_mnl == 'Summary' && input.mnl_sum_check != undefined && (input.mnl_sum_check.indexOf('confint') >= 0 || input.mnl_sum_check.indexOf('rrr') >= 0)) ||
                     (input.tabs_mnl == 'Plot' && input.mnl_plots == 'coef')",
        sliderInput(
          "mnl_conf_lev", "Confidence level:", min = 0.80,
          max = 0.99, value = state_init("mnl_conf_lev", .95),
          step = 0.01
        )
      ),
      conditionalPanel(
        condition = "input.tabs_mnl == 'Summary'",
        tags$table(
          # tags$td(textInput("mnl_store_res_name", "Store residuals:", state_init("mnl_store_res_name", "residuals_logit"))),
          tags$td(uiOutput("ui_mnl_store_res_name")),
          tags$td(actionButton("mnl_store_res", "Store", icon = icon("plus")), class = "top")
        )
      )
    ),
    help_and_report(
      modal_title = "Multinomial logistic regression (MNL)", fun_name = "mnl",
      help_file = inclRmd(file.path(getOption("radiant.path.model"), "app/tools/help/mnl.Rmd"))
    )
  )
})

mnl_plot <- reactive({
  if (mnl_available() != "available") return()
  if (radiant.data::is_empty(input$mnl_plots, "none")) return()

  plot_height <- 500
  plot_width <- 650
  nrVars <- length(input$mnl_evar) + 1

  if (input$mnl_plots == "dist") plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
  if (input$mnl_plots == "fit") plot_width <- 1.5 * plot_width
  if (input$mnl_plots == "correlations") {
    plot_height <- 150 * nrVars
    plot_width <- 150 * nrVars
  }
  if (input$mnl_plots == "scatter") plot_height <- 300 * nrVars
  if (input$mnl_plots == "coef") {
    nr_coeff <- broom::tidy(.mnl()$model) %>% nrow()
    plot_height <- 300 + 10 * nr_coeff
  }

  list(plot_width = plot_width, plot_height = plot_height)
})

mnl_plot_width <- function()
  mnl_plot() %>% {if (is.list(.)) .$plot_width else 650}

mnl_plot_height <- function()
  mnl_plot() %>% {if (is.list(.)) .$plot_height else 500}

mnl_pred_plot_height <- function()
  if (input$mnl_pred_plot) 500 else 1

## output is called from the main radiant ui.R
output$mnl <- renderUI({
  register_print_output("summary_mnl", ".summary_mnl")
  register_print_output("predict_mnl", ".predict_print_mnl")
  register_plot_output(
    "predict_plot_mnl", ".predict_plot_mnl",
    height_fun = "mnl_pred_plot_height"
  )
  register_plot_output(
    "plot_mnl", ".plot_mnl",
    height_fun = "mnl_plot_height",
    width_fun = "mnl_plot_width"
  )

  ## two separate tabs
  mnl_output_panels <- tabsetPanel(
    id = "tabs_mnl",
    tabPanel(
      "Summary",
      download_link("dl_mnl_coef"), br(),
      verbatimTextOutput("summary_mnl")
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.mnl_pred_plot == true",
        download_link("dlp_mnl_pred"),
        plotOutput("predict_plot_mnl", width = "100%", height = "100%")
      ),
      download_link("dl_mnl_pred"), br(),
      verbatimTextOutput("predict_mnl")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_mnl"),
      plotOutput("plot_mnl", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Estimate",
    tool = "Multinomial logistic regression (MNL)",
    tool_ui = "ui_mnl",
    output_panels = mnl_output_panels
  )
})

mnl_available <- reactive({
  if (not_available(input$mnl_rvar)) {
    "This analysis requires a response variable with two or more levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>%
      suggest_data("titanic")
  } else if (not_available(input$mnl_evar)) {
    "Please select one or more explanatory variables.\n\n" %>%
      suggest_data("titanic")
  } else {
    "available"
  }
})

.mnl <- eventReactive(input$mnl_run, {
  req(input$mnl_lev)
  req(input$mnl_wts == "None" || available(input$mnl_wts))
  withProgress(message = "Estimating model", value = 1, {
    lgi <- mnl_inputs()
    lgi$envir <- r_data
    do.call(mnl, lgi)
  })
})

.summary_mnl <- reactive({
  if (not_pressed(input$mnl_run)) return("** Press the Estimate button to estimate the model **")
  if (mnl_available() != "available") return(mnl_available())
  do.call(summary, c(list(object = .mnl()), mnl_sum_inputs()))
})

.predict_mnl <- reactive({
  if (not_pressed(input$mnl_run)) return("** Press the Estimate button to estimate the model **")
  if (mnl_available() != "available") return(mnl_available())
  if (radiant.data::is_empty(input$mnl_predict, "none")) return("** Select prediction input **")
  if ((input$mnl_predict == "data" || input$mnl_predict == "datacmd") && radiant.data::is_empty(input$mnl_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$mnl_predict == "cmd" && radiant.data::is_empty(input$mnl_pred_cmd)) {
    return("** Enter prediction commands **")
  }

  withProgress(message = "Generating predictions", value = 1, {
    lgi <- mnl_pred_inputs()
    lgi$object <- .mnl()
    lgi$envir <- r_data
    do.call(predict, lgi)
  })
})

.predict_print_mnl <- reactive({
  .predict_mnl() %>%
    {if (is.character(.)) cat(., "\n") else print(.)}
})

.predict_plot_mnl <- reactive({
  req(
    pressed(input$mnl_run), input$mnl_pred_plot,
    available(input$mnl_xvar),
    !radiant.data::is_empty(input$mnl_predict, "none")
  )

  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_mnl()), mnl_pred_plot_inputs()))
  })
})

.plot_mnl <- reactive({
  if (not_pressed(input$mnl_run)) {
    return("** Press the Estimate button to estimate the model **")
  } else if (radiant.data::is_empty(input$mnl_plots, "none")) {
    return("Please select a mnl regression plot from the drop-down menu")
  } else if (mnl_available() != "available") {
    return(mnl_available())
  }

  if (input$mnl_plots %in% c("correlations", "scatter")) req(input$mnl_nrobs)
  if (input$mnl_plots == "correlations") {
    capture_plot(do.call(plot, c(list(x = .mnl()), mnl_plot_inputs())))
  } else {
    withProgress(message = "Generating plots", value = 1, {
      do.call(plot, c(list(x = .mnl()), mnl_plot_inputs(), shiny = TRUE))
    })
  }
})

observeEvent(input$mnl_report, {
  outputs <- c("summary")
  inp_out <- list("", "")
  inp_out[[1]] <- clean_args(mnl_sum_inputs(), mnl_sum_args[-1])
  figs <- FALSE
  if (!radiant.data::is_empty(input$mnl_plots, "none")) {
    inp_out[[2]] <- clean_args(mnl_plot_inputs(), mnl_plot_args[-1])
    inp_out[[2]]$custom <- FALSE
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }

  if (!radiant.data::is_empty(input$mnl_store_res_name)) {
    name <- input$mnl_store_res_name
    if (!radiant.data::is_empty(name)) {
      name <- unlist(strsplit(name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
        fix_names() %T>%
        updateTextInput(session, "mnl_store_res_name", value = .) %>%
        deparse(control = getOption("dctrl"), width.cutoff = 500L)
    }
    xcmd <- paste0(input$dataset, " <- store(",
      input$dataset, ", result, name = ", name, ")\n"
    )
  } else {
    xcmd <- ""
  }

  if (!radiant.data::is_empty(input$mnl_predict, "none") &&
     (!radiant.data::is_empty(input$mnl_pred_data) || !radiant.data::is_empty(input$mnl_pred_cmd))) {
    pred_args <- clean_args(mnl_pred_inputs(), mnl_pred_args[-1])

    if (!radiant.data::is_empty(pred_args$pred_cmd)) {
      pred_args$pred_cmd <- strsplit(pred_args$pred_cmd, ";")[[1]]
    } else {
      pred_args$pred_cmd <- NULL
    }

    if (!radiant.data::is_empty(pred_args$pred_data)) {
      pred_args$pred_data <- as.symbol(pred_args$pred_data)
    } else {
      pred_args$pred_data <- NULL
    }

    inp_out[[2 + figs]] <- pred_args
    outputs <- c(outputs, "pred <- predict")

    xcmd <- paste0(xcmd, "print(pred, n = 10)")
    if (input$mnl_predict %in% c("data", "datacmd")) {
      name <- input$mnl_store_pred_name
      if (!radiant.data::is_empty(name)) {
        name <- unlist(strsplit(input$mnl_store_pred_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
          fix_names() %>%
          deparse(., control = getOption("dctrl"), width.cutoff = 500L)
      }
      xcmd <- paste0(xcmd, "\n", input$mnl_pred_data, " <- store(",
        input$mnl_pred_data, ", pred, name = ", name, ")"
      )
    }

    # xcmd <- paste0(xcmd, "\n# write.csv(pred, file = \"~/mnl_predictions.csv\", row.names = FALSE)")

    if (input$mnl_pred_plot && !radiant.data::is_empty(input$mnl_xvar)) {
      inp_out[[3 + figs]] <- clean_args(mnl_pred_plot_inputs(), mnl_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  update_report(
    inp_main = clean_args(mnl_inputs(), mnl_args),
    fun_name = "mnl",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = mnl_plot_width(),
    fig.height = mnl_plot_height(),
    xcmd = xcmd
  )
})

observeEvent(input$mnl_store_res, {
  req(pressed(input$mnl_run))
  robj <- .mnl()
  if (!is.list(robj)) return()
  fixed <- unlist(strsplit(input$mnl_store_res_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
    fix_names() %>%
    paste0(collapse = ", ")
  updateTextInput(session, "mnl_store_res_name", value = fixed)
  withProgress(
    message = "Storing residuals", value = 1,
    r_data[[input$dataset]] <- store(r_data[[input$dataset]], robj, name = fixed)
  )
})

observeEvent(input$mnl_store_pred, {
  req(!radiant.data::is_empty(input$mnl_pred_data), pressed(input$mnl_run))
  pred <- .predict_mnl()
  if (is.null(pred)) return()
  fixed <- unlist(strsplit(input$mnl_store_pred_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
    fix_names() %>%
    paste0(collapse = ", ")
  updateTextInput(session, "mnl_store_pred_name", value = fixed)
  withProgress(
    message = "Storing predictions", value = 1,
    r_data[[input$mnl_pred_data]] <- store(
      r_data[[input$mnl_pred_data]], pred,
      name = fixed
    )
  )
})

dl_mnl_coef <- function(path) {
  if (pressed(input$mnl_run)) {
    write.coeff(.mnl(), file = path)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_mnl_coef",
  fun = dl_mnl_coef,
  fn = function() paste0(input$dataset, "_mnl_coef"),
  type = "csv",
  caption = "Save coefficients"
)

dl_mnl_pred <- function(path) {
  if (pressed(input$mnl_run)) {
    write.csv(.predict_mnl(), file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_mnl_pred",
  fun = dl_mnl_pred,
  fn = function() paste0(input$dataset, "_mnl_pred"),
  type = "csv",
  caption = "Save predictions"
)

download_handler(
  id = "dlp_mnl_pred",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_mnl_pred"),
  type = "png",
  caption = "Save mnl prediction plot",
  plot = .predict_plot_mnl,
  width = plot_width,
  height = mnl_pred_plot_height
)

download_handler(
  id = "dlp_mnl",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_", input$mnl_plots, "_logit"),
  type = "png",
  caption = "Save mnl plot",
  plot = .plot_logistic,
  width = mnl_plot_width,
  height = mnl_plot_height
)

