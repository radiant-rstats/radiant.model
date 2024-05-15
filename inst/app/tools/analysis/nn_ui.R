nn_plots <- c(
  "None" = "none",
  "Network" = "net",
  "Permutation Importance" = "vip",
  "Prediction plots" = "pred_plot",
  "Partial Dependence" = "pdp",
  "Olden" = "olden",
  "Garson" = "garson",
  "Dashboard" = "dashboard"
)

## list of function arguments
nn_args <- as.list(formals(nn))

## list of function inputs selected by user
nn_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  nn_args$data_filter <- if (input$show_filter) input$data_filter else ""
  nn_args$arr <- if (input$show_filter) input$data_arrange else ""
  nn_args$rows <- if (input$show_filter) input$data_rows else ""
  nn_args$dataset <- input$dataset
  for (i in r_drop(names(nn_args))) {
    nn_args[[i]] <- input[[paste0("nn_", i)]]
  }
  nn_args
})

nn_pred_args <- as.list(if (exists("predict.nn")) {
  formals(predict.nn)
} else {
  formals(radiant.model:::predict.nn)
})

# list of function inputs selected by user
nn_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(nn_pred_args)) {
    nn_pred_args[[i]] <- input[[paste0("nn_", i)]]
  }

  nn_pred_args$pred_cmd <- nn_pred_args$pred_data <- ""
  if (input$nn_predict == "cmd") {
    nn_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$nn_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$nn_predict == "data") {
    nn_pred_args$pred_data <- input$nn_pred_data
  } else if (input$nn_predict == "datacmd") {
    nn_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$nn_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    nn_pred_args$pred_data <- input$nn_pred_data
  }
  nn_pred_args
})

nn_plot_args <- as.list(if (exists("plot.nn")) {
  formals(plot.nn)
} else {
  formals(radiant.model:::plot.nn)
})

## list of function inputs selected by user
nn_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(nn_plot_args)) {
    nn_plot_args[[i]] <- input[[paste0("nn_", i)]]
  }
  nn_plot_args
})

nn_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
  formals(plot.model.predict)
} else {
  formals(radiant.model:::plot.model.predict)
})

# list of function inputs selected by user
nn_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(nn_pred_plot_args)) {
    nn_pred_plot_args[[i]] <- input[[paste0("nn_", i)]]
  }
  nn_pred_plot_args
})

output$ui_nn_rvar <- renderUI({
  req(input$nn_type)

  withProgress(message = "Acquiring variable information", value = 1, {
    if (input$nn_type == "classification") {
      vars <- two_level_vars()
    } else {
      isNum <- .get_class() %in% c("integer", "numeric", "ts")
      vars <- varnames()[isNum]
    }
  })

  init <- if (input$nn_type == "classification") {
    if (is.empty(input$logit_rvar)) isolate(input$nn_rvar) else input$logit_rvar
  } else {
    if (is.empty(input$reg_rvar)) isolate(input$nn_rvar) else input$reg_rvar
  }

  selectInput(
    inputId = "nn_rvar",
    label = "Response variable:",
    choices = vars,
    selected = state_single("nn_rvar", vars, init),
    multiple = FALSE
  )
})

output$ui_nn_lev <- renderUI({
  req(input$nn_type == "classification")
  req(available(input$nn_rvar))
  levs <- .get_data()[[input$nn_rvar]] %>%
    as_factor() %>%
    levels()

  init <- if (is.empty(input$logit_lev)) isolate(input$nn_lev) else input$logit_lev
  selectInput(
    inputId = "nn_lev", label = "Choose level:",
    choices = levs,
    selected = state_init("nn_lev", init)
  )
})

output$ui_nn_evar <- renderUI({
  if (not_available(input$nn_rvar)) {
    return()
  }
  vars <- varnames()
  if (length(vars) > 0) {
    vars <- vars[-which(vars == input$nn_rvar)]
  }

  init <- if (input$nn_type == "classification") {
    # input$logit_evar
    if (is.empty(input$logit_evar)) isolate(input$nn_evar) else input$logit_evar
  } else {
    # input$reg_evar
    if (is.empty(input$reg_evar)) isolate(input$nn_evar) else input$reg_evar
  }

  selectInput(
    inputId = "nn_evar",
    label = "Explanatory variables:",
    choices = vars,
    selected = state_multiple("nn_evar", vars, init),
    multiple = TRUE,
    size = min(10, length(vars)),
    selectize = FALSE
  )
})

# function calls generate UI elements
output_incl("nn")
output_incl_int("nn")

output$ui_nn_wts <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$nn_evar)) {
    vars <- base::setdiff(vars, input$nn_evar)
    names(vars) <- varnames() %>%
      {
        .[match(vars, .)]
      } %>%
      names()
  }
  vars <- c("None", vars)

  selectInput(
    inputId = "nn_wts", label = "Weights:", choices = vars,
    selected = state_single("nn_wts", vars),
    multiple = FALSE
  )
})

output$ui_nn_store_pred_name <- renderUI({
  init <- state_init("nn_store_pred_name", "pred_nn") %>%
    sub("\\d{1,}$", "", .) %>%
    paste0(., ifelse(is.empty(input$nn_size), "", input$nn_size))
  textInput(
    "nn_store_pred_name",
    "Store predictions:",
    init
  )
})

output$ui_nn_store_res_name <- renderUI({
  req(input$dataset)
  textInput("nn_store_res_name", "Store residuals:", "", placeholder = "Provide variable name")
})

## reset prediction and plot settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "nn_predict", selected = "none")
  updateSelectInput(session = session, inputId = "nn_plots", selected = "none")
})

## reset prediction settings when the model type changes
observeEvent(input$nn_type, {
  updateSelectInput(session = session, inputId = "nn_predict", selected = "none")
  updateSelectInput(session = session, inputId = "nn_plots", selected = "none")
})

output$ui_nn_predict_plot <- renderUI({
  predict_plot_controls("nn")
})

output$ui_nn_plots <- renderUI({
  req(input$nn_type)
  if (input$nn_type != "regression") {
    nn_plots <- head(nn_plots, -1)
  }
  selectInput(
    "nn_plots", "Plots:",
    choices = nn_plots,
    selected = state_single("nn_plots", nn_plots)
  )
})

output$ui_nn_nrobs <- renderUI({
  nrobs <- nrow(.get_data())
  choices <- c("1,000" = 1000, "5,000" = 5000, "10,000" = 10000, "All" = -1) %>%
    .[. < nrobs]
  selectInput(
    "nn_nrobs", "Number of data points plotted:",
    choices = choices,
    selected = state_single("nn_nrobs", choices, 1000)
  )
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(nn_args, "nn", tabs = "tabs_nn", label = "Estimate model", relabel = "Re-estimate model")

output$ui_nn <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(
      condition = "input.tabs_nn == 'Summary'",
      wellPanel(
        actionButton("nn_run", "Estimate model", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_nn == 'Summary'",
        radioButtons(
          "nn_type",
          label = NULL, c("classification", "regression"),
          selected = state_init("nn_type", "classification"),
          inline = TRUE
        ),
        uiOutput("ui_nn_rvar"),
        uiOutput("ui_nn_lev"),
        uiOutput("ui_nn_evar"),
        uiOutput("ui_nn_wts"),
        tags$table(
          tags$td(numericInput(
            "nn_size",
            label = "Size:", min = 1, max = 20,
            value = state_init("nn_size", 1), width = "77px"
          )),
          tags$td(numericInput(
            "nn_decay",
            label = "Decay:", min = 0, max = 1,
            step = .1, value = state_init("nn_decay", .5), width = "77px"
          )),
          tags$td(numericInput(
            "nn_seed",
            label = "Seed:",
            value = state_init("nn_seed", 1234), width = "77px"
          )),
          width = "100%"
        )
      ),
      conditionalPanel(
        condition = "input.tabs_nn == 'Predict'",
        selectInput(
          "nn_predict",
          label = "Prediction input type:", reg_predict,
          selected = state_single("nn_predict", reg_predict, "none")
        ),
        conditionalPanel(
          "input.nn_predict == 'data' | input.nn_predict == 'datacmd'",
          selectizeInput(
            inputId = "nn_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("nn_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.nn_predict == 'cmd' | input.nn_predict == 'datacmd'",
          returnTextAreaInput(
            "nn_pred_cmd", "Prediction command:",
            value = state_init("nn_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., carat = 1; cut = 'Ideal') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.nn_predict != 'none'",
          checkboxInput("nn_pred_plot", "Plot predictions", state_init("nn_pred_plot", FALSE)),
          conditionalPanel(
            "input.nn_pred_plot == true",
            uiOutput("ui_nn_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.nn_predict == 'data' | input.nn_predict == 'datacmd'",
          tags$table(
            tags$td(uiOutput("ui_nn_store_pred_name")),
            tags$td(actionButton("nn_store_pred", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_nn == 'Plot'",
        uiOutput("ui_nn_plots"),
        conditionalPanel(
          condition = "input.nn_plots == 'pdp' | input.nn_plots == 'pred_plot'",
          uiOutput("ui_nn_incl"),
          uiOutput("ui_nn_incl_int")
        ),
        conditionalPanel(
          condition = "input.nn_plots == 'dashboard'",
          uiOutput("ui_nn_nrobs")
        )
      ),
      conditionalPanel(
        condition = "input.tabs_nn == 'Summary'",
        tags$table(
          tags$td(uiOutput("ui_nn_store_res_name")),
          tags$td(actionButton("nn_store_res", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
        )
      )
    ),
    help_and_report(
      modal_title = "Neural Network",
      fun_name = "nn",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/nn.md"))
    )
  )
})

nn_plot <- reactive({
  if (nn_available() != "available") {
    return()
  }
  if (is.empty(input$nn_plots, "none")) {
    return()
  }
  res <- .nn()
  if (is.character(res)) {
    return()
  }
  plot_width <- 650
  if ("dashboard" %in% input$nn_plots) {
    plot_height <- 750
  } else if (input$nn_plots %in% c("pdp", "pred_plot")) {
    nr_vars <- length(input$nn_incl) + length(input$nn_incl_int)
    plot_height <- max(250, ceiling(nr_vars / 2) * 250)
    if (length(input$nn_incl_int) > 0) {
      plot_width <- plot_width + min(2, length(input$nn_incl_int)) * 90
    }
  } else {
    mlt <- if ("net" %in% input$nn_plots) 45 else 30
    plot_height <- max(500, length(res$model$coefnames) * mlt)
  }

  list(plot_width = plot_width, plot_height = plot_height)
})

nn_plot_width <- function() {
  nn_plot() %>%
    (function(x) if (is.list(x)) x$plot_width else 650)
}

nn_plot_height <- function() {
  nn_plot() %>%
    (function(x) if (is.list(x)) x$plot_height else 500)
}

nn_pred_plot_height <- function() {
  if (input$nn_pred_plot) 500 else 1
}

## output is called from the main radiant ui.R
output$nn <- renderUI({
  register_print_output("summary_nn", ".summary_nn")
  register_print_output("predict_nn", ".predict_print_nn")
  register_plot_output(
    "predict_plot_nn", ".predict_plot_nn",
    height_fun = "nn_pred_plot_height"
  )
  register_plot_output(
    "plot_nn", ".plot_nn",
    height_fun = "nn_plot_height",
    width_fun = "nn_plot_width"
  )

  ## three separate tabs
  nn_output_panels <- tabsetPanel(
    id = "tabs_nn",
    tabPanel(
      "Summary",
      verbatimTextOutput("summary_nn")
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.nn_pred_plot == true",
        download_link("dlp_nn_pred"),
        plotOutput("predict_plot_nn", width = "100%", height = "100%")
      ),
      download_link("dl_nn_pred"), br(),
      verbatimTextOutput("predict_nn")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_nn"),
      plotOutput("plot_nn", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Estimate",
    tool = "Neural Network",
    tool_ui = "ui_nn",
    output_panels = nn_output_panels
  )
})

nn_available <- reactive({
  req(input$nn_type)
  if (not_available(input$nn_rvar)) {
    if (input$nn_type == "classification") {
      "This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>%
        suggest_data("titanic")
    } else {
      "This analysis requires a response variable of type integer\nor numeric and one or more explanatory variables.\nIf these variables are not available please select another dataset.\n\n" %>%
        suggest_data("diamonds")
    }
  } else if (not_available(input$nn_evar)) {
    if (input$nn_type == "classification") {
      "Please select one or more explanatory variables.\n\n" %>%
        suggest_data("titanic")
    } else {
      "Please select one or more explanatory variables.\n\n" %>%
        suggest_data("diamonds")
    }
  } else {
    "available"
  }
})

.nn <- eventReactive(input$nn_run, {
  nni <- nn_inputs()
  nni$envir <- r_data
  withProgress(
    message = "Estimating model", value = 1,
    do.call(nn, nni)
  )
})

.summary_nn <- reactive({
  if (not_pressed(input$nn_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (nn_available() != "available") {
    return(nn_available())
  }
  summary(.nn())
})

.predict_nn <- reactive({
  if (not_pressed(input$nn_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (nn_available() != "available") {
    return(nn_available())
  }
  if (is.empty(input$nn_predict, "none")) {
    return("** Select prediction input **")
  }

  if ((input$nn_predict == "data" || input$nn_predict == "datacmd") && is.empty(input$nn_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$nn_predict == "cmd" && is.empty(input$nn_pred_cmd)) {
    return("** Enter prediction commands **")
  }

  withProgress(message = "Generating predictions", value = 1, {
    nni <- nn_pred_inputs()
    nni$object <- .nn()
    nni$envir <- r_data
    do.call(predict, nni)
  })
})

.predict_print_nn <- reactive({
  .predict_nn() %>%
    {
      if (is.character(.)) cat(., "\n") else print(.)
    }
})

.predict_plot_nn <- reactive({
  req(
    pressed(input$nn_run), input$nn_pred_plot,
    available(input$nn_xvar),
    !is.empty(input$nn_predict, "none")
  )

  # if (not_pressed(input$nn_run)) return(invisible())
  # if (nn_available() != "available") return(nn_available())
  # req(input$nn_pred_plot, available(input$nn_xvar))
  # if (is.empty(input$nn_predict, "none")) return(invisible())
  # if ((input$nn_predict == "data" || input$nn_predict == "datacmd") && is.empty(input$nn_pred_data)) {
  #   return(invisible())
  # }
  # if (input$nn_predict == "cmd" && is.empty(input$nn_pred_cmd)) {
  #   return(invisible())
  # }

  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_nn()), nn_pred_plot_inputs()))
  })
})

.plot_nn <- reactive({
  if (not_pressed(input$nn_run)) {
    return("** Press the Estimate button to estimate the model **")
  } else if (nn_available() != "available") {
    return(nn_available())
  }
  req(input$nn_size)
  if (is.empty(input$nn_plots, "none")) {
    return("Please select a neural network plot from the drop-down menu")
  }
  pinp <- nn_plot_inputs()
  pinp$shiny <- TRUE
  pinp$size <- NULL
  if (input$nn_plots == "dashboard") {
    req(input$nn_nrobs)
  }

  if (input$nn_plots == "net") {
    .nn() %>%
      (function(x) if (is.character(x)) invisible() else capture_plot(do.call(plot, c(list(x = x), pinp))))
  } else {
    withProgress(message = "Generating plots", value = 1, {
      do.call(plot, c(list(x = .nn()), pinp))
    })
  }
})

observeEvent(input$nn_store_res, {
  req(pressed(input$nn_run))
  robj <- .nn()
  if (!is.list(robj)) {
    return()
  }
  fixed <- fix_names(input$nn_store_res_name)
  updateTextInput(session, "nn_store_res_name", value = fixed)
  withProgress(
    message = "Storing residuals", value = 1,
    r_data[[input$dataset]] <- store(r_data[[input$dataset]], robj, name = fixed)
  )
})

observeEvent(input$nn_store_pred, {
  req(!is.empty(input$nn_pred_data), pressed(input$nn_run))
  pred <- .predict_nn()
  if (is.null(pred)) {
    return()
  }
  fixed <- fix_names(input$nn_store_pred_name)
  updateTextInput(session, "nn_store_pred_name", value = fixed)
  withProgress(
    message = "Storing predictions", value = 1,
    r_data[[input$nn_pred_data]] <- store(
      r_data[[input$nn_pred_data]], pred,
      name = fixed
    )
  )
})

nn_report <- function() {
  if (is.empty(input$nn_evar)) {
    return(invisible())
  }

  outputs <- c("summary")
  inp_out <- list(list(prn = TRUE), "")
  figs <- FALSE

  if (!is.empty(input$nn_plots, "none")) {
    inp <- check_plot_inputs(nn_plot_inputs())
    inp$size <- NULL
    inp_out[[2]] <- clean_args(inp, nn_plot_args[-1])
    inp_out[[2]]$custom <- FALSE
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }

  if (!is.empty(input$nn_store_res_name)) {
    fixed <- fix_names(input$nn_store_res_name)
    updateTextInput(session, "nn_store_res_name", value = fixed)
    xcmd <- paste0(input$dataset, " <- store(", input$dataset, ", result, name = \"", fixed, "\")\n")
  } else {
    xcmd <- ""
  }

  if (!is.empty(input$nn_predict, "none") &&
    (!is.empty(input$nn_pred_data) || !is.empty(input$nn_pred_cmd))) {
    pred_args <- clean_args(nn_pred_inputs(), nn_pred_args[-1])

    if (!is.empty(pred_args$pred_cmd)) {
      pred_args$pred_cmd <- strsplit(pred_args$pred_cmd, ";\\s*")[[1]]
    } else {
      pred_args$pred_cmd <- NULL
    }

    if (!is.empty(pred_args$pred_data)) {
      pred_args$pred_data <- as.symbol(pred_args$pred_data)
    } else {
      pred_args$pred_data <- NULL
    }

    inp_out[[2 + figs]] <- pred_args
    outputs <- c(outputs, "pred <- predict")
    xcmd <- paste0(xcmd, "print(pred, n = 10)")
    if (input$nn_predict %in% c("data", "datacmd")) {
      fixed <- fix_names(input$nn_store_pred_name)
      updateTextInput(session, "nn_store_pred_name", value = fixed)
      xcmd <- paste0(
        xcmd, "\n", input$nn_pred_data, " <- store(",
        input$nn_pred_data, ", pred, name = \"", fixed, "\")"
      )
    }

    if (input$nn_pred_plot && !is.empty(input$nn_xvar)) {
      inp_out[[3 + figs]] <- clean_args(nn_pred_plot_inputs(), nn_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  nn_inp <- nn_inputs()
  if (input$nn_type == "regression") {
    nn_inp$lev <- NULL
  }

  update_report(
    inp_main = clean_args(nn_inp, nn_args),
    fun_name = "nn",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = nn_plot_width(),
    fig.height = nn_plot_height(),
    xcmd = xcmd
  )
}

dl_nn_pred <- function(path) {
  if (pressed(input$nn_run)) {
    write.csv(.predict_nn(), file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_nn_pred",
  fun = dl_nn_pred,
  fn = function() paste0(input$dataset, "_nn_pred"),
  type = "csv",
  caption = "Save predictions"
)

download_handler(
  id = "dlp_nn_pred",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_nn_pred"),
  type = "png",
  caption = "Save neural network prediction plot",
  plot = .predict_plot_nn,
  width = plot_width,
  height = nn_pred_plot_height
)

download_handler(
  id = "dlp_nn",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_nn"),
  type = "png",
  caption = "Save neural network plot",
  plot = .plot_nn,
  width = nn_plot_width,
  height = nn_plot_height
)

observeEvent(input$nn_report, {
  r_info[["latest_screenshot"]] <- NULL
  nn_report()
})

observeEvent(input$nn_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_nn_screenshot")
})

observeEvent(input$modal_nn_screenshot, {
  nn_report()
  removeModal() ## remove shiny modal after save
})
