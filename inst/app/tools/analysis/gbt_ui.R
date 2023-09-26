gbt_plots <- c(
  "None" = "none",
  "Permutation Importance" = "vip",
  "Prediction plots" = "pred_plot",
  "Partial Dependence" = "pdp",
  "Dashboard" = "dashboard"
)

## list of function arguments
gbt_args <- as.list(formals(gbt))

## list of function inputs selected by user
gbt_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  gbt_args$data_filter <- if (input$show_filter) input$data_filter else ""
  gbt_args$arr <- if (input$show_filter) input$data_arrange else ""
  gbt_args$rows <- if (input$show_filter) input$data_rows else ""
  gbt_args$dataset <- input$dataset
  for (i in r_drop(names(gbt_args))) {
    gbt_args[[i]] <- input[[paste0("gbt_", i)]]
  }
  gbt_args
})

gbt_plot_args <- as.list(if (exists("plot.gbt")) {
  formals(plot.gbt)
} else {
  formals(radiant.model:::plot.gbt)
})

## list of function inputs selected by user
gbt_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(gbt_plot_args)) {
    gbt_plot_args[[i]] <- input[[paste0("gbt_", i)]]
  }
  gbt_plot_args
})

gbt_pred_args <- as.list(if (exists("predict.gbt")) {
  formals(predict.gbt)
} else {
  formals(radiant.model:::predict.gbt)
})

# list of function inputs selected by user
gbt_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(gbt_pred_args)) {
    gbt_pred_args[[i]] <- input[[paste0("gbt_", i)]]
  }

  gbt_pred_args$pred_cmd <- gbt_pred_args$pred_data <- ""
  if (input$gbt_predict == "cmd") {
    gbt_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$gbt_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$gbt_predict == "data") {
    gbt_pred_args$pred_data <- input$gbt_pred_data
  } else if (input$gbt_predict == "datacmd") {
    gbt_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$gbt_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    gbt_pred_args$pred_data <- input$gbt_pred_data
  }
  gbt_pred_args
})

gbt_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
  formals(plot.model.predict)
} else {
  formals(radiant.model:::plot.model.predict)
})

# list of function inputs selected by user
gbt_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(gbt_pred_plot_args)) {
    gbt_pred_plot_args[[i]] <- input[[paste0("gbt_", i)]]
  }
  gbt_pred_plot_args
})

output$ui_gbt_rvar <- renderUI({
  req(input$gbt_type)

  withProgress(message = "Acquiring variable information", value = 1, {
    if (input$gbt_type == "classification") {
      vars <- two_level_vars()
    } else {
      isNum <- .get_class() %in% c("integer", "numeric", "ts")
      vars <- varnames()[isNum]
    }
  })

  init <- if (input$gbt_type == "classification") {
    if (is.empty(input$logit_rvar)) isolate(input$gbt_rvar) else input$logit_rvar
  } else {
    if (is.empty(input$reg_rvar)) isolate(input$gbt_rvar) else input$reg_rvar
  }

  selectInput(
    inputId = "gbt_rvar",
    label = "Response variable:",
    choices = vars,
    selected = state_single("gbt_rvar", vars, init),
    multiple = FALSE
  )
})

output$ui_gbt_lev <- renderUI({
  req(input$gbt_type == "classification")
  req(available(input$gbt_rvar))
  levs <- .get_data()[[input$gbt_rvar]] %>%
    as_factor() %>%
    levels()

  init <- if (is.empty(input$logit_lev)) isolate(input$gbt_lev) else input$logit_lev
  selectInput(
    inputId = "gbt_lev", label = "Choose first level:",
    choices = levs,
    selected = state_init("gbt_lev", init)
  )
})

output$ui_gbt_evar <- renderUI({
  if (not_available(input$gbt_rvar)) {
    return()
  }
  vars <- varnames()
  if (length(vars) > 0) {
    vars <- vars[-which(vars == input$gbt_rvar)]
  }

  init <- if (input$gbt_type == "classification") {
    # input$logit_evar
    if (is.empty(input$logit_evar)) isolate(input$gbt_evar) else input$logit_evar
  } else {
    # input$reg_evar
    if (is.empty(input$reg_evar)) isolate(input$gbt_evar) else input$reg_evar
  }

  selectInput(
    inputId = "gbt_evar",
    label = "Explanatory variables:",
    choices = vars,
    selected = state_multiple("gbt_evar", vars, init),
    multiple = TRUE,
    size = min(10, length(vars)),
    selectize = FALSE
  )
})

# function calls generate UI elements
output_incl("gbt")
output_incl_int("gbt")

output$ui_gbt_wts <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$gbt_evar)) {
    vars <- base::setdiff(vars, input$gbt_evar)
    names(vars) <- varnames() %>%
      (function(x) x[match(vars, x)]) %>%
      names()
  }
  vars <- c("None", vars)

  selectInput(
    inputId = "gbt_wts", label = "Weights:", choices = vars,
    selected = state_single("gbt_wts", vars),
    multiple = FALSE
  )
})

output$ui_gbt_store_pred_name <- renderUI({
  init <- state_init("gbt_store_pred_name", "pred_gbt")
  textInput(
    "gbt_store_pred_name",
    "Store predictions:",
    init
  )
})

# output$ui_gbt_store_res_name <- renderUI({
#   req(input$dataset)
#   textInput("gbt_store_res_name", "Store residuals:", "", placeholder = "Provide variable name")
# })

## reset prediction and plot settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "gbt_predict", selected = "none")
  updateSelectInput(session = session, inputId = "gbt_plots", selected = "none")
})

## reset prediction settings when the model type changes
observeEvent(input$gbt_type, {
  updateSelectInput(session = session, inputId = "gbt_predict", selected = "none")
  updateSelectInput(session = session, inputId = "gbt_plots", selected = "none")
})

output$ui_gbt_predict_plot <- renderUI({
  predict_plot_controls("gbt")
})

output$ui_gbt_plots <- renderUI({
  req(input$gbt_type)
  if (input$gbt_type != "regression") {
    gbt_plots <- head(gbt_plots, -1)
  }
  selectInput(
    "gbt_plots", "Plots:",
    choices = gbt_plots,
    selected = state_single("gbt_plots", gbt_plots)
  )
})

output$ui_gbt_nrobs <- renderUI({
  nrobs <- nrow(.get_data())
  choices <- c("1,000" = 1000, "5,000" = 5000, "10,000" = 10000, "All" = -1) %>%
    .[. < nrobs]
  selectInput(
    "gbt_nrobs", "Number of data points plotted:",
    choices = choices,
    selected = state_single("gbt_nrobs", choices, 1000)
  )
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(gbt_args, "gbt", tabs = "tabs_gbt", label = "Estimate model", relabel = "Re-estimate model")

output$ui_gbt <- renderUI({
  # req(input$dataset)
  tagList(
    conditionalPanel(
      condition = "input.tabs_gbt == 'Summary'",
      wellPanel(
        actionButton("gbt_run", "Estimate model", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_gbt == 'Summary'",
        radioButtons(
          "gbt_type",
          label = NULL, c("classification", "regression"),
          selected = state_init("gbt_type", "classification"),
          inline = TRUE
        ),
        uiOutput("ui_gbt_rvar"),
        uiOutput("ui_gbt_lev"),
        uiOutput("ui_gbt_evar"),
        uiOutput("ui_gbt_wts"),
        with(tags, table(
          tr(
            td(numericInput(
              "gbt_max_depth",
              label = "Max depth:", min = 1, max = 20,
              value = state_init("gbt_max_depth", 6)
            ), width = "50%"),
            td(numericInput(
              "gbt_learning_rate",
              label = "Learning rate:", min = 0, max = 1, step = 0.1,
              value = state_init("gbt_learning_rate", 0.3)
            ), width = "50%")
          ),
          width = "100%"
        )),
        with(tags, table(
          tr(
            td(numericInput(
              "gbt_min_split_loss",
              label = "Min split loss:", min = 0.00001, max = 1000,
              step = 0.01, value = state_init("gbt_min_split_loss", 0)
            ), width = "50%"),
            td(numericInput(
              "gbt_min_child_weight",
              label = "Min child weight:", min = 1, max = 100,
              step = 1, value = state_init("gbt_min_child_weight", 1)
            ), width = "50%")
          ),
          width = "100%"
        )),
        with(tags, table(
          tr(
            td(numericInput(
              "gbt_subsample",
              label = "Sub-sample:", min = 0.1, max = 1,
              value = state_init("gbt_subsample", 1)
            ), width = "50%"),
            td(numericInput(
              "gbt_nrounds",
              label = "# rounds:",
              value = state_init("gbt_nrounds", 100)
            ), width = "50%")
          ),
          width = "100%"
        )),
        with(tags, table(
          tr(
            td(numericInput(
              "gbt_early_stopping_rounds",
              label = "Early stopping:", min = 1, max = 10,
              step = 1, value = state_init("gbt_early_stopping_rounds", 3)
            ), width = "50%"),
            td(numericInput(
              "gbt_seed",
              label = "Seed:",
              value = state_init("gbt_seed", 1234)
            ), width = "50%")
          ),
          width = "100%"
        ))
      ),
      conditionalPanel(
        condition = "input.tabs_gbt == 'Predict'",
        selectInput(
          "gbt_predict",
          label = "Prediction input type:", reg_predict,
          selected = state_single("gbt_predict", reg_predict, "none")
        ),
        conditionalPanel(
          "input.gbt_predict == 'data' | input.gbt_predict == 'datacmd'",
          selectizeInput(
            inputId = "gbt_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("gbt_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.gbt_predict == 'cmd' | input.gbt_predict == 'datacmd'",
          returnTextAreaInput(
            "gbt_pred_cmd", "Prediction command:",
            value = state_init("gbt_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., carat = 1; cut = 'Ideal') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.gbt_predict != 'none'",
          checkboxInput("gbt_pred_plot", "Plot predictions", state_init("gbt_pred_plot", FALSE)),
          conditionalPanel(
            "input.gbt_pred_plot == true",
            uiOutput("ui_gbt_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.gbt_predict == 'data' | input.gbt_predict == 'datacmd'",
          tags$table(
            tags$td(uiOutput("ui_gbt_store_pred_name")),
            tags$td(actionButton("gbt_store_pred", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_gbt == 'Plot'",
        uiOutput("ui_gbt_plots"),
        conditionalPanel(
          condition = "input.gbt_plots == 'dashboard'",
          uiOutput("ui_gbt_nrobs")
        ),
        conditionalPanel(
          condition = "input.gbt_plots == 'pdp' | input.gbt_plots == 'pred_plot'",
          uiOutput("ui_gbt_incl"),
          uiOutput("ui_gbt_incl_int")
        )
      ),
      # conditionalPanel(
      #   condition = "input.tabs_gbt == 'Summary'",
      #   tags$table(
      #     tags$td(uiOutput("ui_gbt_store_res_name")),
      #     tags$td(actionButton("gbt_store_res", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
      #   )
      # )
    ),
    help_and_report(
      modal_title = "Gradient Boosted Trees",
      fun_name = "gbt",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/gbt.md"))
    )
  )
})

gbt_plot <- reactive({
  # req(input$gbt_plots)
  if (gbt_available() != "available") {
    return()
  }
  if (is.empty(input$gbt_plots, "none")) {
    return()
  }
  res <- .gbt()
  if (is.character(res)) {
    return()
  }
  nr_vars <- length(res$evar)
  plot_height <- 500
  plot_width <- 650
  if ("dashboard" %in% input$gbt_plots) {
    plot_height <- 750
  } else if (input$gbt_plots %in% c("pdp", "pred_plot")) {
    nr_vars <- length(input$gbt_incl) + length(input$gbt_incl_int)
    plot_height <- max(250, ceiling(nr_vars / 2) * 250)
    if (length(input$gbt_incl_int) > 0) {
      plot_width <- plot_width + min(2, length(input$gbt_incl_int)) * 90
    }
  } else if ("vimp" %in% input$rf_plots) {
    plot_height <- max(500, nr_vars * 35)
  } else if ("vip" %in% input$rf_plots) {
    plot_height <- max(500, nr_vars * 35)
  }

  list(plot_width = plot_width, plot_height = plot_height)
})

gbt_plot_width <- function() {
  gbt_plot() %>%
    (function(x) if (is.list(x)) x$plot_width else 650)
}

gbt_plot_height <- function() {
  gbt_plot() %>%
    (function(x) if (is.list(x)) x$plot_height else 500)
}

gbt_pred_plot_height <- function() {
  if (input$gbt_pred_plot) 500 else 1
}

## output is called from the main radiant ui.R
output$gbt <- renderUI({
  register_print_output("summary_gbt", ".summary_gbt")
  register_print_output("predict_gbt", ".predict_print_gbt")
  register_plot_output(
    "predict_plot_gbt", ".predict_plot_gbt",
    height_fun = "gbt_pred_plot_height"
  )
  register_plot_output(
    "plot_gbt", ".plot_gbt",
    height_fun = "gbt_plot_height",
    width_fun = "gbt_plot_width"
  )

  ## three separate tabs
  gbt_output_panels <- tabsetPanel(
    id = "tabs_gbt",
    tabPanel(
      "Summary",
      verbatimTextOutput("summary_gbt")
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.gbt_pred_plot == true",
        download_link("dlp_gbt_pred"),
        plotOutput("predict_plot_gbt", width = "100%", height = "100%")
      ),
      download_link("dl_gbt_pred"), br(),
      verbatimTextOutput("predict_gbt")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_gbt"),
      plotOutput("plot_gbt", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Trees",
    tool = "Gradient Boosted Trees",
    tool_ui = "ui_gbt",
    output_panels = gbt_output_panels
  )
})

gbt_available <- reactive({
  req(input$gbt_type)
  if (not_available(input$gbt_rvar)) {
    if (input$gbt_type == "classification") {
      "This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>%
        suggest_data("titanic")
    } else {
      "This analysis requires a response variable of type integer\nor numeric and one or more explanatory variables.\nIf these variables are not available please select another dataset.\n\n" %>%
        suggest_data("diamonds")
    }
  } else if (not_available(input$gbt_evar)) {
    if (input$gbt_type == "classification") {
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

.gbt <- eventReactive(input$gbt_run, {
  gbti <- gbt_inputs()
  gbti$envir <- r_data
  if (is.empty(gbti$max_depth)) gbti$max_depth <- 6
  if (is.empty(gbti$learning_rate)) gbti$learning_rate <- 0.3
  if (is.empty(gbti$min_split_loss)) gbti$min_split_loss <- 0.01
  if (is.empty(gbti$min_child_weight)) gbti$min_child_weight <- 1
  if (is.empty(gbti$subsample)) gbti$subsample <- 1
  if (is.empty(gbti$nrounds)) gbti$nrounds <- 100
  if (is.empty(gbti$early_stopping_rounds)) gbti["early_stopping_rounds"] <- list(NULL)

  withProgress(
    message = "Estimating model", value = 1,
    do.call(gbt, gbti)
  )
})

.summary_gbt <- reactive({
  if (not_pressed(input$gbt_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (gbt_available() != "available") {
    return(gbt_available())
  }
  summary(.gbt())
})

.predict_gbt <- reactive({
  if (not_pressed(input$gbt_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (gbt_available() != "available") {
    return(gbt_available())
  }
  if (is.empty(input$gbt_predict, "none")) {
    return("** Select prediction input **")
  }

  if ((input$gbt_predict == "data" || input$gbt_predict == "datacmd") && is.empty(input$gbt_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$gbt_predict == "cmd" && is.empty(input$gbt_pred_cmd)) {
    return("** Enter prediction commands **")
  }

  withProgress(message = "Generating predictions", value = 1, {
    gbti <- gbt_pred_inputs()
    gbti$object <- .gbt()
    gbti$envir <- r_data
    do.call(predict, gbti)
  })
})

.predict_print_gbt <- reactive({
  .predict_gbt() %>%
    (function(x) if (is.character(x)) cat(x, "\n") else print(x))
})

.predict_plot_gbt <- reactive({
  req(
    pressed(input$gbt_run), input$gbt_pred_plot,
    available(input$gbt_xvar),
    !is.empty(input$gbt_predict, "none")
  )

  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_gbt()), gbt_pred_plot_inputs()))
  })
})

.plot_gbt <- reactive({
  if (not_pressed(input$gbt_run)) {
    return("** Press the Estimate button to estimate the model **")
  } else if (gbt_available() != "available") {
    return(gbt_available())
  } else if (is.empty(input$gbt_plots, "none")) {
    return("Please select a gradient boosted trees plot from the drop-down menu")
  }
  # pinp <- list(plots = input$gbt_plots, shiny = TRUE)
  # if (input$gbt_plots == "dashboard") {
  #   req(input$gbt_nrobs)
  #   pinp <- c(pinp, nrobs = as_integer(input$gbt_nrobs))
  # } else if (input$gbt_plots == "pdp") {
  #   pinp <- c(pinp)
  # }
  pinp <- gbt_plot_inputs()
  pinp$shiny <- TRUE
  if (input$gbt_plots == "dashboard") {
    req(input$gbt_nrobs)
  }
  check_for_pdp_pred_plots("gbt")
  withProgress(message = "Generating plots", value = 1, {
    do.call(plot, c(list(x = .gbt()), pinp))
  })
})

# observeEvent(input$gbt_store_res, {
#   req(pressed(input$gbt_run))
#   robj <- .gbt()
#   if (!is.list(robj)) return()
#   fixed <- fix_names(input$gbt_store_res_name)
#   updateTextInput(session, "gbt_store_res_name", value = fixed)
#   withProgress(
#     message = "Storing residuals", value = 1,
#     r_data[[input$dataset]] <- store(r_data[[input$dataset]], robj, name = fixed)
#   )
# })

observeEvent(input$gbt_store_pred, {
  req(!is.empty(input$gbt_pred_data), pressed(input$gbt_run))
  pred <- .predict_gbt()
  if (is.null(pred)) {
    return()
  }
  fixed <- fix_names(input$gbt_store_pred_name)
  updateTextInput(session, "gbt_store_pred_name", value = fixed)
  withProgress(
    message = "Storing predictions", value = 1,
    r_data[[input$gbt_pred_data]] <- store(
      r_data[[input$gbt_pred_data]], pred,
      name = fixed
    )
  )
})

gbt_report <- function() {
  if (is.empty(input$gbt_rvar)) {
    return(invisible())
  }

  outputs <- c("summary")
  inp_out <- list(list(prn = TRUE), "")
  figs <- FALSE

  if (!is.empty(input$gbt_plots, "none")) {
    inp <- check_plot_inputs(gbt_plot_inputs())
    inp_out[[2]] <- clean_args(inp, gbt_plot_args[-1])
    inp_out[[2]]$custom <- FALSE
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }

  if (!is.empty(input$gbt_store_res_name)) {
    fixed <- fix_names(input$gbt_store_res_name)
    updateTextInput(session, "gbt_store_res_name", value = fixed)
    xcmd <- paste0(input$dataset, " <- store(", input$dataset, ", result, name = \"", fixed, "\")\n")
  } else {
    xcmd <- ""
  }

  if (!is.empty(input$gbt_predict, "none") &&
    (!is.empty(input$gbt_pred_data) || !is.empty(input$gbt_pred_cmd))) {
    pred_args <- clean_args(gbt_pred_inputs(), gbt_pred_args[-1])

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
    if (input$gbt_predict %in% c("data", "datacmd")) {
      fixed <- fix_names(input$gbt_store_pred_name)
      updateTextInput(session, "gbt_store_pred_name", value = fixed)
      xcmd <- paste0(
        xcmd, "\n", input$gbt_pred_data, " <- store(",
        input$gbt_pred_data, ", pred, name = \"", fixed, "\")"
      )
    }

    if (input$gbt_pred_plot && !is.empty(input$gbt_xvar)) {
      inp_out[[3 + figs]] <- clean_args(gbt_pred_plot_inputs(), gbt_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  gbt_inp <- gbt_inputs()
  if (input$gbt_type == "regression") {
    gbt_inp$lev <- NULL
  }

  update_report(
    inp_main = clean_args(gbt_inp, gbt_args),
    fun_name = "gbt",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = gbt_plot_width(),
    fig.height = gbt_plot_height(),
    xcmd = xcmd
  )
}

dl_gbt_pred <- function(path) {
  if (pressed(input$gbt_run)) {
    write.csv(.predict_gbt(), file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_gbt_pred",
  fun = dl_gbt_pred,
  fn = function() paste0(input$dataset, "_gbt_pred"),
  type = "csv",
  caption = "Save predictions"
)

download_handler(
  id = "dlp_gbt_pred",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_gbt_pred"),
  type = "png",
  caption = "Save gradient boosted trees prediction plot",
  plot = .predict_plot_gbt,
  width = plot_width,
  height = gbt_pred_plot_height
)

download_handler(
  id = "dlp_gbt",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_gbt"),
  type = "png",
  caption = "Save gradient boosted trees plot",
  plot = .plot_gbt,
  width = gbt_plot_width,
  height = gbt_plot_height
)

observeEvent(input$gbt_report, {
  r_info[["latest_screenshot"]] <- NULL
  gbt_report()
})

observeEvent(input$gbt_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_gbt_screenshot")
})

observeEvent(input$modal_gbt_screenshot, {
  gbt_report()
  removeModal() ## remove shiny modal after save
})
