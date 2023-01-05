nb_plots <- c(
  "None" = "none",
  "Variable importance" = "vimp",
  "Correlations" = "correlations"
)

## list of function arguments
nb_args <- as.list(formals(nb))

## list of function inputs selected by user
nb_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  nb_args$data_filter <- if (input$show_filter) input$data_filter else ""
  nb_args$arr <- if (input$show_filter) input$data_arrange else ""
  nb_args$rows <- if (input$show_filter) input$data_rows else ""
  nb_args$dataset <- input$dataset
  for (i in r_drop(names(nb_args))) {
    nb_args[[i]] <- input[[paste0("nb_", i)]]
  }
  nb_args
})

nb_plot_args <- as.list(if (exists("plot.nb")) {
  formals(plot.nb)
} else {
  formals(radiant.model:::plot.nb)
})

## list of function inputs selected by user
nb_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(nb_plot_args)) {
    nb_plot_args[[i]] <- input[[paste0("nb_", i)]]
  }
  nb_plot_args
})

nb_pred_args <- as.list(if (exists("predict.nb")) {
  formals(predict.nb)
} else {
  formals(radiant.model:::predict.nb)
})

## list of function inputs selected by user
nb_pred_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(nb_pred_args)) {
    nb_pred_args[[i]] <- input[[paste0("nb_", i)]]
  }

  nb_pred_args$pred_cmd <- nb_pred_args$pred_data <- ""
  if (input$nb_predict == "cmd") {
    nb_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$nb_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$nb_predict == "data") {
    nb_pred_args$pred_data <- input$nb_pred_data
  } else if (input$nb_predict == "datacmd") {
    nb_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$nb_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    nb_pred_args$pred_data <- input$nb_pred_data
  }
  nb_pred_args
})

nb_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
  formals(plot.model.predict)
} else {
  formals(radiant.model:::plot.model.predict)
})

## list of function inputs selected by user
nb_pred_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(nb_pred_plot_args)) {
    nb_pred_plot_args[[i]] <- input[[paste0("nb_", i)]]
  }
  nb_pred_plot_args
})

output$ui_nb_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    isFct <- "factor" == .get_class()
    vars <- varnames()[isFct]
  })

  init <- if (is.empty(input$logit_rvar)) isolate(input$nb_rvar) else input$logit_rvar
  selectInput(
    inputId = "nb_rvar", label = "Response variable:", choices = vars,
    selected = state_single("nb_rvar", vars, init), multiple = FALSE
  )
})

output$ui_nb_lev <- renderUI({
  req(available(input$nb_rvar))
  levs <- .get_data()[[input$nb_rvar]] %>%
    as.factor() %>%
    levels() %>%
    c("All levels", .)

  selectInput(
    inputId = "nb_lev", label = "Choose level:",
    choices = levs, selected = state_init("nb_lev", "")
  )
})

output$ui_nb_evar <- renderUI({
  req(available(input$nb_rvar))
  notVar <- .get_class() != "date"
  vars <- varnames()[notVar]
  if (length(vars) > 0 && input$nb_rvar %in% vars) {
    vars <- vars[-which(vars == input$nb_rvar)]
  }

  ## initialize to variables selected for logistic regression
  init <- if (is.empty(input$logit_evar)) isolate(input$nb_evar) else input$logit_evar
  selectInput(
    inputId = "nb_evar", label = "Explanatory variables:",
    choices = vars,
    selected = state_multiple("nb_evar", vars, init),
    multiple = TRUE, size = min(10, length(vars)),
    selectize = FALSE
  )
})

## reset prediction settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "nb_predict", selected = "none")
  updateSelectInput(session = session, inputId = "nb_plots", selected = "none")
})

output$ui_nb_store_pred_name <- renderUI({
  req(input$nb_rvar)
  levs <- .get_data()[[input$nb_rvar]] %>%
    as.factor() %>%
    levels() %>%
    fix_names() %>%
    paste(collapse = ", ")
  textInput(
    "nb_store_pred_name",
    "Store predictions:",
    state_init("nb_store_pred_name", levs)
  )
})

output$ui_nb_predict_plot <- renderUI({
  req(input$nb_rvar)
  var_colors <- ".class" %>% set_names(input$nb_rvar)
  predict_plot_controls("nb", vars_color = var_colors, init_color = ".class")
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(nb_args, "nb", tabs = "tabs_nb", label = "Estimate model", relabel = "Re-estimate model")

output$ui_nb <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(
      condition = "input.tabs_nb == 'Summary'",
      wellPanel(
        actionButton("nb_run", "Estimate model", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_nb == 'Summary'",
        uiOutput("ui_nb_rvar"),
        uiOutput("ui_nb_evar"),
        numericInput("nb_laplace", label = "Laplace:", min = 0, value = state_init("nb_laplace", 0))
      ),
      conditionalPanel(
        condition = "input.tabs_nb == 'Predict'",
        selectInput(
          "nb_predict",
          label = "Prediction input type:", reg_predict,
          selected = state_single("nb_predict", reg_predict, "none")
        ),
        conditionalPanel(
          "input.nb_predict == 'data' | input.nb_predict == 'datacmd'",
          selectizeInput(
            inputId = "nb_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("nb_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.nb_predict == 'cmd' | input.nb_predict == 'datacmd'",
          returnTextAreaInput(
            "nb_pred_cmd", "Prediction command:",
            value = state_init("nb_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., carat = 1; cut = 'Ideal') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.nb_predict != 'none'",
          checkboxInput("nb_pred_plot", "Plot predictions", state_init("nb_pred_plot", FALSE)),
          conditionalPanel(
            "input.nb_pred_plot == true",
            uiOutput("ui_nb_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.nb_predict == 'data' | input.nb_predict == 'datacmd'",
          tags$table(
            tags$td(uiOutput("ui_nb_store_pred_name")),
            tags$td(actionButton("nb_store_pred", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_nb == 'Plot'",
        selectInput(
          "nb_plots", "Plots:",
          choices = nb_plots,
          selected = state_single("nb_plots", nb_plots)
        ),
        conditionalPanel(
          condition = "input.nb_plots != 'none'",
          uiOutput("ui_nb_lev")
        )
      )
    ),
    help_and_report(
      modal_title = "Naive Bayes",
      fun_name = "nb",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/nb.md"))
    )
  )
})

nb_plot <- reactive({
  if (nb_available() != "available") {
    return()
  }
  if (is.empty(input$nb_plots, "none")) {
    return()
  }
  req(input$nb_lev)

  nb_res <- .nb()
  if (is.character(nb_res)) {
    return()
  }

  n_vars <- length(nb_res$vars)
  if (input$nb_plots == "correlations") {
    plot_height <- 150 * n_vars
    plot_width <- 150 * n_vars
  } else {
    if (input$nb_lev == "All levels") {
      n_lev <- length(nb_res$lev) - 1
    } else {
      n_lev <- 2
    }
    plot_height <- 300 + 20 * n_vars * n_lev
    plot_width <- 650
  }
  list(plot_width = plot_width, plot_height = plot_height)
})

nb_plot_width <- function() {
  nb_plot() %>%
    {
      if (is.list(.)) .$plot_width else 650
    }
}

nb_plot_height <- function() {
  nb_plot() %>%
    {
      if (is.list(.)) .$plot_height else 500
    }
}

nb_pred_plot_height <- function() {
  if (input$nb_pred_plot) 500 else 1
}

## output is called from the main radiant ui.R
output$nb <- renderUI({
  register_print_output("summary_nb", ".summary_nb")
  register_print_output("predict_nb", ".predict_print_nb")
  register_plot_output(
    "predict_plot_nb", ".predict_plot_nb",
    height_fun = "nb_pred_plot_height"
  )
  register_plot_output(
    "plot_nb", ".plot_nb",
    height_fun = "nb_plot_height",
    width_fun = "nb_plot_width"
  )

  ## two separate tabs
  nb_output_panels <- tabsetPanel(
    id = "tabs_nb",
    tabPanel("Summary", verbatimTextOutput("summary_nb")),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.nb_pred_plot == true",
        download_link("dlp_nb_pred"),
        plotOutput("predict_plot_nb", width = "100%", height = "100%")
      ),
      download_link("dl_nb_pred"), br(),
      verbatimTextOutput("predict_nb")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_nb"),
      plotOutput("plot_nb", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Estimate",
    tool = "Naive Bayes",
    tool_ui = "ui_nb",
    output_panels = nb_output_panels
  )
})

nb_available <- reactive({
  if (not_available(input$nb_rvar)) {
    return("This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))
  } else if (not_available(input$nb_evar)) {
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("titanic"))
  } else {
    "available"
  }
})

.nb <- eventReactive(input$nb_run, {
  withProgress(message = "Estimating model", value = 1, {
    nbi <- nb_inputs()
    nbi$envir <- r_data
    do.call(nb, nbi)
  })
})

.summary_nb <- reactive({
  if (not_pressed(input$nb_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (nb_available() != "available") {
    return(nb_available())
  }
  summary(.nb())
})

.predict_nb <- reactive({
  if (nb_available() != "available") {
    return(nb_available())
  }
  if (not_pressed(input$nb_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (is.empty(input$nb_predict, "none")) {
    return("** Select prediction input **")
  }

  if ((input$nb_predict == "data" || input$nb_predict == "datacmd") && is.empty(input$nb_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$nb_predict == "cmd" && is.empty(input$nb_pred_cmd)) {
    return("** Enter prediction commands **")
  }

  withProgress(message = "Generating predictions", value = 1, {
    nbi <- nb_pred_inputs()
    nbi$object <- .nb()
    nbi$envir <- r_data
    do.call(predict, nbi)
  })
})

.predict_print_nb <- reactive({
  .predict_nb() %>%
    {
      if (is.character(.)) cat(., "\n") else print(.)
    }
})

.predict_plot_nb <- reactive({
  req(
    pressed(input$nb_run), input$nb_pred_plot,
    available(input$nb_xvar),
    !is.empty(input$nb_predict, "none")
  )

  ## needs more testing ...
  # if (nb_available() != "available") return(nb_available())
  # # req(input$nb_pred_plot, available(input$nb_xvar))
  # if (not_pressed(input$nb_run)) return(invisible())
  # if (is.empty(input$nb_predict, "none")) return(invisible())
  # if ((input$nb_predict == "data" || input$nb_predict == "datacmd") && is.empty(input$nb_pred_data)) {
  #   return(invisible())
  # }
  # if (input$nb_predict == "cmd" && is.empty(input$nb_pred_cmd)) {
  #   return(invisible())
  # }

  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_nb()), nb_pred_plot_inputs()))
  })
})

.plot_nb <- reactive({
  if (not_pressed(input$nb_run)) {
    return("** Press the Estimate button to estimate the model **")
  } else if (is.empty(input$nb_plots, "none")) {
    return("Please select a naive Bayes plot from the drop-down menu")
  } else if (nb_available() != "available") {
    return(nb_available())
  }
  req(input$nb_lev)
  if (input$nb_plots == "correlations") {
    capture_plot(do.call(plot, c(list(x = .nb()), nb_plot_inputs())))
  } else {
    withProgress(message = "Generating plots", value = 1, {
      do.call(plot, c(list(x = .nb()), nb_plot_inputs(), shiny = TRUE))
    })
  }
})

observeEvent(input$nb_store_pred, {
  req(!is.empty(input$nb_pred_data), pressed(input$nb_run))
  pred <- .predict_nb()
  if (is.null(pred)) {
    return()
  }
  fixed <- unlist(strsplit(input$nb_store_pred_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
    fix_names() %>%
    paste0(collapse = ", ")
  updateTextInput(session, "nb_store_pred_name", value = fixed)
  withProgress(
    message = "Storing predictions", value = 1,
    r_data[[input$nb_pred_data]] <- store(
      r_data[[input$nb_pred_data]], pred,
      name = fixed
    )
  )
})

nb_report <- function() {
  if (is.empty(input$nb_evar)) {
    return(invisible())
  }
  outputs <- c("summary")
  inp_out <- list("", "")
  figs <- FALSE
  if (!is.empty(input$nb_plots, "none")) {
    inp <- check_plot_inputs(nb_plot_inputs())
    inp_out[[2]] <- clean_args(inp, nb_plot_args[-1])
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }
  xcmd <- ""
  if (!is.empty(input$nb_predict, "none") &&
    (!is.empty(input$nb_pred_data) || !is.empty(input$nb_pred_cmd))) {
    pred_args <- clean_args(nb_pred_inputs(), nb_pred_args[-1])

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
    xcmd <- paste0("print(pred, n = 10)")
    if (input$nb_predict %in% c("data", "datacmd")) {
      name <- input$nb_store_pred_name
      if (!is.empty(name)) {
        name <- unlist(strsplit(input$nb_store_pred_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
          fix_names() %>%
          deparse(., control = getOption("dctrl"), width.cutoff = 500L)
      }
      xcmd <- paste0(
        xcmd, "\n", input$nb_pred_data, " <- store(",
        input$nb_pred_data, ", pred, name = ", name, ")"
      )
    }

    if (input$nb_pred_plot && !is.empty(input$nb_xvar)) {
      inp_out[[3 + figs]] <- clean_args(nb_pred_plot_inputs(), nb_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  update_report(
    inp_main = clean_args(nb_inputs(), nb_args),
    fun_name = "nb",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = nb_plot_width(),
    fig.height = nb_plot_height(),
    xcmd = xcmd
  )
}

dl_nb_pred <- function(path) {
  if (pressed(input$nb_run)) {
    write.csv(.predict_nb(), file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_nb_pred",
  fun = dl_nb_pred,
  fn = function() paste0(input$dataset, "_nb_pred"),
  type = "csv",
  caption = "Save predictions"
)

download_handler(
  id = "dlp_nb_pred",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_nb_pred"),
  type = "png",
  caption = "Save naive Bayes prediction plot",
  plot = .predict_plot_nb,
  width = plot_width,
  height = nb_pred_plot_height
)

download_handler(
  id = "dlp_nb",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_nb"),
  type = "png",
  caption = "Save naive Bayes plot",
  plot = .plot_nb,
  width = nb_plot_width,
  height = nb_plot_height
)

observeEvent(input$nb_report, {
  r_info[["latest_screenshot"]] <- NULL
  nb_report()
})

observeEvent(input$nb_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_nb_screenshot")
})

observeEvent(input$modal_nb_screenshot, {
  nb_report()
  removeModal() ## remove shiny modal after save
})