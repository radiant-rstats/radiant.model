ctree_plots <- c(
  "None" = "none", "Prune" = "prune",
  "Tree" = "tree",
  "Permutation Importance" = "vip",
  "Prediction plots" = "pred_plot",
  "Partial Dependence" = "pdp",
  "Dashboard" = "dashboard"
)

## list of function arguments
crtree_args <- as.list(formals(crtree))

## list of function inputs selected by user
crtree_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  crtree_args$data_filter <- if (input$show_filter) input$data_filter else ""
  crtree_args$arr <- if (input$show_filter) input$data_arrange else ""
  crtree_args$rows <- if (input$show_filter) input$data_rows else ""
  crtree_args$dataset <- input$dataset
  for (i in r_drop(names(crtree_args))) {
    crtree_args[[i]] <- input[[paste0("crtree_", i)]]
  }
  crtree_args
})

crtree_pred_args <- as.list(if (exists("predict.crtree")) {
  formals(predict.crtree)
} else {
  formals(radiant.model:::predict.crtree)
})

# list of function inputs selected by user
crtree_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(crtree_pred_args)) {
    crtree_pred_args[[i]] <- input[[paste0("crtree_", i)]]
  }

  crtree_pred_args$pred_cmd <- crtree_pred_args$pred_data <- ""
  if (input$crtree_predict == "cmd") {
    crtree_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$crtree_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$crtree_predict == "data") {
    crtree_pred_args$pred_data <- input$crtree_pred_data
  } else if (input$crtree_predict == "datacmd") {
    crtree_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$crtree_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    crtree_pred_args$pred_data <- input$crtree_pred_data
  }
  crtree_pred_args
})

crtree_plot_args <- as.list(if (exists("plot.crtree")) {
  formals(plot.crtree)
} else {
  formals(radiant.model:::plot.crtree)
})

## list of function inputs selected by user
crtree_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(crtree_plot_args)) {
    crtree_plot_args[[i]] <- input[[paste0("crtree_", i)]]
  }
  crtree_plot_args
})

crtree_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
  formals(plot.model.predict)
} else {
  formals(radiant.model:::plot.model.predict)
})

# list of function inputs selected by user
crtree_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(crtree_pred_plot_args)) {
    crtree_pred_plot_args[[i]] <- input[[paste0("crtree_", i)]]
  }
  crtree_pred_plot_args
})

output$ui_crtree_rvar <- renderUI({
  req(input$crtree_type)

  withProgress(message = "Acquiring variable information", value = 1, {
    if (input$crtree_type == "classification") {
      vars <- two_level_vars()
    } else {
      isNum <- .get_class() %in% c("integer", "numeric", "ts")
      vars <- varnames()[isNum]
    }
  })
  selectInput(
    inputId = "crtree_rvar", label = "Response variable:", choices = vars,
    selected = state_single("crtree_rvar", vars), multiple = FALSE
  )
})

output$ui_crtree_lev <- renderUI({
  req(input$crtree_type == "classification")
  req(available(input$crtree_rvar))
  levs <- .get_data()[[input$crtree_rvar]] %>%
    as.factor() %>%
    levels()

  selectInput(
    inputId = "crtree_lev", label = "Choose level:",
    choices = levs, selected = state_init("crtree_lev")
  )
})

output$ui_crtree_evar <- renderUI({
  req(available(input$crtree_rvar))
  vars <- varnames()
  if (length(vars) > 0) {
    vars <- vars[-which(vars == input$crtree_rvar)]
  }

  init <- if (input$crtree_type == "classification") input$logit_evar else input$reg_evar
  selectInput(
    inputId = "crtree_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("crtree_evar", vars, init),
    multiple = TRUE, size = min(10, length(vars)), selectize = FALSE
  )
})

output_incl <- function(model) {
  output[[glue("ui_{model}_incl")]] <- renderUI({
    req(available(input[[glue("{model}_evar")]]))
    vars <- input[[glue("{model}_evar")]]
    id <- glue("{model}_incl")
    selectInput(
      inputId = id, label = "Explanatory variables to include:", choices = vars,
      selected = state_multiple(id, vars, c()),
      multiple = TRUE, size = min(10, length(vars)), selectize = FALSE
    )
  })
}

output_incl_int <- function(model) {
  output[[glue("ui_{model}_incl_int")]] <- renderUI({
    req(available(input[[glue("{model}_evar")]]))
    choices <- character(0)
    vars <- input[[glue("{model}_evar")]]
    id <- glue("{model}_incl_int")
    ## list of interaction terms to show
    if (length(vars) > 1) {
      choices <- c(choices, iterms(vars, 2))
    } else {
      updateSelectInput(session, glue("{model}_incl_int"), choices = choices, selected = choices)
      return()
    }
    selectInput(
      inputId = id,
      label = "2-way interactions to explore:",
      choices = choices,
      selected = state_multiple(id, choices),
      multiple = TRUE,
      size = min(8, length(choices)),
      selectize = FALSE
    )
  })
}

# function calls generate UI elements
output_incl("crtree")
output_incl_int("crtree")

output$ui_crtree_wts <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$crtree_evar)) {
    vars <- base::setdiff(vars, input$crtree_evar)
    names(vars) <- varnames() %>%
      (function(x) x[match(vars, x)]) %>%
      names()
  }
  vars <- c("None", vars)

  selectInput(
    inputId = "crtree_wts", label = "Weights:", choices = vars,
    selected = state_single("crtree_wts", vars),
    multiple = FALSE
  )
})

## reset prediction settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "crtree_predict", selected = "none")
  updateSelectInput(session = session, inputId = "crtree_plots", selected = "none")
})

observeEvent(input$crtree_cost, {
  if (!is.empty(input$crtree_cost)) {
    updateNumericInput(session = session, inputId = "crtree_prior", value = NA)
  }
})

output$ui_crtree_predict_plot <- renderUI({
  predict_plot_controls("crtree")
})

output$ui_crtree_width <- renderUI({
  init <- ifelse(is.empty(input$get_screen_width), 900, (input$get_screen_width - 400))
  init <- init - init %% 100
  numericInput(
    "crtree_width",
    label = "Width:",
    value = state_init("crtree_width", init),
    step = 100, min = 600, max = 3000
  )
})

output$ui_crtree_store_res_name <- renderUI({
  req(input$dataset)
  textInput("crtree_store_res_name", "Store residuals:", "", placeholder = "Provide variable name")
})

output$ui_crtree_nrobs <- renderUI({
  nrobs <- nrow(.get_data())
  choices <- c("1,000" = 1000, "5,000" = 5000, "10,000" = 10000, "All" = -1) %>%
    .[. < nrobs]
  selectInput(
    "crtree_nrobs", "Number of data points plotted:",
    choices = choices,
    selected = state_single("crtree_nrobs", choices, 1000)
  )
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(crtree_args, "crtree", tabs = "tabs_crtree", label = "Estimate model", relabel = "Re-estimate model")

output$ui_crtree <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(
      condition = "input.tabs_crtree == 'Summary'",
      wellPanel(
        actionButton("crtree_run", "Estimate model", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_crtree == 'Summary'",
        radioButtons(
          "crtree_type",
          label = NULL, c("classification", "regression"),
          selected = state_init("crtree_type", "classification"),
          inline = TRUE
        ),
        uiOutput("ui_crtree_rvar"),
        uiOutput("ui_crtree_lev"),
        uiOutput("ui_crtree_evar"),
        # uiOutput("ui_crtree_wts"),
        conditionalPanel(
          condition = "input.crtree_type == 'classification'",
          tags$table(
            tags$td(numericInput(
              "crtree_prior",
              label = "Prior:",
              value = state_init("crtree_prior", .5, na.rm = FALSE),
              min = 0, max = 1, step = 0.1,
              width = "116px"
            )),
            tags$td(numericInput(
              "crtree_minsplit",
              label = "Min obs.:",
              value = state_init("crtree_minsplit", 2)
            ))
          ),
          tags$table(
            tags$td(numericInput(
              "crtree_cost",
              label = "Cost:",
              value = state_init("crtree_cost", NA)
            )),
            tags$td(numericInput(
              "crtree_margin",
              label = "Margin:",
              value = state_init("crtree_margin", NA)
            ))
          )
        ),
        tags$table(
          tags$td(numericInput(
            "crtree_cp",
            label = "Complexity:", min = 0,
            max = 1, step = 0.001,
            value = state_init("crtree_cp", 0.001), width = "116px"
          )),
          tags$td(numericInput(
            "crtree_nodes",
            label = "Max. nodes:", min = 2,
            value = state_init("crtree_nodes", NA), width = "100%"
          ))
        ),
        tags$table(
          tags$td(numericInput(
            "crtree_pcp",
            label = "Prune compl.:", min = 0, step = 0.001,
            value = state_init("crtree_pcp", NA), width = "116px"
          )),
          # tags$td(numericInput(
          #   "crtree_K", label = "K-folds:",
          #   value = state_init("crtree_K", 10), width = "116px"
          # )),
          tags$td(numericInput(
            "crtree_seed",
            label = "Seed:",
            value = state_init("crtree_seed", 1234), width = "100%"
          ))
        ),
        conditionalPanel(
          condition = "input.tabs_crtree == 'Summary'",
          tags$table(
            tags$td(uiOutput("ui_crtree_store_res_name")),
            tags$td(actionButton("crtree_store_res", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_crtree == 'Predict'",
        selectInput(
          "crtree_predict",
          label = "Prediction input type:", reg_predict,
          selected = state_single("crtree_predict", reg_predict, "none")
        ),
        conditionalPanel(
          "input.crtree_predict == 'data' | input.crtree_predict == 'datacmd'",
          selectizeInput(
            inputId = "crtree_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("crtree_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.crtree_predict == 'cmd' | input.crtree_predict == 'datacmd'",
          returnTextAreaInput(
            "crtree_pred_cmd", "Prediction command:",
            value = state_init("crtree_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., carat = 1; cut = 'Ideal') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.crtree_predict != 'none'",
          checkboxInput("crtree_pred_plot", "Plot predictions", state_init("crtree_pred_plot", FALSE)),
          conditionalPanel(
            "input.crtree_pred_plot == true",
            uiOutput("ui_crtree_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.crtree_predict == 'data' | input.crtree_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("crtree_store_pred_name", "Store predictions:", state_init("crtree_store_pred_name", "pred_crtree"))),
            tags$td(actionButton("crtree_store_pred", "Store", icon("plus", verify_fa = FALSE)), class = "top")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_crtree == 'Plot'",
        selectInput(
          "crtree_plots", "Plots:",
          choices = ctree_plots,
          selected = state_single("crtree_plots", ctree_plots, "none")
        ),
        conditionalPanel(
          condition = "input.crtree_plots == 'dashboard'",
          uiOutput("ui_crtree_nrobs")
        ),
        conditionalPanel(
          condition = "input.crtree_plots == 'pdp' | input.crtree_plots == 'pred_plot'",
          uiOutput("ui_crtree_incl"),
          uiOutput("ui_crtree_incl_int")
        ),
        conditionalPanel(
          condition = "input.crtree_plots == 'tree'",
          tags$table(
            tags$td(
              selectInput(
                "crtree_orient",
                label = "Plot direction:",
                c("Left-right" = "LR", "Top-down" = "TD", "Right-left" = "RL", "Bottom-Top" = "BT"),
                state_init("crtree_orient", "LR"), width = "116px"
              ),
              style = "padding-top:16.5px;"
            ),
            tags$td(uiOutput("ui_crtree_width"), width = "100%")
          )
        )
      )
    ),
    help_and_report(
      modal_title = "Classification and regression trees",
      fun_name = "crtree",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/crtree.md"))
    )
  )
})

crtree_plot <- reactive({
  if (crtree_available() != "available") {
    return()
  }
  if (is.empty(input$crtree_plots, "none")) {
    return()
  }
  res <- .crtree()
  nr_vars <- length(res$evar)

  plot_height <- 500
  plot_width <- 650
  if ("dashboard" %in% input$crtree_plots) {
    plot_height <- 750
  } else if (input$crtree_plots %in% c("pdp", "pred_plot")) {
    nr_vars <- length(input$crtree_incl) + length(input$crtree_incl_int)
    plot_height <- max(250, ceiling(nr_vars / 2) * 250)
    if (length(input$crtree_incl_int) > 0) {
      plot_width <- plot_width + min(2, length(input$crtree_incl_int)) * 90
    }
  } else if ("vip" %in% input$crtree_plots) {
    plot_height <- max(500, nr_vars * 30)
  }

  list(plot_width = plot_width, plot_height = plot_height)
})

crtree_plot_width <- function() {
  crtree_plot() %>%
    (function(x) if (is.list(x)) x$plot_width else 650)
}

crtree_plot_height <- function() {
  crtree_plot() %>%
    (function(x) if (is.list(x)) x$plot_height else 500)
}

crtree_pred_plot_height <- function() {
  if (input$crtree_pred_plot) 500 else 0
}

output$diagrammer_crtree <- renderUI({
  DiagrammeR::DiagrammeROutput(
    "crtree_plot",
    width = input$crtree_width,
    height = "100%"
  )
})

## output is called from the main radiant ui.R
output$crtree <- renderUI({
  register_print_output("summary_crtree", ".summary_crtree")
  register_print_output("predict_crtree", ".predict_print_crtree")
  register_plot_output("predict_plot_crtree", ".predict_plot_crtree")
  register_plot_output(
    "plot_crtree", ".plot_crtree",
    height_fun = "crtree_plot_height",
    width_fun = "crtree_plot_width"
  )

  ## two separate tabs
  crtree_output_panels <- tabsetPanel(
    id = "tabs_crtree",
    tabPanel("Summary", verbatimTextOutput("summary_crtree")),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.crtree_pred_plot == true",
        download_link("dlp_crtree_pred"),
        plotOutput("predict_plot_crtree", width = "100%", height = "100%")
      ),
      download_link("dl_crtree_pred"), br(),
      verbatimTextOutput("predict_crtree")
    ),
    tabPanel(
      "Plot",
      conditionalPanel(
        "input.crtree_plots == 'tree'",
        HTML("<i title='Save plot' class='fa fa-download action-button alignright' href='#crtree_screenshot2' id='crtree_screenshot2' onclick='generate_crtree_plot();'></i>"),
        uiOutput("diagrammer_crtree")
      ),
      conditionalPanel(
        "input.crtree_plots != 'tree'",
        download_link("dlp_crtree"),
        plotOutput("plot_crtree", width = "100%", height = "100%")
      )
    )
  )
  stat_tab_panel(
    menu = "Model > Estimate",
    tool = "Classification and regression trees",
    tool_ui = "ui_crtree",
    output_panels = crtree_output_panels
  )
})

output$crtree_plot <- DiagrammeR::renderDiagrammeR({
  cr <- .crtree()
  if (is.null(cr)) {
    invisible()
  } else {
    withProgress(
      message = "Generating tree diagramm", value = 1,
      plot(cr, plots = "tree", orient = input$crtree_orient, width = paste0(input$crtree_width, "px"))
    )
  }
})

crtree_available <- reactive({
  if (not_available(input$crtree_rvar)) {
    return("This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))
  } else if (not_available(input$crtree_evar)) {
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("titanic"))
  } else {
    "available"
  }
})

.crtree <- eventReactive(input$crtree_run, {
  req(input$crtree_evar)
  withProgress(message = "Estimating model", value = 1, {
    crti <- crtree_inputs()
    crti$envir <- r_data
    do.call(crtree, crti)
  })
})

.summary_crtree <- reactive({
  if (not_pressed(input$crtree_run)) {
    "** Press the Estimate button to estimate the model **"
  } else if (crtree_available() != "available") {
    crtree_available()
  } else {
    summary(.crtree())
  }
})

.predict_crtree <- reactive({
  if (not_pressed(input$crtree_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (crtree_available() != "available") {
    return(crtree_available())
  }
  if (is.empty(input$crtree_predict, "none")) {
    return("** Select prediction input **")
  }

  if ((input$crtree_predict == "data" || input$crtree_predict == "datacmd") &&
    is.empty(input$crtree_pred_data)) {
    "** Select data for prediction **"
  } else if (input$crtree_predict == "cmd" && is.empty(input$crtree_pred_cmd)) {
    "** Enter prediction commands **"
  } else {
    withProgress(message = "Generating predictions", value = 1, {
      cri <- crtree_pred_inputs()
      cri$object <- .crtree()
      cri$envir <- r_data
      do.call(predict, cri)
    })
  }
})

.predict_print_crtree <- reactive({
  pc <- .predict_crtree()
  if (is.character(pc)) cat(pc, "\n") else print(pc)
})

.predict_plot_crtree <- reactive({
  req(
    pressed(input$crtree_run), input$crtree_pred_plot,
    available(input$crtree_xvar),
    !is.empty(input$crtree_predict, "none")
  )

  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_crtree()), crtree_pred_plot_inputs()))
  })
})

.plot_crtree <- reactive({
  if (not_pressed(input$crtree_run)) {
    "** Press the Estimate button to estimate the model **"
  } else if (crtree_available() != "available") {
    crtree_available()
  } else if (is.empty(input$crtree_plots)) {
    return("Please select a plot type from the drop-down menu")
  }
  ret <- .crtree()
  pinp <- crtree_plot_inputs()
  pinp$shiny <- TRUE
  if (input$crtree_plots == "dashboard") {
    req(input$crtree_nrobs)
  }
  check_for_pdp_pred_plots("crtree")
  if (length(ret) == 0 || is.character(ret)) {
    "No model results to plot. Specify a model and press the Estimate button"
  } else {
    withProgress(message = "Generating plots", value = 1, {
      do.call(plot, c(list(x = ret), pinp))
    })
  }
})

observeEvent(input$crtree_store_res, {
  req(pressed(input$crtree_run))
  robj <- .crtree()
  if (!is.list(robj)) {
    return()
  }
  fixed <- fix_names(input$crtree_store_res_name)
  updateTextInput(session, "crtree_store_res_name", value = fixed)
  withProgress(
    message = "Storing residuals", value = 1,
    r_data[[input$dataset]] <- store(r_data[[input$dataset]], robj, name = fixed)
  )
})

observeEvent(input$crtree_store_pred, {
  req(!is.empty(input$crtree_pred_data), pressed(input$crtree_run))
  pred <- .predict_crtree()
  if (is.null(pred)) {
    return()
  }
  fixed <- fix_names(input$crtree_store_pred_name)
  updateTextInput(session, "crtree_store_pred_name", value = fixed)
  withProgress(
    message = "Storing predictions", value = 1,
    r_data[[input$crtree_pred_data]] <- store(
      r_data[[input$crtree_pred_data]], pred,
      name = fixed
    )
  )
})

crtree_report <- function() {
  if (is.empty(input$crtree_evar)) {
    return(invisible())
  }

  outputs <- c("summary")
  inp_out <- list(list(prn = TRUE), "")
  figs <- FALSE

  if (!is.empty(input$crtree_store_res_name)) {
    fixed <- fix_names(input$crtree_store_res_name)
    updateTextInput(session, "crtree_store_res_name", value = fixed)
    xcmd <- paste0(input$dataset, " <- store(", input$dataset, ", result, name = \"", fixed, "\")\n")
  } else {
    xcmd <- ""
  }

  if (!is.empty(input$crtree_predict, "none") &&
    (!is.empty(input$crtree_pred_data) || !is.empty(input$crtree_pred_cmd))) {
    pred_args <- clean_args(crtree_pred_inputs(), crtree_pred_args[-1])

    if (!is.empty(pred_args$pred_cmd)) {
      pred_args$pred_cmd <- strsplit(pred_args$pred_cmd, ";")[[1]]
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
    if (input$crtree_predict %in% c("data", "datacmd")) {
      fixed <- fix_names(input$crtree_store_pred_name)
      updateTextInput(session, "crtree_store_pred_name", value = fixed)
      xcmd <- paste0(
        xcmd, "\n", input$crtree_pred_data, " <- store(",
        input$crtree_pred_data, ", pred, name = \"", fixed, "\")"
      )
    }

    if (input$crtree_pred_plot && !is.empty(input$crtree_xvar)) {
      inp_out[[3 + figs]] <- clean_args(crtree_pred_plot_inputs(), crtree_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  if (input$crtree_plots != "none") {
    width <- ifelse(is.empty(input$crtree_width), "\"900px\"", paste0("\"", input$crtree_width, "px\""))
    orient <- ifelse(is.empty(input$crtree_orient), "\"TD\"", paste0("\"", input$crtree_orient, "\""))
    if (input$crtree_plots == "tree") {
      xcmd <- paste0(xcmd, "\n# plot(result, plots = \"prune\", custom = FALSE)")
      xcmd <- paste0(xcmd, "\nplot(result, orient = ", orient, ", width = ", width, ") %>% render()")
    } else if (input$crtree_plots == "prune") {
      figs <- TRUE
      xcmd <- paste0(xcmd, "\nplot(result, plots = \"prune\", custom = FALSE)")
      xcmd <- paste0(xcmd, "\n# plot(result, orient = ", orient, ", width = ", width, ") %>% render()")
    } else if (input$crtree_plots == "vip") {
      figs <- TRUE
      xcmd <- paste0(xcmd, "\nplot(result, plots = \"vip\", custom = FALSE)")
      xcmd <- paste0(xcmd, "\n# plot(result, orient = ", orient, ", width = ", width, ") %>% render()")
    } else if (input$crtree_plots %in% c("pdp", "pred_plot")) {
      figs <- TRUE
      incl <- ""
      dctrl <- getOption("dctrl")
      if (length(input$crtree_incl) > 0) {
        cmd <- deparse(input$crtree_incl, control = dctrl, width.cutoff = 500L)
        incl <- glue(", incl = {cmd}")
      }
      if (length(input$crtree_incl_int) > 0) {
        cmd <- deparse(input$crtree_incl_int, control = dctrl, width.cutoff = 500L)
        incl <- glue("{incl}, incl_int = {cmd}")
      }
      xcmd <- paste0(xcmd, "\nplot(result, plots = \"", input$crtree_plots, "\"", incl, ", custom = FALSE)")
      xcmd <- paste0(xcmd, "\n# plot(result, orient = ", orient, ", width = ", width, ") %>% render()")
    }
  }

  crtree_inp <- crtree_inputs()
  if (input$crtree_type == "regression") {
    crtree_inp$prior <- crtree_inp$cost <- crtree_inp$margin <- crtree_inp$lev <- NULL
  }

  update_report(
    inp_main = clean_args(crtree_inp, crtree_args),
    fun_name = "crtree",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = crtree_plot_width(),
    fig.height = crtree_plot_height(),
    xcmd = xcmd
  )
}

dl_crtree_pred <- function(path) {
  if (pressed(input$crtree_run)) {
    .predict_crtree() %>% write.csv(file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_crtree_pred",
  fun = dl_crtree_pred,
  fn = function() paste0(input$dataset, "_crtree_pred"),
  type = "csv",
  caption = "Save crtree predictions"
)

download_handler(
  id = "dlp_crtree_pred",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_crtree_pred"),
  type = "png",
  caption = "Save decision tree prediction plot",
  plot = .predict_plot_crtree,
  width = plot_width,
  height = crtree_pred_plot_height
)

download_handler(
  id = "dlp_crtree",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_crtree"),
  type = "png",
  caption = "Save decision tree plot",
  plot = .plot_crtree,
  width = crtree_plot_width,
  height = crtree_plot_height
)

observeEvent(input$crtree_report, {
  r_info[["latest_screenshot"]] <- NULL
  crtree_report()
})

observeEvent(input$crtree_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_crtree_screenshot")
})

observeEvent(input$modal_crtree_screenshot, {
  crtree_report()
  removeModal() ## remove shiny modal after save
})

observeEvent(input$crtree_screenshot2, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_crtree_screenshot2")
})

observeEvent(input$modal_crtree_screenshot2, {
  crtree_report()
  removeModal() ## remove shiny modal after save
})
