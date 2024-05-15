ebin_plots <- list(
  "Lift" = "lift",
  "Gains" = "gains",
  "Profit" = "profit",
  "Expected profit" = "expected_profit",
  "ROME" = "rome"
)
ebin_train <- list(
  "All" = "All",
  "Training" = "Training",
  "Test" = "Test",
  "Both" = "Both"
)
uplift_plots <- list(
  "Incremental uplift" = "inc_uplift",
  "Uplift" = "uplift",
  "Incremental profit" = "inc_profit"
)

# list of function arguments
ebin_args <- as.list(formals(evalbin))

ebin_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  ebin_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ebin_args$arr <- if (input$show_filter) input$data_arrange else ""
  ebin_args$rows <- if (input$show_filter) input$data_rows else ""
  ebin_args$dataset <- input$dataset
  for (i in r_drop(names(ebin_args))) {
    ebin_args[[i]] <- input[[paste0("ebin_", i)]]
  }
  ebin_args
})

# list of function arguments
uplift_args <- as.list(formals(uplift))

uplift_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  uplift_args$data_filter <- if (input$show_filter) input$data_filter else ""
  uplift_args$arr <- if (input$show_filter) input$data_arrange else ""
  uplift_args$rows <- if (input$show_filter) input$data_rows else ""
  uplift_args$dataset <- input$dataset
  for (i in r_drop(names(uplift_args))) {
    uplift_args[[i]] <- input[[paste0("uplift_", i)]]
    if (is.empty(uplift_args[[i]])) {
      uplift_args[[i]] <- input[[paste0("ebin_", i)]]
    }
  }
  uplift_args
})

###############################################################
# Evaluate model evalbin
###############################################################
output$ui_ebin_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    # vars <- two_level_vars()
    vars <- groupable_vars()
  })
  selectInput(
    inputId = "ebin_rvar", label = "Response variable:", choices = vars,
    selected = state_single("ebin_rvar", vars), multiple = FALSE
  )
})

output$ui_ebin_lev <- renderUI({
  req(available(input$ebin_rvar))
  rvar <- .get_data()[[input$ebin_rvar]]
  levs <- unique(rvar)
  if (length(levs) > 50) {
    HTML("<label>More than 50 levels. Please choose another response variable</label>")
  } else {
    selectInput(
      inputId = "ebin_lev", label = "Choose level:",
      choices = levs,
      selected = state_init("ebin_lev")
    )
  }
})

output$ui_ebin_tvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    vars <- setdiff(two_level_vars(), input$ebin_rvar)
  })
  selectInput(
    inputId = "ebin_tvar", label = "Treatment variable:", choices = vars,
    selected = state_single("ebin_tvar", vars), multiple = FALSE
  )
})

output$ui_ebin_tlev <- renderUI({
  req(available(input$ebin_tvar))
  tvar <- .get_data()[[input$ebin_tvar]]
  levs <- unique(tvar)
  if (length(levs) > 50) {
    HTML("<label>More than 50 levels. Please choose another response variable</label>")
  } else {
    selectInput(
      inputId = "ebin_tlev", label = "Choose level:",
      choices = levs,
      selected = state_init("ebin_tlev")
    )
  }
})

output$ui_ebin_pred <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  vars <- varnames()[isNum]
  selectInput(
    inputId = "ebin_pred", label = "Select stored predictions:", choices = vars,
    selected = state_multiple("ebin_pred", vars, isolate(input$ebin_pred)),
    multiple = TRUE, size = min(4, length(vars)), selectize = FALSE
  )
})

output$ui_ebin_train <- renderUI({
  selectInput(
    "ebin_train",
    label = "Show results for:", ebin_train,
    selected = state_single("ebin_train", ebin_train, "All")
  )
})

output$ui_uplift_name <- renderUI({
  req(input$dataset)
  textInput("uplift_name", "Store uplift table as:", "", placeholder = "Provide a table name")
})

## add a spinning refresh icon if the model needs to be (re)estimated
run_refresh(ebin_args, "ebin", init = "pred", label = "Evaluate models", relabel = "Re-evaluate models")

output$ui_evalbin <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ebin_run", "Evaluate models", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
    ),
    wellPanel(
      uiOutput("ui_ebin_rvar"),
      uiOutput("ui_ebin_lev"),
      conditionalPanel(
        "input.tabs_evalbin == 'Uplift'",
        uiOutput("ui_ebin_tvar"),
        uiOutput("ui_ebin_tlev")
      ),
      uiOutput("ui_ebin_pred"),
      conditionalPanel(
        "input.tabs_evalbin != 'Confusion'",
        numericInput(
          "ebin_qnt",
          label = "# quantiles:",
          value = state_init("ebin_qnt", 10), min = 2
        )
      ),
      tags$table(
        tags$td(numericInput(
          "ebin_cost",
          label = "Cost:",
          value = state_init("ebin_cost", 1)
        )),
        tags$td(numericInput(
          "ebin_margin",
          label = "Margin:",
          value = state_init("ebin_margin", 2)
        )),
        tags$td(numericInput(
          "ebin_scale",
          label = "Scale:",
          value = state_init("ebin_scale", 1) # , width = "80px"
        ))
      ),
      uiOutput("ui_ebin_train"),
      conditionalPanel(
        "input.tabs_evalbin == 'Evaluate'",
        checkboxInput("ebin_show_tab", "Show model performance table", state_init("ebin_show_tab", FALSE)),
        checkboxGroupInput(
          "ebin_plots", "Plots:", ebin_plots,
          selected = state_group("ebin_plots", "gains"),
          inline = TRUE
        )
      ),
      conditionalPanel(
        "input.tabs_evalbin == 'Uplift'",
        checkboxInput("uplift_show_tab", "Show uplift table", state_init("uplift_show_tab", FALSE)),
        checkboxGroupInput(
          "uplift_plots", "Plots:", uplift_plots,
          selected = state_group("uplift_plots", "inc_uplift"),
          inline = TRUE
        )
      ),
      conditionalPanel(
        "input.tabs_evalbin == 'Confusion'",
        tags$table(
          tags$td(
            checkboxInput("ebin_show_plots", "Show plots", state_init("ebin_show_plots", FALSE))
          ),
          tags$td(
            HTML("&nbsp;&nbsp;&nbsp;")
          ),
          tags$td(
            conditionalPanel(
              "input.ebin_show_plots == true",
              checkboxInput("ebin_scale_y", "Scale free", state_init("ebin_scale_y", TRUE))
            )
          )
        )
      )
    ),
    conditionalPanel(
      "input.tabs_evalbin == 'Evaluate'",
      help_and_report(
        modal_title = "Evaluate classification",
        fun_name = "evalbin",
        help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/evalbin.md"))
      )
    ),
    conditionalPanel(
      "input.tabs_evalbin == 'Confusion'",
      help_and_report(
        modal_title = "Confusion matrix",
        fun_name = "confusion",
        help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/evalbin.md"))
      )
    ),
    conditionalPanel(
      "input.tabs_evalbin == 'Uplift'",
      wellPanel(
        tags$table(
          tags$td(uiOutput("ui_uplift_name")),
          tags$td(actionButton("uplift_store", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
        )
      ),
      help_and_report(
        modal_title = "Evaluate uplift",
        fun_name = "uplift",
        help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/evalbin.md"))
      )
    )
  )
})

ebin_plot_width <- function() 700
ebin_plot_height <- function() {
  if (is.empty(input$ebin_plots)) 200 else length(input$ebin_plots) * 500
}

confusion_plot_width <- function() 650
confusion_plot_height <- function() 800

uplift_plot_width <- function() 700
uplift_plot_height <- function() {
  if (is.empty(input$uplift_plots)) 200 else length(input$uplift_plots) * 500
}

# output is called from the main radiant ui.R
output$evalbin <- renderUI({
  register_print_output("summary_evalbin", ".summary_evalbin")
  register_plot_output(
    "plot_evalbin", ".plot_evalbin",
    width_fun = "ebin_plot_width",
    height_fun = "ebin_plot_height"
  )
  register_print_output("summary_confusion", ".summary_confusion")
  register_plot_output(
    "plot_confusion", ".plot_confusion",
    width_fun = "confusion_plot_width",
    height_fun = "confusion_plot_height"
  )
  # register_print_output("summary_performance", ".summary_performance")

  register_print_output("summary_uplift", ".summary_uplift")
  register_plot_output(
    "plot_uplift", ".plot_uplift",
    width_fun = "uplift_plot_width",
    height_fun = "uplift_plot_height"
  )

  # one output with components stacked
  ebin_output_panels <- tabsetPanel(
    id = "tabs_evalbin",
    tabPanel(
      "Evaluate",
      download_link("dl_ebin_tab"), br(),
      verbatimTextOutput("summary_evalbin"),
      download_link("dlp_evalbin"),
      plotOutput("plot_evalbin", height = "100%")
    ),
    tabPanel(
      "Confusion",
      download_link("dl_confusion_tab"), br(),
      verbatimTextOutput("summary_confusion"),
      conditionalPanel(
        condition = "input.ebin_show_plots == true",
        download_link("dlp_confusion"),
        plotOutput("plot_confusion", height = "100%")
      )
    ),
    tabPanel(
      "Uplift",
      download_link("dl_uplift_tab"), br(),
      verbatimTextOutput("summary_uplift"),
      download_link("dlp_uplift"),
      plotOutput("plot_uplift", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Evaluate",
    tool = "Evaluate classification",
    tool_ui = "ui_evalbin",
    output_panels = ebin_output_panels
  )
})

.evalbin <- eventReactive(input$ebin_run, {
  if (!is.empty(r_info[["filter_error"]])) {
    "An invalid filter has been set for this dataset. Please\nadjust the filter in the Data > View tab and try again" %>%
      add_class("evalbin")
  } else {
    withProgress(message = "Evaluating models", value = 1, {
      ebi <- ebin_inputs()
      ebi$envir <- r_data
      do.call(evalbin, ebi)
    })
  }
})

.summary_evalbin <- reactive({
  if (not_pressed(input$ebin_run)) {
    return("** Press the Evaluate button to evaluate models **")
  }
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
    is.empty(input$ebin_lev)) {
    return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
  }
  summary(.evalbin(), prn = input$ebin_show_tab)
})

.plot_evalbin <- reactive({
  if (not_pressed(input$ebin_run)) {
    return("** Press the Evaluate button to evaluate models **")
  }
  isolate({
    if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
      is.empty(input$ebin_lev)) {
      return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>%
        suggest_data("titanic"))
    } else if (!input$ebin_train %in% c("", "All") && (!input$show_filter || (input$show_filter && is.empty(input$data_filter) && is.empty(input$data_rows)))) {
      return("**\nFilter or Slice required. To set a filter or slice go to\nData > View and click the filter checkbox\n**")
    }
  })
  plot(.evalbin(), plots = input$ebin_plots, shiny = TRUE)
})

.confusion <- eventReactive(input$ebin_run, {
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
    is.empty(input$ebin_lev)) {
    return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>%
      suggest_data("titanic"))
  }
  if (!input$ebin_train %in% c("", "All") && (!input$show_filter || (input$show_filter && is.empty(input$data_filter) && is.empty(input$data_rows)))) {
    return("**\nFilter or Slice required. To set a filter or slice go to\nData > View and click the filter checkbox\n**")
  }
  withProgress(message = "Evaluating models", value = 1, {
    ebi <- ebin_inputs()
    ebi$envir <- r_data
    do.call(confusion, ebi)
  })
})

.summary_confusion <- reactive({
  if (not_pressed(input$ebin_run)) {
    return("** Press the Evaluate button to evaluate models **")
  }
  isolate({
    if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
      is.empty(input$ebin_lev)) {
      return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
    }
  })
  summary(.confusion())
})

.plot_confusion <- reactive({
  if (not_pressed(input$ebin_run)) {
    return(invisible())
  }
  isolate({
    if (not_available(input$ebin_rvar) || not_available(input$ebin_pred)) {
      return(" ")
    }
    req(input$ebin_train, !is_not(input$ebin_scale_y))
  })
  plot(.confusion(), scale_y = input$ebin_scale_y)
})

.uplift <- eventReactive(input$ebin_run, {
  if (not_available(input$ebin_rvar) || not_available(input$ebin_tvar) || not_available(input$ebin_pred) ||
    is.empty(input$ebin_lev) || is.empty(input$ebin_tlev)) {
    return("This analysis requires a response variable of type factor, a treatment variable, and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>%
      suggest_data("kaggle_uplift"))
  }
  if (!input$ebin_train %in% c("", "All") && (!input$show_filter || (input$show_filter && is.empty(input$data_filter) && is.empty(input$data_rows)))) {
    return("**\nFilter or Slice required. To set a filter or slice go to\nData > View and click the filter checkbox\n**")
  }

  withProgress(message = "Evaluating uplift", value = 1, {
    uli <- uplift_inputs()
    uli$envir <- r_data
    do.call(uplift, uli)
  })
})

.summary_uplift <- reactive({
  if (not_pressed(input$ebin_run)) {
    return("** Press the Evaluate button to evaluate models **")
  }
  if (not_available(input$ebin_rvar) || not_available(input$ebin_tvar) || not_available(input$ebin_pred) ||
    is.empty(input$ebin_lev) || is.empty(input$ebin_tlev)) {
    return("This analysis requires a response variable of type factor, a treatment variable, and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>%
      suggest_data("kaggle_uplift"))
  }
  summary(.uplift(), prn = input$uplift_show_tab)
})

.plot_uplift <- reactive({
  if (not_pressed(input$ebin_run)) {
    return("** Press the Evaluate button to evaluate models **")
  }
  isolate({
    if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
      is.empty(input$ebin_lev)) {
      return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>%
        suggest_data("kaggle_uplift"))
    } else if (!input$ebin_train %in% c("", "All") && (!input$show_filter || (input$show_filter && is.empty(input$data_filter) && is.empty(input$data_rows)))) {
      return("**\nFilter or Slice required. To set a filter or slice go to\nData > View and click the filter checkbox\n**")
    }
  })
  plot(.uplift(), plots = input$uplift_plots, shiny = TRUE)
})

evalbin_report <- function() {
  if (is.empty(input$ebin_rvar) || is.empty(input$ebin_pred)) {
    return(invisible())
  }

  outputs <- c("summary")
  inp_out <- list(list(prn = input$ebin_show_tab), "")
  figs <- FALSE
  if (length(input$ebin_plots) > 0) {
    inp_out[[2]] <- list(plots = input$ebin_plots, custom = FALSE)
    outputs <- c("summary", "plot")
    figs <- TRUE
  }
  update_report(
    inp_main = clean_args(ebin_inputs(), ebin_args),
    fun_name = "evalbin",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = ebin_plot_width(),
    fig.height = ebin_plot_height()
  )
}

confusion_report <- function() {
  if (is.empty(input$ebin_rvar) || is.empty(input$ebin_pred)) {
    return(invisible())
  }

  inp_out <- list("", "")
  outputs <- "summary"
  figs <- FALSE

  if (isTRUE(input$ebin_show_plots)) {
    if (!input$ebin_scale_y) {
      inp_out[[2]] <- list(scale_y = input$ebin_scale_y, custom = FALSE)
    } else {
      inp_out[[2]] <- list(custom = FALSE)
    }
    outputs <- c("summary", "plot")
    figs <- TRUE
  }

  # qnt might be set in the Evaluate tab but is not needed to calculate
  # the confusion matrix
  ebi <- ebin_inputs()
  ebi$qnt <- NULL

  update_report(
    inp_main = clean_args(ebi, ebin_args),
    fun_name = "confusion",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = confusion_plot_width(),
    fig.height = 1.5 * confusion_plot_height()
  )
}

observeEvent(input$uplift_store, {
  req(input$uplift_name)
  dat <- .uplift()
  if (is.null(dat)) {
    return()
  }
  dataset <- fix_names(input$uplift_name)
  if (input$uplift_name != dataset) {
    updateTextInput(session, inputId = "expl_name", value = dataset)
  }
  # rows <- input$explore_rows_all
  # dat$tab <- dat$tab %>%
  #   (function(x) if (is.null(rows)) x else x[rows, , drop = FALSE]) %>%
  #   (function(x) if (is.empty(input$expl_tab_slice)) x else slice_data(x, input$expl_tab_slice))
  r_data[[dataset]] <- dat$dataset
  register(dataset)
  updateSelectInput(session, "dataset", selected = input$dataset)

  ## See https://shiny.posit.co//reference/shiny/latest/modalDialog.html
  showModal(
    modalDialog(
      title = "Uplift Table Stored",
      span(
        paste0("The uplift table '", dataset, "' was successfully added to the
                datasets dropdown. Add code to Report > Rmd or
                Report > R to (re)create the results by clicking
                the report icon on the bottom left of your screen.")
      ),
      footer = modalButton("OK"),
      size = "m",
      easyClose = TRUE
    )
  )
})

uplift_report <- function() {
  if (is.empty(input$ebin_rvar) || is.empty(input$ebin_pred)) {
    return(invisible())
  }

  outputs <- c("summary")
  inp_out <- list(list(prn = input$uplift_show_tab), "")
  figs <- FALSE
  if (length(input$uplift_plots) > 0) {
    inp_out[[2]] <- list(plots = input$uplift_plots, custom = FALSE)
    outputs <- c("summary", "plot")
    figs <- TRUE
  }

  if (!is.empty(input$uplift_name)) {
    dataset <- fix_names(input$uplift_name)
    if (input$uplift_name != dataset) {
      updateTextInput(session, inputId = "uplift_name", value = dataset)
    }
    xcmd <- paste0(dataset, " <- result$dataset\nregister(\"", dataset, "\")")
  } else {
    xcmd <- ""
  }

  update_report(
    inp_main = clean_args(uplift_inputs(), uplift_args),
    fun_name = "uplift",
    inp_out = inp_out,
    outputs = outputs,
    xcmd = xcmd,
    figs = figs,
    fig.width = uplift_plot_width(),
    fig.height = uplift_plot_height()
  )
}

dl_ebin_tab <- function(path) {
  dat <- .evalbin()$dataset
  if (!is.empty(dat)) write.csv(dat, file = path, row.names = FALSE)
}

download_handler(
  id = "dl_ebin_tab",
  fun = dl_ebin_tab,
  fn = function() paste0(input$dataset, "_evalbin"),
  type = "csv",
  caption = "Save model evaluations"
)

dl_confusion_tab <- function(path) {
  dat <- .confusion()$dataset
  if (!is.empty(dat)) write.csv(dat, file = path, row.names = FALSE)
}

download_handler(
  id = "dl_confusion_tab",
  fun = dl_confusion_tab,
  fn = function() paste0(input$dataset, "_confusion"),
  type = "csv",
  caption = "Save model performance metrics"
)

dl_uplift_tab <- function(path) {
  dat <- .uplift()$dataset
  if (!is.empty(dat)) write.csv(dat, file = path, row.names = FALSE)
}

download_handler(
  id = "dl_uplift_tab",
  fun = dl_uplift_tab,
  fn = function() paste0(input$dataset, "_uplift"),
  type = "csv",
  caption = "Save uplift evaluations"
)

download_handler(
  id = "dlp_evalbin",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_evalbin"),
  type = "png",
  caption = "Save model evaluation plot",
  plot = .plot_evalbin,
  width = ebin_plot_width,
  height = ebin_plot_height
)

download_handler(
  id = "dlp_confusion",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_confusion"),
  type = "png",
  caption = "Save confusion plots",
  plot = .plot_confusion,
  width = confusion_plot_width,
  height = confusion_plot_height
)

download_handler(
  id = "dlp_uplift",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_uplift"),
  type = "png",
  caption = "Save uplift plots",
  plot = .plot_uplift,
  width = uplift_plot_width,
  height = uplift_plot_height
)

observeEvent(input$confusion_report, {
  r_info[["latest_screenshot"]] <- NULL
  confusion_report()
})

observeEvent(input$confusion_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_confusion_screenshot")
})

observeEvent(input$modal_confusion_screenshot, {
  confusion_report()
  removeModal() ## remove shiny modal after save
})

observeEvent(input$evalbin_report, {
  r_info[["latest_screenshot"]] <- NULL
  evalbin_report()
})

observeEvent(input$evalbin_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_evalbin_screenshot")
})

observeEvent(input$modal_evalbin_screenshot, {
  evalbin_report()
  removeModal() ## remove shiny modal after save
})

observeEvent(input$uplift_report, {
  r_info[["latest_screenshot"]] <- NULL
  uplift_report()
})

observeEvent(input$uplift_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_uplift_screenshot")
})

observeEvent(input$modal_uplift_screenshot, {
  uplift_report()
  removeModal() ## remove shiny modal after save
})
