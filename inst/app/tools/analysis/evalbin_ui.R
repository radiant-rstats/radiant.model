ebin_plots <- list(
  "Lift" = "lift",
  "Gains" = "gains",
  "Profit" = "profit",
  "ROME" = "rome"
)
ebin_train <- list(
  "All" = "All",
  "Training" = "Training",
  "Test" = "Test",
  "Both" = "Both"
)

# list of function arguments
ebin_args <- as.list(formals(evalbin))

ebin_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  ebin_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ebin_args$dataset <- input$dataset
  for (i in r_drop(names(ebin_args)))
    ebin_args[[i]] <- input[[paste0("ebin_", i)]]
  ebin_args
})

###############################################################
# Evaluate model evalbin
###############################################################
output$ui_ebin_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    vars <- two_level_vars()
  })
  selectInput(
    inputId = "ebin_rvar", label = "Response variable:", choices = vars,
    selected = state_single("ebin_rvar", vars), multiple = FALSE
  )
})

output$ui_ebin_lev <- renderUI({
  req(available(input$ebin_rvar))
  levs <- .get_data()[[input$ebin_rvar]] %>%
    as.factor() %>%
    levels()
  selectInput(
    inputId = "ebin_lev", label = "Choose level:",
    choices = levs,
    selected = state_init("ebin_lev")
  )
})

output$ui_ebin_pred <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric")
  vars <- varnames()[isNum]
  selectInput(
    inputId = "ebin_pred", label = "Select stored predictions:", choices = vars,
    selected = state_multiple("ebin_pred", vars, isolate(input$ebin_pred)),
    multiple = TRUE, size = min(4, length(vars)), selectize = FALSE
  )
})

output$ui_ebin_train <- renderUI({
  selectInput(
    "ebin_train", label = "Show results for:", ebin_train,
    selected = state_single("ebin_train", ebin_train, "All")
  )
})

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  sapply(r_drop(names(ebin_args)), function(x) input[[paste0("ebin_", x)]])

  ## notify user when the regression needs to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$ebin_run)) {
    if (isTRUE(attr(ebin_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "ebin_run", "Re-evaluate models", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "ebin_run", "Evaluate models", icon = icon("play"))
    }
  }
})

output$ui_evalbin <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ebin_run", "Evaluate models", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      uiOutput("ui_ebin_rvar"),
      uiOutput("ui_ebin_lev"),
      uiOutput("ui_ebin_pred"),
      conditionalPanel(
        "input.tabs_evalbin != 'Confusion'",
        numericInput(
          "ebin_qnt", label = "# quantiles:",
          value = state_init("ebin_qnt", 10), min = 2
        )
      ),
      tags$table(
        tags$td(numericInput(
          "ebin_cost", label = "Cost:",
          value = state_init("ebin_cost", 1)
        )),
        tags$td(numericInput(
          "ebin_margin", label = "Margin:",
          value = state_init("ebin_margin", 2), width = "117px"
        ))
      ),
      uiOutput("ui_ebin_train"),
      conditionalPanel(
        "input.tabs_evalbin == 'Evaluate'",
        checkboxInput("ebin_show_tab", "Show model performance table" , state_init("ebin_show_tab", FALSE)),
        checkboxGroupInput(
          "ebin_plots", "Plots:", ebin_plots,
          selected = state_group("ebin_plots", "gains"),
          inline = TRUE
        )
      ),
      conditionalPanel(
        "input.tabs_evalbin == 'Confusion'",
        tags$table(
          tags$td(
            checkboxInput("ebin_show_plots", "Show plots" , state_init("ebin_show_plots", FALSE))
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
      "input.tabs_evalbin != 'Confusion'",
      help_and_report(
        modal_title = "Evaluate classification",
        fun_name = "evalbin",
        help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/evalbin.md"))
      )
    ),
    conditionalPanel(
      "input.tabs_evalbin == 'Confusion'",
      help_and_report(
        modal_title = "Model evaluate confusion matrix",
        fun_name = "confusion",
        help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/evalbin.md"))
      )
    )
  )
})

ebin_plot_width <- function() 700
ebin_plot_height <- function() {
  if (is_empty(input$ebin_plots)) 200 else length(input$ebin_plots) * 500
}

confusion_plot_width <- function() 650
confusion_plot_height <- function() 800

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
  register_print_output("summary_performance", ".summary_performance")

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
  if (!is_empty(r_info[["filter_error"]])) {
    "An invalid filter has been set for this dataset. Please\nadjust the filter in the Data > View tab and try again" %>%
      add_class("evalbin")
  } else {
    withProgress(message = "Evaluating models", value = 1, {
      do.call(evalbin, ebin_inputs())
    })
  }
})

.summary_evalbin <- reactive({
  if (not_pressed(input$ebin_run)) return("** Press the Evaluate button to evaluate models **")
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
    is_empty(input$ebin_lev)) {
    return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
  }
  summary(.evalbin(), prn = input$ebin_show_tab)
})

.plot_evalbin <- reactive({
  if (not_pressed(input$ebin_run)) return("** Press the Evaluate button to evaluate models **")
  isolate({
    if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
        is_empty(input$ebin_lev)) {
      return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>%
        suggest_data("titanic"))
    } else if (!input$ebin_train %in% c("", "All") && (!input$show_filter || (input$show_filter && is_empty(input$data_filter)))) {
      return("** Filter required. To set a filter go to Data > View and click the filter checkbox **")
    }
  })
  plot(.evalbin(), plots = input$ebin_plots, shiny = TRUE)
})

.confusion <- eventReactive(input$ebin_run, {
  if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
    is_empty(input$ebin_lev)) {
    return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>%
      suggest_data("titanic"))
  }
  if (!input$ebin_train %in% c("", "All") && (!input$show_filter || (input$show_filter && is_empty(input$data_filter)))) {
    return("** Filter required. To set a filter go to Data > View and click the filter checkbox **")
  }
  do.call(confusion, ebin_inputs())
})

.summary_confusion <- reactive({
  if (not_pressed(input$ebin_run)) return("** Press the Evaluate button to evaluate models **")
  isolate({
    if (not_available(input$ebin_rvar) || not_available(input$ebin_pred) ||
      is_empty(input$ebin_lev)) {
      return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
    }
  })
  summary(.confusion())
})

.plot_confusion <- reactive({
  if (not_pressed(input$ebin_run)) return(invisible())
  isolate({
    if (not_available(input$ebin_rvar) || not_available(input$ebin_pred)) return(" ")
    req(input$ebin_train, !is_not(input$ebin_scale_y))
  })
  plot(.confusion(), scale_y = input$ebin_scale_y)
})

observeEvent(input$evalbin_report, {
  if (is_empty(input$ebin_rvar) || is_empty(input$ebin_pred)) return(invisible())

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
})

observeEvent(input$confusion_report, {
  if (is_empty(input$ebin_rvar) || is_empty(input$ebin_pred)) return(invisible())

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

  update_report(
    inp_main = clean_args(ebin_inputs(), ebin_args),
    fun_name = "confusion",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = confusion_plot_width(),
    fig.height = 1.5 * confusion_plot_height()
  )
})

dl_ebin_tab <- function(path) {
  dat <- .evalbin()$dataset
  if (!is_empty(dat)) write.csv(dat, file = path, row.names = FALSE)
}

download_handler(
  id = "dl_ebin_tab",
  fun = dl_ebin_tab,
  fn = function() paste0(input$dataset, "_evalbin"),
  type = "csv",
  caption = "Save model evaluations"
)

dl_confusion_tab <- function(path) {
  print(.confusion())
  dat <- .confusion()$dataset
  if (!is_empty(dat)) write.csv(dat, file = path, row.names = FALSE)
}

download_handler(
  id = "dl_confusion_tab",
  fun = dl_confusion_tab,
  fn = function() paste0(input$dataset, "_confusion"),
  type = "csv",
  caption = "Save model performance metrics"
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
