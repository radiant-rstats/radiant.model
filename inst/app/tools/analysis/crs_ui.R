# list of function arguments
crs_args <- as.list(formals(crs))

crs_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  crs_args$data_filter <- if (input$show_filter) input$data_filter else ""
  crs_args$dataset <- input$dataset
  for (i in r_drop(names(crs_args)))
    crs_args[[i]] <- input[[paste0("crs_", i)]]
  crs_args
})

###############################################################
# Evaluate model evalbin
###############################################################
output$ui_crs_id <- renderUI({
  vars <- c("None" = "", varnames())
  selectInput(
    inputId = "crs_id", label = "User id:", choices = vars,
    selected = state_single("crs_id", vars), multiple = FALSE
  )
})

output$ui_crs_prod <- renderUI({
  req(available(input$crs_id))
  vars <- varnames()
  vars <- vars[-which(vars %in% input$crs_id)]

  selectInput(
    inputId = "crs_prod", label = "Product id:", choices = vars,
    selected = state_single("crs_prod", vars), multiple = FALSE
  )
})

output$ui_crs_pred <- renderUI({
  req(input$crs_prod)
  if (available(input$crs_prod)) {
    levs <- .get_data()[[input$crs_prod]] %>% as.factor() %>% levels()
  } else {
    levs <- c()
  }

  selectInput(
    "crs_pred", "Choose products to recommend:",
    choices = levs,
    selected = state_init("crs_pred", levs),
    multiple = TRUE,
    size = min(3, length(levs)),
    selectize = FALSE
  )
})

output$ui_crs_rate <- renderUI({
  req(input$crs_prod)
  vars <- varnames()
  vars <- vars[-which(c(input$crs_id, input$crs_prod) %in% vars)]

  selectInput(
    inputId = "crs_rate", label = "Ratings variable:", choices = vars,
    selected = state_single("crs_rate", vars), multiple = FALSE
  )
})

output$ui_crs_store_pred_name <- renderUI({
  req(input$dataset)
  textInput("crs_store_pred_name", NULL, "", placeholder = "Provide data name")
})

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  sapply(r_drop(names(crs_args)), function(x) input[[paste0("crs_", x)]])

  ## notify user when the regression needs to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$crs_run)) {
    if (is.null(input$crs_pred)) {
      updateTabsetPanel(session, "tabs_crs ", selected = "Summary")
      updateActionButton(session, "crs_run", "Estimate model", icon = icon("play"))
    } else if (isTRUE(attr(crs_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "crs_run", "Re-estimate model", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "crs_run", "Estimate model", icon = icon("play"))
    }
  }
})

output$ui_crs <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("crs_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    conditionalPanel(
      "input.tabs_crs == 'Summary'",
      wellPanel(
        uiOutput("ui_crs_id"),
        uiOutput("ui_crs_prod"),
        uiOutput("ui_crs_pred"),
        uiOutput("ui_crs_rate"),
        HTML("<label>Store recommendations:</label>"),
        tags$table(
          tags$td(uiOutput("ui_crs_store_pred_name")),
          tags$td(actionButton("crs_store_pred", "Store", icon = icon("plus")), style = "padding-top:5px;")
        )
      )
    ),
    help_and_report(
      modal_title = "Collaborative Filtering",
      fun_name = "crs",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/crs.md"))
    )
  )
})

crs_plot <- eventReactive(input$crs_run, {
  if (length(input$crs_pred) == 0) {
    plot_height <- 500
    plot_width <- 650
  } else {
    plot_height <- ceiling(length(input$crs_pred) / 3) * 220
    plot_width <- min(4, length(input$crs_pred)) * 220
  }
  list(plot_width = plot_width, plot_height = plot_height)
})

crs_plot_width <- function()
  crs_plot() %>% {if (is.list(.)) .$plot_width else 650}

crs_plot_height <- function()
  crs_plot() %>% {if (is.list(.)) .$plot_height else 500}

# output is called from the main radiant ui.R
output$crs <- renderUI({
  register_print_output("summary_crs", ".summary_crs")
  register_plot_output(
    "plot_crs", ".plot_crs",
    width_fun = "crs_plot_width",
    height_fun = "crs_plot_height"
  )

  # one output with components stacked
  crs_output_panels <- tabsetPanel(
    id = "tabs_crs",
    tabPanel(
      "Summary",
      download_link("dl_crs_recommendations"), br(),
      verbatimTextOutput("summary_crs")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_crs"),
      plotOutput("plot_crs", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Recommend",
    tool = "Collaborative Filtering",
    tool_ui = "ui_crs",
    output_panels = crs_output_panels
  )
})

.crs <- eventReactive(input$crs_run, {
  if (is_empty(input$crs_id)) {
    "This analysis requires a user id, a product id, and product ratings.\nIf these variables are not available please select another dataset.\n\n" %>%
      suggest_data("ratings")
  } else if (!input$show_filter || is_empty(input$data_filter)) {
    "A data filter must be set to generate recommendations using\ncollaborative filtering. Add a filter in the Data > View tab.\nNote that the users in the training sample should not overlap\nwith the users in the test sample." %>%
      add_class("crs")
  } else if (!is_empty(r_info[["filter_error"]])) {
    "An invalid filter has been set for this dataset. Please\nadjust the filter in the Data > View tab and try again" %>%
      add_class("crs")
  } else if (length(input$crs_pred) < 1) {
    "Please select one or more products to generate recommendations" %>%
      add_class("crs")
  } else {
    withProgress(message = "Estimating model", value = 1, {
      do.call(crs, crs_inputs())
    })
  }
})

.summary_crs <- reactive({
  if (not_pressed(input$crs_run)) {
    "** Press the Estimate button to generate recommendations **"
  } else if (is_empty(input$crs_id)) {
    "This analysis requires a user id, a product id, and product ratings.\nIf these variables are not available please select another dataset.\n\n" %>%
      suggest_data("ratings")
  } else {
    summary(.crs())
  }
})

.plot_crs <- reactive({
  if (not_pressed(input$crs_run)) return("** Press the Estimate button to generate recommendations **")
  isolate({
    if (is_empty(input$crs_id)) return(invisible())
    withProgress(message = "Generating plots", value = 1, {
      plot.crs(.crs())
    })
  })
})

## Add reporting option
observeEvent(input$crs_report, {
  crs <- .crs()
  if (is.character(crs)) {
    return(invisible())
  } else if (!any(is.na(crs$act))) {
    outputs <- c("summary", "plot")
    figs <- TRUE
  } else {
    outputs <- "summary"
    figs <- FALSE
  }
  if (nrow(crs$recommendations) > 36) {
    inp_out <- list(list(n = 36), "")
  } else {
    inp_out <- list("", "")
  }
  if (!is_empty(input$crs_store_pred_name)) {
    fixed <- fix_names(input$crs_store_pred_name)
    updateTextInput(session, "crs_store_pred_name", value = fixed)
    xcmd <- paste0(fixed, " <- result$recommendations\nregister(\"", fixed, "\")")
  } else {
    xcmd <- ""
  }

  update_report(
    inp_main = clean_args(crs_inputs(), crs_args),
    fun_name = "crs",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = crs_plot_width(),
    fig.height = crs_plot_height(),
    xcmd = xcmd
  )
})

## Store results
observeEvent(input$crs_store_pred, {
  req(input$crs_store_pred_name)
  pred <- .crs()
  if (!is.data.frame(pred$recommendations)) {
    return("No data selected to generate recommendations")
  }
  fixed <- fix_names(input$crs_store_pred_name)
  updateTextInput(session, "crs_store_pred_name", value = fixed)
  r_data[[fixed]] <- pred$recommendations
  register(fixed)

  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  showModal(
    modalDialog(
      title = "Data Stored",
      span(
        paste0("Dataset '", fixed, "' was successfully added
                to the datasets dropdown. Add code to Report > Rmd or
                Report > R to (re)create the dataset by clicking the
                report icon on the bottom left of your screen.")
      ),
      footer = modalButton("OK"),
      size = "s",
      easyClose = TRUE
    )
  )
})

dl_crs_recommendations <- function(path) {
  pred <- .crs()
  if (!is.data.frame(pred$recommendations)) {
    write.csv("No recommendations available", file = path, row.names = FALSE)
  } else {
    write.csv(pred$recommendations, file = path, row.names = FALSE)
  }
}

download_handler(
  id = "dl_crs_recommendations",
  fun = dl_crs_recommendations,
  fn = function() paste0(input$dataset, "_recommendations"),
  type = "csv",
  caption = "Save collaborative filtering recommendations"
)

download_handler(
  id = "dlp_crs",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_recommendations"),
  type = "png",
  caption = "Save collaborative filtering plot",
  plot = .plot_crs,
  width = crs_plot_width,
  height = crs_plot_height
)
