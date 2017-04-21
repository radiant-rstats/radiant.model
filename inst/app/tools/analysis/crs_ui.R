# list of function arguments
crs_args <- as.list(formals(crs))

crs_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  crs_args$data_filter <- if (input$show_filter) input$data_filter else ""
  crs_args$dataset <- input$dataset
  for (i in r_drop(names(crs_args)))
    crs_args[[i]] <- input[[paste0("crs_",i)]]
  crs_args
})

###############################################################
# Evaluate model evalbin
###############################################################
output$ui_crs_id <- renderUI({
  vars <- c("None" = "", varnames())
  selectInput(inputId = "crs_id", label = "User id:", choices = vars,
    selected = state_single("crs_id", vars), multiple = FALSE)
})

output$ui_crs_prod <- renderUI({
  req(available(input$crs_id))
  vars <- varnames()
  vars <- vars[-which(vars %in% input$crs_id)]

  selectInput(inputId = "crs_prod", label = "Product id:", choices = vars,
    selected = state_single("crs_prod", vars), multiple = FALSE)
})

output$ui_crs_pred <- renderUI({
  req(input$crs_prod)
  if (available(input$crs_prod))
    levs <- .getdata()[[input$crs_prod]] %>% as.factor %>% levels
  else
    levs <- c()

  selectInput(inputId = "crs_pred", label = "Choose products to recommend:",
              choices = levs,
              selected = state_init("crs_pred", levs),
              multiple = TRUE, size = min(3, length(levs)),
              selectize = FALSE)
})

output$ui_crs_rate <- renderUI({
  req(input$crs_prod)
  vars <- varnames()
  vars <- vars[-which(c(input$crs_id, input$crs_prod) %in% vars)]

  selectInput(inputId = "crs_rate", label = "Ratings variable:", choices = vars,
    selected = state_single("crs_rate", vars), multiple = FALSE)
})

output$ui_crs <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("crs_run", "Estimate", width = "100%")
    ),
  	wellPanel(
      uiOutput("ui_crs_id"),
      uiOutput("ui_crs_prod"),
      uiOutput("ui_crs_pred"),
      uiOutput("ui_crs_rate")
  	),
    ## to store results
    wellPanel(
      tags$table(
        # tags$td(textInput("crs_name", "Store recommendations:", "recommendations_cf")),
        tags$td(textInput("crs_name", "Store recommendations:", paste0(input$dataset, "_cf"))),
        tags$td(actionButton("crs_store_pred", "Store"), style="padding-top:30px;")
      )
    ),
    help_and_report(modal_title = "Collaborative Filtering",
  	                fun_name = "crs",
  	                help_file = inclMD(file.path(getOption("radiant.path.model"),"app/tools/help/crs.md")))
	)
})

crs_plot <- reactive({
  plot_height <- ceiling(length(input$crs_pred) / 4) * 200
  plot_width <- 650
  list(plot_width = plot_width, plot_height = plot_height)
})

crs_plot_width <- function()
  crs_plot() %>% { if (is.list(.)) .$plot_width else 650 }

crs_plot_height <- function()
  crs_plot() %>% { if (is.list(.)) .$plot_height else 500 }

# output is called from the main radiant ui.R
output$crs <- renderUI({
	register_print_output("summary_crs", ".summary_crs")
	register_plot_output("plot_crs", ".plot_crs",
                       	width_fun = "crs_plot_width",
                       	height_fun = "crs_plot_height")

	# one output with components stacked
  crs_output_panels <- tabsetPanel(
     id = "tabs_crs",
     tabPanel("Summary",
       downloadLink("dl_crs_recommendations", "", class = "fa fa-download alignright"), br(),
       verbatimTextOutput("summary_crs")
     ),
     tabPanel("Plot",
       plot_downloader("crs", height = crs_plot_height()),
       plotOutput("plot_crs", height = "100%")
    )
  )

	stat_tab_panel(menu = "Model > Recommend",
	              tool = "Collaborative Filtering",
	              tool_ui = "ui_crs",
	             	output_panels = crs_output_panels)
})

.crs <- eventReactive(input$crs_run, {
  if (is_empty(input$crs_id))
    return("This analysis requires a user id, a product id, and product ratings.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("cf"))

  if (!input$show_filter || is_empty(input$data_filter))
    return("A data filter must be set to generate recommendations using\ncollaborative filtering. Add a filter in the Data > View tab.\nNote that the users in the training sample should not overlap\nwith the users in the validation sample." %>% add_class("crs"))

  if (!is_empty(r_data$filter_error))
    return("An invalid filter has been set for this dataset. Please\nadjust the filter in the Data > View tab and try again" %>% add_class("crs"))

  if(length(input$crs_pred) < 1) return("Please select one or more products to generate recommendations" %>% add_class("crs"))

  withProgress(message = "Estimating model", value = 1, {
	  do.call(crs, crs_inputs())
  })
})

.summary_crs <- reactive({
  if (is_empty(input$crs_id))
    return("This analysis requires a user id, a product id, and product ratings.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("cf"))

  if (not_pressed(input$crs_run)) return("** Press the Estimate button to generate recommendations **")
  summary(.crs())
})

.plot_crs <- reactive({
  if (is_empty(input$crs_id)) return(invisible())
  if (not_pressed(input$crs_run)) return("** Press the Estimate button to generate recommendations **")
  plot.crs(.crs())
})

## Add reporting option
observeEvent(input$crs_report, {
  crs <- .crs() 
  inp_out <- list(list(n = 36),"")
  if (is.character(crs)) {
    return(invisible())
  } else if (!any(is.na(crs$act))) {
    outputs <- c("summary","plot")
    figs <- TRUE
  } else {
    outputs <- "summary"
    figs <- FALSE
  }
  xcmd <- paste0("# store(result, name = \"", input$crs_name,"\")")
  xcmd <- paste0(xcmd, "\n# write.csv(result$recommendations, file = \"~/recommendations_crs.csv\", row.names = FALSE)")

  update_report(inp_main = clean_args(crs_inputs(), crs_args),
                fun_name = "crs",
                inp_out = inp_out,
                outputs = outputs,
                figs = figs,
                fig.width = crs_plot_width(),
                fig.height = crs_plot_height(),
                xcmd = xcmd)
 })

output$dl_crs_recommendations <- downloadHandler(
  filename = function() { "recommendations_crs.csv" },
  content = function(file) {
    pred <- .crs()
    if (!is.data.frame(pred$recommendations)) {
      write.csv("No recommendations available", file = file, row.names = FALSE)
    } else {
      write.csv(pred$recommendations, file = file, row.names = FALSE)
    }
  }
)

## Store results
observeEvent(input$crs_store_pred, {
  pred <- .crs()
  if (!is.data.frame(pred$recommendations)) return("No data selected to generate recommendations")
  store(pred, input$crs_name)

  ## alert user about new dataset
  session$sendCustomMessage(type = "message",
    message = paste0("Dataset '", input$crs_name, "' was successfully added to the datasets dropdown. Add code to R > Report to (re)create the dataset by clicking the report icon on the bottom left of your screen.")
  )
})
