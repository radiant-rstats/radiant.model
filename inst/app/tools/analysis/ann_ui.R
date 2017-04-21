ann_plots <- c("None" = "", "Network" = "net", "Olden" = "olden", "Garson" = "garson")

## list of function arguments
ann_args <- as.list(formals(ann))

## list of function inputs selected by user
ann_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  ann_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ann_args$dataset <- input$dataset
  for (i in r_drop(names(ann_args)))
    ann_args[[i]] <- input[[paste0("ann_",i)]]
  ann_args
})

ann_pred_args <- as.list(if (exists("predict.ann")) formals(predict.ann)
                         else formals(radiant.model:::predict.ann))

# list of function inputs selected by user
ann_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(ann_pred_args))
    ann_pred_args[[i]] <- input[[paste0("ann_",i)]]

  ann_pred_args$pred_cmd <- ann_pred_args$pred_data <- ""
  if (input$ann_predict == "cmd") {
    ann_pred_args$pred_cmd <- gsub("\\s", "", input$ann_pred_cmd) %>% gsub("\"","\'",.)
  } else if (input$ann_predict == "data") {
    ann_pred_args$pred_data <- input$ann_pred_data
  } else if (input$ann_predict == "datacmd") {
    ann_pred_args$pred_cmd <- gsub("\\s", "", input$ann_pred_cmd) %>% gsub("\"","\'",.)
    ann_pred_args$pred_data <- input$ann_pred_data
  }
  ann_pred_args
})

ann_pred_plot_args <- as.list(if (exists("plot.model.predict")) formals(plot.model.predict)
                                else formals(radiant.model:::plot.model.predict))

# list of function inputs selected by user
ann_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(ann_pred_plot_args))
    ann_pred_plot_args[[i]] <- input[[paste0("ann_",i)]]
  ann_pred_plot_args
})

output$ui_ann_rvar <- renderUI({
  req(input$ann_type)

  withProgress(message = "Acquiring variable information", value = 1, {
    if (input$ann_type == "classification") {
      vars <- two_level_vars()
    } else {
      isNum <- .getclass() %in% c("numeric","integer")
      vars <- varnames()[isNum]
    }
  })

  init <- if (input$ann_type == "classification") input$logit_rvar else input$reg_rvar

  selectInput(inputId = "ann_rvar", label = "Response variable:", choices = vars,
  	selected = state_single("ann_rvar",vars, init), multiple = FALSE)
})

output$ui_ann_lev <- renderUI({
  req(input$ann_type == "classification")
  req(input$ann_rvar)
  levs <- c()
  if (available(input$ann_rvar))
    levs <- .getdata()[[input$ann_rvar]] %>% as.factor %>% levels

  selectInput(inputId = "ann_lev", label = "Choose level:",
              choices = levs,
              selected = state_init("ann_lev"))
})

output$ui_ann_evar <- renderUI({
  if (not_available(input$ann_rvar)) return()
	notChar <- "character" != .getclass()
  vars <- varnames()[notChar]
  if (length(vars) > 0)
    vars <- vars[-which(vars == input$ann_rvar)]

  init <- if (input$ann_type == "classification") input$logit_evar else input$reg_evar

  selectInput(inputId = "ann_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("ann_evar", vars, init),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_ann_wts <- renderUI({
  isNum <- .getclass() %in% c("numeric","integer")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$ann_evar)) {
    vars <- setdiff(vars, input$ann_evar)
    names(vars) <- varnames() %>% {.[match(vars, .)]} %>% names
  }
  vars <- c("None", vars)

  selectInput(inputId = "ann_wts", label = "Weights:", choices = vars,
    selected = state_single("ann_wts", vars),
    multiple = FALSE)
})

## reset prediction settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "ann_predict", selected = "none")
})

output$ui_ann_predict_plot <- renderUI({
  predict_plot_controls("ann")
})

output$ui_ann <- renderUI({
  tagList(
    wellPanel(
      actionButton("ann_run", "Estimate", width = "100%")
    ),
    conditionalPanel(condition = "input.tabs_ann == 'Predict'",
      wellPanel(
        selectInput("ann_predict", label = "Prediction input:", reg_predict,
          selected = state_single("ann_predict", reg_predict, "none")),
        conditionalPanel("input.ann_predict == 'data' | input.ann_predict == 'datacmd'",
          selectizeInput(inputId = "ann_pred_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_single("ann_pred_data", c("None" = "",r_data$datasetlist)), multiple = FALSE)
        ),
        conditionalPanel("input.ann_predict == 'cmd' | input.ann_predict == 'datacmd'",
          returnTextAreaInput("ann_pred_cmd", "Prediction command:",
            value = state_init("ann_pred_cmd", ""))
        ),
        conditionalPanel(condition = "input.ann_predict != 'none'",
          checkboxInput("ann_pred_plot", "Plot predictions", state_init("ann_pred_plot", FALSE)),
          conditionalPanel("input.ann_pred_plot == true",
            uiOutput("ui_ann_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel("input.ann_predict == 'data' | input.ann_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("ann_store_pred_name", "Store predictions:", state_init("ann_store_pred_name","predict_ann"))),
            tags$td(actionButton("ann_store_pred", "Store"), style="padding-top:30px;")
          )
        )
      )
    ),
    conditionalPanel(condition = "input.tabs_ann == 'Plot'",
      wellPanel(
        selectInput("ann_plots", "Plots:", choices = ann_plots, 
          selected = state_single("ann_plots", ann_plots))
      )
    ),
    wellPanel(
      radioButtons("ann_type", label = NULL, c("classification","regression"),
        selected = state_init("ann_type", "classification"),
        inline = TRUE),
	    uiOutput("ui_ann_rvar"),
      uiOutput("ui_ann_lev"),
	    uiOutput("ui_ann_evar"),
      uiOutput("ui_ann_wts"),
      tags$table(
        tags$td(numericInput("ann_size", label = "Size:", min = 1, max = 20,
          value = state_init("ann_size",1), width = "77px")),
        tags$td(numericInput("ann_decay", label = "Decay:", min = 0, max = 1,
          step = .1, value = state_init("ann_decay",.5), width = "77px")),
        tags$td(numericInput("ann_seed", label = "Seed:",
          value = state_init("ann_seed", 1234), width = "77px"))
      ),
      conditionalPanel(condition = "input.tabs_ann == 'Summary'",
        tags$table(
          tags$td(textInput("ann_store_res_name", "Store residuals:", state_init("ann_store_res_name","residuals_ann"))),
          tags$td(actionButton("ann_store_res", "Store"), style="padding-top:30px;")
        )
      )
    ),
  	help_and_report(modal_title = "Neural Network (ANN)",
  	                fun_name = "ann",
  	                help_file = inclMD(file.path(getOption("radiant.path.model"),"app/tools/help/ann.md")))
	)
})

ann_plot <- reactive({

  if (ann_available() != "available") return()
  req(input$ann_plots)
  res <- .ann()
  if (is.character(res)) return()
  mlt <- if ("net" %in% input$ann_plots) 40 else 15
  plot_height <- max(500, length(res$model$coefnames) * mlt)
  list(plot_width = 650, plot_height = plot_height)
})

ann_plot_width <- function()
  ann_plot() %>% { if (is.list(.)) .$plot_width else 650 }

ann_plot_height <- function()
  ann_plot() %>% { if (is.list(.)) .$plot_height else 500 }

ann_pred_plot_height <- function()
  if (input$ann_pred_plot) 500 else 0


## output is called from the main radiant ui.R
output$ann <- renderUI({

		register_print_output("summary_ann", ".summary_ann")
    register_plot_output("plot_ann_net", ".plot_ann_net",
                          height_fun = "ann_plot_height",
                          width_fun = "ann_plot_width")
    register_print_output("predict_ann", ".predict_print_ann")
    register_plot_output("predict_plot_ann", ".predict_plot_ann",
                          height_fun = "ann_pred_plot_height")
		register_plot_output("plot_ann", ".plot_ann",
                          height_fun = "ann_plot_height",
                          width_fun = "ann_plot_width")

		## two separate tabs
		ann_output_panels <- tabsetPanel(
	    id = "tabs_ann",
	    tabPanel("Summary",
        verbatimTextOutput("summary_ann")),
        # plot_downloader("ann_net", height = ann_plot_height()),
        # plotOutput("plot_ann_net", width = "100%", height = "100%")),
      tabPanel("Predict",
        conditionalPanel("input.ann_pred_plot == true",
          plot_downloader("ann", height = ann_pred_plot_height(), po = "dlp_", pre = ".predict_plot_"),
          plotOutput("predict_plot_ann", width = "100%", height = "100%")
        ),
        downloadLink("dl_ann_pred", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("predict_ann")
      ),
	    tabPanel("Plot", plot_downloader("ann", height = ann_plot_height()),
               plotOutput("plot_ann", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Model > Estimate",
		              tool = "Neural Network (ANN)",
		              tool_ui = "ui_ann",
		             	output_panels = ann_output_panels)

})

ann_available <- reactive({
  if (not_available(input$ann_rvar))
    return("This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))

  if (not_available(input$ann_evar))
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("titanic"))

  "available"
})

# .ann <- eventReactive(input$ann_run | input$ann_pause == TRUE, {
.ann <- eventReactive(input$ann_run, {
  withProgress(message = "Estimating model", value = 1,
	  do.call(ann, ann_inputs())
  )
})

.summary_ann <- reactive({
  if (ann_available() != "available") return(ann_available())
  if (not_pressed(input$ann_run)) return("** Press the Estimate button to estimate the model **")

  summary(.ann())
})

.predict_ann <- reactive({
  if (ann_available() != "available") return(ann_available())
  if (not_pressed(input$ann_run)) return("** Press the Estimate button to estimate the model **")
  if (is_empty(input$ann_predict, "none")) return("** Select prediction input **")

  if((input$ann_predict == "data" || input$ann_predict == "datacmd") && is_empty(input$ann_pred_data))
    return("** Select data for prediction **")
  if(input$ann_predict == "cmd" && is_empty(input$ann_pred_cmd))
    return("** Enter prediction commands **")

  withProgress(message = "Generating predictions", value = 1, {
    do.call(predict, c(list(object = .ann()), ann_pred_inputs()))
  })
})

.predict_print_ann <- reactive({
  .predict_ann() %>% {if (is.character(.)) cat(.,"\n") else print(.)}
})

.predict_plot_ann <- reactive({
  if (ann_available() != "available") return(ann_available())
  req(input$ann_pred_plot, available(input$ann_xvar))
  if (not_pressed(input$ann_run)) return(invisible())
  if (is_empty(input$ann_predict, "none")) return(invisible())
  if((input$ann_predict == "data" || input$ann_predict == "datacmd") && is_empty(input$ann_pred_data))
    return(invisible())
  if(input$ann_predict == "cmd" && is_empty(input$ann_pred_cmd))
    return(invisible())

  do.call(plot, c(list(x = .predict_ann()), ann_pred_plot_inputs()))
})

.plot_ann <- reactive({
  if (ann_available() != "available")
    return(ann_available())

  req(input$ann_size)

  if (is_empty(input$ann_plots))
    return("Please select a neural network plot from the drop-down menu")
  if (not_pressed(input$ann_run))
    return("** Press the Estimate button to estimate the model **")

  pinp <- list(plots = input$ann_plots, shiny  = TRUE)

  if (input$ann_plots == "net") {
    .ann() %>% { if (is.character(.)) invisible() else capture_plot( do.call(plot, c(list(x = .), pinp))) }
  } else {
    do.call(plot, c(list(x = .ann()), pinp))
  }
})

observeEvent(input$ann_store_pred, {
  req(!is_empty(input$ann_pred_data), pressed(input$ann_run))
  pred <- .predict_ann()
  if (is.null(pred)) return()
  withProgress(message = 'Storing predictions', value = 1,
    store(pred, data = input$ann_pred_data, name = input$ann_store_pred_name)
  )
})

observeEvent(input$ann_store_res, {
  req(pressed(input$ann_run))
  robj <- .ann()
  if (!is.list(robj)) return()
  withProgress(message = 'Storing residuals', value = 1,
    store(robj, name = input$ann_store_res_name)
  )
})

output$dl_ann_pred <- downloadHandler(
  filename = function() { "ann_predictions.csv" },
  content = function(file) {
    if (pressed(input$ann_run)) {
      .predict_ann() %>% write.csv(file = file, row.names = FALSE)
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)

observeEvent(input$ann_report, {
  if (is_empty(input$ann_evar)) return(invisible())

  outputs <- c("summary")
  inp_out <- list(list(prn = TRUE),"")
  xcmd <- ""
  figs <- FALSE

  if (!is_empty(input$ann_plots)) {
    # inp_out[[2]] <- clean_args(ann_plot_inputs(), ann_plot_args[-1])
    inp_out[[2]] <- list(plots = input$ann_plots, custom = FALSE)
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }

  if (!is_empty(input$ann_predict, "none") &&
      (!is_empty(input$ann_pred_data) || !is_empty(input$ann_pred_cmd))) {

    pred_args <- clean_args(ann_pred_inputs(), ann_pred_args[-1])
    inp_out[[2 + figs]] <- pred_args

    outputs <- c(outputs,"pred <- predict")

    xcmd <- paste0(xcmd, "\nprint(pred, n = 10)")
    if (input$ann_predict %in% c("data","datacmd"))
      xcmd <- paste0(xcmd, "\nstore(pred, data = \"", input$ann_pred_data, "\", name = \"", input$ann_store_pred_name,"\")")
    xcmd <- paste0(xcmd, "\n# write.csv(pred, file = \"~/ann_predictions.csv\", row.names = FALSE)")

    if (input$ann_pred_plot && !is_empty(input$ann_xvar)) {
      inp_out[[3 + figs]] <- clean_args(ann_pred_plot_inputs(), ann_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  update_report(inp_main = clean_args(ann_inputs(), ann_args),
                fun_name = "ann",
                inp_out = inp_out,
                outputs = outputs,
                figs = figs,
                fig.width = ann_plot_width(),
                fig.height = ann_plot_height(),
                xcmd = xcmd)
})
