ctree_plots <- c("None" = "", "Prune" = "prune" , "Tree" = "tree", "Importance" = "imp")

## list of function arguments
crtree_args <- as.list(formals(crtree))

## list of function inputs selected by user
crtree_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  crtree_args$data_filter <- if (input$show_filter) input$data_filter else ""
  crtree_args$dataset <- input$dataset
  for (i in r_drop(names(crtree_args)))
    crtree_args[[i]] <- input[[paste0("crtree_",i)]]
  crtree_args
})

crtree_pred_args <- as.list(if (exists("predict.crtree")) formals(predict.crtree)
                         else formals(radiant.model:::predict.crtree))

# list of function inputs selected by user
crtree_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(crtree_pred_args))
    crtree_pred_args[[i]] <- input[[paste0("crtree_",i)]]

  crtree_pred_args$pred_cmd <- crtree_pred_args$pred_data <- ""
  if (input$crtree_predict == "cmd") {
    crtree_pred_args$pred_cmd <- gsub("\\s", "", input$crtree_pred_cmd) %>% gsub("\"","\'",.)
  } else if (input$crtree_predict == "data") {
    crtree_pred_args$pred_data <- input$crtree_pred_data
  } else if (input$crtree_predict == "datacmd") {
    crtree_pred_args$pred_cmd <- gsub("\\s", "", input$crtree_pred_cmd) %>% gsub("\"","\'",.)
    crtree_pred_args$pred_data <- input$crtree_pred_data
  }
  crtree_pred_args
})

crtree_pred_plot_args <- as.list(if (exists("plot.model.predict")) formals(plot.model.predict)
                                else formals(radiant.model:::plot.model.predict))

# list of function inputs selected by user
crtree_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(crtree_pred_plot_args))
    crtree_pred_plot_args[[i]] <- input[[paste0("crtree_",i)]]
  crtree_pred_plot_args
})

output$ui_crtree_rvar <- renderUI({
  req(input$crtree_type)

  withProgress(message = "Acquiring variable information", value = 1, {
    if (input$crtree_type == "classification") {
      vars <- two_level_vars()
    } else {
      isNum <- .getclass() %in% c("numeric","integer")
      vars <- varnames()[isNum]
    }
  })
  selectInput(inputId = "crtree_rvar", label = "Response variable:", choices = vars,
  	selected = state_single("crtree_rvar",vars), multiple = FALSE)
})

output$ui_crtree_lev <- renderUI({
  req(input$crtree_type == "classification")
  req(input$crtree_rvar)
  levs <- c()
  if (available(input$crtree_rvar))
    levs <- .getdata()[[input$crtree_rvar]] %>% as.factor %>% levels

  selectInput(inputId = "crtree_lev", label = "Choose level:",
              choices = levs,
              selected = state_init("crtree_lev"))
})

output$ui_crtree_evar <- renderUI({
  if (not_available(input$crtree_rvar)) return()
	notChar <- "character" != .getclass()
  vars <- varnames()[notChar]
  if (length(vars) > 0)
    vars <- vars[-which(vars == input$crtree_rvar)]

  init <- if (input$crtree_type == "classification") input$logit_evar else input$reg_evar

  selectInput(inputId = "crtree_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("crtree_evar", vars, init),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_crtree_wts <- renderUI({
  isNum <- .getclass() %in% c("numeric","integer")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$crtree_evar)) {
    vars <- setdiff(vars, input$crtree_evar)
    names(vars) <- varnames() %>% {.[match(vars, .)]} %>% names
  }
  vars <- c("None", vars)

  selectInput(inputId = "crtree_wts", label = "Weights:", choices = vars,
    selected = state_single("crtree_wts", vars),
    multiple = FALSE)
})

## reset prediction settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "crtree_predict", selected = "none")
})

output$ui_crtree_predict_plot <- renderUI({
  predict_plot_controls("crtree")
})

output$ui_crtree <- renderUI({
  tagList(
    wellPanel(
      actionButton("crtree_run", "Estimate", width = "100%")
    ),
    conditionalPanel(condition = "input.tabs_crtree == 'Predict'",
      wellPanel(
        selectInput("crtree_predict", label = "Prediction input:", reg_predict,
          selected = state_single("crtree_predict", reg_predict, "none")),
        conditionalPanel("input.crtree_predict == 'data' | input.crtree_predict == 'datacmd'",
          selectizeInput(inputId = "crtree_pred_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_single("crtree_pred_data", c("None" = "",r_data$datasetlist)), multiple = FALSE)
        ),
        conditionalPanel("input.crtree_predict == 'cmd' | input.crtree_predict == 'datacmd'",
          returnTextAreaInput("crtree_pred_cmd", "Prediction command:",
            value = state_init("crtree_pred_cmd", ""))
        ),
        conditionalPanel(condition = "input.crtree_predict != 'none'",
          checkboxInput("crtree_pred_plot", "Plot predictions", state_init("crtree_pred_plot", FALSE)),
          conditionalPanel("input.crtree_pred_plot == true",
            uiOutput("ui_crtree_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel("input.crtree_predict == 'data' | input.crtree_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("crtree_store_pred_name", "Store predictions:", state_init("crtree_store_pred_name","predict_crtree"))),
            tags$td(actionButton("crtree_store_pred", "Store"), style="padding-top:30px;")
          )
        )
      )
    ),
    conditionalPanel(condition = "input.tabs_crtree == 'Plot'",
      wellPanel(
        selectInput("crtree_plots", "Plots:", choices = ctree_plots,
          selected = state_single("crtree_plots", ctree_plots)),
        conditionalPanel(condition = "input.crtree_plots == 'tree'",
          radioButtons(inputId = "crtree_orient", label = "Plot direction:",
            c("Left-right" = "LR", "Top-down" = "TD"),
            state_init("crtree_orient", "LR"), inline = TRUE)
        )
      )
    ),

    wellPanel(
      radioButtons("crtree_type", label = NULL, c("classification","regression"),
        selected = state_init("crtree_type", "classification"),
        inline = TRUE),
	    uiOutput("ui_crtree_rvar"),
      uiOutput("ui_crtree_lev"),
	    uiOutput("ui_crtree_evar"),
      # uiOutput("ui_crtree_wts"),
      conditionalPanel(condition = "input.crtree_type == 'classification'",
        tags$table(
          tags$td(numericInput("crtree_prior", label = "Prior:",
            value = state_init("crtree_prior", .5, na.rm = FALSE))),
          tags$td(numericInput("crtree_cost", label = "Cost:",
            value = state_init("crtree_cost", NA))),
          tags$td(numericInput("crtree_margin", label = "Margin:",
            value = state_init("crtree_margin", NA)))
        )
      ),
      tags$table(
        tags$td(numericInput("crtree_cp", label = "Complexity:", min = 0,
          max = 1, step = 0.01,
          value = state_init("crtree_cp", 0.001), width = "116px")),
        tags$td(numericInput("crtree_nodes", label = "Max. nodes:", min = 2,
          value = state_init("crtree_nodes", NA), width = "100%"))
      ),
      tags$table(
        tags$td(numericInput("crtree_K", label = "K-folds:",
          value = state_init("crtree_K", 10), width = "116px")),
        tags$td(numericInput("crtree_seed", label = "Seed:",
          value = state_init("crtree_seed", 1234), width = "100%"))
      ),

      conditionalPanel(condition = "input.tabs_crtree == 'Summary'",
        tags$table(
          tags$td(textInput("crtree_store_res_name", "Store residuals:", state_init("crtree_store_res_name","residuals_crtree"))),
          tags$td(actionButton("crtree_store_res", "Store"), style="padding-top:30px;")
        )
      )
    ),
  	help_and_report(modal_title = "Classification and regression trees",
  	                fun_name = "crtree",
  	                help_file = inclMD(file.path(getOption("radiant.path.model"),"app/tools/help/crtree.md")))
	)
})

crtree_plot_width <- function() 650

crtree_plot_height <- function() 500

crtree_plot_height <- function() {
  if (crtree_available() != "available") return(500)
  300 + 20 * length(.crtree()$vars)
}

crtree_pred_plot_height <- function()
  if (input$crtree_pred_plot) 500 else 0

## output is called from the main radiant ui.R
output$crtree <- renderUI({

		register_print_output("summary_crtree", ".summary_crtree")
    register_print_output("predict_crtree", ".predict_print_crtree")
    register_plot_output("predict_plot_crtree", ".predict_plot_crtree")
    register_plot_output("plot_crtree", ".plot_crtree",
                          height_fun = "crtree_plot_height",
                          width_fun = "crtree_plot_width")

		## two separate tabs
		crtree_output_panels <- tabsetPanel(
	    id = "tabs_crtree",
      tabPanel("Summary", verbatimTextOutput("summary_crtree")),
      tabPanel("Predict",
        conditionalPanel("input.crtree_pred_plot == true",
          plot_downloader("crtree", height = crtree_pred_plot_height(), po = "dlp_", pre = ".predict_plot_"),
          plotOutput("predict_plot_crtree", width = "100%", height = "100%")
        ),
        downloadLink("dl_crtree_pred", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("predict_crtree")
      ),
	    tabPanel("Plot",
        conditionalPanel("input.crtree_plots == 'tree'",
          actionLink("crtree_save_plot", "", class = "fa fa-download alignright", onclick = "window.print();"),
          DiagrammeR::DiagrammeROutput("crtree_plot", width = "100%", height = "100%")
        ),
        conditionalPanel("input.crtree_plots != 'tree'",
          plot_downloader("crtree", height = crtree_plot_height()),
          plotOutput("plot_crtree", width = "100%", height = "100%")
        )
      )
	  )
		stat_tab_panel(menu = "Model > Estimate",
		              tool = "Classification and regression trees",
		              tool_ui = "ui_crtree",
		             	output_panels = crtree_output_panels)

})

output$crtree_plot <- DiagrammeR::renderDiagrammeR({
  cr <- .crtree()
  if (is.null(cr)) {
    invisible()
  } else {
    withProgress(message = 'Generating tree diagramm', value = 1,
      plot(cr, plots = "tree", orient = input$crtree_orient)
    )
  }
})

.plot_crtree <- reactive({
  if (crtree_available() != "available") return(crtree_available())
  if (not_pressed(input$crtree_run)) return("** Press the Estimate button to estimate the model **")
  if (is_empty(input$crtree_plots)) return("Please select a plot type from the drop-down menu")

  if (input$crtree_plots == "prune")
    plot(.crtree(), plots = "prune", shiny = TRUE)
  else if (input$crtree_plots == "imp")
    plot(.crtree(), plots = "imp", shiny = TRUE)
})

crtree_available <- reactive({
  if (not_available(input$crtree_rvar))
    return("This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))

  if (not_available(input$crtree_evar))
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("titanic"))

  "available"
})

.crtree <- eventReactive(input$crtree_run, {
  withProgress(message = "Estimating model", value = 1,
	  do.call(crtree, crtree_inputs())
  )
})

.summary_crtree <- reactive({
  if (crtree_available() != "available") return(crtree_available())
  if (not_pressed(input$crtree_run)) return("** Press the Estimate button to estimate the model **")

  summary(.crtree())
})


.predict_crtree <- reactive({
  if (crtree_available() != "available") return(crtree_available())
  if (not_pressed(input$crtree_run)) return("** Press the Estimate button to estimate the model **")
  if (is_empty(input$crtree_predict, "none")) return("** Select prediction input **")

  if((input$crtree_predict == "data" || input$crtree_predict == "datacmd") && is_empty(input$crtree_pred_data))
    return("** Select data for prediction **")
  if(input$crtree_predict == "cmd" && is_empty(input$crtree_pred_cmd))
    return("** Enter prediction commands **")

  withProgress(message = "Generating predictions", value = 1, {
    do.call(predict, c(list(object = .crtree()), crtree_pred_inputs()))
  })
})

.predict_print_crtree <- reactive({
  .predict_crtree() %>% {if (is.character(.)) cat(.,"\n") else print(.)}
})

.predict_plot_crtree <- reactive({
  if (crtree_available() != "available") return(crtree_available())
  req(input$crtree_pred_plot, available(input$crtree_xvar))
  if (not_pressed(input$crtree_run)) return(invisible())
  if (is_empty(input$crtree_predict, "none")) return(invisible())
  if((input$crtree_predict == "data" || input$crtree_predict == "datacmd") && is_empty(input$crtree_pred_data))
    return(invisible())
  if(input$crtree_predict == "cmd" && is_empty(input$crtree_pred_cmd))
    return(invisible())

  do.call(plot, c(list(x = .predict_crtree()), crtree_pred_plot_inputs()))
})

observeEvent(input$crtree_store_pred, {
  req(!is_empty(input$crtree_pred_data), pressed(input$crtree_run))
  pred <- .predict_crtree()
  if (is.null(pred)) return()
  withProgress(message = 'Storing predictions', value = 1,
    store(pred, data = input$crtree_pred_data, name = input$crtree_store_pred_name)
  )
})

observeEvent(input$crtree_store_res, {
  req(pressed(input$crtree_run))
  robj <- .crtree()
  if (!is.list(robj)) return()
  withProgress(message = 'Storing residuals', value = 1,
    store(robj, name = input$crtree_store_res_name)
  )
})

output$dl_crtree_pred <- downloadHandler(
  filename = function() { "crtree_predictions.csv" },
  content = function(file) {
    if (pressed(input$crtree_run)) {
      .predict_crtree() %>% write.csv(file = file, row.names = FALSE)
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)

observeEvent(input$crtree_report, {
  if (is_empty(input$crtree_evar)) return(invisible())
  outputs <- c("summary")
  inp_out <- list("","")
  xcmd <- ""
  figs <- FALSE
  if (!is_empty(input$crtree_predict, "none") &&
      (!is_empty(input$crtree_pred_data) || !is_empty(input$crtree_pred_cmd))) {

    pred_args <- clean_args(crtree_pred_inputs(), crtree_pred_args[-1])
    inp_out[[2 + figs]] <- pred_args

    outputs <- c(outputs,"pred <- predict")

    xcmd <- paste0(xcmd, "print(pred, n = 10)")
    if (input$crtree_predict %in% c("data","datacmd"))
      xcmd <- paste0(xcmd, "\nstore(pred, data = \"", input$crtree_pred_data, "\", name = \"", input$crtree_store_pred_name,"\")")
    xcmd <- paste0(xcmd, "\n# write.csv(pred, file = \"~/crtree_predictions.csv\", row.names = FALSE)\n")

    if (input$crtree_pred_plot && !is_empty(input$crtree_xvar)) {
      inp_out[[3 + figs]] <- clean_args(crtree_pred_plot_inputs(), crtree_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  if (input$crtree_plots == "tree") {
    xcmd <- paste0(xcmd, "#plot(result, plots = \"prune\", custom = FALSE)")
    xcmd <- paste0(xcmd, "\nrender(plot(result, orient = \"", input$crtree_orient, "\"))")
  } else if (input$crtree_plots == "prune") {
    figs <- TRUE
    xcmd <- paste0(xcmd, "plot(result, plots = \"prune\", custom = FALSE)")
    xcmd <- paste0(xcmd, "\n#render(plot(result, orient = \"", input$crtree_orient, "\"))")
  } else {
    figs <- TRUE
    xcmd <- paste0(xcmd, "plot(result, plots = \"imp\", custom = FALSE)")
    xcmd <- paste0(xcmd, "\n#render(plot(result, orient = \"", input$crtree_orient, "\"))")
  }

  ci <- crtree_inputs()
  if (input$crtree_type == "regression") {
    ci$prior <- ci$cost <- ci$margin <- NULL
  }

  update_report(inp_main = clean_args(ci, crtree_args),
                fun_name = "crtree",
                inp_out = inp_out,
                outputs = outputs,
                figs = figs,
                fig.width = crtree_plot_width(),
                fig.height = crtree_plot_height(),
                xcmd = xcmd)
})
