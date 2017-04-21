## list of function arguments
nb_args <- as.list(formals(nb))

## list of function inputs selected by user
nb_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  nb_args$data_filter <- if (input$show_filter) input$data_filter else ""
  nb_args$dataset <- input$dataset
  for (i in r_drop(names(nb_args)))
    nb_args[[i]] <- input[[paste0("nb_",i)]]
  nb_args
})

nb_pred_args <- as.list(if (exists("predict.nb")) formals(predict.nb)
                         else formals(radiant.model:::predict.nb))

## list of function inputs selected by user
nb_pred_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(nb_pred_args))
    nb_pred_args[[i]] <- input[[paste0("nb_",i)]]

  nb_pred_args$pred_cmd <- nb_pred_args$pred_data <- ""
  if (input$nb_predict == "cmd") {
    nb_pred_args$pred_cmd <- gsub("\\s", "", input$nb_pred_cmd) %>% gsub("\"","\'",.)
  } else if (input$nb_predict == "data") {
    nb_pred_args$pred_data <- input$nb_pred_data
  } else if (input$nb_predict == "datacmd") {
    nb_pred_args$pred_cmd <- gsub("\\s", "", input$nb_pred_cmd) %>% gsub("\"","\'",.)
    nb_pred_args$pred_data <- input$nb_pred_data
  }
  nb_pred_args
})

nb_pred_plot_args <- as.list(if (exists("plot.model.predict")) formals(plot.model.predict)
                                else formals(radiant.model:::plot.model.predict))

## list of function inputs selected by user
nb_pred_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(nb_pred_plot_args))
    nb_pred_plot_args[[i]] <- input[[paste0("nb_",i)]]
  nb_pred_plot_args
})

output$ui_nb_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
  	isFct <- "factor" == .getclass()
    vars <- varnames()[isFct]
  })
  selectInput(inputId = "nb_rvar", label = "Response variable:", choices = vars,
  	selected = state_single("nb_rvar",vars), multiple = FALSE)
})

output$ui_nb_evar <- renderUI({
  if (not_available(input$nb_rvar)) return()
	notVar <- !.getclass() %in% c("character","date")
  vars <- varnames()[notVar]
  if (length(vars) > 0)
    vars <- vars[-which(vars == input$nb_rvar)]

  init <- input$logit_evar

  selectInput(inputId = "nb_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("nb_evar", vars, init),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

## reset prediction settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "nb_predict", selected = "none")
})

output$ui_nb_predict_plot <- renderUI({
	req(input$nb_rvar)
  var_colors <- ".class" %>% set_names(input$nb_rvar)
  predict_plot_controls("nb", vars_color = var_colors, init_color = ".class")
})

output$ui_nb <- renderUI({
  tagList(
    wellPanel(
      actionButton("nb_run", "Estimate", width = "100%")
    ),
    conditionalPanel(condition = "input.tabs_nb == 'Predict'",
      wellPanel(
        selectInput("nb_predict", label = "Prediction input:", reg_predict,
          selected = state_single("nb_predict", reg_predict, "none")),
        conditionalPanel("input.nb_predict == 'data' | input.nb_predict == 'datacmd'",
          selectizeInput(inputId = "nb_pred_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_single("nb_pred_data", c("None" = "",r_data$datasetlist)), multiple = FALSE)
        ),
        conditionalPanel("input.nb_predict == 'cmd' | input.nb_predict == 'datacmd'",
          returnTextAreaInput("nb_pred_cmd", "Prediction command:",
            value = state_init("nb_pred_cmd", ""))
        ),
        conditionalPanel(condition = "input.nb_predict != 'none'",
          checkboxInput("nb_pred_plot", "Plot predictions", state_init("nb_pred_plot", FALSE)),
          conditionalPanel("input.nb_pred_plot == true",
            uiOutput("ui_nb_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel("input.nb_predict == 'data' | input.nb_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("nb_store_pred_name", "Store predictions:", state_init("nb_store_pred_name","predict_nb"))),
            tags$td(actionButton("nb_store_pred", "Store"), style="padding-top:30px;")
          )
        )
      )
    ),
    wellPanel(
      uiOutput("ui_nb_rvar"),
      uiOutput("ui_nb_evar"),
      numericInput("nb_laplace", label = "Laplace:", min = 0, value = state_init("nb_laplace",0))
    ),
  	help_and_report(modal_title = "Naive Bayes",
  	                fun_name = "nb",
  	                help_file = inclMD(file.path(getOption("radiant.path.model"),"app/tools/help/nb.md")))
	)
})

nb_plot_width <- function() 650
nb_plot_height <- function() {
  if (nb_available() != "available") return(500)
  n_lev <- length(.nb()$lev) - 1
  n_vars <- length(.nb()$vars)
  plot_height <- 300 + 20 * n_vars * n_lev
}
nb_pred_plot_height <- function() if (input$nb_pred_plot) 500 else 0

## output is called from the main radiant ui.R
output$nb <- renderUI({

		register_print_output("summary_nb", ".summary_nb")
    register_print_output("predict_nb", ".predict_print_nb")
    register_plot_output("predict_plot_nb", ".predict_plot_nb",
                          height_fun = "nb_pred_plot_height")
		register_plot_output("plot_nb", ".plot_nb",
                          height_fun = "nb_plot_height",
                          width_fun = "nb_plot_width")

		## two separate tabs
		nb_output_panels <- tabsetPanel(
	    id = "tabs_nb",
	    tabPanel("Summary", verbatimTextOutput("summary_nb")),
      tabPanel("Predict",
        conditionalPanel("input.nb_pred_plot == true",
          plot_downloader("nb", height = nb_pred_plot_height(), po = "dlp_", pre = ".predict_plot_"),
          plotOutput("predict_plot_nb", width = "100%", height = "100%")
        ),
        downloadLink("dl_nb_pred", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("predict_nb")
      ),
	    tabPanel("Plot", plot_downloader("nb", height = nb_plot_height()),
               plotOutput("plot_nb", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Model > Estimate",
		              tool = "Naive Bayes",
		              tool_ui = "ui_nb",
		             	output_panels = nb_output_panels)

})

nb_available <- reactive({
  if (not_available(input$nb_rvar))
    return("This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))

  if (not_available(input$nb_evar))
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("titanic"))

  "available"
})

.nb <- eventReactive(input$nb_run, {
  withProgress(message = "Estimating model", value = 1,
	  do.call(nb, nb_inputs())
  )
})

.summary_nb <- reactive({
  if (nb_available() != "available") return(nb_available())
  if (not_pressed(input$nb_run)) return("** Press the Estimate button to estimate the model **")

  summary(.nb())
})

.predict_nb <- reactive({
  if (nb_available() != "available") return(nb_available())
  if (not_pressed(input$nb_run)) return("** Press the Estimate button to estimate the model **")
  if (is_empty(input$nb_predict, "none")) return("** Select prediction input **")

  if((input$nb_predict == "data" || input$nb_predict == "datacmd") && is_empty(input$nb_pred_data))
    return("** Select data for prediction **")
  if(input$nb_predict == "cmd" && is_empty(input$nb_pred_cmd))
    return("** Enter prediction commands **")

  withProgress(message = "Generating predictions", value = 1, {
    do.call(predict, c(list(object = .nb()), nb_pred_inputs()))
  })
})

.predict_print_nb <- reactive({
  .predict_nb() %>% {if (is.character(.)) cat(.,"\n") else print(.)}
})

.predict_plot_nb <- reactive({
  if (nb_available() != "available") return(nb_available())
  req(input$nb_pred_plot, available(input$nb_xvar))
  if (not_pressed(input$nb_run)) return(invisible())
  if (is_empty(input$nb_predict, "none")) return(invisible())
  if((input$nb_predict == "data" || input$nb_predict == "datacmd") && is_empty(input$nb_pred_data))
    return(invisible())
  if(input$nb_predict == "cmd" && is_empty(input$nb_pred_cmd))
    return(invisible())

  do.call(plot, c(list(x = .predict_nb()), nb_pred_plot_inputs()))
})

.plot_nb <- reactive({
  if (nb_available() != "available") return(nb_available())
  if (not_pressed(input$nb_run)) return("** Press the Estimate button to estimate the model **")
  plot(.nb())
})

observeEvent(input$nb_store_pred, {
  req(!is_empty(input$nb_pred_data), pressed(input$nb_run))
  pred <- .predict_nb()
  if (is.null(pred)) return()
  withProgress(message = 'Storing predictions', value = 1,
    store(pred, data = input$nb_pred_data, name = input$nb_store_pred_name)
  )
})

observeEvent(input$nb_store_res, {
  req(pressed(input$nb_run))
  robj <- .nb()
  if (!is.list(robj)) return()
  withProgress(message = 'Storing residuals', value = 1,
    store(robj, name = input$nb_store_res_name)
  )
})

output$dl_nb_pred <- downloadHandler(
  filename = function() { "nb_predictions.csv" },
  content = function(file) {
    if (pressed(input$nb_run)) {
      .predict_nb() %>% write.csv(file = file, row.names = FALSE)
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)

observeEvent(input$nb_report, {
  outputs <- c("summary")
  inp_out <- list("","")
  figs <- TRUE
  outputs <- c(outputs, "plot")
  xcmd <- ""
  if (!is_empty(input$nb_predict, "none") &&
      (!is_empty(input$nb_pred_data) || !is_empty(input$nb_pred_cmd))) {

    pred_args <- clean_args(nb_pred_inputs(), nb_pred_args[-1])
    inp_out[[2 + figs]] <- pred_args

    outputs <- c(outputs,"pred <- predict")
    dataset <- if (input$nb_predict %in% c("data","datacmd")) input$nb_pred_data else input$dataset

    xcmd <- paste0("print(pred, n = 10)")
    if (input$nb_predict %in% c("data","datacmd"))
      xcmd <- paste0(xcmd, "\nstore(pred, data = \"", input$nb_pred_data, "\", name = \"", input$nb_store_pred_name,"\")")
    xcmd <- paste0(xcmd, "\n# write.csv(pred, file = \"~/nb_predictions.csv\", row.names = FALSE)")

    if (input$nb_pred_plot && !is_empty(input$nb_xvar)) {
      inp_out[[3 + figs]] <- clean_args(nb_pred_plot_inputs(), nb_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }

  update_report(inp_main = clean_args(nb_inputs(), nb_args),
                fun_name = "nb",
                inp_out = inp_out,
                outputs = outputs,
                figs = figs,
                fig.width = nb_plot_width(),
                fig.height = nb_plot_height(),
                xcmd = xcmd)
})
