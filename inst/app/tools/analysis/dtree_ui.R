#######################################
## Create decision tree
#######################################
dtree_example <-
"name: Sign contract
variables:
    legal fees: 5000
type: decision
Sign with Movie Company:
    cost: legal fees
    type: chance
    Small Box Office:
        p: 0.3
        payoff: 200000
    Medium Box Office:
        p: 0.6
        payoff: 1000000
    Large Box Office:
        p: 0.1
        payoff: 3000000
Sign with TV Network:
    payoff: 900000"

dtree_max_min <- c("Max" = "max", "Min" = "min")

output$ui_dtree_list <- renderUI({
  dtree_list <- r_data$dtree_list
  if (length(dtree_list) == 0) return(invisible())
  selectInput(inputId = "dtree_list", label = NULL,
    choices = dtree_list, selected = state_init("dtree_list", dtree_list[1]),
    multiple = FALSE, selectize = FALSE, width = "110px")
})

output$ui_dtree_name <- renderUI({
  dtree_name <- input$dtree_list
  if (length(dtree_name) == 0) dtree_name <- dtree_name()
  if (is_empty(dtree_name)) dtree_name <- "dtree"
  textInput("dtree_name", NULL, dtree_name, width = "100px")
})

output$ui_dtree_remove <- renderUI({
  req(length(r_data[["dtree_list"]]) > 1)
  actionButton("dtree_remove", "Remove")
})

output$ui_dtree_sense_name <- renderUI({

  dte <- dtree_eval()
  if (length(dte) < 2) return(HTML("No variables are available for sensitivity analysis. If the input file does contain a variables section, press the Calculate button to show the list of available variables."))
  vars <- dte$vars
  if (is_empty(vars)) return(HTML("No variables are available for sensitivity analysis. If the input file does contain a variables section, press the Calculate button to show the list of available variables."))
  vars[names(vars)] <- names(vars)

  selectInput("dtree_sense_name", label = "Sensitivity to changes in:",
    choices = vars, multiple = FALSE,
    selected = state_single("dtree_sense_name",""))
})

output$ui_dtree_sense_decision <- renderUI({

  dte <- dtree_eval()
  if (length(dte) < 2) return(invisible())
  jl <- dte$jl
  if (is.null(jl)) return(invisible())

  ## all decisions in the tree
  decs <-
    jl$Get(function(x) if (length(x$parent$decision) > 0) x$payoff) %>%
    na.omit %>%
    names

  selectizeInput("dtree_sense_decision", label = "Decisions to evaluate:",
    choices = decs, multiple = TRUE,
    selected = state_multiple("dtree_sense_decision",decs, decs),
    options = list(plugins = list("remove_button")))
})

output$ui_dtree_sense <- renderUI({
  req(input$dtree_sense_name)
  req(input$dtree_sense_decision)
  tagList(
    HTML("<label>Add variable: <i id='dtree_sense_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
          <i id='dtree_sense_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
    with(tags, table(
        td(numericInput("dtree_sense_min", "Min:", value = state_init("dtree_sense_min"))),
        td(numericInput("dtree_sense_max", "Max:", value = state_init("dtree_sense_max"))),
        td(numericInput("dtree_sense_step", "Step:", value = state_init("dtree_sense_step")))
    )),
    textinput_maker(id = "sense", lab = "Add variable", rows = "2", pre = "dtree_")
  )
})

observeEvent(input$dtree_sense_add, {
  var_updater(input$dtree_sense_add, "dtree_sense",
    c(input$dtree_sense_name, input$dtree_sense_min, input$dtree_sense_max, input$dtree_sense_step))
})

observeEvent(input$dtree_sense_del, {
  var_remover("dtree_sense")
})

output$dtree <- renderUI({
  tabsetPanel(
    id = "tabs_dtree",
    tabPanel("Model",
    with(tags,
      table(
        td(help_modal("Decision analysis","dtree_help1", help_file = inclRmd(file.path(getOption("radiant.path.model"),"app/tools/help/dtree.Rmd")))),
        td(HTML("&nbsp;&nbsp;")),
        td(HTML("<i title='Report results' class='fa fa-edit action-button shiny-bound-input' href='' id='dtree_report1'></i>")),
        td(HTML("&nbsp;&nbsp;")),
        td(HTML("&nbsp;&nbsp;")),
        td(radioButtons(inputId = "dtree_opt", label = NULL,
           dtree_max_min, selected = state_init("dtree_opt", "max"), inline = TRUE)),
        td(actionButton("dtree_eval", "Calculate"), style="padding-top:5px;"),
        td(uiOutput("ui_dtree_name"), style="padding-top:-5px"),
        td(uiOutput("ui_dtree_list"), style="padding-top:0px;"),
        td(downloadButton("dtree_save_yaml", "Save input"), style="padding-top:5px;"),
        td(downloadButton("dtree_save", "Save output"), style="padding-top:5px;"),
        td(uiOutput("ui_dtree_remove"), style="padding-top:5px;"),
        td(HTML("<div class='form-group shiny-input-container'><input id='dtree_load_yaml' name='dtree_load_yaml' type='file' accept='.yaml'/></div>"))
      )
    ),
    shinyAce::aceEditor("dtree_edit", mode = "yaml",
      vimKeyBinding = getOption("radiant.vim.keys", default = FALSE),
      wordWrap = TRUE, height = "auto",
      value = state_init("dtree_edit", dtree_example),
      hotkeys = list(dtree_run = list(win = "CTRL-ENTER", mac = "CMD-ENTER"))),
    verbatimTextOutput("dtree_print")
  ),
  tabPanel("Plot",
    actionLink("dtree_save_plot", "", class = "fa fa-download alignright", onclick = "window.print();"),
    with(tags, table(
      td(help_modal("Decision analysis","dtree_help2", help_file = inclRmd(file.path(getOption("radiant.path.model"),"app/tools/help/dtree.Rmd"))), style="padding-top:30px;"),
      td(HTML("&nbsp;&nbsp;")),
      td(HTML("<i title='Report results' class='fa fa-edit action-button shiny-bound-input' href='' id='dtree_report2'></i>"), style="padding-top:30px;"),
      td(HTML("&nbsp;&nbsp;&nbsp;")),
      td(radioButtons(inputId = "dtree_final", label = "Plot decision tree:",
        c("Initial" = FALSE, "Final" = TRUE),
        selected = state_init("dtree_final", FALSE), inline = TRUE)),
      td(actionButton("dtree_eval_plot", "Calculate"), style="padding-top:30px;"),
      td(numericInput("dtree_dec", "Decimals", value = state_init("dtree_dec", 2),
         min = 0, max = 10, width = "70px")),
      td(textInput("dtree_symbol", "Symbol", state_init("dtree_symbol", "$"), width = "70px"))
    )),
    DiagrammeR::DiagrammeROutput("dtree_plot", height = "600px")
  ),
  tabPanel("Sensitivity",
    sidebarLayout(
      sidebarPanel(

        conditionalPanel(condition = "input.dtree_sense_name == null",
          wellPanel(
            actionButton("dtree_eval_sense", "Calculate", width = "100%")
          )
        ),
        conditionalPanel(condition = "input.dtree_sense_name != null",
          wellPanel(
            actionButton("dtree_eval_sensitivity", "Evaluate sensitivity", width = "100%")
          )
        ),
        wellPanel(
          ## vary one 'variable' value through some range
          ## select a payoff or only the final payoff?
          uiOutput("ui_dtree_sense_decision"),
          uiOutput("ui_dtree_sense_name"),
          uiOutput("ui_dtree_sense")
        ),
        help_and_report(modal_title = "Decision analysis", fun_name = "dtree",
                        help_file = inclRmd(file.path(getOption("radiant.path.model"),"app/tools/help/dtree.Rmd")))
      ),
      mainPanel(
        plot_downloader("dtree_sensitivity", height = dtree_sense_height()),
        plotOutput("plot_dtree_sensitivity")
      )
    )
  ))
})

vals_dtree <- reactiveValues(dtree_run = 0, dtree_report = 0)

observe({
  input$dtree_run
  input$dtree_eval_plot
  input$dtree_eval_sense
  if (!is.null(input$dtree_eval)) isolate(vals_dtree$dtree_run %<>% add(1))
})

dtree_name <- function() {
  isolate({
    dtree_name <- input$dtree_name
    if (is_empty(dtree_name)) {
      dtree_name <- stringr::str_match(input$dtree_edit, "\\s*name:\\s*(.*)\\n\\s*type:")[2]
      if (is.na(dtree_name)) {
        dtree_name <- "dtree"
      } else {
        dtree_name %<>% tolower %>% gsub("[^[:alnum:] ]", "", .) %>%
          gsub("\\s+","_",.) %>% gsub("^([0-9]+)",".",.)
      }
    }
    dtree_name
  })
}

dtree_eval <- eventReactive(vals_dtree$dtree_run > 1, {
  req(vals_dtree$dtree_run != 1)
  ## update settings and get data.tree name
  dtree_name <- dtree_namer()

  if (input$dtree_edit != "") {
    withProgress(message = 'Creating decision tree', value = 1, {
      dtree(input$dtree_edit, opt = input$dtree_opt)
    })
  }
})

output$dtree_print <- renderPrint({
  dtree_eval() %>% {if (is.null(.)) cat("** Click the calculate button to generate results **") else summary(.)}
})

dtree_plot_args <- as.list(if (exists("plot.dtree")) formals(plot.dtree)
                         else formals(radiant:::plot.dtree))

## list of function inputs selected by user
dtree_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(dtree_plot_args))
    dtree_plot_args[[i]] <- input[[paste0("dtree_",i)]]
  dtree_plot_args
})

output$dtree_plot <- DiagrammeR::renderDiagrammeR({
  if (is_empty(input$dtree_final)) return(invisible())
  dt <- dtree_eval()
  if (is.null(dt)) {
    return(invisible())
  } else {
    pinp <- dtree_plot_inputs()
    # pinp$shiny <- TRUE
    # DiagrammeR::DiagrammeR(do.call(plot, c(list(x = dt), pinp)))

    pinp$shiny <- TRUE
    do.call(plot, c(list(x = dt), pinp))
  }
})

## Evaluate tree sensitivity
.plot_dtree_sensitivity <- eventReactive(input$dtree_eval_sensitivity, {
  if (is_not(input$dtree_sense_decision))
    return("At least one decision should be selected for evaluation")

  if (is_empty(input$dtree_sense))
    return("No variables were specified for evaluation.\nClick the + icon to add variables for sensitivity evaluation")

  withProgress(message = 'Conducting sensitivity analysis', value = 1,
    sensitivity(dtree_eval(), gsub("\n+", "", input$dtree_sense), input$dtree_sense_decision, shiny = TRUE)
  )
})

dtree_sense_width <- reactive({
  650
})

dtree_sense_height <- eventReactive(input$dtree_eval_sensitivity, {
  if (is_empty(input$dtree_sense, "")) 650 else length(strsplit(input$dtree_sense, ";")[[1]]) * 400
})

output$plot_dtree_sensitivity <- renderPlot({
  req(input$dtree_eval_sensitivity, cancelOutput = TRUE)
  isolate({
  .plot_dtree_sensitivity() %>%
    { if (is.character(.)) {
        plot(x = 1, type = 'n', main = paste0("\n\n\n\n\n\n\n\n",.) ,
             axes = FALSE, xlab = "", ylab = "")
      } else {
        withProgress(message = 'Making plot', value = 1, print(.))
      }
    }
  })
}, width = dtree_sense_width, height = dtree_sense_height)


output$dtree_save <- downloadHandler(
  filename = function() {"dtree.txt"},
  content = function(file) {
    isolate({
      capture.output(dtree(input$dtree_edit) %>% summary) %>% cat(.,file=file,sep="\n")
    })
  }
)

output$dtree_save_yaml <- downloadHandler(
  filename = function() {"dtree.yaml"},
  content = function(file) {
    isolate({
      cat(paste0(input$dtree_edit,"\n"), file = file)
    })
  }
)

observeEvent(input$dtree_load_yaml, {
  ## loading yaml file from disk
  inFile <- input$dtree_load_yaml
  yaml_file <- paste0(readLines(inFile$datapath), collapse = "\n")
  dtree_name <- sub(paste0(".",tools::file_ext(inFile$name)),"",inFile$name)
  r_data[[dtree_name]] <- yaml_file
  r_data[["dtree_list"]] <- c(dtree_name, r_data[["dtree_list"]]) %>% unique
  updateSelectInput(session = session, inputId = "dtree_list", selected = dtree_name)
  shinyAce::updateAceEditor(session, "dtree_edit", value = yaml_file)
})

observeEvent(input$dtree_list, {
  isolate({
    dtree_name <- input$dtree_name
    if (is_empty(dtree_name)) dtree_name <- dtree_name()
    r_data[[dtree_name]] <- input$dtree_edit
  })

  shinyAce::updateAceEditor(session, "dtree_edit", value = r_data[[input$dtree_list]])
})

observeEvent(input$dtree_report1, {
  vals_dtree$dtree_report %<>% add(1)
})

observeEvent(input$dtree_report2, {
  vals_dtree$dtree_report %<>% add(1)
})

observeEvent(input$dtree_report, {
  vals_dtree$dtree_report %<>% add(1)
})


observeEvent(input$dtree_edit, {
  if (!is_empty(input$dtree_edit))
    r_state$dtree_edit <<- input$dtree_edit
})


dtree_namer <- reactive({
  dtree_name <- input$dtree_name
  if (is_empty(dtree_name)) dtree_name <- input$dtree_list
  if (is_empty(dtree_name)) dtree_name <- dtree_name()

  r_data[[dtree_name]] <- input$dtree_edit
  r_data[["dtree_list"]] <- c(dtree_name, r_data[["dtree_list"]]) %>% unique
  updateSelectInput(session = session, inputId = "dtree_list", selected = dtree_name)
  dtree_name
})

## remove yaml input
observeEvent(input$dtree_remove, {
  dtree_name <- input$dtree_list
  r_data[["dtree_list"]] <- setdiff(r_data[["dtree_list"]], dtree_name)
  r_data[[dtree_name]] <- NULL
})

.dtree_report <- observeEvent(vals_dtree$dtree_report, {
  req(vals_dtree$dtree_report > 0)
  outputs <- c("summary")
  inp_out <- list("", "")
  figs <- FALSE
  if (!is_empty(input$dtree_sense) && !is_not(input$dtree_sense_decision)) {
    inp_out[[2]] <- list(vars = gsub("\n+", "", input$dtree_sense), decs = input$dtree_sense_decision)
    outputs <- c(outputs, "sensitivity")
    figs <- TRUE
  }

  ## update settings and get data.tree name
  dtree_name <- dtree_namer()
  id <- sample(seq_len(1000000),1)
  xcmd <-
    clean_args(dtree_plot_inputs(), dtree_plot_args[-1]) %>%
    deparse(control = c("keepNA"), width.cutoff = 500L) %>%
    {if (. == "list()") "render(plot(result))"
     else paste0(sub("list(", "render(plot(result, ", ., fixed = TRUE),")")} %>%
     gsub("[\"\']TRUE[\'\"]", "TRUE", .)

  # update_report(inp_main = list(yl = dtree_name, opt = input$dtree_opt),
  update_report(inp_main = list(yl = dtree_name, opt = input$dtree_opt),
                fun_name = "dtree",
                inp_out = inp_out,
                outputs = outputs,
                figs = figs,
                fig.width = dtree_sense_width(),
                fig.height = dtree_sense_height(),
                xcmd = xcmd)
})
