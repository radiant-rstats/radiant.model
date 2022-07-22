#######################################
## Create decision tree
#######################################
dtree_example <- "name: Sign contract
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
  dtree_list <- r_info[["dtree_list"]]
  req(dtree_list)
  selectInput(
    inputId = "dtree_list", label = NULL,
    choices = dtree_list, selected = state_init("dtree_list", dtree_list[1]),
    multiple = FALSE, selectize = FALSE, width = "110px"
  )
})

output$ui_dtree_name <- renderUI({
  dtree_name <- input$dtree_list[1]
  if (length(dtree_name) == 0) dtree_name <- dtree_name()
  if (radiant.data::is_empty(dtree_name)) dtree_name <- "dtree"
  textInput("dtree_name", NULL, dtree_name, width = "100px")
})

output$ui_dtree_remove <- renderUI({
  req(length(r_info[["dtree_list"]]) > 1)
  actionButton("dtree_remove", "Remove", icon = icon("trash", verify_fa = FALSE), class = "btn-danger")
})

dtreeIsNum <- function(x) {
  if (!grepl("[A-Za-z]+", x)) {
    x <- try(eval(parse(text = x), envir = r_data), silent = TRUE)
    if (inherits(x, "try-error")) {
      FALSE
    } else {
      if (sshhr(is.na(as.numeric(x)))) FALSE else TRUE
    }
  } else {
    FALSE
  }
}

output$ui_dtree_sense_name <- renderUI({
  dte <- dtree_run()
  mess <- HTML("No variables are available for sensitivity analysis. If the input file does contain a variables section, press the Calculate button to show the list of available variables.")
  if (!inherits(dte, "list")) {
    return(mess)
  }
  vars <- dte$yl$variables
  if (radiant.data::is_empty(vars)) {
    return(mess)
  }
  vars <- vars[!is.na(sshhr(sapply(vars, dtreeIsNum)))]
  if (length(vars) == 0) {
    return(mess)
  }
  vars[names(vars)] <- names(vars)

  selectInput(
    "dtree_sense_name",
    label = "Sensitivity to changes in:",
    choices = vars, multiple = FALSE,
    selected = state_single("dtree_sense_name", vars)
  )
})

output$ui_dtree_sense_decision <- renderUI({
  dte <- dtree_run()
  if (inherits(dte, "list") && !is.null(dte[["jl"]])) {
    ## all decisions in the tree
    decs <-
      dte$jl$Get(function(x) if (length(x$parent$decision) > 0) x$payoff) %>%
      na.omit() %>%
      names()
  } else {
    decs <- ""
  }

  selectizeInput(
    "dtree_sense_decision",
    label = "Decisions to evaluate:",
    choices = decs, multiple = TRUE,
    selected = state_multiple("dtree_sense_decision", decs, decs),
    options = list(
      placeholder = "Select decisions to evaluate",
      plugins = list("remove_button")
    )
  )
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
    textinput_maker(id = "sense", lab = "Add variable", rows = 3, pre = "dtree_")
  )
})

observeEvent(input$dtree_sense_add, {
  var_updater(
    input$dtree_sense_add, "dtree_sense",
    input$dtree_sense_name, c(input$dtree_sense_min, input$dtree_sense_max, input$dtree_sense_step),
    fix = FALSE
  )
})

observeEvent(input$dtree_sense_del, {
  var_remover("dtree_sense")
})

output$dtree <- renderUI({
  tabsetPanel(
    id = "tabs_dtree",
    tabPanel(
      "Model",
      with(
        tags,
        table(
          td(help_modal("Decision analysis", "dtree_help1", help_file = inclRmd(file.path(getOption("radiant.path.model"), "app/tools/help/dtree.Rmd")))),
          td(HTML("&nbsp;&nbsp;")),
          td(HTML("<i title='Report results' class='fa fa-edit action-button shiny-bound-input' href='#dtree_report1' id='dtree_report1'></i>")),
          td(HTML("<i title='Report results & Screenshot' class='fa fa-camera action-button shiny-bound-input aligncenter' href='#dtree_screenshot1' id='dtree_screenshot1' onclick='generate_screenshot();'></i>")),
          td(HTML("&nbsp;&nbsp;")),
          td(
            radioButtons(
              inputId = "dtree_opt", label = NULL,
              dtree_max_min, selected = state_init("dtree_opt", "max"), inline = TRUE
            ),
            style = "padding-top:10px;"
          ),
          td(actionButton("dtree_run", "Calculate tree", icon = icon("play", verify_fa = FALSE), class = "btn-success"), class = "top_mini"),
          td(uiOutput("ui_dtree_name"), class = "top_mini"),
          td(uiOutput("ui_dtree_list"), class = "top_mini"),
          td(uiOutput("ui_dtree_remove"), class = "top_mini"),
          td(file_upload_button(
            "dtree_load_yaml",
            label = NULL, accept = ".yaml",
            buttonLabel = "Load input", title = "Load decision tree input file (.yaml)",
            class = "btn-primary"
          ), class = "top_mini"),
          td(download_button("dtree_save_yaml", "Save input", class = "btn-primary"), class = "top_mini"),
          td(download_button("dtree_save", "Save output"), class = "top_mini")
        )
      ),
      shinyAce::aceEditor(
        "dtree_edit",
        mode = "yaml",
        theme = getOption("radiant.ace_theme", default = "tomorrow"),
        wordWrap = TRUE,
        debounce = 0,
        height = "auto",
        value = state_init("dtree_edit", dtree_example) %>% gsub("\t", "    ", .),
        placeholder = "Provide structured input for a decision tree. Then click the\n\"Calculate tree\" button to generate results. Click the ? icon\non the top left of your screen for help and examples",
        vimKeyBinding = getOption("radiant.ace_vim.keys", default = FALSE),
        hotkeys = list(hotkey = list(win = "CTRL-ENTER|SHIFT-ENTER", mac = "CMD-ENTER|SHIFT-ENTER")),
        tabSize = 4,
        showInvisibles = TRUE,
        useSoftTabs = TRUE,
        autoComplete = "live",
        setBehavioursEnabled = FALSE
      ),
      verbatimTextOutput("dtree_print")
    ),
    tabPanel(
      "Plot",
      HTML("<i title='Save plot' class='fa fa-download action-button shiny-bound-input alignright' href='#dtree_screenshot3' id='dtree_screenshot3' onclick='generate_dtree_plot();'></i>"),
      with(tags, table(
        td(help_modal("Decision analysis", "dtree_help2", help_file = inclRmd(file.path(getOption("radiant.path.model"), "app/tools/help/dtree.Rmd"))), style = "padding-top:30px;"),
        td(HTML("&nbsp;&nbsp;")),
        td(HTML("<i title='Report results' class='fa fa-edit action-button shiny-bound-input' href='' id='dtree_report2'></i>"), style = "padding-top:30px;"),
        td(HTML("<i title='Report results & Screenshot' class='fa fa-camera action-button shiny-bound-input aligncenter' href='#dtree_screenshot2' id='dtree_screenshot2' onclick='generate_screenshot();'></i>"), style = "padding-top:30px;"),
        td(HTML("&nbsp;&nbsp;&nbsp;")),
        td(
          radioButtons(
            inputId = "dtree_final", label = "Plot decision tree:",
            c("Initial" = FALSE, "Final" = TRUE),
            selected = state_init("dtree_final", FALSE), inline = TRUE
          ),
          class = "top_small"
        ),
        td(HTML("&nbsp;&nbsp;&nbsp;")),
        td(
          radioButtons(
            inputId = "dtree_orient", label = "Plot direction:",
            c("Left-right" = "LR", "Top-down" = "TD"), inline = TRUE
          ),
          class = "top_small"
        ),
        td(actionButton("dtree_run_plot", "Calculate tree", icon = icon("play", verify_fa = FALSE), class = "btn-success"), class = "top"),
        td(numericInput(
          "dtree_dec", "Decimals",
          value = state_init("dtree_dec", 2),
          min = 0, max = 10, width = "70px"
        )),
        td(textInput("dtree_symbol", "Symbol", state_init("dtree_symbol", "$"), width = "70px"))
      )),
      DiagrammeR::DiagrammeROutput(
        "dtree_plot",
        width = isolate(ifelse(length(input$get_screen_width) == 0, "1600px", paste0(input$get_screen_width - 80, "px"))),
        height = "100%"
      )
    ),
    tabPanel(
      "Sensitivity",
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.dtree_sense_name == null",
            wellPanel(
              actionButton("dtree_run_sense", "Calculate tree", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
            )
          ),
          conditionalPanel(
            condition = "input.dtree_sense_name != null",
            wellPanel(
              actionButton("dtree_run_sensitivity", "Evaluate sensitivity", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
            )
          ),
          wellPanel(
            ## vary one 'variable' value through some range
            ## select a payoff or only the final payoff?
            uiOutput("ui_dtree_sense_decision"),
            uiOutput("ui_dtree_sense_name"),
            uiOutput("ui_dtree_sense")
          ),
          help_and_report(
            modal_title = "Decision analysis", fun_name = "dtree",
            help_file = inclRmd(file.path(getOption("radiant.path.model"), "app/tools/help/dtree.Rmd"))
          )
        ),
        mainPanel(
          download_link("dlp_dtree_sensitivity"),
          plotOutput("plot_dtree_sensitivity")
        )
      )
    )
  )
})

tree_types <- c("name:", "variables:", "type:", "cost:", "payoff:", "p:")
## Create auto complete list
observe({
  req(input$dtree_name, input$dtree_edit)
  comps <- list(
    `tree-input` = c("name:", "variables:", "type: decision", "type: chance", "cost: 0", "payoff: 0", "p: 0.5")
  )

  trees <- r_info[["dtree_list"]]
  if (length(trees) < 2) {
    trees <- input$dtree_name
  } else {
    comps[["dtree_list"]] <- paste0("dtree('", trees, "')")
  }

  ## update active tree when session is stopped
  session$onSessionEnded(function() {
    isolate({
      r_data[[input$dtree_name]] <- input$dtree_edit
    })
  })

  for (tree in trees) {
    rows <- strsplit(input$dtree_edit, "\n")[[1]]
    comps[[tree]] <- gsub("\\s*([^#]+:).*", "\\1", rows) %>%
      gsub("^\\s+", "", .) %>%
      unique() %>%
      .[!. %in% tree_types] %>%
      gsub(":$", "", .) %>%
      .[!grepl("^#", .)]
  }

  ## only using 'static' auto-completion (i.e., not local ('text') or R-language ('rlang'))
  shinyAce::updateAceEditor(
    session, "dtree_edit",
    autoCompleters = "static",
    autoCompleteList = comps
  )
})

vals_dtree <- reactiveValues(dtree_edit_hotkey = 0)

observe({
  input$dtree_edit_hotkey
  input$dtree_run_plot
  input$dtree_run_sense
  if (!is.null(input$dtree_run)) isolate(vals_dtree$dtree_edit_hotkey %<>% add(1))
})

dtree_name <- function() {
  isolate({
    dtree_name <- input$dtree_name
    if (radiant.data::is_empty(dtree_name)) {
      dtree_name <- stringr::str_match(input$dtree_edit, "^\\s*name:\\s*(.*)\\n")[2]
      if (is.na(dtree_name)) {
        dtree_name <- "dtree"
      }
    }
    fix_names(dtree_name)
  })
}

dtree_run <- eventReactive(vals_dtree$dtree_edit_hotkey > 1, {
  req(vals_dtree$dtree_edit_hotkey != 1)
  validate(
    need(!radiant.data::is_empty(input$dtree_edit), "No decision tree input available")
  )

  ## update settings and get data.tree name
  dtree_name <- dtree_namer()

  ## ensure correct spacing
  yl <- gsub(":([^ $])", ": \\1", input$dtree_edit) %>%
    gsub(":[ ]{2,}", ": ", .) %>%
    gsub(":[ ]+\\n", ":\n", .) %>%
    gsub("\\n[ ]*\\n", "\n", .) %>%
    gsub(":\\s*([-]{0,1})(\\.[0-9]+\\s*\\n)", ": \\10\\2", ., perl = TRUE) %>%
    gsub("(\\n[ ]+)([0-9]+)", "\\1_\\2", .)

  shinyAce::updateAceEditor(session, "dtree_edit", value = yl)

  if (input$dtree_edit != "") {
    withProgress(message = "Creating decision tree", value = 1, {
      dtree(yl, opt = input$dtree_opt, envir = r_data)
    })
  }
})

output$dtree_print <- renderPrint({
  dtree_run() %>%
    (function(x) if (is.null(x)) cat("** Click the calculate button to generate results **") else summary(x, input = FALSE, output = TRUE))
})

dtree_plot_args <- as.list(if (exists("plot.dtree")) {
  formals(plot.dtree)
} else {
  formals(radiant.model:::plot.dtree)
})

## list of function inputs selected by user
dtree_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(dtree_plot_args)) {
    dtree_plot_args[[i]] <- input[[paste0("dtree_", i)]]
  }
  dtree_plot_args
})

output$dtree_plot <- DiagrammeR::renderDiagrammeR({
  req(input$dtree_final)
  dt <- dtree_run()
  if (is.null(dt)) {
    invisible()
  } else {
    pinp <- dtree_plot_inputs()
    do.call(plot, c(list(x = dt), pinp))
  }
})

## Evaluate tree sensitivity
.plot_dtree_sensitivity <- eventReactive(input$dtree_run_sensitivity, {
  if (radiant.data::is_empty(input$dtree_sense_decision)) {
    "At least one decision should be selected for evaluation"
  } else if (radiant.data::is_empty(input$dtree_sense)) {
    "No variables were specified for evaluation.\nClick the + icon to add variables for sensitivity evaluation"
  } else {
    withProgress(
      message = "Conducting sensitivity analysis", value = 1,
      sensitivity(dtree_run(), gsub("\n+", "", input$dtree_sense), input$dtree_sense_decision, envir = r_data, shiny = TRUE)
    )
  }
})

dtree_sense_width <- reactive({
  650
})

dtree_sense_height <- eventReactive(input$dtree_run_sensitivity, {
  if (radiant.data::is_empty(input$dtree_sense, "")) {
    650
  } else {
    strsplit(input$dtree_sense, ";") %>%
      unlist() %>%
      unique() %>%
      length() * 400
  }
})

output$plot_dtree_sensitivity <- renderPlot(
  {
    req(input$dtree_run_sensitivity, cancelOutput = TRUE)
    req(input$dtree_sense_name, cancelOutput = TRUE)
    isolate({
      .plot_dtree_sensitivity() %>%
        {
          if (is.character(.)) {
            plot(
              x = 1, type = "n", main = paste0("\n\n\n\n\n\n\n\n", .),
              axes = FALSE, xlab = "", ylab = ""
            )
          } else {
            withProgress(message = "Making plot", value = 1, print(.))
          }
        }
    })
  },
  width = dtree_sense_width,
  height = dtree_sense_height,
  res = 96
)

observeEvent(input$dtree_load_yaml, {
  ## loading yaml file from disk
  if (getOption("radiant.shinyFiles", FALSE)) {
    path <- shinyFiles::parseFilePaths(sf_volumes, input$dtree_load_yaml)
    if (inherits(path, "try-error") || radiant.data::is_empty(path$datapath)) {
      return()
    } else {
      path <- path$datapath
    }
    inFile <- data.frame(
      name = basename(path),
      datapath = path,
      stringsAsFactors = FALSE
    )
  } else {
    inFile <- input$dtree_load_yaml
  }

  yaml_file <- paste0(readLines(inFile$datapath), collapse = "\n")

  ## remove characters that may cause problems in shinyAce
  yaml_file %<>% gsub("[\x80-\xFF]", "", .) %>% gsub("\r", "\n", .)

  dtree_name <- sub(paste0(".", tools::file_ext(inFile$name)), "", inFile$name) %>%
    fix_names()
  r_data[[dtree_name]] <- yaml_file
  if (!bindingIsActive(as.symbol(dtree_name), env = r_data)) {
    shiny::makeReactiveBinding(dtree_name, env = r_data)
  }
  r_info[["dtree_list"]] <- c(dtree_name, r_info[["dtree_list"]]) %>% unique()
  updateSelectInput(session = session, inputId = "dtree_list", selected = dtree_name)
  shinyAce::updateAceEditor(session, "dtree_edit", value = gsub("\t", "    ", yaml_file))
})

observeEvent(input$dtree_list, {
  dtree_name <- fix_names(input$dtree_name)
  if (radiant.data::is_empty(dtree_name)) dtree_name <- dtree_name()
  r_data[[dtree_name]] <- input$dtree_edit

  yl <- r_data[[input$dtree_list[1]]]
  if (is.list(yl)) {
    yl <- yaml::as.yaml(yl, indent = 4)
  }

  shinyAce::updateAceEditor(session, "dtree_edit", value = gsub("\t", "    ", yl))
})

observeEvent(input$dtree_edit, {
  if (!radiant.data::is_empty(input$dtree_edit)) r_state$dtree_edit <<- input$dtree_edit
})

dtree_namer <- reactive({
  dtree_name_org <- input$dtree_name

  if (radiant.data::is_empty(dtree_name_org)) {
    dtree_name <- input$dtree_list[1]
    if (radiant.data::is_empty(dtree_name)) {
      dtree_name <- dtree_name()
    } else {
      dtree_name <- fix_names(dtree_name)
    }
  } else {
    dtree_name <- fix_names(dtree_name_org)
  }

  r_data[[dtree_name]] <- input$dtree_edit
  r_info[["dtree_list"]] <- c(dtree_name, setdiff(r_info[["dtree_list"]], dtree_name_org)) %>% unique()
  if (!bindingIsActive(as.symbol(dtree_name), env = r_data)) {
    shiny::makeReactiveBinding(dtree_name, env = r_data)
  }
  updateSelectInput(session = session, inputId = "dtree_list", selected = dtree_name, choices = r_info[["dtree_list"]])
  dtree_name
})

## remove yaml input
observeEvent(input$dtree_remove, {
  dtree_name <- input$dtree_list[1]
  r_info[["dtree_list"]] <- base::setdiff(r_info[["dtree_list"]], dtree_name)
  r_data[[dtree_name]] <- NULL
})

dtree_report <- function() {
  isolate({
    outputs <- c("summary")
    inp_out <- list(list(input = TRUE, output = FALSE), "")
    figs <- FALSE
    if (!radiant.data::is_empty(input$dtree_sense) && !is_not(input$dtree_sense_decision)) {
      vars <- strsplit(input$dtree_sense, ";")[[1]] %>% gsub("\n+", "", .)
      inp_out[[2]] <- list(
        vars = vars,
        decs = input$dtree_sense_decision,
        custom = FALSE
      )
      outputs <- c(outputs, "sensitivity")
      figs <- TRUE
    }

    ## update settings and get data.tree name
    dtree_name <- dtree_namer()
    xcmd <- clean_args(dtree_plot_inputs(), dtree_plot_args[-1]) %>%
      deparse(control = getOption("dctrl"), width.cutoff = 500L) %>%
      {
        if (. == "list()") {
          "plot(result) %>% render()"
        } else {
          paste0(sub("list(", "plot(result, ", ., fixed = TRUE), " %>% render()")
        }
      } %>%
      gsub("[\"\']TRUE[\'\"]", "TRUE", .)

    inp <- list(yl = dtree_name)
    if (input$dtree_opt == "min") inp$opt <- "min"

    ret <- update_report(
      inp_main = inp,
      fun_name = "dtree",
      inp_out = inp_out,
      outputs = outputs,
      figs = figs,
      fig.width = dtree_sense_width(),
      fig.height = dtree_sense_height(),
      xcmd = xcmd
    )

    ret
  })
}

dl_dtree_save <- function(path) {
  capture.output(dtree(input$dtree_edit, envir = r_data) %>%
    summary(input = FALSE, output = TRUE)) %>%
    cat(file = path, sep = "\n")
}

download_handler(
  id = "dtree_save",
  label = "Save output",
  fun = dl_dtree_save,
  fn = function() {
    ifelse(
      radiant.data::is_empty(input$dtree_name),
      "dtree",
      paste0(input$dtree_name, "_dtree_output")
    )
  },
  type = "txt",
  caption = "Save decision tree output",
  btn = "button",
)

dl_dtree_save_yaml <- function(path) {
  cat(paste0(input$dtree_edit, "\n"), file = path)
}

download_handler(
  id = "dtree_save_yaml",
  label = "Save input",
  fun = dl_dtree_save_yaml,
  fn = function() {
    ifelse(
      radiant.data::is_empty(input$dtree_name),
      "dtree",
      paste0(input$dtree_name, "_dtree_input")
    )
  },
  type = "yaml",
  caption = "Save decision tree input",
  btn = "button",
  class = "btn-primary"
)

download_handler(
  id = "dlp_dtree_sensitivity",
  fun = download_handler_plot,
  fn = function() {
    ifelse(
      radiant.data::is_empty(input$dtree_name),
      "dtree_sensitivity",
      paste0(input$dtree_name, "_dtree_sensitivity")
    )
  },
  type = "png",
  caption = "Save decision tree sensitivity plot",
  plot = .plot_dtree_sensitivity,
  width = dtree_sense_width,
  height = dtree_sense_height
)

observeEvent(input$dtree_report, {
  r_info[["latest_screenshot"]] <- NULL
  dtree_report()
})

observeEvent(input$dtree_report1, {
  r_info[["latest_screenshot"]] <- NULL
  dtree_report()
})

observeEvent(input$dtree_report2, {
  r_info[["latest_screenshot"]] <- NULL
  dtree_report()
})

observeEvent(input$dtree_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_dtree_screenshot")
})

observeEvent(input$dtree_screenshot1, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_dtree_screenshot1")
})

observeEvent(input$dtree_screenshot2, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_dtree_screenshot2")
})

observeEvent(input$dtree_screenshot3, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_dtree_screenshot3")
})

observeEvent(input$modal_dtree_screenshot, {
  dtree_report()
  removeModal()
})

observeEvent(input$modal_dtree_screenshot1, {
  dtree_report()
  removeModal()
})

observeEvent(input$modal_dtree_screenshot2, {
  dtree_report()
  removeModal()
})

observeEvent(input$modal_dtree_screenshot3, {
  dtree_report()
  removeModal()
})