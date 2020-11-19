#######################################
## Simulate data
#######################################

#### Add a "function" input for custom functions
#### This should produce named functions in Report > Rmd
#### that are passed to the simulation and repeat calls
#### Perhaps use an environment instead of a list when you
#### setup a simulation

#### Try putting all input$sim_... and input$rep_... into a list
#### so you can have multiple simulations in the state file and
#### can restore them in the GUI
#### This should be similar to the dtree setup
####
#### Also checkout https://github.com/daattali/advanced-shiny/tree/master/update-input0

sim_types <- list(
  `Probability distributions` = c(
    "Binomial" = "binom",
    "Discrete" = "discrete",
    "Log normal" = "lnorm",
    "Normal" = "norm",
    "Poisson" = "pois",
    "Uniform" = "unif"
  ),
  `Deterministic` = c(
    "Constant" = "const",
    "Data" = "data",
    "Grid search" = "grid",
    "Sequence" = "sequ"
  )
)

sim_types_vec <- c(sim_types[[1]], sim_types[[2]])

sim_args <- as.list(formals(simulater))

## list of function inputs selected by user
sim_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(sim_args))
    sim_args[[i]] <- input[[paste0("sim_", i)]]

  for (i in sim_types_vec)
    if (!i %in% input$sim_types) sim_args[[i]] <- ""

  if (!isTRUE(input$sim_add_functions))
    sim_args[["funcs"]] <- ""

  sim_args
})

rep_args <- as.list(formals(repeater))

## list of function inputs selected by user
rep_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  rep_args$dataset <- input$sim_name
  for (i in r_drop(names(rep_args)))
    rep_args[[i]] <- input[[paste0("rep_", i)]]

  if (is_empty(input$rep_fun)) rep_args$fun <- "none"

  rep_args
})

rep_sum_args <- as.list(if (exists("summary.repeater")) {
  formals(summary.repeater)
} else {
  formals(radiant.model:::summary.repeater)
})

## list of function inputs selected by user
rep_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(rep_sum_args))
    rep_sum_args[[i]] <- input[[paste0("rep_", i)]]
  rep_sum_args
})

rep_plot_args <- as.list(if (exists("plot.repeater")) {
  formals(plot.repeater)
} else {
  formals(radiant.model:::plot.repeater)
})

## list of function inputs selected by user
rep_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(rep_plot_args))
    rep_plot_args[[i]] <- input[[paste0("rep_", i)]]
  rep_plot_args
})

textinput_maker <- function(
  id = "const", lab = "Constant", rows = 3, pre = "sim_",
  placeholder = "Provide values in the input boxes above and then press the + symbol",
  allow_tab = TRUE

) {

  if (allow_tab) {
    onkeydown <- ""
  } else {
    onkeydown <- "if(event.keyCode===9){var v=this.value,s=this.selectionStart,e=this.selectionEnd;this.value=v.substring(0, s)+'\t'+v.substring(e);this.selectionStart=this.selectionEnd=s+1;return false;}"
  }

  ## avoid all sorts of 'helpful' behavior from your browser
  ## based on https://stackoverflow.com/a/35514029/1974918
  id <- paste0(pre, id)
  tags$textarea(
    state_init(id), id = id,
    type = "text",
    rows = rows,
    placeholder = placeholder,
    autocomplete = "off",
    autocorrect = "off",
    autocapitalize = "off",
    spellcheck = "false",
    class = "form-control",
    onkeydown = onkeydown
  )
}

output$ui_sim_types <- renderUI({
  selectizeInput(
    "sim_types", label = "Select types:",
    choices = sim_types, multiple = TRUE,
    selected = state_multiple("sim_types", sim_types_vec),
    options = list(placeholder = "Select types", plugins = list("remove_button"))
  )
})

output$ui_sim_data <- renderUI({
  choices <- c("None" = "none", r_info[["datasetlist"]])
  selectizeInput(
    inputId = "sim_data", label = "Input data for calculations:",
    choices = choices,
    selected = state_single("sim_data", choices, isolate(input$sim_data)),
    multiple = FALSE
  )
})

sim_vars <- reactive({
  input$sim_run
  if (is_empty(input$sim_name)) {
    character(0)
  } else {
    if (is.null(r_data[[input$sim_name]])) {
      character(0)
    } else {
      colnames(r_data[[input$sim_name]])
    }
  }
})

output$ui_rep_vars <- renderUI({
  vars <- sim_vars()
  req(vars)
  form <- input$sim_form %>% sim_cleaner()

  if (!is_empty(form)) {
    s <- gsub(" ", "", form) %>% sim_splitter("=")
    svars <- c()
    for (i in 1:length(s)) {
      if (grepl("^\\s*#", s[[i]][1])) next
      if (grepl("\\s*<-\\s*function\\s*\\(", s[[i]][1])) next
      if (grepl(s[[i]][1], s[[i]][2])) next
      svars <- c(svars, s[[i]][1])
    }
    if (length(svars) > 0) vars <- base::setdiff(vars, svars)
  }

  selectizeInput(
    "rep_vars", label = "Variables to re-simulate:",
    choices = vars, multiple = TRUE,
    selected = state_multiple("rep_vars", vars, isolate(input$rep_vars)),
    options = list(placeholder = "Select variables", plugins = list("remove_button"))
  )
})

output$ui_rep_sum_vars <- renderUI({
  vars <- sim_vars()
  req(!is_empty(vars))
  selectizeInput(
    "rep_sum_vars", label = "Output variables:",
    choices = vars, multiple = TRUE,
    selected = state_multiple("rep_sum_vars", vars, isolate(input$rep_sum_vars)),
    options = list(
      placeholder = "Select variables",
      plugins = list("remove_button", "drag_drop")
    )
  )
})

output$ui_rep_grid_vars <- renderUI({
  const <- input$sim_const %>% sim_cleaner()
  if (const != "") {
    s <- const %>% sim_splitter()
    vars <- c()
    for (i in 1:length(s))
      vars <- c(vars, s[[i]][1])
  }
  req(!is_empty(vars))
  selectizeInput(
    "rep_grid_vars", label = "Name:",
    choices = vars, multiple = FALSE,
    selected = state_single("rep_grid_vars", vars)
  )
})

output$ui_rep_byvar <- renderUI({
  vars <- c("Simulation" = ".sim", "Repeat" = ".rep")
  selectizeInput(
    "rep_byvar", label = "Group by:", choices = vars,
    selected = state_single("rep_byvar", vars), multiple = FALSE,
    options = list(placeholder = "Select group-by variable")
  )
})

output$ui_rep_fun <- renderUI({
  choices <- list(
    "sum" = "sum", "mean" = "mean", "median" = "median",
    "min" = "min", "max" = "max", "sd" = "sd", "var" = "var",
    "sdprop" = "sdprop", "varprop" = "varprop",
    "p01" = "p01", "p025" = "p025", "p05" = "p05", "p10" = "p10",
    "p25" = "p25", "p75" = "p75", "p90" = "p90", "p95" = "p95",
    "p975" = "p975", "p99" = "p99",
    "first" = "first", "last" = "last"
  )
  selectizeInput(
    inputId = "rep_fun", label = "Apply function:",
    choices = choices,
    selected = state_multiple("rep_fun", choices, isolate(input$rep_fun)),
    multiple = TRUE,
    options = list(placeholder = "None", plugins = list("remove_button"))
  )
})

var_updater <- function(variable, var_str, var_name, var_inputs, fix = TRUE) {
  if (is.null(variable) || variable == 0) return()
  if (is_empty(var_inputs[1]) || any(is.na(var_inputs[-1]))) {
    showModal(
      modalDialog(
        title = "Inputs required",
        span("Please provide all required inputs"),
        footer = modalButton("OK"),
        size = "s",
        easyClose = TRUE
      )
    )
  } else {
    if (fix) {
      var_name <- fix_names(var_name)
    }
    inp <- paste(c(var_name, var_inputs), collapse = " ")
    if (is_empty(input[[var_str]])) {
      val <- paste0(inp, ";")
    } else {
      val <- paste0(input[[var_str]], "\n", inp, ";")
    }

    updateTextInput(session = session, var_str, value = val)
  }
}

var_remover <- function(variable) {
  input[[variable]] %>%
    strsplit("\n") %>%
    unlist() %>%
    head(., -1) %>%
    paste0(collapse = "\n") %>%
    updateTextInput(session = session, variable, value = .)
}

observeEvent(input$sim_binom_add, {
  var_updater(
    input$sim_binom_add, "sim_binom",
    input$sim_binom_name, c(input$sim_binom_n, input$sim_binom_p)
  )
})

observeEvent(input$sim_discrete_add, {
  v <- input$sim_discrete_val
  p <- input$sim_discrete_prob

  v <- gsub(",", " ", v) %>% strsplit("\\s+") %>% unlist()
  p <- gsub(",", " ", p) %>% strsplit("\\s+") %>% unlist()

  lp <- length(p)
  lv <- length(v)
  if (lv != lp && lv %% lp == 0) p <- rep(p, lv / lp)

  var_updater(
    input$sim_discrete_add, "sim_discrete",
    input$sim_discrete_name, paste0(c(v, p), collapse = " ")
  )
})

observeEvent(input$sim_lnorm_add, {
  var_updater(input$sim_lnorm_add, "sim_lnorm", input$sim_lnorm_name, c(input$sim_lnorm_mean, input$sim_lnorm_sd))
})

observeEvent(input$sim_norm_add, {
  var_updater(input$sim_norm_add, "sim_norm", input$sim_norm_name, c(input$sim_norm_mean, input$sim_norm_sd))
})

observeEvent(input$sim_pois_add, {
  var_updater(input$sim_pois_add, "sim_pois", input$sim_pois_name, input$sim_pois_lambda)
})

observeEvent(input$sim_unif_add, {
  var_updater(input$sim_unif_add, "sim_unif", input$sim_unif_name, c(input$sim_unif_min, input$sim_unif_max))
})

observeEvent(input$sim_const_add, {
  var_updater(input$sim_const_add, "sim_const", input$sim_const_name, input$sim_const_nr)
})

observeEvent(input$sim_sequ_add, {
  var_updater(input$sim_sequ_add, "sim_sequ", input$sim_sequ_name, c(input$sim_sequ_min, input$sim_sequ_max))
})

observeEvent(input$rep_grid_add, {
  var_updater(
    input$rep_grid_add, "rep_grid",
    input$rep_grid_name, c(input$rep_grid_min, input$rep_grid_max, input$rep_grid_step)
  )
  updateNumericInput(session = session, "rep_nr", value = NA)
})

observeEvent(input$sim_grid_add, {
  var_updater(
    input$sim_grid_add, "sim_grid",
    input$sim_grid_name, c(input$sim_grid_min, input$sim_grid_max, input$sim_grid_step)
  )
})

observeEvent(c(input$sim_grid, input$sim_types), {
  if ("grid" %in% input$sim_types && !is_empty(input$sim_grid)) {
    updateNumericInput(session = session, "sim_nr", value = NA)
  } else {
    val <- ifelse(is_empty(r_state$sim_nr), 1000, r_state$sim_nr)
    updateNumericInput(session = session, "sim_nr", value = val)
  }
})

observeEvent(c(input$rep_grid, input$rep_byvar), {
  if (isTRUE(input$rep_byvar %in% c(".rep", "rep")) && !is_empty(input$rep_grid)) {
    updateNumericInput(session = session, "rep_nr", value = NA)
  } else {
    val <- ifelse(is_empty(r_state$rep_nr), 12, r_state$rep_nr)
    updateNumericInput(session = session, "rep_nr", value = val)
  }
})

observeEvent(input$sim_binom_del, {
  var_remover("sim_binom")
})

observeEvent(input$sim_discrete_del, {
  var_remover("sim_discrete")
})

observeEvent(input$sim_norm_del, {
  var_remover("sim_norm")
})

observeEvent(input$sim_lnorm_del, {
  var_remover("sim_lnorm")
})

observeEvent(input$sim_pois_del, {
  var_remover("sim_pois")
})

observeEvent(input$sim_unif_del, {
  var_remover("sim_unif")
})
observeEvent(input$sim_const_del, {
  var_remover("sim_const")
})

observeEvent(input$rep_grid_del, {
  var_remover("rep_grid")
})

observeEvent(input$sim_sequ_del, {
  var_remover("sim_sequ")
})

observeEvent(input$sim_grid_del, {
  var_remover("sim_grid")
})

## add a spinning refresh icon if the simulation needs to be (re)run
run_refresh(sim_args, "sim", init = "types", label = "Run simulation", relabel = "Re-run simulation", data = FALSE)

## add a spinning refresh icon if the repeated simulation needs to be (re)run
run_refresh(rep_args, "rep", init = "sum_vars", label = "Repeat simulation", data = FALSE)

output$ui_simulater <- renderUI({
  tagList(
    conditionalPanel(
      condition = "input.tabs_simulate == 'Simulate'",
      wellPanel(
        actionButton("sim_run", "Run simulation", width = "100%", icon = icon("play"), class = "btn-success")
      ),
      wellPanel(
        uiOutput("ui_sim_types")
      ),
      ## Using && to check that input.glm_sum_check is not null (must be &&)
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('binom') >= 0",
        wellPanel(
          HTML("<label>Binomial variables: <i id='sim_binom_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_binom_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_binom_name", "Name:", value = state_init("sim_binom_name", ""))),
            td(numericInput("sim_binom_n", "n:", value = state_init("sim_binom_n"), min = 1)),
            td(numericInput("sim_binom_p", "p:", value = state_init("sim_binom_p"), min = 0))
          )),
          textinput_maker("binom", "Binomial")
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('const') >= 0",
        wellPanel(
          HTML("<label>Constant variables: <i id='sim_const_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_const_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_const_name", "Name:", value = state_init("sim_const_name", ""))),
            td(numericInput("sim_const_nr", "Value:", value = state_init("sim_const_nr")))
          )),
          textinput_maker("const", "Constant")
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('discrete') >= 0",
        wellPanel(
          HTML("<label>Discrete variables: <i id='sim_discrete_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_discrete_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_discrete_name", "Name:", value = state_init("sim_discrete_name", ""))),
            td(textInput("sim_discrete_val", "Values:", value = state_init("sim_discrete_val"))),
            td(textInput("sim_discrete_prob", "Prob.:", value = state_init("sim_discrete_prob")))
          )),
          textinput_maker("discrete", "Discrete")
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('lnorm') >= 0",
        wellPanel(
          HTML("<label>Log-normal variables: <i id='sim_lnorm_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_lnorm_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_lnorm_name", "Name:", value = state_init("sim_lnorm_name", ""))),
            td(numericInput("sim_lnorm_mean", "Mean:", value = state_init("sim_lnorm_mean"))),
            td(numericInput("sim_lnorm_sd", "St.dev.:", value = state_init("sim_lnorm_sd"), min = 1))
          )),
          textinput_maker("lnorm", "Log normal")
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('norm') >= 0",
        wellPanel(
          HTML("<label>Normal variables: <i id='sim_norm_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_norm_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_norm_name", "Name:", value = state_init("sim_norm_name", ""))),
            td(numericInput("sim_norm_mean", "Mean:", value = state_init("sim_norm_mean"))),
            td(numericInput("sim_norm_sd", "St.dev.:", value = state_init("sim_norm_sd"), min = 0))
          )),
          textinput_maker("norm", "Normal"),
          checkboxInput("sim_nexact", "Use exact specifications", state_init("sim_nexact", FALSE)),
          textInput("sim_ncorr", "Correlations:", value = state_init("sim_ncorr"))
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('pois') >= 0",
        wellPanel(
          HTML("<label>Poisson variables: <i id='sim_pois_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_pois_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_pois_name", "Name:", value = state_init("sim_pois_name", ""))),
            td(numericInput("sim_pois_lambda", "Lambda:", value = state_init("sim_pois_lambda")))
          )),
          textinput_maker("pois", "Poisson")
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('unif') >= 0",
        wellPanel(
          HTML("<label>Uniform variables: <i id='sim_unif_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_unif_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_unif_name", "Name:", value = state_init("sim_unif_name", ""))),
            td(numericInput("sim_unif_min", "Min:", value = state_init("sim_unif_min"))),
            td(numericInput("sim_unif_max", "Max:", value = state_init("sim_unif_max")))
          )),
          textinput_maker("unif", "Uniform")
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('sequ') >= 0",
        wellPanel(
          HTML("<label>Sequence variables: <i id='sim_sequ_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_sequ_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_sequ_name", "Name:", value = state_init("sim_sequ_name", ""))),
            td(numericInput("sim_sequ_min", "Min:", value = state_init("sim_sequ_min"))),
            td(numericInput("sim_sequ_max", "Max:", value = state_init("sim_sequ_max")))
          )),
          textinput_maker("sequ", "Sequence")
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('grid') >= 0",
        wellPanel(
          HTML("<label>Grid search: <i id='sim_grid_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='sim_grid_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("sim_grid_name", "Name:", value = state_init("sim_grid_name", ""))),
            td(numericInput("sim_grid_min", "Min:", value = state_init("sim_grid_min"))),
            td(numericInput("sim_grid_max", "Max:", value = state_init("sim_grid_max"))),
            td(numericInput("sim_grid_step", "Step:", value = state_init("sim_grid_step")))
          )),
          textinput_maker("grid")
        )
      ),
      conditionalPanel(
        "input.sim_types && input.sim_types.indexOf('data') >= 0",
        wellPanel(
          uiOutput("ui_sim_data")
        )
      ),
      wellPanel(
        with(tags, table(
          td(numericInput(
            "sim_seed", "Set random seed:",
            value = state_init("sim_seed", 1234),
          )),
          td(numericInput(
            "sim_nr", "# sims:", min = 1, max = 10 ^ 6,
            value = state_init("sim_nr", 1000),
            width = "95px"
          ))
        )),
        with(tags, table(
          td(textInput("sim_name", "Simulated data:", state_init("sim_name", "simdat"))),
          td(numericInput("sim_dec", label = "Decimals:", value = state_init("sim_dec", 4), min = 0, width = "95px"))
        )),
        with(tags, table(
          td(checkboxInput("sim_add_functions", "Add functions", state_init("sim_add_functions", FALSE))),
          td(HTML("&nbsp; &nbsp;")),
          td(checkboxInput("sim_show_plots", "Show plots", state_init("sim_show_plots", FALSE)))
        ))
      ),
      help_and_report(
        modal_title = "Simulate", fun_name = "simulater",
        help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/simulater.md"))
      )
    ),
    conditionalPanel(
      condition = "input.tabs_simulate == 'Repeat'",
      wellPanel(
        actionButton("rep_run", "Repeat simulation", width = "100%", icon = icon("play"), class = "btn-success")
      ),
      wellPanel(
        uiOutput("ui_rep_vars"),
        uiOutput("ui_rep_sum_vars")
      ),
      wellPanel(
        uiOutput("ui_rep_byvar"),
        conditionalPanel(
          condition = "input.rep_byvar == '.rep'",
          HTML("<label>Grid search: <i id='rep_grid_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
                <i id='rep_grid_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
          with(tags, table(
            td(textInput("rep_grid_name", "Name:", value = state_init("rep_grid_name", ""))),
            td(numericInput("rep_grid_min", "Min:", value = state_init("rep_grid_min"))),
            td(numericInput("rep_grid_max", "Max:", value = state_init("rep_grid_max"))),
            td(numericInput("rep_grid_step", "Step:", value = state_init("rep_grid_step")))
          )),
          textinput_maker("grid", "", pre = "rep_")
        ),
        uiOutput("ui_rep_fun")
      ),
      wellPanel(
        with(tags, table(
          td(numericInput(
            "rep_seed", "Set random seed:",
            value = state_init("rep_seed", 1234)
          )),
          td(numericInput(
            "rep_nr", "# reps:", min = 1, max = 10 ^ 6,
            value = state_init("rep_nr", 12),
            width = "95px"
          ))
        )),
        with(tags, table(
          td(textInput("rep_name", "Repeat data:", state_init("rep_name", "repdat"))),
          td(numericInput("rep_dec", label = "Decimals:", value = state_init("rep_dec", 4), min = 0, max = 10, width = "95px"))
        )),
        with(tags, table(
          # td(checkboxInput("rep_add_functions", "Add functions", state_init("rep_add_functions", FALSE))),
          # td(HTML("&nbsp; &nbsp;")),
          td(checkboxInput("rep_show_plots", "Show plots", state_init("rep_show_plots", FALSE)))
        ))
      ),
      help_and_report(
        modal_title = "Repeat simulation", fun_name = "repeater",
        help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/simulater.md"))
      )
    )
  )
})

## output is called from the main radiant ui.R
output$simulater <- renderUI({
  register_print_output("summary_simulate", ".summary_simulate")
  register_plot_output(
    "plot_simulate", ".plot_simulate",
    width_fun = "sim_plot_width",
    height_fun = "sim_plot_height"
  )

  register_print_output("summary_repeat", ".summary_repeat")
  register_plot_output(
    "plot_repeat", ".plot_repeat",
    width_fun = "rep_plot_width",
    height_fun = "rep_plot_height"
  )

  ## mulitple tabs with components stacked
  sim_output_panels <- tabsetPanel(
    id = "tabs_simulate",
    tabPanel(
      "Simulate",
      HTML("<label>Simulation formulas:</label>"),
      shinyAce::aceEditor(
        "sim_form",
        mode = "r",
        theme = getOption("radiant.ace_theme", default = "tomorrow"),
        wordWrap = TRUE,
        debounce = 0,
        height = "120px",
        value = state_init("sim_form", "") %>% fix_smart(),
        placeholder = "Use formulas to perform calculations on simulated variables\n(e.g., demand = 5 * price). Press the Run simulation button\nto run the simulation. Click the ? icon on the bottom left\nof your screen for help and examples",
        vimKeyBinding = getOption("radiant.ace_vim.keys", default = FALSE),
        tabSize = getOption("radiant.ace_tabSize", 2),
        useSoftTabs = getOption("radiant.ace_useSoftTabs", TRUE),
        showInvisibles = getOption("radiant.ace_showInvisibles", FALSE),
        autoScrollEditorIntoView = TRUE,
        minLines = 7,
        maxLines = 20
      ),
      conditionalPanel(
        "input.sim_add_functions == true",
        HTML("</br><label>Simulation functions:</label>"),
        shinyAce::aceEditor(
          "sim_funcs",
          mode = "r",
          theme = getOption("radiant.ace_theme", default = "tomorrow"),
          wordWrap = TRUE,
          debounce = 0,
          height = "120px",
          value = state_init("sim_funcs", "") %>% fix_smart(),
          placeholder = "Create your own R functions (e.g., add = function(x, y) {x + y}).\nCall these functions from the 'formula' input and press the Run\nsimulation button to run the simulation. Click the ? icon on the\nbottom left of your screen for help and examples",
          vimKeyBinding = getOption("radiant.ace_vim.keys", default = FALSE),
          tabSize = getOption("radiant.ace_tabSize", 2),
          useSoftTabs = getOption("radiant.ace_useSoftTabs", TRUE),
          showInvisibles = getOption("radiant.ace_showInvisibles", FALSE),
          autoScrollEditorIntoView = TRUE,
          minLines = 7,
          maxLines = 20,
          autoComplete = "live",
          autoCompleters = c("static", "text"),
          autoCompleteList = isolate(radiant_sim_auto())
        )
      ),
      HTML("</br><label>Simulation summary:</label>"),
      verbatimTextOutput("summary_simulate"),
      conditionalPanel(
        condition = "input.sim_show_plots == true",
        HTML("</br><label>Simulation plots:</label>"),
        download_link("dlp_simulate"),
        plotOutput("plot_simulate", height = "100%")
      )
    ),
    tabPanel(
      "Repeat",
      HTML("<label>Repeated simulation formulas:</label>"),
      shinyAce::aceEditor(
        "rep_form",
        mode = "r",
        theme = getOption("radiant.ace_theme", default = "tomorrow"),
        wordWrap = TRUE,
        debounce = 0,
        height = "120px",
        value = state_init("rep_form", "") %>% fix_smart(),
        placeholder = "Press the Repeat simulation button to repeat the simulation specified in the\nSimulate tab. Use formulas to perform additional calculations on the repeated\nsimulation data. Click the ? icon on the bottom left of your screen for help\nand examples",
        vimKeyBinding = getOption("radiant.ace_vim.keys", default = FALSE),
        tabSize = getOption("radiant.ace_tabSize", 2),
        useSoftTabs = getOption("radiant.ace_useSoftTabs", TRUE),
        showInvisibles = getOption("radiant.ace_showInvisibles", FALSE),
        autoScrollEditorIntoView = TRUE,
        minLines = 7,
        maxLines = 20
      ),
      conditionalPanel(
        "input.rep_add_functions == true",
        HTML("</br><label>Repeated simulation functions:</label>"),
        shinyAce::aceEditor(
          "rep_funcs",
          mode = "r",
          theme = getOption("radiant.ace_theme", default = "tomorrow"),
          wordWrap = TRUE,
          debounce = 0,
          height = "120px",
          value = state_init("rep_funcs", "") %>% fix_smart(),
          placeholder = "Create your own R functions (e.g., add = function(x, y) {x + y}).\nCall these functions from the 'formula' input and press the Run\nsimulation button to run the simulation. Click the ? icon on the\nbottom left of your screen for help and examples",
          vimKeyBinding = getOption("radiant.ace_vim.keys", default = FALSE),
          tabSize = getOption("radiant.ace_tabSize", 2),
          useSoftTabs = getOption("radiant.ace_useSoftTabs", TRUE),
          showInvisibles = getOption("radiant.ace_showInvisibles", FALSE),
          autoScrollEditorIntoView = TRUE,
          minLines = 7,
          maxLines = 20
        )
      ),
      HTML("</br><label>Repeated simulation summary:</label>"),
      verbatimTextOutput("summary_repeat"),
      conditionalPanel(
        condition = "input.rep_show_plots == true",
        HTML("</br><label>Repeated simulation plots:</label>"),
        download_link("dlp_repeat"),
        plotOutput("plot_repeat", height = "100%")
      )
    )
  )

  stat_tab_panel(
    menu = "Model > Decide",
    tool = "Simulate",
    data = NULL,
    tool_ui = "ui_simulater",
    output_panels = sim_output_panels
  )
})

## creating autocomplete list for simuate - function editor
radiant_sim_auto <- reactive({
    pkgs <- c("stats", "base", "radiant.data") %>%
    sapply(function(x) grep("^[A-Za-z]", getNamespaceExports(x), value = TRUE)) %>%
    set_names(., paste0("{", names(.), "}"))

    inp <- clean_args(sim_inputs(), sim_args) %>% lapply(report_cleaner)
    nms <- base::intersect(c(sim_types_vec, "form"), names(inp))
    auto_nms <- list()

    for (i in nms) {
      auto_nms[[paste0("{sim ", i, "}")]] <- strsplit(inp[[i]], ";")[[1]] %>%
        strsplit(., "(\\s+|=)") %>%
        base::Filter(length, .) %>%
        sapply(., `[[`, 1)
    }

    c(pkgs, auto_nms)
})

## auto completion for r-functions and defined variables
observe({
  req(isTRUE(input$sim_add_functions))
  shinyAce::updateAceEditor(
    session, "sim_funcs",
    autoCompleters = c("static", "text"),
    autoCompleteList = radiant_sim_auto()
  )
})

.simulater <- eventReactive(input$sim_run, {
  validate(
    need(
      !is_empty(input$sim_types) || !is_empty(input$sim_form),
      "No formulas or simulated variables specified"
    )
  )
  fixed <- fix_names(input$sim_name)
  updateTextInput(session, "sim_name", value = fixed)
  withProgress(message = "Running simulation", value = 0.5, {
    inp <- sim_inputs()
    inp$name <- fixed
    inp$envir <- r_data
    sim <- do.call(simulater, inp)
    if (is.data.frame(sim)) {
      r_data[[fixed]] <- sim
      register(fixed)
    }
    sim
  })
})

.summary_simulate <- eventReactive({c(input$sim_run, input$sim_dec)}, {
  if (not_pressed(input$sim_run)) {
    "** Press the Run simulation button to simulate data **"
  } else {
    summary(.simulater(), dec = input$sim_dec)
  }
})

sim_plot_width <- function() 650
sim_plot_height <- function() {
  sim <- .simulater()
  if (is.character(sim)) {
    300
  } else {
    if (dim(sim)[1] == 0) {
      300
    } else {
      ceiling(sum(sapply(sim, does_vary)) / 2) * 300
    }
  }
}

.plot_simulate <- eventReactive(input$sim_run, {
  req(input$sim_show_plots)
  withProgress(message = "Generating simulation plots", value = 1, {
    .simulater() %>%
      {if (is_empty(.)) invisible() else plot(., shiny = TRUE)}
  })
})

.repeater <- eventReactive(input$rep_run, {
  fixed <- fix_names(input$rep_name)
  updateTextInput(session, "rep_name", value = fixed)

  withProgress(message = "Repeated simulation", value = 0.5, {
    inp <- rep_inputs()
    inp$name <- fixed
    inp$envir <- r_data
    rep <- do.call(repeater, inp)
    if (is.data.frame(rep)) {
      r_data[[fixed]] <- rep
      register(fixed)
    }
    rep
  })
})

.summary_repeat <- eventReactive({c(input$rep_run, input$rep_dec)}, {
  if (not_pressed(input$rep_run)) {
    "** Press the Repeat simulation button **"
  } else if (length(input$rep_sum_vars) == 0) {
    "Select at least one Output variable"
  } else if (input$rep_byvar == ".sim" && is_empty(input$rep_nr)) {
    "Please specify the number of repetitions in '# reps'"
  } else {
    summary(.repeater(), dec = input$rep_dec)
  }
})

rep_plot_width <- function() 650
rep_plot_height <- function() {
  if (length(input$rep_sum_vars) == 0) return(300)
  rp <- .repeater()
  if (is.character(rp)) {
    300
  } else {
    if (dim(rp)[1] == 0) {
      300
    } else {
      ceiling(sum(sapply(select(rp, -1), does_vary)) / 2) * 300
    }
  }
}

.plot_repeat <- eventReactive(input$rep_run, {
  req(input$rep_show_plots)
  req(length(input$rep_sum_vars) > 0)
  if (input$rep_byvar == ".sim" && is_empty(input$rep_nr)) {
    return(invisible())
  } # else if (input$rep_byvar == "rep" && is_empty(input$rep_grid)) {
    # return(invisible())
  # }
  object <- .repeater()
  if (is.null(object)) return(invisible())
  withProgress(message = "Generating repeated simulation plots", value = 1, {
    inp <- rep_plot_inputs()
    inp$shiny <- TRUE
    inp$x <- object
    do.call(plot, inp)
  })
})

report_cleaner <- function(x) x %>% gsub("\n", ";", .) %>% gsub("[;]{2,}", ";", .)

observeEvent(input$simulater_report, {
  sim_dec <- input$sim_dec %>% ifelse(is_empty(.), 3, .)
  outputs <- "summary"
  inp_out <- list(list(dec = sim_dec), "")
  figs <- FALSE

  if (isTRUE(input$sim_show_plots)) {
    outputs <- c("summary", "plot")
    inp_out[[2]] <- list(custom = FALSE)
    figs <- TRUE
  }

  ## report cleaner turns seed and nr into strings
  inp <- clean_args(sim_inputs(), sim_args) %>% lapply(report_cleaner)
  sim_name <- fix_names(input$sim_name)
  updateTextInput(session, "sim_name", value = sim_name)

  if (!is_empty(inp$seed)) inp$seed <- as_numeric(inp$seed)
  if (!is_empty(inp$nr)) inp$nr <- as_numeric(inp$nr)
  if (!"norm" %in% names(inp)) {
    inp$ncorr <- inp$nexact <- NULL
  } else {
    if (is_empty(inp$ncorr)) inp$ncorr <- NULL
    if (!is_empty(inp$nexact)) inp$nexact <- as.logical(inp$nexact)
  }
  for (i in c(sim_types_vec, "form")) {
    if (i %in% names(inp)) {
      inp[[i]] <- strsplit(inp[[i]], ";")[[1]]
    }
  }
  if (length(inp[["form"]]) == 1 && grepl("^#", inp[["form"]])) {
    inp[["form"]] <- NULL
  }
  if (is_empty(inp$data)) {
    inp$data <- NULL
  } else {
    inp$data <- as.symbol(inp$data)
  }

  pre_cmd <- paste0(sim_name, " <- ")
  if (!is_empty(input$sim_funcs)) {
    ## dealing with user defined functions in simulate tab
    pre_cmd <- gsub("    ", "  ", input$sim_funcs) %>%
      gsub("\t", "  ", .) %>%
      paste0("\n\n", pre_cmd)
    funcs <- parse(text = input$sim_funcs)
    lfuncs <- list()
    for (i in seq_len(length(funcs))) {
      tmp <- strsplit(as.character(funcs[i]), "(\\s*=|\\s*<-)")[[1]][1]
      lfuncs[[tmp]] <- as.symbol(tmp)
    }
    if (length(lfuncs) == 0) {
      pre_cmd <- paste0(sim_name, " <- ")
      inp$funcs <- NULL
    } else {
      inp$funcs <- lfuncs
    }
  }
  inp$name <- NULL
  update_report(
    inp_main = inp,
    fun_name = "simulater",
    inp_out = inp_out,
    pre_cmd = pre_cmd,
    xcmd = paste0("register(\"", sim_name, "\")"),
    outputs = outputs,
    inp = sim_name,
    figs = figs,
    fig.width = sim_plot_width(),
    fig.height = sim_plot_height()
  )
})

observeEvent(input$repeater_report, {
  rep_dec <- input$rep_dec %>% ifelse(is_empty(.), 3, .)
  outputs <- "summary"
  inp_out <- list(list(dec = rep_dec), "")
  figs <- FALSE

  if (isTRUE(input$rep_show_plots)) {
    outputs <- c("summary", "plot")
    inp_out[[2]] <- list(custom = FALSE)
    figs <- TRUE
  }

  ## report cleaner turns seed and nr into strings
  inp <- clean_args(rep_inputs(), rep_args) %>% lapply(report_cleaner)
  rep_name <- fix_names(input$rep_name)
  updateTextInput(session, "rep_name", value = rep_name)
  inp$dataset <- fix_names(input$sim_name)
  updateTextInput(session, "sim_name", value = inp$dataset)

  if (!is_empty(inp$seed)) inp$seed <- as_numeric(inp$seed)
  if (!is_empty(inp$nr)) inp$nr <- as_numeric(inp$nr)
  if (input$rep_byvar == ".sim") inp$grid <- NULL

  if (!is_empty(inp[["form"]])) {
    inp[["form"]] <- strsplit(inp[["form"]], ";")[[1]]
    if (length(inp[["form"]]) == 1 && grepl("^#", inp[["form"]])) {
      inp[["form"]] <- NULL
    }
  }
  if (!is_empty(inp[["grid"]])) {
    inp[["grid"]] <- strsplit(inp[["grid"]], ";")[[1]]
  }
  inp$name <- NULL
  update_report(
    inp_main = inp,
    fun_name = "repeater",
    inp_out = inp_out,
    pre_cmd = paste0(rep_name, " <- "),
    xcmd = paste0("register(\"", rep_name, "\")"),
    outputs = outputs,
    inp = rep_name,
    figs = figs,
    fig.width = rep_plot_width(),
    fig.height = rep_plot_height()
  )
})

download_handler(
  id = "dlp_simulate",
  fun = download_handler_plot,
  fn = function() paste0(input$sim_name, "_sim"),
  type = "png",
  caption = "Save simulation plots",
  plot = .plot_simulate,
  width = sim_plot_width,
  height = sim_plot_height
)

download_handler(
  id = "dlp_repeat",
  fun = download_handler_plot,
  fn = function() paste0(input$rep_name, "_rep"),
  type = "png",
  caption = "Save repeated simulation plots",
  plot = .plot_repeat,
  width = rep_plot_width,
  height = rep_plot_height
)
