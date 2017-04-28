#' Simulate data for decision analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/simulater.html} for an example in Radiant
#'
#' @param const A string listing the constants to include in the analysis (e.g., "cost = 3; size = 4")
#' @param lnorm A string listing the log-normally distributed random variables to include in the analysis (e.g., "demand 2000 1000" where the first number is the log-mean and the second is the log-standard deviation)
#' @param norm A string listing the normally distributed random variables to include in the analysis (e.g., "demand 2000 1000" where the first number is the mean and the second is the standard deviation)
#' @param unif A string listing the uniformly distributed random variables to include in the analysis (e.g., "demand 0 1" where the first number is the minimum value and the second is the maximum value)
#' @param discrete A string listing the random variables with a discrete distribution to include in the analysis (e.g., "price 5 8 .3 .7" where the first set of numbers are the values and the second set the probabilities
#' @param binom A string listing the random variables with a binomail distribution to include in the analysis (e.g., "crash 100 .01") where the first number is the number of trials and the second is the probability of success)
#' @param sequ A string listing the start and end for a sequence to include in the analysis (e.g., "trend 1 100 1"). The number of 'steps' is determined by the number of simulations.
#' @param grid A string listing the start, end, and step for a set of sequences to include in the analysis (e.g., "trend 1 100 1"). The number of rows in the expanded will over ride the number of simulations
#' @param data Name of a dataset to be used in the calculations
#' @param form A string with the formula to evaluate (e.g., "profit = demand * (price - cost)")
#' @param seed To repeat a simulation with the same randomly generated values enter a number into Random seed input box.
#' @param name To save the simulated data for further analysis specify a name in the Sim name input box. You can then investigate the simulated data by choosing the specified name from the Datasets dropdown in any of the other Data tabs.
#' @param nr Number of simulations
#' @param dat Data list from previous simulation. Used by repeater function
#'
#' @return A data.frame with the created variables
#'
#' @examples
#' result <- simulater(const = "cost 3", norm = "demand 2000 1000",
#'                    discrete = "price 5 8 .3 .7",
#'                    form = "profit = demand * (price - cost)")
#'
#' @seealso \code{\link{summary.simulater}} to summarize results
#' @seealso \code{\link{plot.simulater}} to plot results
#'
#' @export
simulater <- function(const = "",
                      lnorm = "",
                      norm = "",
                      unif = "",
                      discrete = "",
                      binom = "",
                      sequ = "",
                      grid = "",
                      data = "",
                      form = "",
                      seed = "",
                      name = "",
                      nr = 1000,
                      dat = NULL) {

  ## remove any non-numbers from seed, including spaces
  seed %>% gsub("[^0-9]","",.) %>% { if (!is_empty(.)) set.seed(seed) }

  if (is.null(dat)) {
    dat <- list()
  } else {
    ## needed because number may be NA and missing if grid used in Simulate
    nr <- attr(dat,"sim_call")$nr
  }

  grid %<>% sim_cleaner
  if (grid != "") {
    s <- grid %>% sim_splitter
    for (i in 1:length(s)) {
      if (is_empty(s[[i]][4])) s[[i]][4] <- 1
      s[[i]] %>% { dat[[.[1]]] <<- seq(as.numeric(.[2]) , as.numeric(.[3]), as.numeric(.[4]))}
    }
    dat <- as.list(expand.grid(dat) %>% as_data_frame)
    nr <- length(dat[[1]])
  }

  if (is_empty(nr)) {
    mess <- c("error",paste0("Please specify the number of simulations in '# sims'"))
    return(add_class(mess, "simulater"))
  }

  ## parsing constant
  const %<>% sim_cleaner
  if (const != "") {
    s <- const %>% sim_splitter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- as.numeric(.[2]) %>% rep(nr) }
  }

  ## parsing uniform
  unif %<>% sim_cleaner
  if (unif != "") {
    s <- unif %>% sim_splitter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- runif(nr, as.numeric(.[2]) , as.numeric(.[3]))}
  }

  ## parsing log normal
  lnorm %<>% sim_cleaner
  if (lnorm != "") {
    s <- lnorm %>% sim_splitter
    for (i in 1:length(s)) {
      sdev <- as.numeric(s[[i]][3])
      if (!sdev > 0) {
        mess <- c("error",paste0("All log-normal variables should have a standard deviation larger than 0.\nPlease review the input carefully"))
        return(add_class(mess, "simulater"))
      }
      s[[i]] %>% { dat[[.[1]]] <<- rlnorm(nr, as.numeric(.[2]), sdev)}
    }
  }

  ## parsing normal
  norm %<>% sim_cleaner
  if (norm != "") {
    s <- norm %>% sim_splitter
    for (i in 1:length(s)) {
      sdev <- as.numeric(s[[i]][3])
      if (!sdev > 0) {
        mess <- c("error",paste0("All normal variables should have a standard deviation larger than 0.\nPlease review the input carefully"))
        return(add_class(mess, "simulater"))
      }
      s[[i]] %>% { dat[[.[1]]] <<- rnorm(nr, as.numeric(.[2]) , sdev)}
    }
  }

  ## parsing binomial
  binom %<>% sim_cleaner
  if (binom != "") {
    s <- binom %>% sim_splitter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- rbinom(nr, as_integer(.[2]) , as_numeric(.[3]))}
  }

  ## parsing sequence
  sequ %<>% sim_cleaner
  if (sequ != "") {
    s <- sequ %>% sim_splitter
    for (i in 1:length(s))
      s[[i]] %>% { dat[[.[1]]] <<- seq(as.numeric(.[2]) , as.numeric(.[3]), length.out = as.numeric(nr))}
  }

  ## adding data to dat list
  if (data != "" && data != "none") {
    sdat <- getdata(data)
    for (i in colnames(sdat))
      dat[[i]] <- sdat[[i]]
  }

  ## parsing discrete
  discrete %<>% sim_cleaner
  if (discrete != "") {
    s <- discrete %>% sim_splitter
    for (i in 1:length(s)) {

      dpar <- s[[i]][-1] %>% gsub(","," ", .) %>% strsplit("\\s+") %>% unlist %>% strsplit("/")
      asNum <- function(x) ifelse(length(x) > 1, as.numeric(x[1])/as.numeric(x[2]), as.numeric(x[1]))
      dpar <- sshhr(try(sapply(dpar, asNum) %>% matrix(ncol = 2), silent = TRUE))

      if (is(dpar, 'try-error') || any(is.na(dpar))) {
        mess <- c("error",paste0("Input for discrete variable # ", i, " contains an error. Please review the input carefully"))
        return(add_class(mess, "simulater"))
      } else if (sum(dpar[,2]) != 1) {
        mess <- c("error",paste0("Probabilities for discrete variable # ", i, " do not sum to 1 (",round(sum(dpar[[2]]),3),")"))
        return(add_class(mess, "simulater"))
      }

      dat[[s[[i]][1]]] <- sample(dpar[,1], nr, replace = TRUE, prob = dpar[,2])
    }
  }

  form %<>% sim_cleaner
  if (form != "") {
    s <- form %>% gsub("\\s+","",.) %>% sim_splitter("=")
    for (i in 1:length(s)) {
      if (grepl("^\\s*#", s[[i]][1], perl = TRUE)) next
      obj <- s[[i]][1]
      fobj <- s[[i]][-1]
      if (length(fobj) > 1) fobj <- paste0(fobj, collapse = "=")
      out <- try(do.call(with, list(dat, parse(text = fobj))), silent = TRUE)
      if (!is(out, 'try-error')) {
        dat[[obj]] <- out
      } else {
        dat[[obj]] <- NA
        mess <- c("error",paste0("Formula was not successfully evaluated:\n\n", strsplit(form,";") %>% unlist %>% paste0(collapse="\n"),"\n\nMessage: ", attr(out,"condition")$message))
        return(add_class(mess, "simulater"))
      }
    }
  }

  ## removing data from dat list
  if (!data %in% c("", "none")) {
    dat[colnames(sdat)] <- NULL
  }

  ## convert list to a data.frame
  dat <- as.data.frame(dat) %>% na.omit

  ## capturing the function call for use in repeat
  sc <- formals()
  smc <- lapply(match.call()[-1], eval)
  sc[names(smc)] <- smc
  sc$nr <- nr
  attr(dat, "sim_call") <- sc

  if (nrow(dat) == 0) {
    mess <- c("error",paste0("The simulated data set has 0 rows"))
    return(add_class(mess, "simulater"))
  }

  name %<>% gsub(" ","",.)
  if (name != "") {
    if (exists("r_environment")) {
      env <- r_environment
    } else if (exists("r_data")) {
      env <- pryr::where("r_data")
    } else {
      return(add_class(dat, "simulater"))
    }

    mess <- paste0("\n### Simulated data\n\nFormula:\n\n",
                   gsub("*","\\*",form, fixed = TRUE) %>% gsub(";","\n\n", .), "\n\nDate: ",
                   lubridate::now())
    env$r_data[[name]] <- dat
    env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique
    env$r_data[[paste0(name,"_descr")]] <- mess
    return(add_class(name, "simulater"))
  }

  add_class(dat, "simulater")
}

## Test settings for simulater function, will not be run when sourced
if (getOption("radiant.testthat", default = FALSE)) {
  main__ <- function() {
    options(radiant.testthat = TRUE)
    library(radiant.model)
    const <- "cost 3"
    normstr <- "demand 2000 1000"
    discrete <- "price 5 8 .3 .7"
    form <- "profit = demand * (price - cost)"
    lnorm <- unif <- binom <- sequ <- grid <- data <- name <- ""
    seed <- 100
    nr <- 1000
    dat <- NULL

    result <- simulater(const = const, norm = normstr, discrete = discrete, form = form, seed = seed)
    # attr(result, "sim_call")
    stopifnot(sum(round(result[1000,],5) == c(3,-141.427660,5,-282.85532)) == 4)
  }
  main__()
}

#' Summary method for the simulater function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/simulater.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{simulater}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- simulater(norm = "demand 2000 1000")
#' summary(result)
#'
#' @seealso \code{\link{simulater}} to generate the results
#' @seealso \code{\link{plot.simulater}} to plot results
#'
#' @export
summary.simulater <- function(object, dec = 4, ...) {

  if (is.character(object)) {
    if (length(object) == 2 && object[1] == "error") {
      return(cat(object[2]))
    } else {
      object <- getdata(object)
    }
  }

  sc <- attr(object, "sim_call")
  clean <- function(x) paste0(gsub(";", "; ", x) %>% gsub("\\n","",.), "\n")

  cat("Simulation\n")
  cat("Simulations:", nrow(object), "\n")
  cat("Random seed:", sc$seed, "\n")
  cat("Sim data   :", sc$name, "\n")
  if (!is_empty(sc$binom))    cat("Binomial   :", clean(sc$binom))
  if (!is_empty(sc$const))    cat("Constant   :", clean(sc$const))
  if (!is_empty(sc$discrete)) cat("Discrete   :", clean(sc$discrete))
  if (!is_empty(sc$lnorm))    cat("Log normal :", clean(sc$lnorm))
  if (!is_empty(sc$norm))     cat("Normal     :", clean(sc$norm))
  if (!is_empty(sc$unif))     cat("Uniform    :", clean(sc$unif))
  if (!is_empty(sc$sequ))     cat("Sequence   :", clean(sc$sequ))
  if (!is_empty(sc$grid))     cat("Grid search:", clean(sc$grid))
  if (!is_empty(sc$data))     cat("Data       :", clean(sc$data))
  if (!is_empty(sc$form))     cat(paste0("Formulas   :\n\t", sc$form %>% gsub(";","\n",.) %>% gsub("\n","\n\t",.), "\n"))
  cat("\n")

  sim_summary(object, dec = ifelse(is.na(dec), 4, dec))
}

#' Plot method for the simulater function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/simulater} for an example in Radiant
#'
#' @param x Return value from \code{\link{simulater}}
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- simulater(const = "cost 3", norm = "demand 2000 1000",
#'                     discrete = "price 5 8 .3 .7",
#'                     form = "profit = demand * (price - cost)")
#' plot(result)
#'
#' @seealso \code{\link{simulater}} to generate the result
#' @seealso \code{\link{summary.simulater}} to summarize results
#'
#' @export
plot.simulater <- function(x, shiny = FALSE, custom = FALSE, ...) {

  if (is.character(x)) {
    if (x[1] == "error") return(invisible())
    object <- getdata(x)
    if (nrow(object) == 0) return(invisible())
  } else {
    object <- x
  }
  rm(x)

  plot_list <- list()
  for (i in colnames(object)) {
    dat <- select_(object, .dots = i)
    if (!does_vary(object[[i]])) next
    plot_list[[i]] <-
      visualize(select_(object, .dots = i), xvar = i, bins = 20, custom = TRUE)
  }

  if (custom)
    if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = min(length(plot_list),2))) %>%
    {if (shiny) . else print(.)}
}

#' Repeat simulation
#'
#' @param nr Number times to repeat the simulation
#' @param vars Variables to use in repeated simulation
#' @param grid Expression to use in grid search for constants

#' @param sum_vars (Numeric) variables to summaries
#' @param byvar Variable(s) to group data by before summarizing
#' @param fun Functions to use for summarizing
#' @param form A string with the formula to apply to the summarized data

#' @param seed To repeat a simulation with the same randomly generated values enter a number into Random seed input box.
#' @param name To save the simulated data for further analysis specify a name in the Sim name input box. You can then investigate the simulated data by choosing the specified name from the Datasets dropdown in any of the other Data tabs.
#' @param sim Return value from the simulater function
#'
#' @examples
#' result <- simulater(const = "var_cost 5;fixed_cost 1000;", norm = "E 0 100;",
#'                     discrete = "price 6 8 .3 .7;",
#'                     form = "demand = 1000 - 50*price + E;
#'                             profit = demand*(price-var_cost) - fixed_cost;
#'                             profit_small = profit < 100",
#'                     seed = "1234")
#' repeater(nr = 12, vars = c("E","price"), sum_vars = "profit",
#'          byvar = "sim", form = "profit_365 = profit < 36500",
#'          seed = "1234", sim = result) %>% head
#'
#' @export
repeater <- function(nr = 12,
                     vars = "",
                     grid = "",
                     sum_vars = "",
                     byvar = "sim",
                     fun = "sum_rm",
                     form = "",
                     seed = "",
                     name = "",
                     sim = "") {

  # nr = 12
  # vars = c("E","price")
  # grid = ""
  # sum_vars = c("profit","E")
  # byvar = "sim"
  # fun = "sum_rm"
  # form = "profit_365 = profit < 36500"
  # seed = "1234"
  # name = ""
  # sim = simulater(const = "var_cost 5;fixed_cost 1000;", norm = "E 0 100;",
  #                 discrete = "price 6 8 .3 .7;",
  #                 form = "demand = 1000 - 50*price + E;
  #                         profit = demand*(price-var_cost) - fixed_cost;
  #                         profit_small = profit < 100",
  #                 seed = "1234")

  if (is_empty(nr)) {
    if (is_empty(grid)) {
      mess <- c("error",paste0("Please specify the number of repetitions in '# reps'"))
      return(add_class(mess, "repeater"))
    } else {
      nr = 1
    }
  }

  dat <- sim; rm(sim)
  if (is.character(dat)) dat <- getdata(dat)

  seed %>% gsub("[^0-9]","",.) %>% { if (!is_empty(.)) set.seed(seed) }

  if (identical(vars, "") && identical(grid, "")) {
    mess <- c("error",paste0("Select variables to re-simulate and/or a specify a constant\nto change using 'Grid search' when Group by is set to Repeat"))
    return(add_class(mess, "repeater"))
  }

  if (identical(vars, "")) vars <- character(0)

  grid_list <- list()
  if (!identical(grid, "")) {
    grid %<>% sim_cleaner
    if (grid != "") {
      s <- grid %>% sim_splitter
      for (i in 1:length(s)) {
        if (is_empty(s[[i]][4])) s[[i]][4] <- 1
        s[[i]] %>% { grid_list[[.[1]]] <<- seq(as.numeric(.[2]) , as.numeric(.[3]), as.numeric(.[4]))}
      }
    }
    ## expanding list of variables but removing ""
    vars <- c(vars, names(grid_list)) %>% unique
  }

  ## from http://stackoverflow.com/a/7664655/1974918
  ## keep those list elements that, e.g., q is in
  nr_sim <- nrow(dat)
  sc <- attr(dat, "sim_call")

  if (!is_empty(sc$data)) vars <- c(sc$data, vars)

  sc$name <- sc$seed <- "" ## cleaning up the sim call
  sc_keep <- grep(paste(vars, collapse = "|"), sc, value=TRUE)

  ## needed in case there is no 'form' in simulate
  sc[1:(which(names(sc) == "seed")-1)] <- ""
  sc[names(sc_keep)] <- sc_keep
  sc$dat <- as.list(dat)

  summarize_sim <- function(object) {
    if (fun != "none") {
      object %<>% group_by_(byvar) %>%
        summarise_at(.cols = sum_vars, .funs = make_funs(fun)) %>%
        set_colnames(c(byvar, sum_vars))

      # if (length(sum_vars) == 1 && length(fun) > 1) colnames(object) <- paste0(sum_vars, "_", colnames(object))
    } else {
      object %<>% select_(.dots = c("rep","sim",sum_vars))
    }
    object
  }

  rep_sim <- function(rep_nr, sfun = function(x) x) {
    bind_cols(
      data_frame(rep = rep(rep_nr, nr_sim), sim = 1:nr_sim),
      do.call(simulater, sc)
    ) %>% na.omit %>% sfun
  }

  rep_grid_sim <- function(gval, sfun = function(x) x) {
    gvars <- names(gval)

    ## removing form ...
    sc_grid <- grep(paste(gvars, collapse = "|"), sc_keep, value=TRUE) %>%
      {.[which(names(.) != "form")]} %>%
      gsub("[ ]{2,}"," ",.)

    for (i in 1:length(gvars)) {
      sc_grid %<>% sub(paste0("[;\n]", gvars[i], " [.0-9]+"), paste0("\n", gvars[i], " ", gval[gvars[i]]), .) %>%
      sub(paste0("^", gvars[i], " [.0-9]+"), paste0(gvars[i], " ", gval[gvars[i]]), .)
    }

    sc[names(sc_grid)] <- sc_grid
    bind_cols(
      data_frame(rep = rep(paste(gval, collapse = "|"), nr_sim), sim = 1:nr_sim),
      do.call(simulater, sc)
    ) %>% na.omit %>% sfun
  }

  if (length(grid_list) == 0) {
    if (byvar == "sim") {
      ret <-
         bind_rows(lapply(1:nr, rep_sim)) %>%
         summarize_sim %>%
         add_class("repeater")
    } else {
      ret <-
        bind_rows(lapply(1:nr, function(x) rep_sim(x, summarize_sim))) %>%
        add_class("repeater")
    }

  } else {
    grid <- expand.grid(grid_list)
    if (byvar == "sim") {
      ret <-
        bind_rows(apply(grid, 1, rep_grid_sim)) %>%
        summarize_sim %>%
        add_class("repeater")
    } else {
      ret <-
        bind_rows(apply(grid, 1, function(x) rep_grid_sim(x, summarize_sim))) %>%
        add_class("repeater")
    }
  }

  form %<>% sim_cleaner
  if (form != "") {
    s <- form %>% gsub("\\s+","",.) %>% sim_splitter("=")
    for (i in 1:length(s)) {
      if (grepl("^#",s[[i]][1])) next
      obj <- s[[i]][1]
      fobj <- s[[i]][-1]
      if (length(fobj) > 1) fobj <- paste0(fobj, collapse = "=")
      out <- try(do.call(with, list(ret, parse(text = fobj))), silent = TRUE)
      if (!is(out, 'try-error')) {
        ret[[obj]] <- out
      } else {
        ret[[obj]] <- NA
        mess <- c("error", paste0("Formula was not successfully evaluated:\n\n", strsplit(form,";") %>% unlist %>% paste0(collapse="\n"),"\n\nMessage: ", attr(out,"condition")$message,"\n\nNote that these formulas can only be applied to selected 'Output variables'"))
        return(add_class(mess, "repeater"))
      }
    }
  }

  ## tbl_df seems to remove attributes
  ret <- as.data.frame(ret)

  ## capturing the function call for use in summary and plot
  rc <- formals()
  rmc <- lapply(match.call()[-1], eval)
  rc[names(rmc)] <- rmc

  rc$sc <- sc[setdiff(names(sc),"dat")]
  attr(ret, "rep_call") <- rc

  name %<>% gsub(" ","",.)
  if (name != "") {
    if (exists("r_environment")) {
      env <- r_environment
    } else if (exists("r_data")) {
      env <- pryr::where("r_data")
    } else {
      return(ret)
    }

    mess <- paste0("\n### Repeated simulation data\n\nFormula:\n\n",
                   gsub("*","\\*",sc$form, fixed = TRUE) %>%
                   gsub("\n","\n\n",.) %>%
                   gsub(";","\n\n", .),
                   "\n\nDate: ",
                   lubridate::now())

    env$r_data[[name]] <- ret
    env$r_data[['datasetlist']] <- c(name, env$r_data[['datasetlist']]) %>% unique
    env$r_data[[paste0(name,"_descr")]] <- mess
    return(add_class(name, "repeater"))
  }

  ret
}

## Test settings for repeater function, will not be run when sourced
if (getOption("radiant.testthat", default = FALSE)) {
  main__ <- function() {
    # options(radiant.testthat = TRUE)
    library(radiant.model)
    rm(list = ls())
    # result <- simulater(const = "cost 3", norm = "demand 2000 1000", discrete = "price 5 8 .3 .7", form = "profit = demand * (price - cost)")
    result <- simulater(const = "C 11;U 3995;", norm = "M 3000 1000;", unif = "L 5040 6860;", discrete = "P 20 18.5 16.5 15 0.25 0.35 0.3 0.1;", form = "## Earnings formula;E=(P-C)*M-L-U;## Probability of higher earnings compared to consulting;GL_gt_C=E > 6667;## Probability of low earnings (related to student loans);GL_2low = E < 5000;## Partnership;EP = ifelse(E<3500, 3500, ifelse(E>9000,9000+.1*(E-9000), E));EPvsE = EP > E", seed = "1234", name = "simdat")
    sim <- result
    vars <- "M"
    nr = 12
    grid = ""
    seed = 1234
    name = ""
    sum_vars <- c("E","EP")
    byvar <- "sim"
    fun <- "sum_rm"
    res <- repeater(vars = "profit", sim = result)
  }
  main__()
}

#' Summarize repeated simulation
#'
#' @param object Return value from \code{\link{repeater}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.repeater <- function(object,
                             dec = 4,
                             ...) {

  if (is.character(object)) {
    if (length(object) == 2 && object[1] == "error") {
      return(cat(object[2]))
    } else {
      object <- getdata(object)
    }
  }

  ## getting the repeater call
  rc <- attr(object, "rep_call")

  ## legacy
  if (is.null(rc)) {
    rc <- list()
    rc[c("nr","byvar","fun","seed","sim", "name", "form")] <- ""
    rc$sc <- list(nr = "")
  }

  ## show results
  cat("Repeated simulation\n")
  cat("Simulations   :", rc$sc$nr, "\n")
  cat("Repetitions   :", rc$nr, "\n")
  cat("Group by      :", ifelse (rc$byvar == "rep", "Repeat", "Simulation"), "\n")
  cfun <- sub("_rm$","", rc$fun)
  cat("Function      :", cfun, "\n")
  cat("Random  seed  :", rc$seed, "\n")
  cat("Simulated data:", rc$sim, "\n")
  cat("Repeat  data  :", rc$name, "\n")

  if (rc$form != "") {
    rc$form %<>% sim_cleaner
    cat(paste0("Formulas      :\n\t", rc$form %>% gsub(";","\n",.) %>% gsub("\n","\n\t",.), "\n"))
  }
  cat("\n")

  sim_summary(select(object,-1), fun = cfun, dec = ifelse (is.na(dec), 4, dec))
}

#' Plot repeated simulation
#'
#' @param x Return value from \code{\link{repeater}}
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.repeater <- function(x, shiny = FALSE, custom = FALSE, ...) {

  if (is.character(x)) {
    if (x[1] == "error") return(invisible())
    object <- getdata(x)
    if (nrow(object) == 0) return(invisible())
  }
  rm(x)

  ## getting the repeater call
  rc <- attr(object, "rep_call")

  ## legacy for version that are radiant split
  if (is.null(rc)) rc <- formals("repeater")
  if (identical(rc$sum_vars, "")) return(invisible())

  cfun <- sub("_rm$","",rc$fun)
  plot_list <- list()
  for (i in colnames(object)[-1]) {
    dat <- select_(object, .dots = i)
    if (!does_vary(object[[i]])) next

    plot_list[[i]] <-
      visualize(select_(object, .dots = i), xvar = i, bins = 20, custom = TRUE)

    if (i %in% rc$sum_vars && !is_empty(cfun, "none"))
      plot_list[[i]] <- plot_list[[i]] + xlab(paste0(cfun, " of ", i))
  }

  if (length(plot_list) == 0) return(invisible())

  if (custom)
    if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = min(length(plot_list),2))) %>%
    {if (shiny) . else print(.)}
}

#' Print simulation summary
#'
#' @param dat Simulated data
#' @param dc Variable classes
#' @param fun Summary function to apply
#' @param dec Number of decimals to show
#'
#' @export
sim_summary <- function(dat, dc = getclass(dat), fun = "", dec = 4) {

  isLogic <- "logical" == dc
  isNum <- !isLogic

  dec <- ifelse(is.na(dec), 4, as_integer(dec))

  if (sum(isNum) > 0) {

    isConst <- !sapply(dat, does_vary) & isNum
    if (sum(isConst) > 0) {
      cn <- names(dc)[isConst]
      cat("Constants:\n")
      select(dat, which(isConst)) %>% na.omit %>% .[1,] %>% as.data.frame %>%
        round(dec) %>% mutate_all(funs(formatC(., big.mark = ",", digits = dec, format = "f"))) %>%
        set_rownames("") %>% set_colnames(cn) %>%
        print
      cat("\n")
    }

    isRnd <- isNum & !isConst
    if (sum(isRnd) > 0) {
      cn <- names(dc)[isRnd]
      cat("Variables:\n")
      select(dat, which(isNum & !isConst)) %>%
        tidyr::gather_("variable", "values", cn) %>%
        group_by_("variable") %>%
        summarise_all(funs(n = length, mean = mean_rm, sd = sd_rm, min = min_rm, `25%` = p25,
                           median = median_rm, `75%` = p75, max = max_rm)) %>%
        { if (fun == "" || fun == "none") . else {.[[1]] <- paste0(fun, " of ", .[[1]])}; . } %>%
        { .[[1]] <- format(.[[1]], justify = "left"); .} %>%
        data.frame(check.names = FALSE) %>%
        formatdf(., dec = dec, mark = ",") %>%
        print(row.names = FALSE)
      cat("\n")
    }
  }

  if (sum(isLogic) > 0) {
    cat("Logicals:\n")
    select(dat, which(isLogic)) %>% summarise_all(funs(sum, mean)) %>% round(dec) %>%
      matrix(ncol = 2) %>% set_colnames(c("TRUE (nr)  ", "TRUE (prop)")) %>%
      set_rownames(names(dat)[isLogic]) %>% print
    cat("\n")
  }
}

#' Clean input command string
#'
#' @param x Input string
#'
#' @return Cleaned string
#'
#' @export
sim_cleaner <- function(x) x %>% gsub("[ ]{2,}"," ",.) %>%
  gsub("[ ]*[\n;]+[ ]*",";",.) %>%
  gsub("[;]{2,}",";",.) %>%
  gsub(";$","",.) %>%
  gsub("^;","",.)

#' Split input command string
#'
#' @param x Input string
#' @param symbol Symbol used to split the command string
#'
#' @return Split input command string
#'
#' @export
sim_splitter <- function(x, symbol = " ") x %>% strsplit(., ";") %>% extract2(1) %>% strsplit(.,symbol)

#' Find maxium value of a vector
#'
#' @param var Variable to find the maximum for
#' @param val Variable to find the value for at the maxium of var
#'
#' @return Value of val at the maximum of var
#'
#' @export
find_max <- function(var, val = "") {
  if (is_empty(val)) stop("Error in find_max (2 inputs required)\nSpecify the variable to evaluate at the maxium of the first input")
  val[which.max(var)]
}

#' Find minimum value of a vector
#'
#' @param var Variable to find the minimum for
#' @param val Variable to find the value for at the maxium of var
#'
#' @return Value of val at the minimum of var
#'
#' @export
find_min <- function(var, val = "") {
  if (is_empty(val)) stop("Error in find_min (2 inputs required)\nSpecify the variable to evaluate at the minimum of the first input")
  val[which.min(var)]
}

#' Standard deviation of weighted sum of variables
#'
#' @param ... A matched number of weights and stocks
#'
#' @return A vector of standard deviation estimates
#'
#' @export
sdw <- function(...) {
  dl <- list(...)
  nr <- length(dl)/2
  w <- data.frame(dl[1:nr])
  d <- data.frame(dl[(nr+1):length(dl)])
  apply(w, 1, function(w) sd(rowSums(sweep(d, 2, w, "*"))))
}
