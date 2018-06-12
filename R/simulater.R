#' Simulate data for decision analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/simulater.html} for an example in Radiant
#'
#' @param const A character vector listing the constants to include in the analysis (e.g., c("cost = 3", "size = 4"))
#' @param lnorm A character vector listing the log-normally distributed random variables to include in the analysis (e.g., "demand 2000 1000" where the first number is the log-mean and the second is the log-standard deviation)
#' @param norm A character vector listing the normally distributed random variables to include in the analysis (e.g., "demand 2000 1000" where the first number is the mean and the second is the standard deviation)
#' @param unif A character vector listing the uniformly distributed random variables to include in the analysis (e.g., "demand 0 1" where the first number is the minimum value and the second is the maximum value)
#' @param discrete A character vector listing the random variables with a discrete distribution to include in the analysis (e.g., "price 5 8 .3 .7" where the first set of numbers are the values and the second set the probabilities
#' @param binom A character vector listing the random variables with a binomial distribution to include in the analysis (e.g., "crash 100 .01") where the first number is the number of trials and the second is the probability of success)
#' @param pois A character vector listing the random variables with a poisson distribution to include in the analysis (e.g., "demand 10") where the number is the lambda value (i.e., the average number of events or the event rate)
#' @param sequ A character vector listing the start and end for a sequence to include in the analysis (e.g., "trend 1 100 1"). The number of 'steps' is determined by the number of simulations
#' @param grid A character vector listing the start, end, and step for a set of sequences to include in the analysis (e.g., "trend 1 100 1"). The number of rows in the expanded will over ride the number of simulations
#' @param data Dataset to be used in the calculations
#' @param form A character vector with the formula to evaluate (e.g., "profit = demand * (price - cost)")
#' @param seed Optional seed used in simulation 
#' @param nexact Logical to indicate if normally distributed random variables should be simulated to the exact specified values
#' @param ncorr A string of correlations used for normally distributed random variables. The number of values should be equal to one or to the number of combinations of variables simulated
#' @param name Deprecated argument
#' @param nr Number of simulations
#' @param dataset Data list from previous simulation. Used by repeater function
#'
#' @return A data.frame with the simulated data
#'
#' @examples
#' simulater(
#'   const = "cost 3", 
#'   norm = "demand 2000 1000", 
#'   discrete = "price 5 8 .3 .7", 
#'   form = "profit = demand * (price - cost)",
#'   seed = 1234
#' ) %>% str()
#'
#' @seealso \code{\link{summary.simulater}} to summarize results
#' @seealso \code{\link{plot.simulater}} to plot results
#'
#' @export
simulater <- function(
  const = "", lnorm = "", norm = "", unif = "", discrete = "",
  binom = "", pois = "", sequ = "", grid = "", data = NULL, 
  form = "", seed = NULL, nexact = FALSE, ncorr = NULL, 
  name = "", nr = 1000, dataset = NULL
) {

  # if (!is_empty(name)) message("Use of the 'name' argument is deprecated. Please remove it from the call to 'simulater'")
  if (!is_empty(seed)) set.seed(as_numeric(seed))
  if (is.null(dataset)) {
    dataset <- list()
  } else {
    ## needed because number may be NA and missing if grid used in Simulate
    nr <- attr(dataset, "sim_call")$nr
  }

  grid %<>% sim_cleaner()
  if (grid != "" && length(dataset) == 0) {
    s <- grid %>% sim_splitter()
      for (i in 1:length(s)) {
      if (is_empty(s[[i]][4])) s[[i]][4] <- 1
      s[[i]] %>% {dataset[[.[1]]] <<- seq(as.numeric(.[2]), as.numeric(.[3]), as.numeric(.[4]))}
    }
    dataset <- as.list(expand.grid(dataset) %>% as.data.frame(stringsAsFactors = FALSE))
    nr <- length(dataset[[1]])
  }

  if (is_empty(nr)) {
    mess <- c("error", paste0("Please specify the number of simulations in '# sims'"))
    return(add_class(mess, "simulater"))
  }

  ## parsing constant
  const %<>% sim_cleaner()
  if (const != "") {
    s <- const %>% sim_splitter()
    for (i in 1:length(s))
      s[[i]] %>% {dataset[[.[1]]] <<- as.numeric(.[2]) %>% rep(nr)}
  }

  ## parsing uniform
  unif %<>% sim_cleaner()
  if (unif != "") {
    s <- unif %>% sim_splitter()
    for (i in 1:length(s))
      s[[i]] %>% {dataset[[.[1]]] <<- runif(nr, as.numeric(.[2]), as.numeric(.[3]))}
  }

  ## parsing log normal
  lnorm %<>% sim_cleaner()
  if (lnorm != "") {
    s <- lnorm %>% sim_splitter()
    for (i in 1:length(s)) {
      sdev <- as.numeric(s[[i]][3])
      if (!sdev > 0) {
        mess <- c("error", paste0("All log-normal variables should have a standard deviation larger than 0.\nPlease review the input carefully"))
        return(add_class(mess, "simulater"))
      }
      s[[i]] %>% {dataset[[.[1]]] <<- rlnorm(nr, as.numeric(.[2]), sdev)}
    }
  }

  ## parsing normal
  norm %<>% sim_cleaner()
  if (norm != "") {
    s <- norm %>% sim_splitter()
    means <- sds <- nms <- c()
    for (i in 1:length(s)) {
      sdev <- as.numeric(s[[i]][3])
      if (!sdev > 0) {
        mess <- c("error", paste0("All normal variables should have a standard deviation larger than 0.\nPlease review the input carefully"))
        return(add_class(mess, "simulater"))
      }
      if (is_empty(ncorr) || length(s) == 1) {
        if (nexact) {
          s[[i]] %>% {
            dataset[[.[1]]] <<- scale(rnorm(nr, 0, 1)) * sdev + as.numeric(.[2])
          }
        } else {
          s[[i]] %>% {
            dataset[[.[1]]] <<- rnorm(nr, as.numeric(.[2]), sdev)
          }
        }
      } else {
        nms <- c(nms, s[[i]][1])
        means <- c(means, as.numeric(s[[i]][2]))
        sds <- c(sds, sdev)
      }
    }
    if (!is_empty(ncorr) && length(nms) > 1) {
      ncorr <- gsub(",", " ", ncorr) %>% strsplit("\\s+") %>% unlist() %>% as.numeric()
      ncorr_nms <- combn(nms, 2) %>% apply(., 2, paste, collapse = "-")
      if (length(ncorr) == 1 && length(ncorr_nms) > 2) {
        ncorr <- rep(ncorr, length(ncorr_nms))
      }
      if (length(ncorr) != length(ncorr_nms)) {
        mess <- c("error", paste0("The number of correlations specified is not equal to\nthe number of pairs of variables to be simulated.\nPlease review the input carefully"))
        return(add_class(mess, "simulater"))
      }
      names(ncorr) <- ncorr_nms
      # corr_mat <- diag(length(nms)) %>% set_colnames(nms) %>% set_rownames(nms)
      # corr_mat[lower.tri(corr_mat, diag = FALSE)] <- ncorr
      # print(corr_mat)
      df <- try(sim_cor(nr, ncorr, means, sds, exact = nexact), silent = TRUE)
      if (is(df, "try-error")) {
        mess <- c("error", paste0("Data with the specified correlation structure could not be generated.\nPlease review the input and try again"))
        return(add_class(mess, "simulater"))
      }

      colnames(df) <- nms
      for (i in nms) {
        dataset[[i]] <- df[[i]]
      }
    }
  }

  ## parsing binomial
  binom %<>% sim_cleaner()
  if (binom != "") {
    s <- binom %>% sim_splitter()
    for (i in 1:length(s))
      s[[i]] %>% {dataset[[.[1]]] <<- rbinom(nr, as_integer(.[2]), as_numeric(.[3]))}
  }

  ## parsing poisson
  pois %<>% sim_cleaner()
  if (pois != "") {
    s <- pois %>% sim_splitter()
    for (i in 1:length(s))
      s[[i]] %>% {dataset[[.[1]]] <<- rpois(nr, as_integer(.[2]))}
  }

  ## parsing sequence
  sequ %<>% sim_cleaner()
  if (sequ != "") {
    s <- sequ %>% sim_splitter()
    for (i in 1:length(s))
      s[[i]] %>% {dataset[[.[1]]] <<- seq(as.numeric(.[2]), as.numeric(.[3]), length.out = as.numeric(nr))}
  }

  ## adding data to dataset list
  if (is.data.frame(data)) {
    for (i in colnames(data)) {
      dataset[[i]] <- data[[i]]
    }
  }

  ## parsing discrete
  discrete %<>% sim_cleaner()
  if (discrete != "") {
    s <- discrete %>% sim_splitter()
    for (i in 1:length(s)) {
      dpar <- s[[i]][-1] %>% gsub(",", " ", .) %>% strsplit("\\s+") %>% unlist() %>% strsplit("/")
      asNum <- function(x) ifelse(length(x) > 1, as.numeric(x[1]) / as.numeric(x[2]), as.numeric(x[1]))
      dpar <- sshhr(try(sapply(dpar, asNum) %>% matrix(ncol = 2), silent = TRUE))

      if (is(dpar, "try-error") || any(is.na(dpar))) {
        mess <- c("error", paste0("Input for discrete variable # ", i, " contains an error. Please review the input carefully"))
        return(add_class(mess, "simulater"))
      } else if (sum(dpar[, 2]) != 1) {
        mess <- c("error", paste0("Probabilities for discrete variable # ", i, " do not sum to 1 (", round(sum(dpar[[2]]), 3), ")"))
        return(add_class(mess, "simulater"))
      }

      dataset[[s[[i]][1]]] <- sample(dpar[, 1], nr, replace = TRUE, prob = dpar[, 2])
    }
  }

  form %<>% sim_cleaner()
  if (form != "") {
    s <- form %>% 
      gsub("\\s+", "", .) %>% 
      sim_splitter("=")
    for (i in 1:length(s)) {
      if (grepl("^\\s*?#", s[[i]][1], perl = TRUE)) next
      obj <- s[[i]][1]
      fobj <- s[[i]][-1]
      if (length(fobj) > 1) fobj <- paste0(fobj, collapse = "=")
      out <- try(do.call(with, list(dataset, parse(text = fobj))), silent = TRUE)
      if (!is(out, "try-error")) {
        dataset[[obj]] <- out
      } else {
        dataset[[obj]] <- NA
        mess <- c(
            "error", paste0("Formula was not successfully evaluated:\n\n", strsplit(form, ";") %>% 
            unlist() %>% 
            paste0(collapse = "\n"), "\n\nMessage: ", attr(out, "condition")$message)
        )
        return(add_class(mess, "simulater"))
      }
    }
  }

  ## removing data from dataset list
  if (is.data.frame(data)) {
    dataset[colnames(data)] <- NULL
    # print(deparse(substitute(data)))
    # attr(dataset, "df_name") <- deparse(substitute(data))
  }

  ## convert list to a data.frame
  dataset <- as.data.frame(dataset, stringsAsFactors = FALSE) %>% na.omit()

  ## capturing the function call for use in repeat
  sc <- formals()
  smc <- lapply(match.call()[-1], eval, envir = parent.frame())
  sc[names(smc)] <- smc
  sc$nr <- nr
  sc$ncorr <- ncorr
  sc$nexact <- nexact
  attr(dataset, "sim_call") <- sc
  attr(dataset, "df_name") <- deparse(substitute(data))

  if (nrow(dataset) == 0) {
    mess <- c("error", paste0("The simulated data set has 0 rows"))
    return(add_class(mess, "simulater"))
  }

  form <- gsub("*", "\\*", form, fixed = TRUE) %>%
    gsub("^\\s*?\\#+[^\\#]", "##### # ", .) %>%
    gsub("[;\n]\\s*?\\#+[^\\#]", "; ##### # ", .) %>%
    gsub(";\\s*", "\n\n", .)

  mess <- paste0("\n### Simulated data\n\nFormulas:\n\n", form, "\n\nDate: ", lubridate::now())

  add_class(set_attr(dataset, "description", mess), "simulater")
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
#' simdat <- simulater(norm = "demand 2000 1000", seed = 1234)
#' summary(simdat)
#'
#' @seealso \code{\link{simulater}} to generate the results
#' @seealso \code{\link{plot.simulater}} to plot results
#'
#' @export
summary.simulater <- function(object, dec = 4, ...) {
  if (is.character(object)) {
    if (length(object) == 2 && object[1] == "error") {
      return(cat(object[2]))
    } 
    stop("To generate summary statistics please provide a simulated dataset as input", call. = FALSE)
  }

  sc <- attr(object, "sim_call")
  clean <- function(x) {
    paste0(x, collapse = ";") %>%
    gsub(";", "; ", .) %>%
      gsub("\\n", "", .) %>%
      paste0(., "\n")
  }

  cat("Simulation\n")
  cat("Simulations:", format_nr(nrow(object), dec = 0), "\n")
  cat("Random seed:", sc$seed, "\n")
  if (is_empty(sc$name)) {
    cat("Sim data   :", deparse(substitute(object)), "\n")
  } else {
    cat("Sim data   :", sc$name, "\n")
  }
  if (!is_empty(sc$binom)) cat("Binomial   :", clean(sc$binom))
  if (!is_empty(sc$discrete)) cat("Discrete   :", clean(sc$discrete))
  if (!is_empty(sc$lnorm)) cat("Log normal :", clean(sc$lnorm))
  if (!is_empty(sc$norm)) cat("Normal     :", clean(ifelse(sc$nexact, paste0(sc$norm, "(exact)"), sc$norm)))
  if (!is_empty(sc$unif)) cat("Uniform    :", clean(sc$unif))
  if (!is_empty(sc$pois)) cat("Poisson    :", clean(sc$pois))
  if (!is_empty(sc$const)) cat("Constant   :", clean(sc$const))
  if (is.data.frame(sc$data)) cat("Data       :", attr(object, "df_name"), "\n")
  if (!is_empty(sc$grid)) cat("Grid search:", clean(sc$grid))
  if (!is_empty(sc$sequ)) cat("Sequence   :", clean(sc$sequ))
  if (!is_empty(sc$form)) cat(paste0("Formulas   :\n\t", paste0(sc$form, collapse = ";") %>% gsub(";", "\n", .) %>% gsub("\n", "\n\t", .), "\n"))
  cat("\n")

  if (!is_empty(sc$ncorr) && is.numeric(sc$ncorr)) {
    cat("Correlations:\n")
    print(sc$ncorr)
    cat("\n")
  }

  sim_summary(object, dec = ifelse(is_empty(dec), 4, round(dec, 0)))
}

#' Plot method for the simulater function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/simulater} for an example in Radiant
#'
#' @param x Return value from \code{\link{simulater}}
#' @param bins Number of bins used for histograms (1 - 50)
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' simdat <- simulater(
#'   const = "cost 3", 
#'   norm = "demand 2000 1000", 
#'   discrete = "price 5 8 .3 .7", 
#'   form = "profit = demand * (price - cost)",
#'   seed = 1234
#' )
#' plot(simdat, bins = 25)
#'
#' @seealso \code{\link{simulater}} to generate the result
#' @seealso \code{\link{summary.simulater}} to summarize results
#'
#' @export
plot.simulater <- function(x, bins = 20, shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) {
    return(invisible())
  } 
  if (nrow(x) == 0) return(invisible())
  plot_list <- list()
  for (i in colnames(x)) {
    dat <- select_at(x, .vars = i)
    if (!does_vary(x[[i]])) next
    plot_list[[i]] <- select_at(x, .vars = i) %>%
      visualize(xvar = i, bins = bins, custom = TRUE)
  }

  if (custom) {
    if (length(plot_list) == 1) {
      return(plot_list[[1]]) 
    } else {
      return(plot_list)
    }
  }

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = min(length(plot_list), 2))) %>% 
    {if (shiny) . else print(.)}
}

#' Repeated simulation
#'
#' @param dataset Return value from the simulater function
#' @param nr Number times to repeat the simulation
#' @param vars Variables to use in repeated simulation
#' @param grid Character vector of expressions to use in grid search for constants
#' @param sum_vars (Numeric) variables to summaries
#' @param byvar Variable(s) to group data by before summarizing
#' @param fun Functions to use for summarizing
#' @param form A character vector with the formula to apply to the summarized data
#' @param seed Seed for the repeated simulation
#' @param name Deprecated argument
#'
#' @examples
#' simdat <- simulater(
#'   const = c("var_cost 5","fixed_cost 1000"), 
#'   norm = "E 0 100;",
#'   discrete = "price 6 8 .3 .7;",
#'   form = c(
#'     "demand = 1000 - 50*price + E", 
#'     "profit = demand*(price-var_cost) - fixed_cost",
#'     "profit_small = profit < 100"
#'   ), 
#'   seed = 1234
#' )
#'
#' repdat <- repeater(
#'   simdat,
#'   nr = 12, 
#'   vars = c("E","price"), 
#'   sum_vars = "profit", 
#'   byvar = "sim", 
#'   form = "profit_365 = profit < 36500", 
#'   seed = 1234, 
#' ) 
#' 
#' head(repdat)
#' summary(repdat)
#' plot(repdat)
#'
#' @seealso \code{\link{summary.repeater}} to summarize results from repeated simulation
#' @seealso \code{\link{plot.repeater}} to plot results from repeated simulation
#'
#' @export
repeater <- function(
  dataset, nr = 12, vars = "", grid = "", sum_vars = "", 
  byvar = "sim", fun = "sum", form = "", seed = NULL, 
  name = ""
) {

  if (byvar == "sim") grid <- ""
  if (is_empty(nr)) {
    if (is_empty(grid)) {
      mess <- c("error", paste0("Please specify the number of repetitions in '# reps'"))
      return(add_class(mess, "repeater"))
    } else {
      nr <- 1
    }
  }

  if (is_string(dataset)) {
    df_name <- dataset
    dataset <- get_data(dataset)
  } else {
    df_name <- deparse(substitute(dataset)) 
  }
  if (!is_empty(seed)) set.seed(as_numeric(seed))

  if (identical(vars, "") && identical(grid, "")) {
    mess <- c("error", paste0("Select variables to re-simulate and/or a specify a constant\nto change using 'Grid search' when Group by is set to Repeat"))
    return(add_class(mess, "repeater"))
  }

  if (identical(vars, "")) vars <- character(0)

  grid_list <- list()
  if (!identical(grid, "")) {
    grid %<>% sim_cleaner()
    if (grid != "") {
      s <- grid %>% sim_splitter()
      for (i in 1:length(s)) {
        if (is_empty(s[[i]][4])) s[[i]][4] <- 1
        s[[i]] %>% {
          grid_list[[.[1]]] <<- seq(as.numeric(.[2]), as.numeric(.[3]), as.numeric(.[4]))
        }
      }
    }
    ## expanding list of variables but removing ""
    vars <- c(vars, names(grid_list)) %>% unique()
  }

  ## from http://stackoverflow.com/a/7664655/1974918
  ## keep those list elements that, e.g., q is in
  nr_sim <- nrow(dataset)
  sc <- attr(dataset, "sim_call")

  ## needed if inputs are provided as vectors
  sc[1:(which(names(sc) == "seed") - 1)] %<>% lapply(paste, collapse = ";")

  if (!is_empty(sc$data)) vars <- c(sc$data, vars)

  sc$name <- sc$seed <- "" ## cleaning up the sim call

  ## using \\b based on https://stackoverflow.com/a/34074458/1974918
  sc_keep <- grep(paste(paste0("\\b", vars, "\\b"), collapse = "|"), sc, value = TRUE)

  ## ensure that only the selected variables of a specific type are resimulated
  ## e.g., if A, B, and C are normal and A should be re-sim'd, don't also re-sim B and C
  for (i in names(sc_keep)) {
    if (i == "form") next
    sc_check <- sim_cleaner(sc_keep[[i]]) %>%
      sim_splitter(";")
    if (length(sc_check) < 2) {
      next
    } else {
      sc_keep[[i]] <- grep(paste(paste0("\\b", vars, "\\b"), collapse = "|"), sc_check, value = TRUE) %>%
        paste0(collapse = ";\n")
    }
  }

  ## needed in case there is no 'form' in simulate
  sc[1:(which(names(sc) == "seed") - 1)] <- ""
  sc[names(sc_keep)] <- sc_keep
  sc$dataset <- as.list(dataset)

  summarize_sim <- function(object) {
    if (fun == "none") {
      object <- select_at(object, .vars = c("rep", "sim", sum_vars))
    } else if (fun %in% c("first", "last")) {
      object <- group_by_at(object, byvar) %>%
        summarise_at(.vars = sum_vars, .funs = fun) %>%
        set_colnames(c(byvar, sum_vars))
    } else {
      object <- group_by_at(object, byvar) %>%
        summarise_at(.vars = sum_vars, .funs = fun, na.rm = TRUE) %>%
        set_colnames(c(byvar, sum_vars))
    }
    object
  }

  rep_sim <- function(rep_nr, sfun = function(x) x) {
    bind_cols(
      data.frame(rep = rep(rep_nr, nr_sim), sim = 1:nr_sim, stringsAsFactors = FALSE),
      do.call(simulater, sc)
    ) %>% 
      na.omit() %>% 
      sfun()
  }

  rep_grid_sim <- function(gval, sfun = function(x) x) {
    gvars <- names(gval)

    ## removing form ...
    sc_grid <- grep(paste(gvars, collapse = "|"), sc_keep, value = TRUE) %>%
      {.[which(names(.) != "form")]} %>%
      gsub("[ ]{2,}", " ", .)

    for (i in 1:length(gvars)) {
      sc_grid %<>% sub(paste0("[;\n]", gvars[i], " [.0-9]+"), paste0("\n", gvars[i], " ", gval[gvars[i]]), .) %>%
        sub(paste0("^", gvars[i], " [.0-9]+"), paste0(gvars[i], " ", gval[gvars[i]]), .)
    }

    sc[names(sc_grid)] <- sc_grid
    bind_cols(
      data.frame(rep = rep(paste(gval, collapse = "|"), nr_sim), sim = 1:nr_sim, stringsAsFactors = FALSE),
      do.call(simulater, sc)
    ) %>% 
    na.omit() %>% 
    sfun()
  }

  if (length(grid_list) == 0) {
    if (byvar == "sim") {
      ret <- bind_rows(lapply(1:nr, rep_sim)) %>%
        summarize_sim() %>%
        add_class("repeater")
    } else {
      ret <- bind_rows(lapply(1:nr, function(x) rep_sim(x, summarize_sim))) %>%
        add_class("repeater")
    }
  } else {
    grid <- expand.grid(grid_list)
    if (byvar == "sim") {
      ret <- bind_rows(apply(grid, 1, rep_grid_sim)) %>%
        summarize_sim() %>%
        add_class("repeater")
    } else {
      ret <- bind_rows(apply(grid, 1, function(x) rep_grid_sim(x, summarize_sim))) %>%
        add_class("repeater")
    }
  }

  form %<>% sim_cleaner()
  if (form != "") {
    s <- form %>% gsub("\\s+", "", .) %>% sim_splitter("=")
    for (i in 1:length(s)) {
      if (grepl("^#", s[[i]][1])) next
      obj <- s[[i]][1]
      fobj <- s[[i]][-1]
      if (length(fobj) > 1) fobj <- paste0(fobj, collapse = "=")
      out <- try(do.call(with, list(ret, parse(text = fobj))), silent = TRUE)
      if (!is(out, "try-error")) {
        ret[[obj]] <- out
      } else {
        ret[[obj]] <- NA
        mess <- c("error", paste0("Formula was not successfully evaluated:\n\n", strsplit(form, ";") %>% unlist() %>% paste0(collapse = "\n"), "\n\nMessage: ", attr(out, "condition")$message, "\n\nNote that these formulas can only be applied to selected 'Output variables'"))
        return(add_class(mess, "repeater"))
      }
    }
  }

  ## tbl_df remove attributes so use as.data.frame for now
  ret <- as.data.frame(ret, stringsAsFactors = FALSE)

  ## capturing the function call for use in summary and plot
  rc <- formals()
  rmc <- lapply(match.call()[-1], eval, envir = parent.frame())
  rc[names(rmc)] <- rmc

  rc$sc <- sc[base::setdiff(names(sc), "dat")]
  attr(ret, "rep_call") <- rc
  attr(ret, "df_name") <- df_name

  mess <- paste0(
    "\n### Repeated simulation data\n\nFormula:\n\n",
    gsub("*", "\\*", sc$form, fixed = TRUE) %>%
      gsub("[;\n]\\s*?\\#+[^\\#]", "; ##### # ", .) %>%
      gsub(";", "\n\n", .),
    "\n\nDate: ",
    lubridate::now()
  )

  add_class(set_attr(ret, "description", mess), "repeater")
}

#' Summarize repeated simulation
#'
#' @param object Return value from \code{\link{repeater}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{repeater}} to run a repeated simulation
#' @seealso \code{\link{plot.repeater}} to plot results from repeated simulation
#'
#' @export
summary.repeater <- function(object, dec = 4, ...) {

  if (is.character(object)) {
    if (length(object) == 2 && object[1] == "error") {
      return(cat(object[2]))
    } 
    stop("To generate summary statistics please provide a simulated dataset as input", call. = FALSE)
  }

  ## getting the repeater call
  rc <- attr(object, "rep_call")

  clean <- function(x) {
    paste0(x, collapse = ";") %>%
      gsub(";", "; ", .) %>%
      gsub("\\n", "", .) %>%
      paste0(., "\n")
  }

  ## show results
  cat("Repeated simulation\n")
  cat("Simulations   :", ifelse(is_empty(rc$sc$nr), "", format_nr(rc$sc$nr, dec = 0)), "\n")
  cat("Repetitions   :", format_nr(ifelse(is_empty(rc$nr), nrow(object), rc$nr), dec = 0), "\n")
  if (!is_empty(rc$vars)) {
    cat("Re-simulated  :", paste0(rc$vars, collapse = ", "), "\n")
  }
  cat("Group by      :", ifelse(rc$byvar == "rep", "Repeat", "Simulation"), "\n")
  cat("Function      :", rc$fun, "\n")
  cat("Random  seed  :", rc$seed, "\n")
  if (is.data.frame(rc$sim)) {
    rc$sim <- attr(rc$sim, "sim_call")$name
  }
  cat("Simulated data:", attr(object, "df_name"), "\n")
  if (is_empty(rc$name)) {
    cat("Repeat  data  :", deparse(substitute(object)), "\n")
  } else {
    cat("Repeat  data  :", rc$name, "\n")
  }

  if (isTRUE(rc$byvar == "rep") && !is_empty(rc$grid)) {
    cat("Grid search.  :", clean(rc$grid))
  }

  if (!is_empty(rc$form)) {
    rc$form %<>% sim_cleaner()
    paste0(
      "Formulas      :\n\t", 
      paste0(rc$form, collapse = ";") %>% 
        gsub(";", "\n", .) %>% 
        gsub("\n", "\n\t", .), 
      "\n"
    ) %>% cat()
  }
  cat("\n")

  sim_summary(select(object, -1), fun = rc$fun, dec = ifelse(is.na(dec), 4, dec))
}

#' Plot repeated simulation
#'
#' @param x Return value from \code{\link{repeater}}
#' @param bins Number of bins used for histograms (1 - 50)
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{repeater}} to run a repeated simulation
#' @seealso \code{\link{summary.repeater}} to summarize results from repeated simulation
#'
#' @export
plot.repeater <- function(x, bins = 20, shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) {
    return(invisible())
  }
  if (nrow(x) == 0) return(invisible())

  ## getting the repeater call
  rc <- attr(x, "rep_call")
  plot_list <- list()
  for (i in colnames(x)[-1]) {
    dat <- select_at(x, .vars = i)
    if (!does_vary(x[[i]])) next

    plot_list[[i]] <- select_at(x, .vars = i) %>%
      visualize(xvar = i, bins = bins, custom = TRUE)

    if (i %in% rc$sum_vars && !is_empty(rc$fun, "none")) {
      plot_list[[i]] <- plot_list[[i]] + labs(x = paste0(rc$fun, " of ", i))
    }
  }

  if (length(plot_list) == 0) return(invisible())

  if (custom) {
    if (length(plot_list) == 1) {
      return(plot_list[[1]]) 
    } else {
      return(plot_list)
    }
  }

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = min(length(plot_list), 2))) %>% 
    {if (shiny) . else print(.)}
}

#' Print simulation summary
#'
#' @param dataset Simulated data
#' @param dc Variable classes
#' @param fun Summary function to apply
#' @param dec Number of decimals to show
#'
#' @seealso \code{\link{simulater}} to run a simulation
#' @seealso \code{\link{repeater}} to run a repeated simulation
#'
#' @examples
#' simulater(
#'   const = "cost 3", 
#'   norm = "demand 2000 1000", 
#'   discrete = "price 5 8 .3 .7", 
#'   form = c("profit = demand * (price - cost)", "profit5K = profit > 5000"),
#'   seed = 1234
#' ) %>% sim_summary()
#'
#' @export
sim_summary <- function(dataset, dc = get_class(dataset), fun = "", dec = 4) {
  isFct <- "factor" == dc
  isNum <- dc %in% c("numeric", "integer", "Duration")
  isChar <- "character" == dc
  isLogic <- "logical" == dc

  dec <- ifelse(is.na(dec), 4, as_integer(dec))

  if (sum(isNum) > 0) {
    isConst <- !sapply(dataset, does_vary) & isNum
    if (sum(isConst) > 0) {
      cn <- names(dc)[isConst]
      cat("Constants:\n")
      select(dataset, which(isConst)) %>%
        na.omit() %>%
        .[1, ] %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        round(dec) %>%
        mutate_all(funs(formatC(., big.mark = ",", digits = dec, format = "f"))) %>%
        set_rownames("") %>%
        set_colnames(cn) %>%
        print()
      cat("\n")
    }

    isRnd <- isNum & !isConst
    if (sum(isRnd) > 0) {
      cn <- names(dc)[isRnd]
      cat("Variables:\n")
      select(dataset, which(isNum & !isConst)) %>%
        gather("variable", "values", !! cn) %>%
        group_by_at(.vars = "variable") %>%
        summarise_all(
          funs(
            n_obs = n_obs, mean = mean, sd = sd, min = min,
            `25%` = p25, median = median, `75%` = p75, max = max
          ),
          na.rm = TRUE
        ) %>%
        {if (fun == "" || fun == "none") { . } else { .[[1]] <- paste0(fun, " of ", .[[1]]) }; .} %>%
        {.[[1]] <- format(.[[1]], justify = "left"); .} %>%
        data.frame(check.names = FALSE, stringsAsFactors = FALSE) %>%
        format_df(., dec = dec, mark = ",") %>%
        print(row.names = FALSE)
      cat("\n")
    }
  }

  if (sum(isLogic) > 0) {
    cat("Logicals:\n")
    select(dataset, which(isLogic)) %>%
      summarise_all(funs(sum, mean), na.rm = TRUE) %>%
      round(dec) %>%
      matrix(ncol = 2) %>%
      set_colnames(c("TRUE (nr)  ", "TRUE (prop)")) %>%
      set_rownames(names(dataset)[isLogic]) %>%
      print()
    cat("\n")
  }

  if (sum(isFct) > 0 | sum(isChar) > 0) {
    cat("Factors:\n")
    select(dataset, which(isFct | isChar)) %>%
      mutate_if(is.character, funs(as_factor)) %>%
      summary() %>%
      print()
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
sim_cleaner <- function(x) {
  gsub("[ ]{2,}", " ", paste(x, collapse = ";")) %>%
    gsub("[ ]*[\n;]+[ ]*", ";", .) %>%
    gsub("[;]{2,}", ";", .) %>%
    gsub(";$", "", .) %>%
    gsub("^;", "", .)
}

#' Split input command string
#'
#' @param x Input string
#' @param symbol Symbol used to split the command string
#'
#' @return Split input command string
#'
#' @export
sim_splitter <- function(x, symbol = " ") {
  strsplit(x, ";") %>%
    extract2(1) %>%
    strsplit(., symbol)
}

#' Find maximum value of a vector
#'
#' @details Find the value of y at the maximum value of x
#' @param x Variable to find the maximum for
#' @param y Variable to find the value for at the maximum of var
#'
#' @return Value of val at the maximum of var
#'
#' @examples
#' find_min(1:10, 20:30)
#'
#' @export
find_max <- function(x, y) {
  if (missing(y)) {
    stop("Error in find_max (2 inputs required)\nSpecify the variable to evaluate at the maximum of the first input")
  }
  y[which.max(x)]
}

#' Find minimum value of a vector
#'
#' @details Find the value of y at the minimum value of x
#' @param x Variable to find the minimum for
#' @param y Variable to find the value for at the maximum of var
#'
#' @return Value of val at the minimum of var
#'
#' @examples
#' find_min(1:10, 20:30)
#'
#' @export
find_min <- function(x, y) {
  if (missing(y)) {
    stop("Error in find_min (2 inputs required)\nSpecify the variable to evaluate at the minimum of the first input")
  }
  y[which.min(x)]
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
  nr <- length(dl) / 2
  w <- data.frame(dl[1:nr], stringsAsFactors = FALSE)
  d <- data.frame(dl[(nr + 1):length(dl)], stringsAsFactors = FALSE)
  apply(w, 1, function(w) sd(rowSums(sweep(d, 2, w, "*"))))
}

#' Simulate correlated normally distributed data
#'
#' @param n The number of values to simulate (i.e., the number of rows in the simulated data)
#' @param rho A vector of correlations to apply to the columns of the simulated data. The number of values should be equal to one or to the number of combinations of variables to be simulated
#' @param means A vector of means. The number of values should be equal to the number of variables to simulate
#' @param sds A vector of standard deviations. The number of values should be equal to the number of variables to simulate
#' @param exact A logical that indicates if the inputs should be interpreted as population of sample characteristics
#'
#' @return A data.frame with the simulated data
#'
#' @examples
#' sim <- sim_cor(100, .74, c(0, 10), c(1, 5), exact = TRUE)
#' cor(sim)
#' sim_summary(sim)
#'
#' @export
sim_cor <- function(n, rho, means, sds, exact = FALSE) {
  nrx <- length(means)
  C <- matrix(1, nrow = nrx, ncol = nrx)
  C[lower.tri(C)] <- C[upper.tri(C)] <- rho

  X <- matrix(rnorm(n * nrx, 0, 1), ncol = nrx)

  if (exact) {
    X <- psych::principal(X, nfactors = nrx, scores = TRUE)$scores
  }

  X <- X %*% chol(C)

  X <- sweep(X, 2, sds, "*")
  X <- sweep(X, 2, means, "+")
  as.data.frame(X, stringsAsFactors = FALSE)
}
