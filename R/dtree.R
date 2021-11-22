#' Parse yaml input for dtree to provide (more) useful error messages
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/dtree.html} for an example in Radiant
#'
#' @param yl A yaml string
#'
#' @return An updated yaml string or a vector messages to return to the users
#'
#' @seealso \code{\link{dtree}} to calculate tree
#' @seealso \code{\link{summary.dtree}} to summarize results
#' @seealso \code{\link{plot.dtree}} to plot results
#'
#' @importFrom radiant.data fix_smart
#'
#' @export
dtree_parser <- function(yl) {
  if (is_string(yl)) yl <- unlist(strsplit(yl, "\n"))

  ## remove characters that may cause problems in shinyAce or DiagrammeR/mermaid.js
  yl <- fix_smart(yl) %>%
    gsub("[\x80-\xFF]", "", .) %>%
    gsub("\t", "    ", .)

  ## container to collect errors
  err <- c()

  ## cheching if : is present in each line
  col_ln <- grepl("(?=:)|(?=^\\s*$)|(?=^\\s*#)", yl, perl = TRUE)
  if (any(!col_ln)) {
    err <- c(err, paste0("Each line in the tree input must have a ':'. Add a ':' in line(s): ", paste0(which(!col_ln), collapse = ", ")))
  }

  ## add a space to input after the : YAML needs this
  yl %<>% gsub(":([^ $])", ": \\1", .) %>% gsub(":\\s{2,}", ": ", .)

  ## replace .4 by 0.4
  yl %<>% gsub("(^\\s*p\\s*:)\\s*(\\.[0-9]+$)", "\\1 0\\2", ., perl = TRUE)

  ## make sure the labels are in lower case
  yl %<>% gsub("(^\\s*)name(\\s*:)", "\\1name\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)variables(\\s*:)", "\\1variables\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)type(\\s*:)", "\\1type\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)p(\\s*:)", "\\1p\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)payoff(\\s*:)", "\\1payoff\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)cost(\\s*:)", "\\1cost\\2", ., ignore.case = TRUE, perl = TRUE)

  ## check type line is followed by a name
  type_id <- yl %>% grepl("^\\s*type\\s*:\\s*(.*)$", ., perl = TRUE) %>% which()
  type_cid <- yl %>% grepl("^\\s*type\\s*:\\s*((chance)|(decision)|())\\s*$", ., perl = TRUE) %>% which()

  if (!identical(type_id, type_cid)) {
    err <- c(err, paste0("Node type should be 'type: chance', or 'type: decision' in line(s): ", paste0(base::setdiff(type_id, type_cid), collapse = ", ")))
  }

  ## can't have # signs anywhere if line is not a comment
  nc_id <- yl %>% grepl("^\\s*#", ., perl = TRUE) %>%
    {. == FALSE} %>%
    which()

  if (length(nc_id) > 0) {
    yl[nc_id] %<>% gsub("#", "//", ., perl = TRUE) %>%
      gsub("(^\\s*)[\\!`@%&\\*-\\+]*\\s*", "\\1", ., perl = TRUE)
  }

  ## Find node names
  nn_id <- gsub("(^\\s*p\\s*:\\s*$)", "\\1 0", yl) %>%
    gsub("(^\\s*type\\s*:\\s*$)", "\\1 0", .) %>%
    gsub("(^\\s*cost\\s*:\\s*$)", "\\1 0", .) %>%
    gsub("(^\\s*payoff\\s*:\\s*$)", "\\1 0", .) %>%
    grepl("^\\s*[^#]+:\\s*$", ., perl = TRUE) %>%
    which()

  ## replace ( ) { } [ ]
  if (length(nn_id) > 0) {
    yl[nn_id] %<>% gsub("[\\(\\)\\{\\}\\[\\]<>\\@;~]", "/", ., perl = TRUE)
  }

  ## non-commented next line after type
  ncnl_id <- c()
  for (i in type_cid) {
    ncnl_id <- c(ncnl_id, nc_id[nc_id > i][1])
  }

  type_nn <- ncnl_id %in% nn_id

  if (!all(type_nn)) {
    err <- c(err, paste0("The node types defined on line(s) ", paste0(type_cid[!type_nn], collapse = ", "), " must be followed by a node name.\nA valid node name could be 'mud slide:'"))
  }

  ## check indent of next line is the same for type defs
  indent_type <- yl[type_cid] %>% gsub("^(\\s*).*", "\\1", .) %>% nchar()

  ## non-commented next node-name after type
  ncnn_id <- c()
  for (i in type_cid) {
    ncnn_id <- c(ncnn_id, nn_id[nn_id > i][1])
  }

  indent_next <- yl[ncnn_id] %>% gsub("^(\\s*).*", "\\1", .) %>% nchar()

  indent_issue <- is.na(indent_next) | indent_type == indent_next

  if (any(!indent_issue)) {
    err <- c(err, paste0("Indent issue in line(s): ", paste0(type_cid[!indent_issue] + 1, collapse = ", "), "\nUse the tab key to ensure a node name is indented the same amount\nas the node type on the preceding line. Check the level of indentation\non each line shown, as well as the indentation on the preceding lines"))
  }

  ## check indent for node names
  indent_name <- yl[nn_id] %>% gsub("^(\\s*).*", "\\1", .) %>% nchar()

  ## check indent of next line for node names
  indent_next <- yl[nn_id + 1] %>% gsub("^(\\s*).*", "\\1", .) %>% nchar()
  indent_issue <- indent_name >= indent_next

  ## can happen when last line in input is a node without a payoff or prob
  indent_issue[is.na(indent_issue)] <- TRUE

  if (any(indent_issue)) {
    err <- c(err, paste0("Indent issue in line(s): ", paste0(nn_id[indent_issue] + 1, collapse = ", "), "\nAlways use the tab key to indent the line(s) after specifying a node name."))
  }

  ## determine return value
  if (length(err) > 0) {
    paste0("\n**\n", paste0(err, collapse = "\n"), "\n**\n") %>% add_class(c("dtree", "dtree-error"))
  } else {
    paste0(yl, collapse = "\n") %>% add_class("dtree")
  }
}

#' Create a decision tree
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/dtree.html} for an example in Radiant
#'
#' @param yl A yaml string or a list (e.g., from yaml::yaml.load_file())
#' @param opt Find the maximum ("max") or minimum ("min") value for each decision node
#' @param base List of variable definitions from a base tree used when calling a sub-tree
#' @param envir Environment to extract data from
#'
#' @return A list with the initial tree, the calculated tree, and a data.frame with results (i.e., payoffs, probabilities, etc.)
#'
#' @importFrom yaml yaml.load
#' @importFrom stringr str_match
#' @importFrom data.tree as.Node Clone isLeaf isNotLeaf Get
#'
#' @seealso \code{\link{summary.dtree}} to summarize results
#' @seealso \code{\link{plot.dtree}} to plot results
#' @seealso \code{\link{sensitivity.dtree}} to plot results
#'
#' @examples
#' yaml::as.yaml(movie_contract) %>% cat()
#' dtree(movie_contract, opt = "max") %>% summary(output = TRUE)
#' dtree(movie_contract)$payoff
#' dtree(movie_contract)$prob
#' dtree(movie_contract)$solution_df
#'
#' @export
dtree <- function(yl, opt = "max", base = character(0), envir = parent.frame()) {

  ## calculations will be effected is scientific notation is used
  options(scipen = max(getOption("scipen"), 100))

  ## Adapted from https://github.com/gluc/useR15/blob/master/01_showcase/02_decision_tree.R
  ## load yaml string-id if list not provide
  if (is_string(yl)) {

    ## get input file from r_data
    if (!grepl("\\n", yl)) {
      yl <- get_data(yl, envir = envir)
      if (inherits(yl, "list")) {
        yl <- yaml::as.yaml(yl, indent = 4)
      }
    }
    yl <- dtree_parser(yl)

    ## return errors if needed
    if (inherits(yl, "dtree-error")) return(yl)

    ## if the name of input-list in r_data is provided
    yl <- try(yaml::yaml.load(yl), silent = TRUE)

    ## used when a string is provided
    if (inherits(yl, "try-error")) {
      err_line <- stringr::str_match(attr(yl, "condition")$message, "^Scanner error:.*line\\s([0-9]*),")[2]
      if (is.na(err_line)) {
        err <- paste0("**\nError reading the tree input:\n", attr(yl, "condition")$message, "\n\nPlease try again. Examples are shown in the help file (?)\n**")
      } else {
        err <- paste0("**\nIndentation issue found in line ", err_line, ".\nThis means that the indentation level is not correct when compared\nto prior or subsequent lines in the tree input. Use tabs to separate\nthe branches in the decision tree. Fix the indentation error and try\nagain. Examples are shown in the help file (?)\n**")
      }
      return(add_class(err, c("dtree", "dtree-error")))
    }
  }

  if (length(yl) == 0) {
    err <- "**\nThe provided tree input list is empty or not in the correct format.\nPlease double check the tree input and try again.\n**"
    return(add_class(err, c("dtree", "dtree-error")))
  }

  ## getting variables from base if available
  if (!is.null(yl$variables) && is.character(yl$variables[1])) {
    yl_tree <- yl$variables[1]
    if (!exists(yl_tree, envir = envir)) {
      err <- "**\nThe tree referenced in the 'variables:' section is not available.\nPlease correct the name and try again.\n**"
      return(add_class(err, c("dtree", "dtree-error")))
    } else if (!is.character(envir[[yl_tree]]) && !inherits(envir[[yl_tree]], "list")) {
      err <- "**\nThe tree referenced in the 'variables:' section is not of type\ncharacter or list and cannot be used.\n**"
      return(add_class(err, c("dtree", "dtree-error")))
    } else if (inherits(envir[[yl_tree]], "list")) {
      yl$variables <- envir[[yl_tree]]$variables %>% .[!grepl("dtree\\(.*\\)", .)]
    } else {
      yl$variables <- envir[[yl_tree]] %>%
        dtree_parser() %>%
        yaml::yaml.load() %>%
        .$variables %>%
        .[!grepl("dtree\\(.*\\)", .)]
    }
  }

  vars <- ""

  ## can call a sub-tree that doesn't have any variables
  if (length(base) > 0) {
    base <- base[!grepl("dtree\\(.*\\)", base)]
    if (is.null(yl$variables)) yl$variables <- base
  }

  if (!is.null(yl$variables)) {
    vars <- yl$variables

    ## overwrite the values in vars that are also in base
    if (length(base) > 0) vars[names(base)] <- base

    vn <- names(vars)

    if (length(vn) > 1) {
      ret <- sapply(vn, function(x) grepl(x, vn, fixed = TRUE)) %>% set_rownames(vn)
      overlap <- colSums(ret) > 1
      if (any(overlap)) {
        cat("Some of the labels in the 'variables:' section are too similar. Each label should\nbe unique and not be part of another label (e.g., 'proceed' is part of 'do not proceed').\nAn easy fix may be to use somewhat longer labels (e.g., 'success' instead of 'S').\nInstead of 'proceed' and 'do not proceed', for example, you could use 'do proceed'\nand 'do not proceed'. To use search-and-replace in the editor press CTRL-F\n(CMD-F on mac) twice. The overlap in labels is described below:\n\n")
        ret <- ret[, overlap, drop = FALSE]
        for (i in 1:ncol(ret)) {
          tmp <- names(ret[ret[, i], i])
          cat(paste0(paste0("'", tmp[1], "'"), " is part of '", paste0(tail(tmp, -1), collapse = "', '"), "'\n"))
        }
        return("\nPlease update the tree input and try again." %>% add_class(c("dtree", "dtree-error")))
      }
    }

    ## is there a subtree to evaluate?
    for (i in vn) {
      if (grepl("dtree\\(.*\\)", vars[i])) {
        tree <- gsub(".*?([\'\"]+[ A-z0-9_\\.\\-]+[\'\"]+).*", "\\1", vars[i]) %>% gsub("[\"\']", "", .)
        if (exists(tree, envir = envir)) {
          cmd <- gsub("\\)\\s*$", paste0(", base = ", list(vars[!grepl("dtree\\(.*\\)", vars)]), "\\)"), vars[i])
          ret <- try(eval(parse(text = cmd), envir = envir), silent = TRUE)
          if (inherits(ret, "try-error") || !inherits(ret, "list")) {
            return("**\nThe reference to another tree was not succesful. It is possible\nthis was caused by a problem earlier in the 'variables:' section\nor because of a typo in the name of the tree you are trying to\nreference. Please check any messages about issues in the 'variables:'\nsection and try again\n**" %>% add_class(c("dtree", "dtree-error")))
          } else {
            if (!is.null(ret$jl)) {
              vars[i] <- ret$jl$Get(function(x) x$payoff)[1]
            } else {
              vars[i] <- "No payoff was specified for one or more nodes ('payoff:'). Please check\neach `payoff:' the tree input and try again"
            }
          }
        } else {
          vars[i] <- paste0("Decision tree \"", tree, "\" is not available")
        }
      }
    }

    for (i in 2:max(2, length(vn))) {
      vars <- gsub(vn[i - 1], paste0("(", vars[[i - 1]], ")"), vars, fixed = TRUE)
      vars <- sapply(vars, function(x) ifelse(grepl("[A-Za-z]+", x), x, eval(parse(text = x), envir = envir)))
    }
    names(vars) <- vn

    isNum <- function(x) sshhr(!is.na(as.numeric(x)))
    isNot <- vars[!sapply(vars, isNum)]
    if (length(isNot) > 0) {
      cat("Not all variables could be resolved to a numeric value.\n")
      print(as.data.frame(isNot, stringsAsFactors = FALSE) %>% set_names(""))
    }

    ## cycle through a nested list recursively
    ## based on http://stackoverflow.com/a/26163152/1974918
    nlapply <- function(x, fun) {
      if (is.list(x)) {
        lapply(x, nlapply, fun)
      } else {
        fun(x)
      }
    }

    if (any(unlist(nlapply(yl, is.null)))) {
      return("**\nOne or more payoffs or probabilities were not specified.\nUpdate the tree input and try again\n**" %>% add_class(c("dtree", "dtree-error")))
    }

    ## based on http://stackoverflow.com/a/14656351/1974918
    tmp <- as.relistable(yl[base::setdiff(names(yl), "variables")]) %>% unlist()

    for (i in seq_along(vn)) {
      # only substitute variable values for probabilities (p)
      # payoffs (payoff) and costs (cost)
      toSub <- grepl("(\\.p$)|(\\.payoff$)|(\\.cost$)|(^p$)|(^payoff$)|(^cost$)", names(tmp))
      tmp[toSub] <- gsub(vn[i], vars[[i]], tmp[toSub], fixed = TRUE)
    }

    ## any characters left in p, payoff, or cost fields?
    isNot <- grepl("(.p$)|(.payoff$)|(.cost$)", names(tmp))
    isNot <- tmp[isNot]
    isNot <- isNot[grepl("[^0-9.+*/() -]+", isNot)]
    if (length(isNot) > 0) {
      names(isNot) <- gsub(".", ":", names(isNot), fixed = TRUE)
      cat("Not all variables could be resolved to a numeric value.\nNote that only basic formula's are allowed but no R-functions\n")
      print(as.data.frame(isNot, stringsAsFactors = FALSE) %>% set_names(""))
      return("\nUpdate the tree input and try again." %>% add_class(c("dtree", "dtree-error")))
    }

    ## convert payoff, probabilities, and costs to numeric
    tmp <- relist(tmp)
    toNum <- function(x) {
      if (!grepl("[A-Za-z]+", x)) {
        px <- try(eval(parse(text = x), envir = envir), silent = TRUE)
        if (inherits(px, "try-error")) {
          message("There was an error parsing: ", x)
        } else {
          px <- sshhr(as.numeric(px))
          if (is.na(px)) {
            message("There was an error parsing: ", x)
          } else {
            x <- px
          }
        }
      }
      x
    }

    tmp <- nlapply(tmp, toNum)

    ## convert list to node object
    jl <- data.tree::as.Node(tmp)
  } else {
    ## convert list to node object
    jl <- data.tree::as.Node(yl)
  }

  ## if type not set and isLeaf set to terminal
  # pt <- . %>% {if (is.null(.$type)) .$Set(type = "terminal")}
  # jl$Do(pt, filterFun = data.tree::isLeaf)

  isNum <- function(x) !is_not(x) && !grepl("[A-Za-z]+", x)

  cost_check <- ""
  cost_checker <- function(x) {
    ## if type not set and isLeaf set to terminal
    if (is.null(x$type)) x$Set(type = "terminal")

    ## costs should not be set in terminal nodes, use payoff instead
    if (isNum(x$cost)) {
      cost_check <<- "One or more terminal nodes have been assigned a cost. Specifying a cost\nusing 'cost:' in the tree input is only useful if it applies to multiple\nnodes in a branch. If the cost only applies to a single terminal node it\nis better to adjust the payoff value for that node instead"
    }
  }

  jl$Do(cost_checker, filterFun = data.tree::isLeaf)

  ## making a copy of the initial Node object
  jl_init <- data.tree::Clone(jl)

  chance_payoff <- function(node) {
    if (!isNum(node$payoff) || !isNum(node$p)) {
      0
    } else {
      node$payoff * node$p
    }
  }

  decision_payoff <- function(node) {
    if (!isNum(node$payoff)) 0 else node$payoff
  }

  prob_checker <- function(node)
    if (!isNum(node$p)) 0 else node$p

  type_none <- ""
  prob_check <- ""
  calc_payoff <- function(x) {
    if (radiant.data::is_empty(x$type)) {
      x$payoff <- 0
      x$type <- "NONE"
      type_none <<- "One or more nodes do not have a 'type'. Check and update the input file"
    } else if (x$type == "chance") {
      x$payoff <- sum(sapply(x$children, chance_payoff))

      probs <- sapply(x$children, prob_checker)
      if (min(probs) < 0) {
        prob_check <<- "One or more probabilities are smalller than 0.\nPlease correct the tree input ('p:') and re-calculate the tree"
      } else if (max(probs) > 1) {
        prob_check <<- "One or more probabilities are larger than 1.\nPlease correct the tree input ('p:') and re-calculate the tree"
      } else if (round(sum(probs), 2) != 1) {
        prob_check <<- "Probabilities for one (or more) chance nodes do not sum to 1.\nPlease correct the tree input ('p:') and re-calculate the tree"
      }
    } else if (x$type == "decision") {
      x$payoff <- get(opt)(sapply(x$children, decision_payoff))
    }

    ## subtract cost if specified
    if (isNum(x$cost)) x$payoff <- x$payoff - x$cost
  }

  err <- try(jl$Do(calc_payoff, traversal = "post-order", filterFun = data.tree::isNotLeaf), silent = TRUE)

  if (inherits(err, "try-error")) {
    err <- paste0("**\nThere was an error calculating payoffs associated with a chance or decision\nnode. Please check that each terminal node has a payoff and that probabilities\nare correctly specificied. Also check the R(studio) console for messages\n**")
    return(err %>% add_class(c("dtree", "dtree-error")))
  }

  decision <- function(x) {
    po <- sapply(x$children, decision_payoff)
    if (isNum(x$cost)) po <- po - x$cost
    x$decision <- names(po[po == x$payoff])
  }

  err <- try(jl$Do(decision, filterFun = function(x) !is.null(x$type) && x$type == "decision"), silent = TRUE)

  if (inherits(err, "try-error")) {
    err <- paste0("**\nThere was an error calculating payoffs associated with a decision node.\nPlease check that each terminal node has a payoff\n**")
    return(err %>% add_class(c("dtree", "dtree-error")))
  }

  payoff <- jl$Get(function(x) x$payoff)
  prob <- jl$Get(function(x) x$p)

  solution_df <- data.frame(
    level = jl$Get(function(x) x$level),
    label = names(payoff),
    payoff = payoff,
    prob = prob,
    cost = jl$Get(function(x) x$cost),
    type = jl$Get(function(x) x$type)
  )

  list(
    jl_init = jl_init, jl = jl, yl = yl, vars = vars, opt = opt,
    type_none = type_none, prob_check = prob_check, cost_check = cost_check,
    payoff = payoff, prob = prob, solution_df = solution_df
  ) %>%
    add_class("dtree")
}

#' Summary method for the dtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/dtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{simulater}}
#' @param input Print decision tree input
#' @param output Print decision tree output
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' dtree(movie_contract, opt = "max") %>% summary(input = TRUE)
#' dtree(movie_contract, opt = "max") %>% summary(input = FALSE, output = TRUE)
#'
#' @importFrom data.tree Traverse Get FormatPercent
#'
#' @seealso \code{\link{dtree}} to generate the results
#' @seealso \code{\link{plot.dtree}} to plot results
#' @seealso \code{\link{sensitivity.dtree}} to plot results
#'
#' @export
summary.dtree <- function(
  object, input = TRUE, output = FALSE,
  dec = 2, ...
) {
  if (is.character(object)) return(cat(object))

  isNum <- function(x) !is_not(x) && !grepl("[A-Za-z]+", x)

  print_money <- function(x) {
    x %>%
      {if (isNum(.)) . else ""} %>%
      formatC(
        digits = dec,
        decimal.mark = ".",
        big.mark = ",",
        format = "f"
      )
  }

  print_percent <- function(x) {
    x %>%
      {if (isNum(.)) . else NA} %>%
      data.tree::FormatPercent()
  }

  rm_terminal <- function(x)
    x %>%
      {if (is.na(.)) "" else .} %>%
      {if (. == "terminal") "" else .}

  format_dtree <- function(jl) {
    ## set parent type
    nt <- jl$Get(function(x) x$parent$type)
    jl$Set(ptype = nt)

    data.tree::Traverse(jl) %>% {
      data.frame(
        ` ` = data.tree::Get(., "levelName"),
        Probability = data.tree::Get(., "p", format = print_percent),
        Payoff = data.tree::Get(., "payoff", format = print_money),
        Cost = data.tree::Get(., "cost", format = print_money),
        Type = data.tree::Get(., "ptype", format = rm_terminal),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    } %>%
      {.[[" "]] <- format(.[[" "]], justify = "left"); .} %>%
      format_df(mark = ",", dec = dec)
  }

  if (input) {
    cat("Decision tree input:\n")
    cat(yaml::as.yaml(object$yl, indent = 4))
    cat("\n")
  }

  if (all(object$vars != "") && output) {
    cat("Variable input values:\n")
    print(as.data.frame(object$vars, stringsAsFactors = FALSE) %>% set_names(""))
  }

  ## initial setup
  if (object$type_none != "") {
    cat(paste0("\n\n**\n", object$type_none, "\n**\n\n"))
  } else if (!radiant.data::is_empty(object$cost_check)) {
    cat(paste0("\n\n**\n", object$cost_check, "\n**\n\n"))
  } else {
    if (object$prob_check != "") {
      cat(paste0("**\n", object$prob_check, "\n**\n\n"))
    }

    if (output) {
      cat("\nInitial decision tree:\n")
      format_dtree(object$jl_init) %>% print(row.names = FALSE)

      cat("\nFinal decision tree:\n")
      format_dtree(object$jl) %>% print(row.names = FALSE)
    }
  }
}

#' Plot method for the dtree function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/dtree.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{dtree}}
#' @param symbol Monetary symbol to use ($ is the default)
#' @param dec Decimal places to round results to
#' @param final If TRUE plot the decision tree solution, else the initial decision tree
#' @param orient Plot orientation: LR for vertical and TD for horizontal
#' @param width Plot width in pixels (default is "900px")
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' dtree(movie_contract, opt = "max") %>% plot()
#' dtree(movie_contract, opt = "max") %>% plot(final = TRUE, orient = "TD")
#'
#' @importFrom data.tree Traverse Get isNotRoot
#' @importFrom DiagrammeR DiagrammeR mermaid
#'
#' @seealso \code{\link{dtree}} to generate the result
#' @seealso \code{\link{summary.dtree}} to summarize results
#' @seealso \code{\link{sensitivity.dtree}} to plot results
#'
#' @export
plot.dtree <- function(x, symbol = "$", dec = 2, final = FALSE, orient = "LR", width = "900px", ...) {

  ## avoid error when dec is missing
  if (is_not(dec)) dec <- 2

  isNum <- function(x) !is_not(x) && !grepl("[A-Za-z]+", x)

  if ("character" %in% class(x)) {
    return(paste0("graph LR\n A[Errors in the input file]") %>% DiagrammeR::DiagrammeR(.))
  }
  if (x$type_none != "") {
    return(paste0("graph LR\n A[Node does not have a type. Please fix the tree input]") %>% DiagrammeR::DiagrammeR(.))
  }

  ## based on https://gist.github.com/gluc/79ef7a0e747f217ca45e
  jl <- if (final) x$jl else x$jl_init

  ## create ids
  jl$Set(id = paste0("id", 1:jl$totalCount))

  ## create start labels
  FromLabel <- function(node) {
    if (node$parent$isRoot) {
      ToLabel(node$parent)
    } else {
      as.character(node$parent$id)
    }
  }

  ## create arrow labels
  EdgeLabel <- function(node) {
    if (node$isRoot) {
      return(" ")
    } else if (node$parent$type == "decision") {
      lbl <- node$name
    } else if (node$parent$type == "chance") {
      lbl <- paste0(node$name, ": ", format_nr(as.numeric(node$p), dec = dec + 2))
    } else if (node$type == "terminal") {
      lbl <- paste0(node$name, ": ", format_nr(as.numeric(node$p), dec = dec + 2))
    }

    if (length(node$parent$decision) > 0 && length(node$name) > 0 && node$name == node$parent$decision) {
      paste0(" === |", lbl, "|")
    } else {
      paste0(" --- |", lbl, "|")
    }
  }

  FormatPayoff <- function(payoff) {
    if (!isNum(payoff)) payoff <- 0
    format_nr(payoff, paste0("\"", symbol, "\""), dec = dec)
  }

  ToLabel <- function(node) {
    po <- if (final) FormatPayoff(node$payoff) else " "
    if (node$type == "decision") {
      lbl <- paste0("[", po, "]")
    } else if (node$type == "chance") {
      lbl <- paste0("((", po, "))")
    } else if (node$type == "terminal") {
      lbl <- paste0("[", FormatPayoff(node$payoff), "]")
    }
    paste0(" ", node$id, lbl)
  }

  style_decision <- jl$Get("id", filterFun = function(x) x$type == "decision" && is.null(x$cost))
  if (is.null(style_decision)) style_decision <- "id_null"
  style_decision_with_cost <- jl$Get("id", filterFun = function(x) x$type == "decision" && !is.null(x$cost))
  if (is.null(style_decision_with_cost)) style_decision_with_cost <- "id_null"
  style_chance <- jl$Get("id", filterFun = function(x) x$type == "chance" && is.null(x$cost))
  if (is.null(style_chance)) style_chance <- "id_null"
  style_chance_with_cost <- jl$Get("id", filterFun = function(x) x$type == "chance" && !is.null(x$cost))
  if (is.null(style_chance_with_cost)) style_chance_with_cost <- "id_null"

  ToolTip <- function(node) {
    if (final == TRUE && !is.null(node$cost)) {
      sym <- ifelse(node$cost < 0, " + ", " - ")
      paste0(format_nr(node$payoff + node$cost, symbol, dec = dec), sym, format_nr(abs(node$cost), symbol, dec = dec)) %>%
        paste0("click ", node$id, " callback \"", ., "\"")
    } else if (!is.null(node$cost)) {
      paste0("Cost: ", format_nr(node$cost, symbol, dec = dec)) %>%
        paste0("click ", node$id, " callback \"", ., "\"")
    } else {
      NA
    }
  }

  style <- paste0(
    "classDef default fill:none, bg:none, stroke-width:0px;
    classDef chance fill:#FF8C00,stroke:#333,stroke-width:1px;
    classDef chance_with_cost fill:#FF8C00,stroke:#333,stroke-width:3px,stroke-dasharray:4,5;
    classDef decision fill:#9ACD32,stroke:#333,stroke-width:1px;
    classDef decision_with_cost fill:#9ACD32,stroke:#333,stroke-width:3px,stroke-dasharray:4,5;
    class ", paste(style_decision, collapse = ","), " decision;
    class ", paste(style_decision_with_cost, collapse = ","), " decision_with_cost;
    class ", paste(style_chance, collapse = ","), " chance;
    class ", paste(style_chance_with_cost, collapse = ","), " chance_with_cost;"
  )

  trv <- data.tree::Traverse(jl, traversal = "level", filterFun = data.tree::isNotRoot)
  df <- data.frame(
    from = data.tree::Get(trv, FromLabel),
    edge = data.tree::Get(trv, EdgeLabel),
    to = data.tree::Get(trv, ToLabel),
    id = data.tree::Get(trv, ToLabel),
    tooltip = data.tree::Get(trv, ToolTip),
    stringsAsFactors = FALSE
  )

  trv <- data.tree::Traverse(jl, traversal = "level", filterFun = data.tree::isRoot)
  ttip <- c(df[["tooltip"]], data.tree::Get(trv, ToolTip)) %>%
    na.omit() %>%
    unique()

  ## use LR or TD
  paste(
    paste0("graph ", orient), paste(paste0(df$from, df$edge, df$to), collapse = "\n"),
    paste(ttip, collapse = "\n"), style, sep = "\n"
  ) %>%
    ## address image size in pdf and html and allow zooming
    # DiagrammeR::mermaid(., width = "100%", height = "100%")
    DiagrammeR::mermaid(width = width, height = "100%")
}

## add a plot title?
# {htmltools::html_print(tagList(tags$h1("A title"), DiagrammeR::mermaid(., width = width, height = "100%")))}
# html_print(tagList(
#   tags$h1("R + mermaid.js = Something Special")
#   ,tags$pre(diagramSpec)
#   ,tags$div(class="mermaid",diagramSpec)
#   ,DiagrammeR()
# ))

#' Evaluate sensitivity of the decision tree
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/dtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{dtree}}
#' @param vars Variables to include in the sensitivity analysis
#' @param decs Decisions to include in the sensitivity analysis
#' @param envir Environment to extract data from
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org} for options.
#' @param ... Additional arguments

#' @examples
#' dtree(movie_contract, opt = "max") %>%
#'   sensitivity(
#'     vars = "legal fees 0 100000 10000",
#'     decs = c("Sign with Movie Company", "Sign with TV Network"),
#'     custom = FALSE
#'   )
#'
#' @seealso \code{\link{dtree}} to generate the result
#' @seealso \code{\link{plot.dtree}} to summarize results
#' @seealso \code{\link{summary.dtree}} to summarize results
#'
#' @export
sensitivity.dtree <- function(
  object, vars = NULL, decs = NULL,
  envir = parent.frame(),
  shiny = FALSE, custom = FALSE, ...
) {

  yl <- object$yl

  if (radiant.data::is_empty(vars)) {
    return("** No variables were specified **")
  } else if (radiant.data::is_empty(decs)) {
    return("** No decisions were specified **")
  }
  vars <- strsplit(vars, ";") %>% unlist() %>% strsplit(" ")

  calc_payoff <- function(x, nm) {
    yl$variables[[nm]] <- x
    ret <- dtree(yl, opt = object$opt, envir = envir)$jl
    ret$Get(function(x) x$payoff)[decs]
  }

  nms <- c()
  sensitivity <- function(x) {
    tmp <- rep("", 4)
    tmp[2:4] <- tail(x, 3)
    tmp[1] <- paste(head(x, -3), collapse = " ")
    nms <<- c(nms, tmp[1])
    df <- data.frame(
      values = tail(tmp, 3) %>% as.numeric() %>% {seq(.[1], .[2], .[3])},
      stringsAsFactors = FALSE
    )

    if (length(decs) == 1) {
      df[[decs]] <- sapply(df$values, calc_payoff, tmp[1])
    } else {
      df <- cbind(df, sapply(df$values, calc_payoff, tmp[1]) %>% t())
    }
    df
  }
  ret <- lapply(vars, sensitivity)
  names(ret) <- nms

  plot_list <- list()
  for (i in names(ret)) {
    dat <- gather(ret[[i]], "decisions", "payoffs", !! base::setdiff(names(ret[[i]]), "values"))
    plot_list[[i]] <-
      ggplot(dat, aes_string(x = "values", y = "payoffs", color = "decisions")) +
      geom_line() + geom_point(aes_string(shape = "decisions"), size = 2) +
      labs(
        title = paste0("Sensitivity of decisions to changes in ", i),
        x = i
      )
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = 1) %>%
        {if (shiny) . else print(.)}
    }
  }
}
