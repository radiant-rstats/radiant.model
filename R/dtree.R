#' Parse yaml input for dtree to provide (more) useful error messages
#'
#' @details See \url{http://vnijs.github.io/radiant/base/dtree.html} for an example in Radiant
#'
#' @param yl A yaml string
#'
#' @return An updated yaml string or a vector messages to return to the users
#'
#' @seealso \code{\link{dtree}} to calculate tree
#' @seealso \code{\link{summary.dtree}} to summarize results
#' @seealso \code{\link{plot.dtree}} to plot results
#'
#' @export
dtree_parser <- function(yl) {
  if (is_string(yl)) yl <- unlist(strsplit(yl, "\n"))

  ## collect errors
  err <- c()

  ## cheching if a : is present
  # yl <- c(yl, "something without a colon")
  col_ln <- grepl("(?=:)|(?=^\\s*$)|(?=^\\s*#)", yl, perl = TRUE)
  if (any(!col_ln))
    err <- c(err, paste0("Each line must have a ':'. Add a ':' in line(s): ", paste0(which(!col_ln), collapse = ", ")))

  ## add a space to input after the : YAML needs this
  yl %<>% gsub(":([^\\s$])",": \\1", .) %>% gsub("  ", " ", .)

  ## replace .4 by 0.4
  # yl <- c(yl, "p: .4")
  yl %<>% gsub("(^\\s*p\\s*:)\\s*(\\.[0-9]+$)","\\1 0\\2", .,  perl = TRUE)

  ## make sure the labels are in lower case
  yl %<>% gsub("(^\\s*)name(\\s*:)","\\1name\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)type(\\s*:)","\\1type\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)p(\\s*:)","\\1p\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)payoff(\\s*:)","\\1payoff\\2", ., ignore.case = TRUE, perl = TRUE)
  yl %<>% gsub("(^\\s*)cost(\\s*:)","\\1cost\\2", ., ignore.case = TRUE, perl = TRUE)

  ## check type line is followed by a name
  # yl <- c(yl, "   type   : another   ")
  type_id <- yl %>% grepl("^\\s*type\\s*:\\s*(.*)$",., perl = TRUE) %>% which
  type_cid <- yl %>% grepl("^\\s*type\\s*:\\s*((chance)|(decision)|())\\s*$",., perl = TRUE) %>% which

  if (!identical(type_id, type_cid))
    err <- c(err, paste0("Node type should be 'type: chance', or 'type: decision' in line(s): ", paste0(setdiff(type_id, type_cid), collapse = ", ")))

  ## can't have # signs anywhere if line is not a comment
  # yl <- c(" # name # 3:")
  # yl <- c(" storm leaves # 4 now:")
  # yl <- c(" storm # leaves # 4 now:")
  # yl %<>% gsub("(^\\s*[^\\s\\#]+\\s*)(\\#)", "\\1//", .,  perl = TRUE)
  # yl %<>% gsub("(^\\s*[^#][^#]+\\s*)#", "\\1//", .,  perl = TRUE)
  ## incase there are 2 # signs - should be able to do that in
  # yl %<>% gsub("(^\\s*[^#][^#]+\\s*)#", "\\1//", .,  perl = TRUE)
  nc_id <- yl %>% grepl("^\\s*#", .,  perl = TRUE) %>% {. == FALSE} %>% which

  if (length(nc_id) > 0) {
    yl[nc_id] %<>% gsub("#", "//", .,  perl = TRUE) %>%
      gsub("(^\\s*)[\\!`@%&\\*-\\+]*\\s*", "\\1", .,  perl = TRUE)
  }

  ## Find node names
  # yl <- c(" # name 3:")
  # yl <- c(" p:   ", "  type: ")
  # yl <- c(" name 3:")
  # nn_id <- yl %>% grepl("^[^:#]+:\\s*$",., perl = TRUE) %>% which
  # nn_id <- yl %>% grepl("^\\s*[^#]+[^:]+:\\s*$",., perl = TRUE) %>% which
  nn_id <-
    yl %>% gsub("(^\\s*p\\s*:\\s*$)","\\1 0",.) %>%
      gsub("(^\\s*type\\s*:\\s*$)","\\1 0",.) %>%
      gsub("(^\\s*cost\\s*:\\s*$)","\\1 0",.) %>%
      gsub("(^\\s*payoff\\s*:\\s*$)","\\1 0",.) %>%
      grepl("^\\s*[^#]+:\\s*$",., perl = TRUE) %>% which

  ## replace ( ) { } [ ]
  if (length(nn_id) > 0)
    yl[nn_id] %<>% gsub("[\\(\\)\\{\\}\\[\\]<>\\@;~]", "/", .,  perl = TRUE)

  ## check that type is followed by a name
  # yl <- c(yl, "   type   :  decision   ")

  ## non-commented next line after type
  ncnl_id <- c()
  for (i in type_cid) {
    ncnl_id <- c(ncnl_id, nc_id[nc_id > i][1])
  }

  # type_nn <- type_cid %in% (nn_id - 1)
  type_nn <- ncnl_id %in% nn_id

  if (!all(type_nn))
    err <- c(err, paste0("The node types defined on line(s) ", paste0(type_cid[!type_nn], collapse = ", "), " must be followed by a node name.\nA valid node name could be 'mud slide:'"))

  ## check indent of next line is the same for type defs
  indent_type <- yl[type_cid] %>% gsub("^(\\s*).*","\\1", .) %>% nchar

  ## non-commented next node-name after type
  ncnn_id <- c()
  for (i in type_cid) {
    ncnn_id <- c(ncnn_id, nn_id[nn_id > i][1])
  }

  indent_next <- yl[ncnn_id] %>% gsub("^(\\s*).*","\\1", .) %>% nchar
  indent_issue <- indent_type == indent_next

  if (any(!indent_issue))
    err <- c(err, paste0("Indent issue in line(s): ", paste0(type_cid[!indent_issue] + 1, collapse = ", "), "\nUse the tab key to ensure a node name is indented the same amount\nas the node type on the preceding line."))

  ## check indent for node names
  indent_name <- yl[nn_id] %>% gsub("^(\\s*).*","\\1", .) %>% nchar

   ## check indent of next line for node names
  indent_next <- yl[nn_id+1] %>% gsub("^(\\s*).*","\\1", .) %>% nchar
  indent_issue <- indent_name >= indent_next
  if (any(indent_issue))
    err <- c(err, paste0("Indent issue in line(s): ", paste0(nn_id[indent_issue] + 1, collapse = ", "), "\nAlways use the tab key to indent the line(s) after specifying a node name."))

  ## determine return value
  if (length(err) > 0) {
    paste0("\n**\n", paste0(err, collapse = "\n"), "\n**\n") %>% set_class(c("dtree", class(.)))
  } else {
    paste0(yl, collapse = "\n")
  }
}

## Test settings for simulater function, will not be run when sourced
if (getOption("radiant.testthat", default = FALSE)) {
  main__ <- function() {
    # options(radiant.testthat = TRUE)
    # library(radiant.model)
    # yl <- readLines("~/gh/radiant.model/tests/dtree/san-carlos-input-vars.yaml") %>% paste0(collapse = "\n")
    # yl <- readLines("~/gh/radiant.model/tests/dtree/jennylind-variables.yaml") %>% paste0(collapse = "\n")
    # yl <- dtree_parser(yl)
    # yl <- yaml::yaml.load(yl)
    # vars <- yl$variables
    # vn <- names(vars)
    # stopifnot(1 == 1)
  }
  main__()
}

#' Create a decision tree
#'
#' @details See \url{http://vnijs.github.io/radiant/base/dtree.html} for an example in Radiant
#'
#' @param yl A yaml string or a list (e.g., from yaml::yaml.load_file())
#' @param opt Find the maximum ("max") or minimum ("min") value for each decision node
#'
#' @return A list with the initial tree and the calculated tree
#'
#' @importFrom yaml yaml.load
#' @importFrom stringr str_match
#' @importFrom data.tree as.Node Clone isLeaf isNotLeaf
#'
#' @seealso \code{\link{summary.dtree}} to summarize results
#' @seealso \code{\link{plot.dtree}} to plot results
#'
#' @export
dtree <- function(yl, opt = "max") {

  ## Adapted from https://github.com/gluc/useR15/blob/master/01_showcase/02_decision_tree.R
  ## load yaml from string if list not provide
  if (is_string(yl)) {

    ## get input file from r_data
    if (!grepl("\\n", yl)) yl <- getdata(yl)

    yl <- dtree_parser(yl)
    ## cleanup the input file

    ####### test #######
    # return(paste0(paste0("\n**\n", yl, collapse = "\n"), "\n**\n") %>% set_class(c("dtree", class(.))))
    ####### test #######

    if ("dtree" %in% class(yl)) return(yl)

    ## if the name of input-list in r_data is provided
    yl <- try(yaml::yaml.load(yl), silent = TRUE)

    ## used when a string is provided
    if (is(yl, 'try-error')) {
      err_line <- stringr::str_match(attr(yl,"condition")$message, "^Scanner error:.*line\\s([0-9]*),")[2]
      if (is.na(err_line))
        err <- paste0("**\nError reading input:\n", attr(yl,"condition")$message, "\n\nPlease try again. Examples are shown in the help file\n**")
      else
        err <- paste0("**\nIndentation error in line ", err_line, ".\nUse tabs to separate the branches in the decision tree.\nFix the indentation error and try again. Examples are shown in the help file\n**")
      return(set_class(err, c("dtree",class(err))))
    }
  }

  if (length(yl) == 0) {
    err <- "**\nThe provided list is empty or not in the correct format.\nPlease check the input file.\n**"
    return(set_class(err, c("dtree",class(err))))
  }

  vars <- ""
  if (!is.null(yl$variables)) {
    vars <- yl$variables
    vn <- names(vars)
    for (i in 2:max(2,length(vn))) {
      vars <- gsub(vn[i-1], paste0("(",vars[[i-1]],")"), vars, fixed = TRUE)
      vars <- sapply(vars, function(x) ifelse(grepl("[A-Za-z]+",x), x, eval(parse(text = x))))
    }
    names(vars) <- vn

    isNum <- function(x) sshhr(!is.na(as.numeric(x)))
    isNot <- vars[!sapply(vars, isNum)]
    if (length(isNot) > 0)  {
      cat("Not all variables resolved to numeric.\n")
      print(as.data.frame(isNot) %>% set_names(""))
      return("\nUpdate the input file and try again." %>% set_class(., c("dtree",class(.))))
    }

    ## based on http://stackoverflow.com/a/14656351/1974918
    tmp <- as.relistable(yl[setdiff(names(yl),"variables")]) %>% unlist
    for (i in seq_along(vn)) tmp <- gsub(vn[i], vars[[i]], tmp, fixed = TRUE)

    ## any characters left in p, payoff, or cost fields?
    isNot <- grepl("(.p$)|(.payoff$)|(.cost$)", names(tmp))
    isNot <- tmp[isNot]
    isNot <- isNot[grepl("[^0-9.-]+", isNot)]
    if (length(isNot) > 0)  {
      names(isNot) <- gsub(".",":",names(isNot), fixed = TRUE)
      cat("Not all variables can be resolved to numeric. Note that\nformula's are only allowed in the 'variables' section\n")
      print(as.data.frame(isNot) %>% set_names(""))
      return("\nUpdate the input file and try again." %>% set_class(., c("dtree",class(.))))
    }

    ## convert payoff, probabilities, and costs to numeric
    yl <- relist(tmp)
    # toNum <- function(x) if (grepl("[A-Za-z]+", x)) x else as.numeric(eval(parse(text = x)))
    toNum <- function(x) if (grepl("[A-Za-z]+", x)) x else as.numeric(x)

    ## cycle through a nested list recursively
    ## based on http://stackoverflow.com/a/26163152/1974918
    nlapply <- function(x, fun){
      if(is.list(x)){
        lapply(x, nlapply, fun)
      } else {
        fun(x)
      }
    }
    yl <- nlapply(yl, toNum)
  }

  ## convert list to node object
  jl <- data.tree::as.Node(yl)

  ## if type not set and isLeaf set to terminal
  pt <- . %>% {if (is.null(.$type)) .$Set(type = "terminal")}
  jl$Do(pt, filterFun = data.tree::isLeaf)

  isNum <- function(x) !is_not(x) && !grepl("[A-Za-z]+", x)

  ## making a copy of the initial Node object
  jl_init <- data.tree::Clone(jl)

  chance_payoff <- function(node) {
    # if (is.null(node$payoff) || is.null(node$p)) {
    if (!isNum(node$payoff) || !isNum(node$p))  0
    else node$payoff * node$p
  }

  decision_payoff <- function(node)
    if(!isNum(node$payoff)) 0 else node$payoff

  type_none <- ""
  calc_payoff <- function(x) {
    if (is_empty(x$type)) {
      x$payoff <- 0
      x$type <- "NONE"
      type_none <<- "One or more nodes do not have a 'type'. Search for 'NONE' in the output\nbelow and then update the input file"
    } else if (x$type == 'chance') x$payoff <- sum(sapply(x$children, chance_payoff))
    else if (x$type == 'decision') x$payoff <- get(opt)(sapply(x$children, decision_payoff))

    ## subtract cost if specified
    if (isNum(x$cost)) x$payoff <- x$payoff - x$cost
  }

  # err <- try(jl$Do(calc_payoff, traversal = "post-order", filterFun = data.tree::isNotLeaf), silent = TRUE)
  err <- jl$Do(calc_payoff, traversal = "post-order", filterFun = data.tree::isNotLeaf)

  if (is(err, 'try-error')) {
    err <- paste0("**\nError calculating payoffs associated with a chance or decision node.\nPlease check that each terminal node has a payoff and that probabilities\nare correctly specificied\n**")
    return(err %>% set_class(c("dtree", class(.))))
  }

  decision <- function(x) {
    po <- sapply(x$children, decision_payoff)
    x$decision <- names(po[po == x$payoff])
  }

  err <- try(jl$Do(decision, filterFun = function(x) !is.null(x$type) && x$type == 'decision'), silent = TRUE)

  if (is(err, 'try-error')) {
    err <- paste0("**\nError calculating payoffs associated with a decision node. Please check\nthat each terminal node has a payoff\n**")
    return(err %>% set_class(c("dtree", class(.))))
  }

  list(jl_init = jl_init, jl = jl, vars = vars, type_none = type_none) %>% set_class(c("dtree",class(.)))
}


#' Summary method for the dtree function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/dtree.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{simulater}}
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom data.tree Traverse Get FormatPercent
#'
#' @seealso \code{\link{dtree}} to generate the results
#' @seealso \code{\link{plot.dtree}} to plot results
#'
#' @export
summary.dtree <- function(object, ...) {

  if (is.character(object)) return(cat(object))

  isNum <- function(x) !is_not(x) && !grepl("[A-Za-z]+", x)

  print_money <- function(x) {
    x %>% {if (isNum(.)) . else ""} %>%
      format(digits = 10, nsmall = 2, decimal.mark = ".", big.mark = ",", scientific = FALSE)
  }

  print_percent <- function(x) data.tree::FormatPercent(x)

  rm_terminal <- function(x)
    x %>% {if (is.na(.)) "" else .} %>% {if (. == "terminal") "" else .}

  format_dtree <- function(jl) {
    ## set parent type
    nt <- jl$Get(function(x) x$parent$type)
    jl$Set(ptype = nt)

    data.tree::Traverse(jl) %>%
      {data.frame(
        ` ` = data.tree::Get(.,"levelName"),
        # Probability = Get(., "p", format = FormatPercent),
        Probability = data.tree::Get(., "p", format = print_percent),
        Payoff = data.tree::Get(., "payoff", format = print_money),
        Cost = data.tree::Get(., "cost", format = print_money),
        Type = data.tree::Get(., "ptype", format = rm_terminal),
        check.names = FALSE
      )
    } %>% { .[[" "]] <- format(.[[" "]], justify = "left"); .}
  }

  if (all(object$vars != "")) {
    cat("Input values:\n")
    # print(as.data.frame(object$vars) %>% set_names("") %>% round(4))
    print(as.data.frame(object$vars) %>% set_names(""))
    cat("\n\n")
  }

  ## initial setup
  if (object$type_none != "") {
    cat(paste0("\n\n**\n",object$type_none,"\n**\n\n"))
  } else {
    cat("Initial decision tree:\n")
    format_dtree(object$jl_init) %>% print(row.names = FALSE)
  }

  cat("\n\nFinal decision tree:\n")
  format_dtree(object$jl) %>% print(row.names = FALSE)
}

#' Plot method for the dtree function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/dtree.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{dtree}}
#' @param symbol Monetary symbol to use ($ is the default)
#' @param dec Decimal places to round results to
#' @param final If TRUE plot the decision tree solution, else the initial decision tree
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom data.tree Traverse Get isNotRoot
#'
#' @seealso \code{\link{dtree}} to generate the result
#' @seealso \code{\link{summary.dtree}} to summarize results
#'
#' @export
plot.dtree <- function(x, symbol = "$", dec = 2, final = FALSE, shiny = FALSE, ...) {

  isNum <- function(x) !is_not(x) && !grepl("[A-Za-z]+", x)

  if (is.character(x)) return(paste0("graph LR\n A[Errors in the input file]\n"))
  if (x$type_none != "") return(paste0("graph LR\n A[Node does not have a type. Fix the input file]\n"))

  ## based on https://gist.github.com/gluc/79ef7a0e747f217ca45e
  jl <- if (final) x$jl else x$jl_init

  ## create ids
  jl$Set(id = paste0("id", 1:jl$totalCount))

  ## create start labels
  FromLabel <- function(node) {
    if (node$parent$isRoot) ToLabel(node$parent)
    else as.character(node$parent$id)
  }

  ## create arrow labels
  EdgeLabel <- function(node) {
    if (node$isRoot) {
      return (" ")
    } else if (node$parent$type == "decision") {
      lbl <- node$name
    } else if (node$parent$type == "chance") {
      lbl <- paste0(node$name,": ", nrprint(as.numeric(node$p), dec = dec+2))
    } else if (node$type == "terminal") {
      lbl <- paste0(node$name,": ", nrprint(as.numeric(node$p), dec = dec+2))
    }

    if (length(node$parent$decision) > 0 && length(node$name) > 0 && node$name == node$parent$decision)
      paste0(" === |", lbl, "|")
    else
      paste0(" --- |", lbl, "|")
  }

  FormatPayoff <- function(payoff) {
    if (!isNum(payoff)) payoff <- 0
    nrprint(payoff, paste0("\"",symbol,"\""), dec = dec)
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

  style_decision <- jl$Get("id", filterFun = function(x) x$type == "decision")
  if (is.null(style_decision)) style_decision <- "id_null"
  style_chance <- jl$Get("id", filterFun = function(x) x$type == "chance" && is.null(x$cost))
  if (is.null(style_chance)) style_chance <- "id_null"
  style_chance_with_cost <- jl$Get("id", filterFun = function(x) x$type == "chance" && !is.null(x$cost))
  if (is.null(style_chance_with_cost)) style_chance_with_cost <- "id_null"

  ToolTip <- function(node) {
    if (final == TRUE && !is.null(node$cost)) {
      paste0(nrprint(node$payoff + node$cost, symbol, dec = dec), " - ", nrprint(node$cost, symbol, dec = dec)) %>%
        paste0("click ", node$id, " callback \"", ., "\"")
    } else if (!is.null(node$cost)) {
      paste0("Cost: ", nrprint(node$cost, symbol, dec = dec)) %>%
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
    class ", paste(style_decision, collapse = ","), " decision;
    class ", paste(style_chance, collapse = ","), " chance;
    class ", paste(style_chance_with_cost, collapse = ","), " chance_with_cost;")

  trv <- data.tree::Traverse(jl, traversal = "level", filterFun = data.tree::isNotRoot)
  df <- data.frame(from = data.tree::Get(trv, FromLabel),
                   edge = data.tree::Get(trv, EdgeLabel),
                   to = data.tree::Get(trv, ToLabel),
                   id = data.tree::Get(trv, ToLabel),
                   tooltip = data.tree::Get(trv, ToolTip))

  paste("graph LR", paste( paste0(df$from, df$edge, df$to), collapse = "\n"),
    paste(unique(na.omit(df$tooltip)), collapse = "\n"),
    style, sep = "\n") %>%
  {if (shiny) . else DiagrammeR::DiagrammeR(.)}
}
