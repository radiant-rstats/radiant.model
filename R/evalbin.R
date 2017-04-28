#' Model evalbin
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param pred Predictions or predictors
#' @param rvar Response variable
#' @param lev The level in the response variable defined as _success_
#' @param qnt Number of bins to create
#' @param cost Cost for each connection (e.g., email or mailing)
#' @param margin Margin on each customer purchase
#' @param train Use data from training ("Training"), validation ("Validation"), both ("Both"), or all data ("All") to evaluate model evalbin
#' @param method Use either ntile or xtile to split the data (default is xtile)
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of results
#'
#' @seealso \code{\link{summary.evalbin}} to summarize results
#' @seealso \code{\link{plot.evalbin}} to plot results
#'
#' @examples
#' result <- evalbin("titanic", c("age","fare"), "survived")
#'
#' @export
evalbin <- function(dataset, pred, rvar,
                    lev = "",
                    qnt = 10,
                    cost = 1,
                    margin = 2,
                    train = "",
                    method = "xtile",
                    data_filter = "") {

	## in case no inputs were provided
	if (is.na(cost)) cost <- 0
	if (is.na(margin)) margin <- 0

  if (!train %in% c("","All") && is_empty(data_filter))
    return("** Filter required. To set a filter go to Data > View and click\n   the filter checkbox **" %>% add_class("evalbin"))

	## to avoid 'global not defined' warnings
	nr_resp <- nr_obs <- cum_resp <- cum_resp_rate <- everything <- NULL
	profit <- ROME <- NULL

	if (is_empty(qnt)) qnt <- 10

	dat_list <- list()
	vars <- c(pred, rvar)
	if (train == "Both") {
		dat_list[["Training"]] <- getdata(dataset, vars, filt = data_filter)
		dat_list[["Validation"]] <- getdata(dataset, vars, filt = paste0("!(",data_filter,")"))
	} else if (train == "Training") {
		dat_list[["Training"]] <- getdata(dataset, vars, filt = data_filter)
	} else if (train == "Validation") {
		dat_list[["Validation"]] <- getdata(dataset, vars, filt = paste0("!(",data_filter,")"))
	} else {
		dat_list[["All"]] <- getdata(dataset, vars, filt = "")
	}

	if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

  qnt_name <- "bins"
  if (method == "xtile") method <- "radiant.data::xtile"

  auc_list <- list()
  prof_list <- c()
  pdat <- list()
	pext <- c(All = "", Training = " (train)", Validation = " (val)")

	for (i in names(dat_list)) {
    lg_list <- list()
    pl <- c()
		dat <- dat_list[[i]]
	  rv <- dat[[rvar]]
	  if (is.factor(rv)) {
	    levs <- levels(rv)
	  } else {
	    levs <- rv %>% as.character %>% as.factor %>% levels
	  }

	  if (lev == "") {
	  	lev <- levs[1]
	  } else {
	  	if (!lev %in% levs) return(add_class("Level provided not found", "evalbin"))
	  }

	  ## transformation to TRUE/FALSE depending on the selected level (lev)
	  dat[[rvar]] <- dat[[rvar]] == lev

	  ## tip for summarise_ from http://stackoverflow.com/a/27592077/1974918
	  ## put summaries in list so you can print and plot
		tot_resp = sum(dat[[rvar]])
		tot_obs = nrow(dat)
		tot_rate = tot_resp / tot_obs

	  for (j in seq_along(pred)) {
	  	pname <- paste0(pred[j], pext[i])
	  	auc_list[[pname]] <- auc(dat[[pred[j]]],dat[[rvar]], TRUE)
	  	lg_list[[pname]] <-
			  dat %>%
			  select_(.dots = c(pred[j],rvar)) %>%
				mutate_(.dots = setNames(paste0(method,"(",pred[j],",", qnt,", rev = TRUE)"), pred[j])) %>%
				setNames(c(qnt_name,rvar)) %>%
		    group_by_(.dots = qnt_name) %>%
			  summarise_(.dots = c(
			    nr_obs = "n()",
			    nr_resp = paste0("sum(",rvar,")")
			  )) %>%
			  mutate(
			    resp_rate = nr_resp / nr_obs,
			    gains = nr_resp / tot_resp
			  ) %>%
			  { if (first(.$resp_rate) < last(.$resp_rate)) mutate_all(., funs(rev))
			  	else . } %>%
			  mutate(
			    profit = margin * cumsum(nr_resp) - cost * cumsum(nr_obs),
			    ROME = profit / (cost * cumsum(nr_obs)),
			    cum_prop = cumsum(nr_obs / tot_obs),
			    cum_resp = cumsum(nr_resp),
			    cum_resp_rate = cum_resp / cumsum(nr_obs),
			    cum_lift = cum_resp_rate / tot_rate,
			    cum_gains = cum_resp / tot_resp
			  ) %>%
			  mutate(pred = pname) %>%
				mutate(ROME = ifelse (is.na(ROME), 0, ROME)) %>%
			  select(pred, everything())

	  	  pl <- c(pl, max(lg_list[[pname]]$profit))
		}
		prof_list <- c(prof_list, pl / abs(max(pl)))
		pdat[[i]] <- bind_rows(lg_list) %>% mutate(profit = profit)
	}
	dat <- bind_rows(pdat) %>% mutate(profit = ifelse (is.na(profit), 0, profit))
	dat$pred <- factor(dat$pred, levels = unique(dat$pred))

	names(prof_list) <- names(auc_list)
	rm(lg_list, pdat)

	as.list(environment()) %>% add_class("evalbin")
}

#' Summary method for the evalbin function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{evalbin}}
#' @param prn Print full table of measures per model and bin
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{evalbin}} to summarize results
#' @seealso \code{\link{plot.evalbin}} to plot results
#'
#' @examples
#' evalbin("titanic", "age", "survived") %>% summary
#' evalbin("titanic", c("age","fare"), "survived") %>% summary
#'
#' @export
summary.evalbin <- function(object, prn = TRUE, ...) {

  if (is.character(object)) return(object)

	cat("Evaluate predictions for binary response models\n")
	cat("Data        :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Results for :", object$train, "\n")
	cat("Predictors  :", paste0(object$pred, collapse=", "), "\n")
	cat("Response    :", object$rvar, "\n")
  cat("Level       :", object$lev, "in", object$rvar, "\n")
	cat("Bins        :", object$qnt, "\n")
	cat("Cost:Margin :", object$cost, ":", object$margin, "\n")
	# prof <- object$prof_list
	# cat("Profit index:", paste0(names(prof), " (", round(prof,3), ")", collapse=", "), "\n")
	# auc <- unlist(object$auc_list)
	# cat("AUC         :", paste0(names(auc), " (", round(auc,3), ")", collapse=", "), "\n\n")

	if (prn)
		print(formatdf(as.data.frame(object$dat), 3), row.names = FALSE)
}

#' Confusion matrix
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param pred Predictions or predictors
#' @param rvar Response variable
#' @param lev The level in the response variable defined as _success_
#' @param cost Cost for each connection (e.g., email or mailing)
#' @param margin Margin on each customer purchase
#' @param train Use data from training ("Training"), validation ("Validation"), both ("Both"), or all data ("All") to evaluate model evalbin
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param ... further arguments passed to or from other methods
#'
#' @return A list of results
#'
#' @seealso \code{\link{summary.confusion}} to summarize results
#' @seealso \code{\link{plot.confusion}} to plot results
#'
#' @importFrom psych cohen.kappa
#'
#' @export
confusion <- function(dataset, pred, rvar,
                      lev = "",
                      cost = 1,
                      margin = 2,
                      train = "",
                      data_filter = "",
                      ...) {


  if (!train %in% c("","All") && is_empty(data_filter))
    return("** Filter required. To set a filter go to Data > View and click the filter checkbox **" %>% add_class("confusion"))

	profit <- NULL

	## in case no inputs were provided
	if (is_not(margin) || is_not(cost)) {
		break_even <- 0.5
	} else if (margin == 0) {
	  break_even <- cost / 1
	} else {
	  break_even <- cost / margin
	}

	dat_list <- list()
	vars <- c(pred, rvar)
	if (train == "Both") {
		dat_list[["Training"]] <- getdata(dataset, vars, filt = data_filter)
		dat_list[["Validation"]] <- getdata(dataset, vars, filt = paste0("!(",data_filter,")"))
	} else if (train == "Training") {
		dat_list[["Training"]] <- getdata(dataset, vars, filt = data_filter)
	} else if (train == "Validation") {
		dat_list[["Validation"]] <- getdata(dataset, vars, filt = paste0("!(",data_filter,")"))
	} else {
		dat_list[["All"]] <- getdata(dataset, vars, filt = "")
	}

	if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

	pdat <- list()
	for (i in names(dat_list)) {
		dat <- dat_list[[i]]
	  rv <- dat[[rvar]]

	  if (lev == "") {
	    if (is.factor(rv))
	      lev <- levels(rv)[1]
	    else
	      lev <- as.character(rv) %>% as.factor %>% levels %>% .[1]
	  } else {
	  	if (!lev %in% dat[[rvar]]) return(add_class("Please update the selected level in the response variable", "confusion"))
	  }

	  ## transformation to TRUE/FALSE depending on the selected level (lev)
	  dat[[rvar]] <- dat[[rvar]] == lev

  	auc_vec <- rep(NA, length(pred)) %>% set_names(pred)
  	for (p in pred) auc_vec[p] <- auc(dat[[p]], dat[[rvar]], TRUE)

  	p_vec <- colMeans(dat[,pred, drop = FALSE]) / mean(dat[[rvar]])

  	dat[, pred] <- select_(dat, .dots = pred) > break_even

  	if (length(pred) > 1) {
   	  dat <- mutate_at(dat, .cols = c(rvar, pred), .funs = funs(factor(., levels = c("FALSE","TRUE"))))
 		} else {
  	  dat[,pred] %<>% apply(2, function(x) factor(x, levels = c("FALSE","TRUE")))
 		}

	  make_tab <- function(x) {
	  	ret <- rep(0L, 4) %>% set_names(c("TN","FN","FP","TP"))
	  	tab <- table(dat[[rvar]], x) %>% as.data.frame
	  	## ensure a value is availble for all four options
	  	for (i in 1:nrow(tab)) {
	  		if (tab[i,1] == "TRUE") {
	  			if (tab[i,2] == "TRUE") 
	  				ret["TP"] <- tab[i,3]
	  			else 
	  				ret["FN"] <- tab[i,3]

	  		} else {
	  			if (tab[i,2] == "TRUE") 
	  				ret["FP"] <- tab[i,3]
	  			else 
	  				ret["TN"] <- tab[i,3]
	  		}
	  	}
	  	return(ret)
	  }
	  ret <- lapply(select_(dat,.dots = pred), make_tab) %>% as.data.frame %>% t %>% as.data.frame
	  ret <- bind_cols(data.frame(Type = rep(i, length(pred)), Predictor = pred), ret,
	                   data.frame(AUC = auc_vec, p.ratio = p_vec))

	  pdat[[i]] <- ret
  }

	dat <- bind_rows(pdat) %>% as.data.frame %>%
	  mutate(total = TN+FN+FP+TP, TPR = TP/(TP+FN), TNR = TN/(TN+FP),
	         precision = TP / (TP + FP),
	         accuracy = (TP + TN) / total,
	         profit = margin * TP - cost * (TP + FP),
	         ROME = profit / (cost * (TP + FP)),
	         contact = (TP + FP) / total,
	         kappa = 0)

	dat <- group_by_(dat, "Type") %>% mutate(index = profit / max(profit)) %>% ungroup
	dat <- mutate(dat, profit = as.integer(round(profit,0)))

	for (i in 1:nrow(dat)) {
		tmp <- dat[i,]
  	dat$kappa[i] <- psych::cohen.kappa(matrix(with(tmp, c(TN,FP,FN,TP)), ncol = 2))[["kappa"]]
	}

	dat <- select_(dat, .dots = c("Type","Predictor", "TP", "FP", "TN", "FN",
	                              "total", "TPR", "TNR", "precision", "accuracy",
	                              "kappa", "profit", "index", "ROME", "contact", "AUC"))

	rm(pdat, dat_list)

	as.list(environment()) %>% add_class("confusion")
}

#' Summary method for the confusion matrix
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{confusion}}
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{confusion}} to generate results
#' @seealso \code{\link{plot.confusion}} to visualize result
#'
#' @export
summary.confusion <- function(object, ...) {

  if (is.character(object)) return(object)

	cat("Confusion matrix\n")
	cat("Data       :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter     :", gsub("\\n","", object$data_filter), "\n")
	cat("Results for:", object$train, "\n")
	cat("Predictors :", paste0(object$pred, collapse=", "), "\n")
	cat("Response   :", object$rvar, "\n")
  cat("Level      :", object$lev, "in", object$rvar, "\n")
	cat("Cost:Margin:", object$cost, ":", object$margin, "\n")
	cat("\n")

	print(formatdf(as.data.frame(object$dat[,1:10]), 3), row.names = FALSE)
	cat("\n")
	print(formatdf(as.data.frame(object$dat[,c(1,2, 11:17)]), 3, mark = ","), row.names = FALSE)
}

#' Plot method for the confusion matrix
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{confusion}}
#' @param vars Measures to plot, i.e., one or more of "TP", "FP", "TN", "FN", "total", "TPR", "TNR", "precision", "accuracy", "kappa", "profit", "index", "ROME", "contact", "AUC"
#' @param scale_y Free scale in faceted plot of the confusion matrix (TRUE or FALSE)
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{confusion}} to generate results
#' @seealso \code{\link{summary.confusion}} to summarize results
#'
#' @export
plot.confusion <- function(x, vars = c("kappa", "index", "ROME", "AUC"),
                           scale_y = TRUE, ...) {

	object <- x; rm(x)
 	if (is.character(object) || is.null(object)) return(invisible())

	dat <- object$dat %>%
	  mutate_at(.cols = c("TN","FN","FP","TP"), .funs = funs(if (is.numeric(.)) . / total else .))

	gather_(dat, "Metric", "Value", vars, factor_key = TRUE) %>%
		mutate(Predictor = factor(Predictor, levels = unique(Predictor))) %>%
		{if (scale_y) {
	    visualize(., xvar = "Predictor", yvar = "Value", type = "bar",
		    facet_row = "Metric", fill = "Type", axes = "scale_y", custom = TRUE) +
			  labs(y = "", x = "Predictor")
			} else {
		    visualize(., xvar = "Predictor", yvar = "Value", type = "bar",
			    facet_row = "Metric", fill = "Type", custom = TRUE) +
			  	labs(y = "", x = "Predictor")
			}
	  }
}

#' Plot method for the evalbin function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{evalbin}}
#' @param plots Plots to return
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{evalbin}} to generate results
#' @seealso \code{\link{summary.evalbin}} to summarize results
#'
#' @examples
#' evalbin("titanic", "age", "survived") %>% plot
#' evalbin("titanic", c("age","fare"), "survived") %>% plot
#' evalbin("titanic", c("age","fare"), "survived", method = "xtile") %>% plot
#' evalbin("titanic", c("age","fare"), "survived") %>% summary
#'
#' @export
plot.evalbin <- function(x, plots = c("lift","gains"), shiny = FALSE, custom = FALSE, ...) {

	## to avoid 'global not defined' warnings
	pred <- cum_prop <- cum_gains <- obs <- profit <- NULL

	object <- x; rm(x)
  if (is.character(object) || is.null(object$dat) || any(is.na(object$dat$cum_lift)) ||
      is.null(plots)) return(invisible())

	plot_list <- list()
	if ("lift" %in% plots) {
		plot_list[["lift"]] <-
			visualize(object$dat, xvar = "cum_prop", yvar = "cum_lift", type = "line", color = "pred", custom = TRUE) +
			geom_point() +
			geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), size = .1, color = "black") +
			labs(y = "Cumulative lift", x = "Proportion of customers")
	}

	if ("gains" %in% plots) {
		dat <-
	    object$dat %>%
		  select(pred, cum_prop, cum_gains) %>%
		  group_by(pred) %>%
		  mutate(obs = 1:n())

		init <- dat %>% filter(obs == 1)
		init[,c("cum_prop", "cum_gains", "obs")] <- 0 
		dat <- bind_rows(init, dat) %>% arrange(pred, obs)

		plot_list[["gains"]] <-
		  visualize(dat, xvar = "cum_prop", yvar = "cum_gains", type = "line", color = "pred", custom = TRUE) +
			geom_point() +
			geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size = .1, color = "black") +
			labs(y = "Cumulative gains", x = "Proportion of customers")
	}

	if ("profit" %in% plots) {
		dat <-
	    object$dat %>%
		  select(pred, cum_prop, profit) %>%
		  group_by(pred) %>%
		  mutate(obs = 1:n())

		init <- dat %>% filter(obs == 1)
		init[,c("profit", "cum_prop", "obs")] <- 0 
		dat <- bind_rows(init, dat) %>% arrange(pred, obs)

		plot_list[["profit"]] <-
			visualize(dat, xvar = "cum_prop", yvar = "profit", type = "line", color = "pred", custom = TRUE) +
			geom_point() +
			geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), size = .1, color = "black") +
			labs(y = "Profit", x = "Proportion of customers")
	}

	if ("rome" %in% plots) {
		plot_list[["rome"]] <-
			visualize(object$dat, xvar = "cum_prop", yvar = "ROME", type = "line", color = "pred", custom = TRUE) +
			geom_point() +
			geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), size = .1, color = "black") +
			labs(y = "Return on Marketing Expenditures (ROME)", x = "Proportion of customers")
	}

	for (i in names(plot_list)) {
		if (length(object$pred) < 2 && object$train != "Both")
			plot_list[[i]] <- plot_list[[i]] + theme(legend.position = "none")
		else
			plot_list[[i]] <- plot_list[[i]] + labs(colour = "Predictor")
	}

  if (custom)
    if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

	sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>%
	 	{if (shiny) . else print(.)}
}

#' Area Under the Curve (AUC)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
#'
#' @param pred Prediction or predictor
#' @param rvar Response variable
#' @param lev The level in the response variable defined as _success_
#'
#' @return AUC statistic
#'
#' @seealso \code{\link{evalbin}} to calculate results
#' @seealso \code{\link{summary.evalbin}} to summarize results
#' @seealso \code{\link{plot.evalbin}} to plot results
#'
#' @examples
#' auc(runif(nrow(mtcars)), mtcars$vs, 1)
#'
#' @export
auc <- function(pred, rvar, lev) {
 	## based on examples in colAUC at ...
 	## https://cran.r-project.org/web/packages/caTools/caTools.pdf
 	if (missing(lev) && is.factor(rvar)) lev <- levels(rvar)[1]
 	stopifnot(length(lev) == 1, lev %in% rvar)
 	x1 <- pred[rvar == lev]
  x2 <- pred[rvar != lev]
  ## need as.numeric to avoid integer-overflows
 	denom <- as.numeric(length(x1)) * length(x2)
	wt <- wilcox.test(x1, x2, exact = FALSE)$statistic / denom
	ifelse (wt < .5, 1 - wt, wt)[["W"]]
}
