#' Model evalreg
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/evalreg.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param pred Predictions or predictors
#' @param rvar Response variable
#' @param train Use data from training ("Training"), validation ("Validation"), both ("Both"), or all data ("All") to evaluate model evalreg
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of results
#'
#' @seealso \code{\link{summary.evalreg}} to summarize results
#' @seealso \code{\link{plot.evalreg}} to plot results
#'
#' @export
evalreg <- function(dataset, pred, rvar,
                    train = "",
                    data_filter = "") {

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

	if (!is_string(dataset)) dataset <- "-----"

	pdat <- list()
	for (i in names(dat_list)) {
		dat <- dat_list[[i]]
	  rv <- dat[[rvar]]

	  ## see http://stackoverflow.com/a/35617817/1974918 about extracting a row
	  ## from a tbl_df
	  pdat[[i]] <-
		  data.frame(
		    Type = rep(i, length(pred)),
		    Predictor = pred,
		    Rsq = cor(rv, dat[pred])^2 %>% .[1,],
		    RSME = summarise_each_(dat, funs(mean((rv - .)^2, na.rm = TRUE) %>% sqrt), vars = pred) %>% unlist,
		    MAE = summarise_each_(dat, funs(mean(abs(rv - .), na.rm = TRUE)), vars = pred) %>% unlist
	    )
  }

	dat <- bind_rows(pdat) %>% as.data.frame
	rm(pdat, dat_list)

	as.list(environment()) %>% add_class("evalreg")
}

#' Summary method for the evalreg function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/evalreg.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{evalreg}}
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{evalreg}} to summarize results
#' @seealso \code{\link{plot.evalreg}} to plot results
#'
#' @export
summary.evalreg <- function(object, ...) {

  if (is.character(object)) return(object)
	cat("Evaluate predictions for regression models\n")
	cat("Data        :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Results for :", object$train, "\n")
	cat("Perdictors  :", paste0(object$pred, collapse=", "), "\n")
	cat("Response    :", object$rvar, "\n\n")
	print(formatdf(object$dat), row.names = FALSE)
}

#' Plot method for the evalreg function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/evalreg.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{evalreg}}
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{evalreg}} to generate results
#' @seealso \code{\link{summary.evalreg}} to summarize results
#'
#' @export
plot.evalreg <- function(x, shiny = FALSE, ...) {

	object <- x; rm(x)
  if (is.character(object) || is.null(object)) return(invisible())

	gather_(object$dat, "Metric", "Value", c("Rsq","RSME","MAE"), factor_key = TRUE) %>%
		mutate(Predictor = factor(Predictor, levels = unique(Predictor))) %>%
		visualize(xvar = "Predictor", yvar = "Value", type = "bar",
		          facet_row = "Metric", fill = "Type", axes = "scale_y", custom = TRUE) +
		ylab("") + xlab("Predictor")
}
