# to avoid 'no visible binding for global variable' NOTE
globalVariables(c(
  ".", "High", "Low",
  ".cooksd", ".fitted", ".resid", ".std.resid", "FN", "FP", "Feature", "Importance", "Predictor",
  "ROME", "TN", "TP", "TPR", "Variable", "cum_gains", "cum_prop", "cum_resp", "cum_resp_rate",
  "index", "index.max", "label", "logit", "n", "nr_obs", "nr_resp", "null.deviance", "obs",
  "precision", "pred", "predictor.value", "total", "variable", "llfull", "llnull", "rnk", "Prediction",
  "C_resp", "C_n", "T_resp", "T_n", "bins", "inc_uplift", "incremental_resp", "cum_profit",
  "incremental_profit", "max_profit"
))

#' radiant.model
#'
#' @name radiant.model
#' @import radiant.data shiny ggplot2
#' @importFrom dplyr mutate_at mutate_if mutate_all summarise_at summarise_all arrange arrange_at select select_at filter mutate mutate_ funs group_by group_by_ summarise summarize summarise_ slice bind_cols bind_rows desc first last min_rank data_frame inner_join arrange_at group_by_at ungroup rename across everything pull
#' @importFrom rlang .data parse_exprs :=
#' @importFrom magrittr %>% %<>% %T>% set_colnames set_rownames set_names extract2
#' @importFrom tidyr spread gather
#' @importFrom lubridate now
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom DiagrammeR DiagrammeROutput renderDiagrammeR DiagrammeR mermaid
#' @importFrom utils head tail relist as.relistable combn capture.output write.table
#' @importFrom stats anova as.formula binomial coef confint cor deviance dnorm glm lm na.omit pnorm predict qnorm sd setNames step update weighted.mean wilcox.test rbinom rlnorm rnorm runif rpois terms quantile
#' @importFrom stats residuals formula model.matrix pt qt confint.default family median logLik relevel terms.formula
#' @importFrom import from
NULL

#' Catalog sales for men's and women's apparel
#' @details Description provided in attr(catalog, "description")
#' @docType data
#' @keywords datasets
#' @name catalog
#' @usage data(catalog)
#' @format A data frame with 200 rows and 5 variables
NULL

#' Direct marketing data
#' @details Description provided in attr(direct_marketing, "description")
#' @docType data
#' @keywords datasets
#' @name direct_marketing
#' @usage data(direct_marketing)
#' @format A data frame with 1,000 rows and 12 variables
NULL

#' Houseprices
#' @details Description provided in attr(houseprices, "description")
#' @docType data
#' @keywords datasets
#' @name houseprices
#' @usage data(houseprices)
#' @format A data frame with 128 home sales and 6 variables
NULL

#' Ideal data for linear regression
#' @details Description provided in attr(ideal,  "description")
#' @docType data
#' @keywords datasets
#' @name ideal
#' @usage data(ideal)
#' @format A data frame with 1,000 rows and 4 variables
NULL

#' Data on DVD sales
#' @details Binary purchase response to coupon value. Description provided in attr(dvd,"description")
#' @docType data
#' @keywords datasets
#' @name dvd
#' @usage data(dvd)
#' @format A data frame with 20,000 rows and 4 variables
NULL

#' Data on ketchup choices
#' @details Choice behavior for a sample of 300 individuals in a panel of households in Springfield, Missouri (USA). Description provided in attr(ketchup,"description")
#' @docType data
#' @keywords datasets
#' @name ketchup
#' @usage data(ketchup)
#' @format A data frame with 2,798 rows and 14 variables
NULL

#' Movie ratings
#' @details Use collaborative filtering to create recommendations based on ratings from existing users. Description provided in attr(ratings, "description")
#' @docType data
#' @keywords datasets
#' @name ratings
#' @usage data(ratings)
#' @format A data frame with 110 rows and 4 variables
NULL

#' Movie contract decision tree
#' @details Use decision analysis to create a decision tree for an actor facing a contract decision
#' @docType data
#' @keywords datasets
#' @name movie_contract
#' @usage data(movie_contract)
#' @format A nested list for decision and chance nodes, probabilities and payoffs
NULL

#' Kaggle uplift
#' @details Use uplift modeling to quantify the effectiveness of an experimental treatment
#' @docType data
#' @keywords datasets
#' @name kaggle_uplift
#' @usage data(kaggle_uplift)
#' @format A data frame with 1,000 rows and 22 variables
NULL
