# to avoid 'no visible binding for global variable' NOTE
globalVariables(c(".","n",".resid","null.deviance"))

#' radiant.model
#'
#' @name radiant.model
#' @docType package
#' @import radiant.data shiny ggplot2
#' @importFrom broom tidy glance
#' @importFrom gridExtra arrangeGrob
#' @importFrom dplyr arrange select select_ filter mutate mutate_ funs group_by group_by_ summarise_ summarise_each summarise_each_ slice as_data_frame bind_cols bind_rows desc first last min_rank
#' @importFrom magrittr %>% %<>% %T>% set_colnames set_rownames
#' @importFrom tidyr spread_
#' @importFrom stats anova as.formula binomial coef confint confint.default cor deviance dnorm glm lm na.omit pnorm predict qnorm sd setNames step update weighted.mean wilcox.test
#' @importFrom utils head
#' @importFrom methods is
#' @importFrom import from
NULL

#' Campus market
#' @details Description provided in attr(campus_market,"description")
#' @docType data
#' @keywords datasets
#' @name campus_market
#' @usage data(campus_market)
#' @format A data frame with ...
NULL

#' Catalog
#' @details Description provided in attr(catalog,"description")
#' @docType data
#' @keywords datasets
#' @name catalog
#' @usage data(catalog)
#' @format A data frame with ...
NULL

#' Direct marketing
#' @details Description provided in attr(direct_marketing,"description")
#' @docType data
#' @keywords datasets
#' @name direct_marketing
#' @usage data(direct_marketing)
#' @format A data frame with ...
NULL

#' Heights
#' @details Description provided in attr(heights,"description")
#' @docType data
#' @keywords datasets
#' @name heights
#' @usage data(heights)
#' @format A data frame with ...
NULL

#' Houseprices
#' @details Description provided in attr(houseprices,"description")
#' @docType data
#' @keywords datasets
#' @name houseprices
#' @usage data(houseprices)
#' @format A data frame with ...
NULL

#' Ideal data for linear regression
#' @details Description provided in attr(ideal,"description")
#' @docType data
#' @keywords datasets
#' @name ideal
#' @usage data(ideal)
#' @format A data frame with ...
NULL
