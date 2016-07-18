# to avoid 'no visible binding for global variable' NOTE
globalVariables(c(".","n",".resid","null.deviance","r_environment",".fitted","Predictor",
                  "total","TN","FN","FP","TP","total"))

#' radiant.model
#'
#' @name radiant.model
#' @docType package
#' @import radiant.data shiny ggplot2
#' @importFrom gridExtra arrangeGrob
#' @importFrom dplyr arrange select select_ filter mutate mutate_ mutate_each_ funs group_by group_by_ summarise summarise_ summarise_each summarise_each_ slice as_data_frame bind_cols bind_rows desc first last min_rank data_frame
#' @importFrom magrittr %>% %<>% %T>% set_colnames set_rownames set_names extract2
#' @importFrom tidyr spread_ gather_
#' @importFrom lubridate now
#' @importFrom pryr where
#' @importFrom DiagrammeR DiagrammeROutput renderDiagrammeR DiagrammeR mermaid
#' @importFrom stats anova as.formula binomial coef confint confint.default cor deviance dnorm glm lm na.omit pnorm predict qnorm sd setNames step update weighted.mean wilcox.test rbinom rlnorm rnorm runif terms
#' @importFrom utils head tail relist as.relistable
#' @importFrom methods is
#' @importFrom import from
NULL

#' Catalog sales for men's and women's apparel
#' @details Description provided in attr(catalog,"description")
#' @docType data
#' @keywords datasets
#' @name catalog
#' @usage data(catalog)
#' @format A data frame with 200 rows and 5 variables
NULL

#' Direct marketing data
#' @details Description provided in attr(direct_marketing,"description")
#' @docType data
#' @keywords datasets
#' @name direct_marketing
#' @usage data(direct_marketing)
#' @format A data frame with 1,000 rows and 12 variables
NULL

#' Houseprices
#' @details Description provided in attr(houseprices,"description")
#' @docType data
#' @keywords datasets
#' @name houseprices
#' @usage data(houseprices)
#' @format A data frame with 128 home sales and 6 variables
NULL

#' Ideal data for linear regression
#' @details Description provided in attr(ideal,"description")
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
