library(glue)
library(radiant.model)
path <- "/Users/vnijs/Dropbox/teaching/CustomerAnalytics/MGT455-2023/week6/creative-gaming"
cg_organic <- readr::read_rds(glue("{path}/data/cg_organic.rds"))
sample_train_org <- readr::read_rds(glue("{path}/data/sample_train_org.rds"))
cg_organic$training <- 0
# cg_organic[sample_train_org, "training"] <- 0
cg_organic$training[sample_train_org] <- 1

result <- logistic(
  cg_organic,
  rvar = "converted",
  evar = c(
    "GameLevel", "NumGameDays",
    "NumGameDays4Plus", "NumInGameMessagesSent",
    "NumSpaceHeroBadges",
    "NumFriendRequestIgnored",
    "NumFriends", "AcquiredSpaceship",
    "AcquiredIonWeapon",
    "TimesLostSpaceship",
    "TimesKilled", "TimesCaptain",
    "TimesNavigator", "PurchasedCoinPackSmall",
    "PurchasedCoinPackLarge",
    "NumAdsClicked", "DaysUser",
    "UserNoConsole", "UserHasOldOS"
  ),
  lev = "1",
  data_filter = "training == 1"
)

summary(result)

plot(result, plots = "vip", custom = FALSE)
vi_scores <- varimp(result)
vi_scores

head(result$model$model)

varimp(result, "converted", lev = 1, data = result$model$model)
varimp(result, data = result$model$model)

varimp(result, "converted", lev = 1, data = filter(cg_organic, training == 0))
varimp(result, data = filter(cg_organic, training == 0))

varimp_plot(result)
varimp_plot(result, data = filter(cg_organic, training == 0))

object <- result
data <- filter(cg_organic, training == 1)
target <- "converted"
lev <- 1


varimp <- function(object, target, lev, data = NULL, seed = 1234) {
  if (is.null(data)) data <- object$model$model

  # needed to avoid rescaling during prediction
  object$check <- setdiff(object$check, c("center", "standardize"))

  arg_list <- list(object, pred_data = data, se = FALSE)
  if (missing(target)) target <- object$rvar
  if (missing(lev) && !is.empty(object$lev) && !is.logical(data[[target]])) {
    data[[target]] <- data[[target]] == object$lev
  } else {
    data[[target]] <- data[[target]] == lev
  }

  head(data$converted)

  fun <- function(object, arg_list) do.call(predict, arg_list)[["Prediction"]]
  if (inherits(object, "rforest")) {
    arg_list$OOB <- FALSE # all 0 importance scores when using OOB
    if (object$type == "classification") {
      fun <- function(object, arg_list) do.call(predict, arg_list)[[object$lev]]
    }
  }

  pred_fun <- function(object, newdata) {
    arg_list$pred_data <- newdata
    fun(object, arg_list)
  }

  set.seed(seed)
  if (object$type == "regression") {
    vimp <- vip::vi(
      object,
      target = target,
      method = "permute",
      metric = "r2", # "rmse"
      pred_wrapper = pred_fun,
      train = data
    )
  } else {
    vimp <- vip::vi(
      object,
      target = target,
      reference_class = TRUE,
      method = "permute",
      metric = "auc",
      pred_wrapper = pred_fun,
      train = data
    )
  }

  vimp %>%
    filter(Importance != 0) %>%
    mutate(Variable = factor(Variable, levels = rev(Variable)))
}