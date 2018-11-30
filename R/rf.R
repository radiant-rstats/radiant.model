
## initial version for randomForests
# cv.randomForest <- function(object, K = 5, seed = 1234, trace = TRUE) {
#   set.seed(seed)
#   dv <- as.character(object$call$formula[[2]])
#   m <- eval(object$call[["data"]])
#   if (missing(mtry)) {
#     vars <- object$call$formula[[3]]
#     mtry <- if (vars == ".") 1:(ncol(m)-1) else 1:length(vars)
#   }
#   rand <- sample(K, nrow(m), replace = TRUE)
#   ## urand <- sort(unique(rand))
#   urand <- seq_len(K)
#   object$call[["keep.forest"]] <- TRUE
#   out <- data.frame(mean = NA, std = NA, mtry = mtry)
#   for (i in mtry) {
#     flearn <- double(length(urand))
#     object$call[["mtry"]] <- i
#     for (j in urand) {
#       if (trace) cat("Working on mtry:", i, "cross validation:",j,"\n")
#       object$call[["data"]] <- quote(m[rand != j, , drop = FALSE])
#       tlearn <- eval(object$call)
#       if (is.factor(m[[dv]])) {
#         plearn <- predict(tlearn, newdata = m[rand == j, , drop = FALSE], type = "prob")
#         flearn[j] <- auc(plearn[,1], m[rand == j, dv], colnames(plearn)[1])
#       } else {
#         plearn <- predict(tlearn, newdata = m[rand == j, , drop = FALSE])
#         flearn[j] <- mean((m[rand == j, dv] - plearn)^2)
#       }
#     }
#     out[i,1:2] <- c(mean(flearn), sd(flearn))
#   }
#
#   if (is.factor(m[[dv]])) out[rev(order(out$mean)),] else  out[order(out$mean),]
# }
