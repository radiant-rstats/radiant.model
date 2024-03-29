% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regress.R
\name{summary.regress}
\alias{summary.regress}
\title{Summary method for the regress function}
\usage{
\method{summary}{regress}(object, sum_check = "", conf_lev = 0.95, test_var = "", dec = 3, ...)
}
\arguments{
\item{object}{Return value from \code{\link{regress}}}

\item{sum_check}{Optional output. "rsme" to show the root mean squared error and the standard deviation of the residuals. "sumsquares" to show the sum of squares table. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates.}

\item{conf_lev}{Confidence level used to estimate confidence intervals (.95 is the default)}

\item{test_var}{Variables to evaluate in model comparison (i.e., a competing models F-test)}

\item{dec}{Number of decimals to show}

\item{...}{further arguments passed to or from other methods}
}
\description{
Summary method for the regress function
}
\details{
See \url{https://radiant-rstats.github.io/docs/model/regress.html} for an example in Radiant
}
\examples{
result <- regress(diamonds, "price", c("carat", "clarity"))
summary(result, sum_check = c("rmse", "sumsquares", "vif", "confint"), test_var = "clarity")
result <- regress(ideal, "y", c("x1", "x2"))
summary(result, test_var = "x2")
ideal \%>\%
  regress("y", "x1:x3") \%>\%
  summary()

}
\seealso{
\code{\link{regress}} to generate the results

\code{\link{plot.regress}} to plot results

\code{\link{predict.regress}} to generate predictions
}
