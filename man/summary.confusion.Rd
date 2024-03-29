% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evalbin.R
\name{summary.confusion}
\alias{summary.confusion}
\title{Summary method for the confusion matrix}
\usage{
\method{summary}{confusion}(object, dec = 3, ...)
}
\arguments{
\item{object}{Return value from \code{\link{confusion}}}

\item{dec}{Number of decimals to show}

\item{...}{further arguments passed to or from other methods}
}
\description{
Summary method for the confusion matrix
}
\details{
See \url{https://radiant-rstats.github.io/docs/model/evalbin.html} for an example in Radiant
}
\examples{
data.frame(buy = dvd$buy, pred1 = runif(20000), pred2 = ifelse(dvd$buy == "yes", 1, 0)) \%>\%
  confusion(c("pred1", "pred2"), "buy") \%>\%
  summary()
}
\seealso{
\code{\link{confusion}} to generate results

\code{\link{plot.confusion}} to visualize result
}
