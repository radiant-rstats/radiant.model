% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn.R
\name{summary.nn}
\alias{summary.nn}
\title{Summary method for the nn function}
\usage{
\method{summary}{nn}(object, prn = TRUE, ...)
}
\arguments{
\item{object}{Return value from \code{\link{nn}}}

\item{prn}{Print list of weights}

\item{...}{further arguments passed to or from other methods}
}
\description{
Summary method for the nn function
}
\details{
See \url{https://radiant-rstats.github.io/docs/model/nn.html} for an example in Radiant
}
\examples{
result <- nn(titanic, "survived", "pclass", lev = "Yes")
summary(result)
}
\seealso{
\code{\link{nn}} to generate results

\code{\link{plot.nn}} to plot results

\code{\link{predict.nn}} for prediction
}
