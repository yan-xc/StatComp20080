% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_C.R
\name{positive mean}
\alias{positive mean}
\alias{positive.mean}
\title{positive mean}
\usage{
positive.mean(X, x)
}
\arguments{
\item{X}{the random sample}

\item{x}{critical value}
}
\value{
mean value of positive
}
\description{
Calculate the mean value of positive.
}
\examples{
\dontrun{
X=rnorm(1e3)
x=1
positive.mean=positive.mean(X,x)
}
}
