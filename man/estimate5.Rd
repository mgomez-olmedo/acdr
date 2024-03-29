% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate5.R
\name{estimate5}
\alias{estimate5}
\title{Function that estimates the probabilities with a  forgetting factor
which is computed by a Bayesian BDEu score of the probability of change
in the last n cases (dividing these cases in two equal parts)}
\usage{
estimate5(x, n, alpha)
}
\arguments{
\item{x}{stream to analyze}

\item{n}{size of window with last cases to consider for estimation}

\item{alpha}{value to consider for tests}
}
\value{
list with data, estimations, values for s, fg and rho
}
\description{
Function that estimates the probabilities with a  forgetting factor
which is computed by a Bayesian BDEu score of the probability of change
in the last n cases (dividing these cases in two equal parts)
}
