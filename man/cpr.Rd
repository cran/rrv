\name{cpr}
\alias{cpr_expected}
\alias{cpr_variance}
\alias{cpr_quantile}
\alias{cpr_median}
\alias{cpr_sd}
\alias{cpr_rrv}
\alias{portfolio.names}
\alias{plot.cppr}
\title{portfolio functions}
\description{As of rrv 0.4.0, cpr (conditional portfolio return) objects are created directly from an rrv object or a matrix and represent functions of weight. cpr_rrv objects return rprv objects, while cppr objects (e.g. cpr_expected) return numerics.}
\usage{
cpr_expected (x)
cpr_variance (x)
cpr_quantile (p, x)
cpr_median (x)
cpr_sd (x)
cpr_rrv (x)
portfolio.names (f)
\method{plot}{cppr}(f, \dots)
}
\arguments{
\item{f}{A cppr object.}
\item{x}{An rrv object or a matrix.}
\item{p}{Quantile as a probability, between 0 and 1.}
\item{\dots}{.}
}


