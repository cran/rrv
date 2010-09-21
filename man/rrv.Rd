\name{rrv}
\alias{rrv}
\alias{rprv}
\alias{print.rrv}
\alias{print.rprv}
\title{random return variables}
\description{Random return variables are objects representing the distributions of portfolio returns, here using ECDFs. The standard rrv object represents the (potentially multivariate) distribution of (potentially multiple) investment returns. The rprv object extends the rrv object, however represents the (always univariate) distribution of the weighted sum of the individual investment returns. Raw returns are realised returns, as a vector or matrix, and weights are a vector summing to one.}
\usage{
rrv (x, \dots)
rprv (x, w, \dots)
\method{print}{rrv}(x, \dots)
\method{print}{rprv}(x, \dots)
}
\arguments{
\item{x}{.}
\item{w}{.}
\item{\dots}{.}
}

