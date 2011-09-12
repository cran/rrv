\name{rrv}
\alias{rrv}
\alias{rprv}
\alias{print.rrv}
\alias{plot.rrv}
\alias{samp.rrv}
\title{random return variables}
\description{Random return variables, rrv objects, are (extended) matrices of realised returns, which in principle, represent a random variable with an empirical distribution. Standard rrv objects are potentially multivariate, and represent returns over one or more investments. Random portfolio return variables, rprv objects, are always univariate, and represent the weighted sum (generally weighted average) of one or more investment returns. rrv objects are created from a matrix or any object that can be converted to a matrix.}
\usage{
rrv (x)
rprv (x, w=rep (1, ncol (x) ) )
\method{print}{rrv}(x, \dots)
\method{plot}{rrv}(x, \dots)
\method{samp}{rrv}(x, n=3, m=n)
}
\arguments{
\item{x}{.}
\item{w}{.}
\item{n}{.}
\item{m}{.}
\item{\dots}{.}
}




