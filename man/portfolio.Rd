\name{portfolio}
\alias{portfolio}
\alias{expectedpr}
\alias{variancepr}
\alias{quantilepr}
\alias{sdpr}
\alias{medianpr}
\alias{plot.pr}
\title{portfolio functions}
\description{The portfolio (constructor) function takes an rrv object (or anything that can be converted into an rrv object) and returns a portfolio object. A portfolio object is also a function, that takes a vector of weights and returns an rprv object. The pr (constructor) functions (pr stands for portfolio return), take a portfolio object and return a pr object. Like portfolio objects, pr objects are also functions of weight, however instead of returning rprv objects they return scalar-parameters (i.e. expectedpr is expected portfolio return).}
\usage{
portfolio (x)
expectedpr (f)
variancepr (f)
quantilepr (f, p)
sdpr (f)
medianpr (f)
\method{plot}{pr}(f, \dots)
}
\arguments{
\item{f}{A portfolio (returned by the portfolio constructor).}
\item{x}{An rrv object or an rrv seed object.}
\item{p}{The pth quantile. i.e. A value between 0 and 1.}
\item{\dots}{.}
}


