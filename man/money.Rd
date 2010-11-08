\name{money}
\alias{money}
\alias{format.money}
\alias{moneyf}
\title{formatting money}
\description{The money (constructor) function creates money objects, extended numeric vectors that support special formatting. The format method formats money objects, in a financial way. The moneyf function is a shortcut for formatting regular numeric vectors. Any non-finite values are returned as NAs.}
\usage{
money (x)
\method{format}{money}(x, ndec=0, cs=NULL, sep=c (".", ","), fill=TRUE, \dots)
moneyf (x, \dots)
}
\arguments{
\item{x}{A money object or numeric vector.}
\item{ndec}{Number of decimal places.}
\item{cs}{Currency symbol. Unless null, this should be a character.}
\item{sep}{Character vector of length two, giving the decimal and thousands separators respectively.}
\item{fill}{Insert empty spaces to beginning and end of the final character vector, to ensure correct alignment.}
\item{\dots}{.}
}


