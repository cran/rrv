\name{money}
\alias{money}
\alias{print.money}
\alias{format.money}
\alias{moneyp}
\alias{moneyf}
\title{formatting money}
\description{The money (constructor) function creates money objects, extended numeric vectors that support special formatting. The format method formats money objects, in a financial way. The moneyf function is a shortcut for formatting regular numeric vectors. Any non-finite values are returned as NAs. The print functions, print and format objects, without quotes.}
\usage{
money (x, cs="", sep=c (".", ","), nd=2, fill=TRUE)
\method{print}{money}(x, \dots)
\method{format}{money}(x, cs, sep, nd, fill, \dots)
moneyp (x, \dots)
moneyf (x, cs="", sep=c (".", ","), nd=2, fill=TRUE)
}
\arguments{
\item{x}{A money object or numeric vector.}
\item{cs}{Currency symbol as a character.}
\item{sep}{Character vector of length two, giving the decimal and thousands separators respectively.}
\item{nd}{Number of decimal places.}
\item{fill}{Insert empty spaces to beginning and end of the final character vector, to ensure correct alignment.}
\item{\dots}{.}
}

