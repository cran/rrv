\name{format}
\alias{money.format}
\title{format money variables}
\description{Format money variables, in a vaguely financial kind of way. This function takes a numeric vector and always returns a character vector. Any non-finite values are returned as NAs. If you want to format a variable in a data.frame object, replace the variable with a formatted character vector (noting this may be turned to a factor), e.g. df$x = money.format (df$x).}
\usage{
money.format (x, ndec=0, cs=NULL, sep=c (".", ","), br=TRUE, fill=TRUE)
}
\arguments{
\item{x}{A numeric vector. Can be length zero or contain non-finite values.}
\item{ndec}{Number of decimal places.}
\item{cs}{Currency symbol. Unless null, this should be a character.}
\item{sep}{Character vector of length two, giving the decimal and thousands separators respectively.}
\item{br}{Include brackets around negative numbers for emphasis.}
\item{fill}{Insert empty spaces to beginning and end of the final character vector, to ensure correct alignment.}
}

