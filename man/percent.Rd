\name{percent}
\alias{percent}
\title{Simple Formatting of Percentages}
\description{
  Returns strings of the same length as \code{p}, displaying the
  \code{100 * p} percentages.
}
\usage{
percent(p, d = 0, sep = " ")
}
\arguments{
  \item{p}{number(s) in \eqn{[0,1]} -- to be ``displayed'' as percentage(s).}
  \item{d}{number of digits after decimal point.}
  \item{sep}{separator to use before the final \code{"\%"}.}
}
\value{
  character vector of the same length as \code{p}.
}
\author{Martin Maechler}
\examples{
percent(0.25)
noquote(percent((1:10)/10))
(pc <- percent((1:10)/30, 1, sep=""))
noquote(pc)
}
\keyword{utilities}
\keyword{print}
