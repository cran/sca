\name{firstpcres}
\alias{firstpcres}
\title{First Principal Component of Residuals given Components}
\description{
  Return the first principal component of residuals of \code{S} given
  the components \code{P}.
}
\usage{
firstpcres(S, P)
}
\arguments{
  \item{S}{correlation/covariance matrix}
  \item{P}{component matrix}
}
\value{
  numeric vector; actually, the first eigenvector of
  \eqn{S - A' (A P)^{-1} A}{S - A' (A P)^(-1) A} where \eqn{A := P' S}.
}
\seealso{\code{\link{sca}}, also for references}
\examples{
}
\keyword{multivariate}
