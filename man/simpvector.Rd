\name{simpvector}
\alias{simpvector}
\title{Simplify a (Principal Component) Vector to a Simple Component}
\description{
  Simplifies the vector \code{x} to become a ``simple'' component vector
  (of the same size).
}
\usage{
simpvector(x)
}
\arguments{
  \item{x}{numeric vector of length \code{n}, say.}
}
%\details{
%  ... %%% FIXME
%}
\value{
  a ``simplified'' version of \code{x}, i.e. an integer vector of the
  same length and each entry with the same signs.
}
\seealso{\code{\link{sca}}, also for references}
\examples{
 x0 <- c(-2:3, 3:-1,0:3,1,1)
 cbind(x0, simpvector(x0)) # entries (-11, 0, 3)
}
\keyword{multivariate}
