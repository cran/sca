\name{quickcrit}
\alias{quickcrit}
\title{Additional Contribution of New Component to the SC System}
\description{
  Compute the additional contribution of a new component to the simple
  component system P on S.
}
\usage{
quickcrit(newcomp, S, P, criterion)
}
\arguments{
  \item{newcomp}{numeric vector, typically the result of
    \code{\link{simpvector}()}.}
  \item{S}{correlation/covariance matrix}
  \item{P}{component matrix}
  \item{criterion}{character string specifying the optimality criterion,
    see \code{\link{sccrit}} for details.}
}
\value{
  ...
}
\seealso{\code{\link{sccrit}}; further \code{\link{sca}}, also for
  references.}
\examples{
}
\keyword{multivariate}
