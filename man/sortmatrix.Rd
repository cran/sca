\name{sortmatrix}
\alias{sortmatrix}
\title{Sort Simple Component Matrix}
\description{
 Reorder the columns of a component matrix \code{P} by decreasing
 variances of components where the block-components come first, the
 difference components afterwards.
}
\usage{
sortmatrix(S, P)
}
\arguments{
  \item{S}{correlation/covariance matrix}
  \item{P}{component matrix}
}
\value{
  numeric matrix which is just \code{P} with columns reordered.
}
\seealso{\code{\link{sca}}, also for references}
\examples{
}
\keyword{multivariate}
