\name{maxmatrix}
\alias{maxmatrix}
\title{Largest Element in Correlation Matrix}
\description{
 return position and value of the largest element of a correlation matrix R
 (without taking into account the diagonal elements)
}
\usage{
maxmatrix(R)
}
\arguments{
  \item{R}{a square symmetric numeric matrix}
}
\value{
  a list with components
  \item{row}{row index of maximum}
  \item{col}{col index of maximum}
  \item{val}{value of maximum, i.e. \code{val == R[row,col]}.}
}
\seealso{\code{\link{sca}}, also for references}
\examples{
data(reflexesC)
maxmatrix(reflexesC) # ->  0.98 at  [1, 2]
}
\keyword{multivariate}
