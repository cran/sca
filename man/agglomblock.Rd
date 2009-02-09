\name{agglomblock}
\alias{agglomblock}
\title{Agglomerate Two Block-Components in SCA}
\description{
  Agglomerate the two block-components which are \emph{closest}
  according to the specified \code{cluster}ing method.
}
\usage{
agglomblock(S, P, cluster = c("median","single","complete"))
}
\arguments{
  \item{S}{correlation/covariance matrix}
  \item{P}{component matrix}
  \item{cluster}{character specifying the clustering method; default
    \code{"median"}, see \code{\link{sca}(*, cluster=)}.}
}
\value{
  New component matrix with one block component less.
}
\seealso{\code{\link{sca}}, also for references}
% \examples{
% }
\keyword{multivariate}
