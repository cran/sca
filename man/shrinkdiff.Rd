\name{shrinkdiff}
\alias{shrinkdiff}
\title{Shrink Component Towards a Simple Difference-Component in SCA}
\description{
  Shrinks a (principal) component towards a simple difference-component
  in \code{\link{sca}}.
}
\usage{
shrinkdiff(zcomp, S, P, criterion)
}
\arguments{
  \item{zcomp}{a component vector to be \emph{simplified}.}
  \item{S}{correlation/covariance matrix}
  \item{P}{component matrix}
  \item{criterion}{character string specifying the optimality criterion,
    see \code{\link{sccrit}} for details.}
}
% \details{
%  ......
% }
\value{
  a list with the components
  \item{scompmax}{a new simple component vector, typically result of
    \code{\link{simpvector}}.}
  \item{critmax}{the (optimal) value of the criterion, achieved for
    \code{scompmax}.}
}
\seealso{\code{\link{sca}}, also for references}
% \examples{
% }
\keyword{multivariate}
