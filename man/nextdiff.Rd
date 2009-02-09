\name{nextdiff}
\alias{nextdiff}
\title{Compute the Next Simple Difference-Component for SCA}
\description{
  Compute the next simple difference-component; this is an auxiliary
  function for \code{\link{sca}}.
}
\usage{
nextdiff(S, P, withinblock, criterion)
}
\arguments{
  \item{S}{correlation/covariance matrix}
  \item{P}{component matrix}
  \item{withinblock}{logical indicating whether any given
    difference-component should only involve variables belonging to the
    same block-component.}
  \item{criterion}{character string specifying the optimality criterion,
    see \code{\link{sccrit}} for details.}
}
\details{
  Uses \code{\link{firstpcres}(S,P)} and subsequently
  \code{\link{shrinkdiff}()}, the latter in a loop when
  \code{withinblock} is true.

  In order to ensure uniqueness, we ensure that the first (non zero)
  entry of the principal component is always \emph{positive}.
}
\value{
  a list with components
  \item{P}{the new component matrix, i.e. the input \code{P} with one
    new column appended.}
  \item{nextpc}{the next principal component with many entries set to 0.}
}
\author{Valentin Rousson \email{rousson@ifspm.unizh.ch} and
  Martin Maechler \email{maechler@stat.math.ethz.ch}.}
\seealso{\code{\link{shrinkdiff}}; \code{\link{sca}}, also for references}
% \examples{
% }
\keyword{multivariate}
