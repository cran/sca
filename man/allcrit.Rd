\name{allcrit}
\alias{allcrit}
\alias{sccrit}
\title{Simple Component Quality Criterion Computation}
\description{
  Compute simple component criterion for components \code{P} on
  cor.matrix \code{S} (cumulative), using \code{sccrit()}.

  Function \code{allcrit()} computes even more criteria, some derived
  from \code{sccrit()}.
}
\usage{
allcrit(S, P, criterion, sortP = TRUE)
 sccrit(S, P, criterion, sortP = TRUE)
}
\arguments{
  \item{S}{correlation/covariance matrix}
  \item{P}{component matrix}
  \item{criterion}{character string specifying the optimality criterion
    to be used in \code{sccrit()} for evaluating a system of simple
    components.  One of \code{"csv"} (corrected sum of variances) or
    \code{"blp"} (best linear predictor).}
  \item{sortP}{logical indicating if \code{P} should be sorted; if true,
    \code{sortmatrix(S,P)} used in lieu of \code{P}.}
}
%-\details{
%-  ~~ If necessary, more details than the __description__  above ~~
%-}
\value{
  \code{sccrit()} returns a numeric vector, the criterion computed
  (cumulatively).

  \code{allcrit()} returns a list with components \code{varpc},
  \code{varsc}, \code{cumpc}, \code{cumsc}, \code{opt}, \code{corsc},
  and \code{maxcor};
  see the description of the \code{allcrit} component in the return value
  of \code{\link{sca}()}.
}
\author{Valentin Rousson \email{rousson@ifspm.unizh.ch} and
  Martin Maechler \email{maechler@stat.math.ethz.ch}.}
\seealso{\code{\link{quickcrit}}, \code{\link{sca}}, also for references.}
\examples{
}
\keyword{multivariate}
