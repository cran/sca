\name{corcomp}
\alias{corcomp}
\alias{covcomp}
\alias{cov2cor}%% << for R versions < 1.8 (and S-plus!)
\title{Covariance and Correlation Matrix of Components P on S}
\description{
  \code{covcomp} returns the variance-covariance matrix of the
  components P on S, and \code{corcomp} returns the correlation matrix.
}
\usage{
corcomp(S, P)
covcomp(S, P)
cov2cor(V)%% << for R versions < 1.8 (and S-plus!)
}
\arguments{
  \item{S}{correlation/covariance matrix of the \eqn{p} original variables.}
  \item{P}{component matrix of dimension \eqn{p \times b}{p x b}.}

  \item{V}{symmetric numeric matrix, usually positive definite such as a
    covariance matrix.}
}
%-\details{
%-  ~~ If necessary, more details than the __description__  above ~~
%-}
\value{
  a square \eqn{b \times b}{b x b} matrix.
}
\author{Valentin Rousson \email{rousson@ifspm.unizh.ch} and
  Martin Maechler \email{maechler@stat.math.ethz.ch}.}
\seealso{\code{\link{sca}}, also for references}
\examples{
data(USJudgeRatings)
S.jr <- cor(USJudgeRatings)
sca.jr <- sca(S.jr, b=4, inter=FALSE)
Vr <- covcomp(S.jr, P = sca.jr$simplemat)
Vr
Cr <- corcomp(S.jr, P = sca.jr$simplemat)
Cr
}
\keyword{multivariate}
