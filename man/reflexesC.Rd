\name{reflexesC}
\alias{reflexesC}
\docType{data}
\title{Human Reflexes Correlation Data}
\description{
  This correlation matrix was published in Jolliffe (2002, p.58).  The data
  consist of measurements of strength of reflexes at ten sites of the body,
  taken for 143 individuals. The variables come in five pairs, corresponding
  to right and left measurements on triceps, biceps, wrists, knees and
  ankles, respectively.
}
\usage{data(reflexesC)}
\format{
  It is a \eqn{10 x 10} correlation matrix, i.e. symmetric, and
  diagonal \code{1}.
  The five pairs of variables are (in this order)
  \code{"triceps.R", "triceps.L", "biceps.R", "biceps.L",
    "wrist.R", "wrist.L", "knee.R", "knee.L",
    "ankle.R", "ankle.L"}.
}
\references{
  Jolliffe, I.T. (2002)
  Principal Component Analysis (2nd ed.).
  Springer, New York.
}
\examples{
data(reflexesC)
symnum(reflexesC)
sca(reflexesC)  # sca gets 97.95\%  of PCA
}
\keyword{datasets}
