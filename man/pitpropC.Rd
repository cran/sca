\name{pitpropC}
\alias{pitpropC}
\docType{data}
\title{Pitprops Strength Correlation Data}
\description{
  This correlation matrix was published in Jeffers (1967) and was calculated
  from 180 observations. The 13 variables were used as explanatory variables
  in a regression problem which arised from a study on the strength of
  pitprops cut from home-grown timber.}
\usage{data(pitpropC)}
\format{
  Its a correlation matrix of 13 variables which have the following meaning:
  \tabular{rll}{
    [,1] \tab  TOPDIAM \tab  Top diameter of the prop in inches\cr
    [,2] \tab  LENGTH  \tab  Length of the prop in inches\cr
    [,3] \tab  MOIST   \tab  Moisture content of the prop, expressed as a percentage of
    				the dry weight\cr
    [,4] \tab  TESTSG  \tab  Specific gravity of the timber at the time of the test\cr
    [,5] \tab  OVENSG  \tab  Oven-dry specific gravity of the timber\cr
    [,6] \tab  RINGTOP \tab  Number of annual rings at the top of the prop\cr
    [,7] \tab  RINGBUT \tab  Number of annual rings at the base of the prop\cr
    [,8] \tab  BOWMAX  \tab  Maximum bow in inches\cr
    [,9] \tab  BOWDIST \tab  Distance of the point of maximum bow from the top of the
				prop in inches\cr
    [,10]\tab  WHORLS  \tab  Number of knot whorls\cr
    [,11]\tab  CLEAR   \tab  Length of clear prop from the top of the prop in inches\cr
    [,12]\tab  KNOTS   \tab  Average number of knots per whorl\cr
    [,13]\tab  DIAKNOT \tab  Average diameter of the knots in inches
  }% tab
}
\details{
  Jeffers (1967) replaced these 13 variables by their first six
  principal components.  As noted by Vines (2000), this is an example
  where simple structure has proven difficult to detect in the past.
}
\references{
  Jeffers, J.N.R. (1967)
  Two case studies in the application of principal components analysis.
  \emph{Appl. Statist.} \bold{16}, 225--236.

  Vines, S.K. (2000)
  Simple principal components.
  \emph{Appl. Statist.} \bold{49}, 441--451.
}
\examples{
data(pitpropC)
symnum(pitpropC)
}
\keyword{datasets}
