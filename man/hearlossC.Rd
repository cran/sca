\name{hearlossC}
\alias{hearlossC}
\docType{data}
\title{Hearing Loss Correlation Data}
\description{
  The data consist of eight measurements of hearing loss taken on 100
  males, aged 39, who had no indication of hearing difficulties.  These
  measurements are decibel loss (in comparison to a reference standard)  at
  frequencies 500Hz, 1000Hz, 2000Hz and 4000Hz for the left and the right
  ear, respectively.
}
\usage{data(hearlossC)}
\format{
  Eight Variables, first the ones for ``Left'', than for the ``Right''.
  The frequences are abbreviated, e.g., \code{2k} for \emph{2000 Hz} or
   \code{5c} for \emph{500 Hz}.
   The variable names are (in this order)
  \code{"Left5c",  "Left1k",  "Left2k",  "Left4k",
    "Right5c", "Right1k", "Right2k", "Right4k"}.
}
% \details{
% ..
% }
\source{
  This is the correlation matrix of data described in Chapter 5 of
  Jackson (1991).
}
\references{
  Jackson, J.E. (1991)
  \emph{A User's Guide to Principal Components}.
  John Wiley, New York.
}
\examples{
data(hearlossC)
symnum(hearlossC)
}
\keyword{datasets}
