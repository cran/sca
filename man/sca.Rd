\name{sca}
\title{Simple Component Analysis -- Interactively}
\alias{sca}
\alias{print.simpcomp}
\description{
  A system of simple components calculated from a correlation (or
  variance-covariance) matrix is built (interactively if \code{interactive =
    TRUE}) following the methodology of Rousson and Gasser (2003).
}
\usage{
sca(S, b = if(interactive) 5, d = 0, qmin = if(interactive) 0 else 5,
    corblocks = if(interactive) 0 else 0.3,
    criterion = c("csv", "blp"), cluster = c("median","single","complete"),
    withinblock = TRUE, invertsigns = FALSE,
    interactive = dev.interactive())
\method{print}{simpcomp}(x, ndec = 2, \dots)
}
\arguments{
  \item{S}{the correlation (or variance-covariance) matrix to be analyzed.}
  \item{b}{the number of block-components initially proposed.}
  \item{d}{the number of difference-components initially proposed.}
  \item{qmin}{if larger than zero, the number of difference-components
    is chosen such that the system contains at least \code{qmin}
    components (overriding argument \code{d}!).}
  \item{corblocks}{if larger than zero, the number of block-components
    is chosen such that correlations among them are all smaller than
    \code{corblocks} (overriding argument \code{b}).}
  \item{criterion}{character string specifying the optimality criterion
    to be used for evaluating a system of simple components.  One of
    \code{"csv"} (corrected sum of variances) or \code{"blp"} (best
    linear predictor); can be abbreviated.}
  \item{cluster}{character string specifying the clustering method to be
    used in the definition of the block-components.  One of
    \code{"single"} (single linkage), \code{"median"} (median linkage)
    or \code{"complete"} (complete linkage) can be abbreviated.}
  \item{withinblock}{a logical indicating whether any given
    difference-component should only involve variables belonging to the
    same block-component.}
  \item{invertsigns}{a logical indicating whether the sign of some
    variables should be inverted initially in order to avoid negative
    correlations.}
  \item{interactive}{a logical indicating whether the system of simple
    components should be built interactively.  If \code{interactive=FALSE}, an
    optimal system of simple components is automatically calculated
    without any intervention of the user (according to \code{b} or
    \code{corblocks}, and to \code{d} or \code{qmin}).

    By default, \code{interactive = \code{\link{dev.interactive}}()} (which is
    true if \code{\link{interactive}()} and \code{\link{.Device}} is an
    interactive graphics device).}

%% print.*()
  \item{x}{an object of class \code{sca}, typically the result of
    \code{\link{sca}(..)}.}
  \item{ndec}{number of decimals \emph{after} the dot, for the
    percentages printed.}
  \item{\dots}{further arguments, passed to and from methods.}
}
\details{
  When confronted with a large number \eqn{p} of variables measuring
  different aspects of a same theme, the practitionner may like to
  summarize the information into a limited number \eqn{q} of components.  A
  \emph{component} is a linear combination of the original variables, and
  the weights in this linear combination are called the \emph{loadings}.
  Thus, a system of components is defined by a \eqn{p} times \eqn{q} dimensional
  matrix of loadings.

  Among all systems of components, principal components (PCs) are
  optimal in many ways.  In particular, the first few PCs extract a
  maximum of the variability of the original variables and they are
  uncorrelated, such that the extracted information is organized in an
  optimal way: we may look at one PC after the other, separately,
  without taking into account the rest.

  Unfortunately PCs are often difficult to interpret. The goal of Simple
  Component Analysis is to replace (or to supplement) the optimal but
  non-interpretable PCs by suboptimal but interpretable \emph{simple
  components}. The proposal of Rousson and Gasser (2003) is to look for
  an optimal system of components, but only among the simple ones,
  according to some definition of optimality and simplicity. The outcome
  of their method is a simple matrix of loadings calculated from the
  correlation matrix \eqn{S} of the original variables.

  Simplicity is not a guarantee for interpretability (but it helps in
  this regard).  Thus, the user may wish to partly modify an optimal
  system of simple components in order to enhance
  interpretability.  While PCs are by definition 100\% optimal, the
  optimal system of simple components proposed by the procedure \code{sca}
  may be, say, 95\%, optimal, whereas the simple system altered by the
  user may be, say, 93\% optimal. It is ultimately to the user to decide
  if the gain in interpretability is worth the loss of optimality.

  The interactive procedure \code{sca} is intended to assist the user in
  his/her choice for an interptetable system of simple components. The
  algorithm consists of three distinct stages and proceeds in an
  interative way. At each step of the procedure, a simple matrix of
  loadings is displayed in a window. The user may alter this matrix by
  clicking on its entries, following the instructions given there.  If
  all the loadings of a component share the same sign, it is a
  ``block-component''.  If some loadings are positive and some loadings
  are negative, it is a ``difference-component''.  Block-components are
  arguably easier to interpret than
  difference-components. Unfortunately, PCs almost always contain only
  one block-component. In the procedure \code{sca}, the user may choose the
  number of block-components in the system, the rationale being to have
  as many block-components such that correlations among them are below
  some cut-off value (typically .3 or .4).

  Simple block-components should define a partition of the original
  variables. This is done in the first stage of the procedure \code{sca}. An
  agglomerative hierarchical clustering procedure is used there.

  The second stage of the procedure \code{sca} consists in the definition of
  simple difference-components.  Those are obtained as simplified
  versions of some appropriate ``residual components''. The idea is to
  retain the large loadings (in absolute value) of these residual
  components and to shrink to zero the small ones. For each
  difference-component, the interactive procedure \code{sca} displays the
  loadings of the corresponding residual component (at the right side of
  the window), such that the user may know which variables are
  especially important for the definition of this component.

  At the third stage of the interactive procedure \code{sca}, it is possible
  to remove some of the difference-components from the system.

  For many examples, it is possible to find a simple system which is 90\%
  or 95\% optimal, and where correlations between components are below 0.3
  or 0.4. When the structure in the correlation matrix is complicated, it
  might be advantageous to invert the sign of some of the variables in
  order to avoid as much as possible negative correlations. This can be
  done using the option `invertsigns=TRUE'.

  In principle, simple components can be calculated from a correlation
  matrix or from a variance-covariance matrix. However, the definition
  of simplicity used is not well adapted to the latter case, such that
  it will result in systems which are far from being 100\%
  optimal. Thus, it is advised to define simple components from a
  correlation matrix, not from a variance-covariance matrix.
}
\value{
  An object of class \code{simpcomp} which is basically as list with
  the following components:
  \item{simplemat}{an integer matrix defining a system of simple
    components.  The rows correspond to variables and the columns correspond to
    components.}
  \item{loadings}{loadings of simple components.  This is a normalized
    (by \code{\link{normmatrix}}) version of \code{simplemat}.}
  \item{allcrit}{a list containing the following components:
    \describe{
      \item{varpc}{a vector containing the percentage of total
	variability accounted by each of the the first \code{nblock + ndiff}
	principal components of \code{S}.}
      \item{varsc}{a vector containing the percentage of total
	variability accounted by each of the simple components defined by
	\code{simplemat}.}
      \item{cumpc}{the sum of varpc, indicating the percentage of total
	variability accounted by the first \code{nblock + ndiff} principal
	components of \code{S}.}
      \item{cumsc}{a score indicating the percentage of total
	variability accounted by the system of simple
	components. \code{cumsc} is calculated according to \code{criterion}.}
      \item{opt}{indicates the optimality of the system of simple
	components and is computed as \code{cumsc/cumpc}.}
      \item{corsc}{correlation matrix of the simple components defined
	by \code{simplemat}.}
      \item{maxcor}{a list with the following components:
	\describe{
	  \item{row}{label of the row of the maximum value in \code{corsc}.}
	  \item{col}{label of the column of the maximum value in \code{corsc}.}
	  \item{val}{maximum value in \code{corsc} (in absolute value).}
	}
      }
  }}% end `allcrit' list
  \item{nblock}{number of block-components in \code{simplemat}.}

  \item{ndiff}{number of difference-components in \code{simplemat}.}

  \item{criterion}{as above.}
  \item{cluster}{as above.}
  \item{withinblock}{as above.}
  \item{invertsigns}{as above}

  \item{vardata}{the correlation (or variance-covariance) matrix which
    was analyzed. In principle it should be equal to argument \code{S} above,
    except if it has been transformed in order to avoid negative
    correlations.}
}
\references{
  Rousson, V. and Gasser, Th. (2003)
  Simple Component Analysis. Submitted.

  Rousson, V. and Gasser, Th. (2003)
  \emph{Some Case Studies of Simple Component Analysis}.
  Manuscript.

  Gervini, D. and Rousson, V. (2003)
  \emph{Some Proposals for Evaluating Systems of Components in Dimension
    Reduction Problems}.
  Submitted.
}
\author{Valentin Rousson \email{rousson@ifspm.unizh.ch} and
  Martin Maechler \email{maechler@stat.math.ethz.ch}.}
\seealso{\code{\link[mva]{pca}} etc.}
\examples{
data(pitpropC)
sc.pitp <- sca(pitpropC, interactive=FALSE)
sc.pitp
## to see it's low-level components:
str(sc.pitp)

## Let `X' be a matrix containing some data set whose rows correspond to
## subjects and whose columns correspond to variables. For example:

%% MM: need a real matrix here!
library(MASS)
Sig <- function(p, rho) { r <- diag(p); r[col(r) != row(r)] <- rho; r}
rmvN <- function(n,p, rho)
        mvrnorm(n, mu=rep(0,p), Sigma= Sig(p, rho))
X <- cbind(rmvN(100, 3, 0.7),
           rmvN(100, 2, 0.9),
           rmvN(100, 4, 0.8))
%% end MM

## An optimal simple system with at least 5 components for the data in `X',
## where the number of block-components is such that correlations among
## them are all smaller than 0.4, can be automatically obtained as:

(r <- sca(cor(X), qmin=5, corblocks=0.4, interactive=FALSE))

## On the other hand, an optimal simple system with two block-components
## and two difference-components for the data in `X' can be automatically
## obtained as:

(r <- sca(cor(X), b=2, d=2, qmin=0, corblocks=0, interactive=FALSE))

## The resulting simple matrix is contained in `r$simplemat'.
## A matrix of scores for such simple components can then be obtained as:

(Z <- scale(X) \%*\% r$loadings)

## On the other hand, scores of simple components calculated from the
## variance-covariance matrix of `X' can be obtained as:

r <- sca(var(X), b=2, d=2, qmin=0, corblocks=0, interactive=FALSE)
Z <- scale(X, scale=FALSE) \%*\% r$loadings

## One can also use the program interactively as follows:

if(interactive()) {
  r <- sca(cor(X), corblocks=0.4, qmin=5, interactive = TRUE)

  ## Since the interactive part of the program is active here, the proposed
  ## system can then be  modified according to the user's wishes. The
  ## result of the procedure will be contained in `r'.
}%interactive

}
\keyword{multivariate}
