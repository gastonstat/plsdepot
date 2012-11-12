\name{simpls}
\alias{simpls}
\title{SIMPLS: Alternative Approach to PLS Regression}
\usage{
  simpls(X, Y, comps = 2)
}
\arguments{
  \item{X}{Numeric matrix or data frame with two or more
  columns (X-block).}

  \item{Y}{Numeric matrix or data frame with two or more
  columns (Y-block).}

  \item{comps}{Number of components to be extracted.
  (\code{TRUE} by default).}
}
\value{
  An object of class \code{"simpls"}, basically a list with
  the following elements:

  \item{x.scores}{scores of the X-block (also known as T
  components)}

  \item{x.wgs}{weights of the X-block}

  \item{y.wgs}{weights of the Y-block}

  \item{cor.xt}{correlations between X and T}

  \item{cor.yt}{correlations between Y and T}

  \item{R2X}{explained variance of X by T}

  \item{R2Y}{explained variance of Y by T}
}
\description{
  The function \code{simpls} performs the SIMPLS Algorithm
  as described in Michel Tenenhaus book \emph{La Regression
  PLS}, chapter 5.
}
\details{
  No missing data are allowed.
}
\examples{
\dontrun{
 # load data linnerud
 data(linnerud)

 # apply inter-battery method
 my_simpls = simpls(linnerud[,1:3], linnerud[,4:6])

 # plot variables
 plot(my_simpls, what="variables")
 }
}
\author{
  Gaston Sanchez
}
\references{
  Tenenhaus, M. (1998) \emph{La Regression PLS. Theorie et
  Pratique.} Paris: Editions TECHNIP.

  de Jong, S. (1993) SIMPLS: An alternative approach to
  partial least squares regression. \emph{Chemometrics and
  Intelligent Laboratory Systems}, 18: 251-263.
}
\seealso{
  \code{\link{plot.simpls}}, \code{\link{simplsca}}
}

