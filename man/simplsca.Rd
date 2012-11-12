\name{simplsca}
\alias{simplsca}
\title{SIMPLS-CA: SIMPLS Canonical Analysis}
\usage{
  simplsca(X, Y, comps = 2)
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
  An object of class \code{"simplsca"}, basically a list
  with the following elements:

  \item{x.scores}{scores of the X-block (also known as T
  components)}

  \item{x.wgs}{weights of the X-block}

  \item{y.scores}{scores of the Y-block (also known as U
  components)}

  \item{y.wgs}{weights of the Y-block}

  \item{cor.xt}{correlations between X and T}

  \item{cor.yu}{correlations between Y and U}

  \item{cor.xu}{correlations between X and U}

  \item{cor.yt}{correlations between Y and T}

  \item{cor.tu}{correlations between T and U}

  \item{R2XT}{explained variance of X by T}

  \item{R2YT}{explained variance of Y by T}

  \item{R2YU}{explained variance of Y by U}

  \item{R2XU}{explained variance of X by U}
}
\description{
  The function \code{simplsca} performs the SIMPLS
  Canonical Analysis algorithm as described in Michel
  Tenenhaus book \emph{La Regression PLS}, chapter 5.
}
\details{
  No missing data are allowed.
}
\examples{
\dontrun{
 # load data linnerud
 data(linnerud)

 # apply inter-battery method
 my_simca = simplsca(linnerud[,1:3], linnerud[,4:6])

 # plot variables
 plot(my_simca, what="variables")
 }
}
\author{
  Gaston Sanchez
}
\references{
  Tenenhaus, M. (1998) \emph{La Regression PLS. Theorie et
  Pratique.} Paris: Editions TECHNIP.
}
\seealso{
  \code{\link{plot.simplsca}}, \code{\link{simpls}}
}

