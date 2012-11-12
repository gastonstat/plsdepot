\name{interbat}
\alias{interbat}
\title{Tucker's Inter-Battery Method of Factor Analysis}
\usage{
  interbat(X, Y, scaled = TRUE)
}
\arguments{
  \item{X}{Numeric matrix or data frame with two or more
  columns (X-block).}

  \item{Y}{Numeric matrix or data frame with two or more
  columns (Y-block).}

  \item{scaled}{Logical value indicating whether to scale
  the data (\code{TRUE} by default).}
}
\value{
  An object of class \code{"interbat"}, basically a list
  with the following elements:

  \item{values}{The extracted eigenvalues}

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

  \item{R2X}{explained variance of X by T}

  \item{R2Y}{explained variance of Y by U}

  \item{com.xu}{communality of X with U}

  \item{com.yt}{communality of Y with T}

  \item{statistic}{Phi statistic values for assessing the
  number of relevant components}
}
\description{
  The function \code{interbat} performs Tucker's
  Inter-Battery method of factor analysis as described in
  Michel Tenenhaus book \emph{La Regression PLS}, chapter 3
}
\details{
  Arguments \code{X} and \code{Y} must contain more than
  one variable. No missing data are allowed.
}
\examples{
\dontrun{
 # load data linnerud
 data(linnerud)

 # apply inter-battery method
 ib = interbat(linnerud[,1:3], linnerud[,4:6])

 # plot variables
 plot(ib, what="variables")

 # plot observations
 plot(ib, what="observations", comps=c(1,1), where=c("t","u"))
 }
}
\author{
  Gaston Sanchez
}
\references{
  Tenenhaus, M. (1998) \emph{La Regression PLS. Theorie et
  Pratique.} Paris: Editions TECHNIP.

  Tucker, L.R. (1958) An inter-battery method of factor
  analysis. \emph{Psychometrika}, 23(2): 111-136.
}
\seealso{
  \code{\link{plot.interbat}}, \code{\link{plsca}}
}

