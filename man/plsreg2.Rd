\name{plsreg2}
\alias{plsreg2}
\title{PLS-R2: Partial Least Squares Regression 2}
\usage{
  plsreg2(predictors, responses, comps = 2, crosval = TRUE)
}
\arguments{
  \item{predictors}{A numeric matrix or data frame
  containing the predictor variables.}

  \item{responses}{A numeric matrix or data frame
  containing the response variables.}

  \item{comps}{The number of extracted PLS components (2 by
  default)}

  \item{crosval}{Logical indicating whether
  cross-validation should be performed (\code{TRUE} by
  default). No cross-validation is done if there is missing
  data or if there are less than 10 observations.}
}
\value{
  An object of class \code{"plsreg2"}, basically a list
  with the following elements:

  \item{x.scores}{components of the predictor variables
  (also known as T-components)}

  \item{x.loads}{loadings of the predictor variables}

  \item{y.scores}{components of the response variables
  (also known as U-components)}

  \item{y.loads}{loadings of the response variables}

  \item{cor.xt}{correlations between X and T}

  \item{cor.yt}{correlations between Y and T}

  \item{cor.xu}{correlations between X and U}

  \item{cor.yu}{correlations between Y and U}

  \item{cor.tu}{correlations between T and U}

  \item{raw.wgs}{weights to calculate the PLS scores with
  the deflated matrices of predictor variables}

  \item{mod.wgs}{modified weights to calculate the PLS
  scores with the matrix of predictor variables}

  \item{std.coefs}{Vector of standardized regression
  coefficients (used with scaled data)}

  \item{reg.coefs}{Vector of regression coefficients (used
  with the original data)}

  \item{y.pred}{Vector of predicted values}

  \item{resid}{Vector of residuals}

  \item{expvar}{table with R-squared coefficients}

  \item{VIP}{Variable Importance for Projection}

  \item{Q2}{table of Q2 indexes (i.e. leave-one-out cross
  validation)}

  \item{Q2cum}{table of cummulated Q2 indexes}
}
\description{
  The function plsreg2 performs partial least squares
  regression for the multivariate case (i.e. more than one
  response variable)
}
\details{
  The minimum number of PLS components \code{comps} to be
  extracted is 2.

  The data is scaled to standardized values (mean=0,
  variance=1).

  The argument \code{crosval} gives the option to perform
  cross-validation. This parameter takes into account how
  \code{comps} is specified. When \code{comps=NULL}, the
  number of components is obtained by cross-validation.
  When a number of components is specified,
  cross-validation results are calculated for each
  component.
}
\examples{
\dontrun{
 ## example of PLSR2 with the vehicles dataset
 data(vehicles)

 # apply plsreg2 extracting 2 components (no cross-validation)
 pls2_one = plsreg2(vehicles[,1:12], vehicles[,13:16], comps=2, crosval=FALSE)

 # apply plsreg2 with selection of components by cross-validation
 pls2_two = plsreg2(vehicles[,1:12], vehicles[,13:16], comps=NULL, crosval=TRUE)

 # apply plsreg2 extracting 5 components with cross-validation
 pls2_three = plsreg2(vehicles[,1:12], vehicles[,13:16], comps=5, crosval=TRUE)

 # plot variables
 plot(pls2_one)
 }
}
\author{
  Gaston Sanchez
}
\references{
  Geladi, P., and Kowlaski, B. (1986) Partial Least Squares
  Regression: A Tutorial. \emph{Analytica Chimica Acta},
  \bold{185}, pp. 1-17.

  Hoskuldsson, A. (1988) PLS Regression Methods.
  \emph{Journal of Chemometrics}, \bold{2}, pp. 211-228.

  Tenenhaus, M. (1998) \emph{La Regression PLS. Theorie et
  Pratique}. Editions TECHNIP, Paris.
}
\seealso{
  \code{\link{plot.plsreg2}}, \code{\link{plsreg1}}.
}

