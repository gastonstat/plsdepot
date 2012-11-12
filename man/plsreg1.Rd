\name{plsreg1}
\alias{plsreg1}
\title{PLS-R1: Partial Least Squares Regression 1}
\usage{
  plsreg1(predictors, response, comps = 2, crosval = TRUE)
}
\arguments{
  \item{predictors}{A numeric matrix or data frame with the
  predictor variables (which may contain missing data).}

  \item{response}{A numeric vector for the reponse
  variable. No missing data allowed.}

  \item{comps}{The number of extracted PLS components (2 by
  default).}

  \item{crosval}{Logical indicating whether
  cross-validation should be performed (\code{TRUE} by
  default). No cross-validation is done if there is missing
  data or if there are less than 10 observations.}
}
\value{
  An object of class \code{"plsreg1"}, basically a list
  with the following elements:

  \item{x.scores}{PLS components (also known as
  T-components)}

  \item{x.loads}{loadings of the predictor variables}

  \item{y.scores}{scores of the response variable (also
  known as U-components)}

  \item{y.loads}{loadings of the response variable}

  \item{cor.xyt}{Correlations between the variables and the
  PLS components}

  \item{raw.wgs}{weights to calculate the PLS scores with
  the deflated matrices of predictor variables}

  \item{mod.wgs}{modified weights to calculate the PLS
  scores with the matrix of predictor variables}

  \item{std.coefs}{Vector of standardized regression
  coefficients}

  \item{reg.coefs}{Vector of regression coefficients (used
  with the original data scale)}

  \item{R2}{Vector of PLS R-squared}

  \item{R2Xy}{explained variance of variables by
  PLS-components}

  \item{y.pred}{Vector of predicted values}

  \item{resid}{Vector of residuals}

  \item{T2}{Table of Hotelling T2 values (used to detect
  atypical observations)}

  \item{Q2}{Table with the cross validation results.
  Includes: PRESS, RSS, Q2, and cummulated Q2. Only
  available when \code{crosval=TRUE}}
}
\description{
  The function plsreg1 performs Partial Least Squares
  Regression for the univariate case (i.e. one response
  variable)
}
\details{
  The minimum number of PLS components (\code{comps}) to be
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
 ## example of PLSR1 with the vehicles dataset
 # predictand variable: price of vehicles
 data(vehicles)

 # apply plsreg1 extracting 2 components (no cross-validation)
 pls1_one = plsreg1(vehicles[,1:12], vehicles[,13,drop=FALSE], comps=2, crosval=FALSE)

 # apply plsreg1 with selection of components by cross-validation
 pls1_two = plsreg1(vehicles[,1:12], vehicles[,13,drop=FALSE], comps=NULL, crosval=TRUE)

 # apply plsreg1 extracting 5 components with cross-validation
 pls1_three = plsreg1(vehicles[,1:12], vehicles[,13,drop=FALSE], comps=5, crosval=TRUE)

 # plot variables
 plot(pls1_one)
 }
}
\author{
  Gaston Sanchez
}
\references{
  Geladi, P., and Kowalski, B. (1986) Partial Least Squares
  Regression: A Tutorial. \emph{Analytica Chimica Acta},
  \bold{185}, pp. 1-17.

  Tenenhaus, M. (1998) \emph{La Regression PLS. Theorie et
  Pratique}. Editions TECHNIP, Paris.

  Tenenhaus, M., Gauchi, J.-P., and Menardo, C. (1995)
  Regression PLS et applications. \emph{Revue de
  statistique appliquee}, \bold{43}, pp. 7-63.
}
\seealso{
  \code{\link{plot.plsreg1}}, \code{\link{plsreg2}}.
}

