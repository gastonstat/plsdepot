\name{carsmissing}
\alias{carsmissing}
\docType{data}
\title{carsmissing data set}
\description{
  Data set with six variables measured on 24 cars
}
\usage{
data(carsmissing)
}
\format{
  A data frame with 6 variables measured on the 24 cars (contains missing values).

  \tabular{lll}{
    \emph{Num} \tab \emph{Variable}	\tab \emph{Description}		\cr
    1   \tab Cylindree 	\tab Engine		\cr
    2   \tab Puissance 	\tab Power  	\cr
    3   \tab Vitese 	\tab Speed		\cr
    4   \tab Poids	 	\tab Weight		\cr
    5   \tab Longueur 	\tab Length		\cr
    6   \tab Largeur	\tab Height		\cr
  }
}
\details{
  This is data contains one missing value in each row
}
\source{
  Tenenhaus M. (1998) \emph{La Regression PLS: theorie et pratique}. Table 5, page 69
}
\references{
  Tenenhaus M. (1998) \emph{La Regression PLS: theorie et pratique} Paris: Editions Technip 
}
\examples{
data(carsmissing)
head(carsmissing)
}
\keyword{datasets}
