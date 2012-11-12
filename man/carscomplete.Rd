\name{carscomplete}
\alias{carscomplete}
\docType{data}
\title{carscomplete data set}
\description{
  Data set with six variables measured on 24 cars (no missing values)
}
\usage{
data(carscomplete)
}
\format{
  A data frame with 6 variables measured on the 24 cars.

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
  This is data with no missing values
}
\source{
  Tenenhaus M. (1998) \emph{La Regression PLS: theorie et pratique}. Table 4, page 67
}
\references{
  Tenenhaus M. (1998) \emph{La Regression PLS: theorie et pratique} Paris: Editions Technip 
}
\examples{
data(carscomplete)
head(carscomplete)
}
\keyword{datasets}
