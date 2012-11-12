\name{linnerud}
\alias{linnerud}
\docType{data}
\title{Linnerud data set}
\description{
  Thris data set contains three physiological and three exercise variables measured on 20 men
}
\usage{
data(linnerud)
}
\format{
  A data frame with 20 observations on the following 6 variables.

  \tabular{lll}{
    \emph{Num} \tab \emph{Variable}	\tab \emph{Description}		\cr
    1   \tab Weight \tab Weight		\cr
    2   \tab Waist 	\tab Waist size \cr
    3   \tab Pulse 	\tab Pulse		\cr
    4   \tab Pulls	\tab Pull-ups	\cr
    5   \tab Squats \tab Squats		\cr
    6   \tab Jumps	\tab Jumps		\cr
  }
}
\source{
  Tenenhaus M. (1998) Table 1, page 15
}
\references{
  Tenenhaus M. (1998) \emph{La Regression PLS: theorie et pratique} Paris: Editions Technip 
}
\examples{
data(linnerud)
head(linnerud)
}
\keyword{datasets}
