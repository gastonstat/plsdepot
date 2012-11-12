\name{ropes}
\alias{ropes}
\docType{data}
\title{Climbing Ropes data set}
\description{
  This dataset gives the measurements of 101 climbing ropes available in the market by spring 2011. The data was collected from the brands websites.
}
\usage{
data(ropes)
}
\format{
  A data frame with 101 observations on the following 7 variables.
  \tabular{lll}{
    \emph{Num} \tab \emph{Variable}  \tab \emph{Description}		\cr
    1   \tab brand 	\tab a factor with the brand names	\cr
    2   \tab diameter 		\tab the diameter of the ropes measured in mm	\cr
    3   \tab weight 	\tab the weight measured in grams per meter\cr
    4   \tab falls	 	\tab the number of UIAA falls	\cr
    5   \tab imp.force		\tab the impact force value 	\cr
    6   \tab stat.elong		\tab the static elongation value 	\cr
    7   \tab dyn.elong		\tab the dynamic elongation value	\cr
  }
}
\source{
  Personal collection of data. (Gaston Sanchez)
}
\examples{
data(ropes)
summary(ropes)
}
\keyword{datasets}
