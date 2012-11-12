\name{cornell}
\alias{cornell}
\docType{data}
\title{Cornell data set}
\description{
  Data describing the composition of 12 octane mixtures (units measured in proportions)
}
\usage{
data(cornell)
}
\format{
  A data frame with 12 observations on the following 8 variables.

  \tabular{lll}{
    \emph{Num} \tab \emph{Variable}	\tab \emph{Description}		\cr
    1   \tab Distillation 	\tab Direct Distillation	\cr
    2   \tab Reformed 		\tab Reformed	\cr
    3   \tab NaphthaTher 	\tab Thermal cracking Naphtha	\cr
    4   \tab NaphthaCat	 	\tab Catalytic cracking Naphtha		\cr
    5   \tab Polymer		\tab Polymer 	\cr
    6   \tab Alkylat		\tab Alkylat 	\cr
    7   \tab NatEssence		\tab Natural Essence	\cr
    8   \tab Octane			\tab Octane		\cr
  }
}
\source{
  Tenenhaus M. (1998) Table 6, page 78
}
\references{
  Tenenhaus M. (1998) \emph{La Regression PLS: theorie et pratique} Paris: Editions Technip 
}
\examples{
data(cornell)
head(cornell)
}
\keyword{datasets}
