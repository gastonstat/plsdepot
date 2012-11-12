\name{vehicles}
\alias{vehicles}
\docType{data}
\title{Vehicles data set}
\description{
  These data are the specification of 30 vehicles in terms of various characteristics.
}
\usage{
vehicles
}
\format{
  A data frame with 30 observations and 16 variables. 

  \tabular{lll}{
    \emph{Num} \tab \emph{Variable}	\tab \emph{Description}		\cr
    1   \tab diesel 		\tab Diesel fuel-type 			\cr
    2   \tab turbo 		\tab Turbo aspiration  			\cr
    3   \tab two.doors 		\tab Vechicles with two doors		\cr
    4   \tab hatchback	 	\tab Hatchback body-style		\cr
    5   \tab wheel.base 	\tab Wheel base 			\cr
    6   \tab length		\tab Length  				\cr
    7   \tab width		\tab Width 				\cr
    8   \tab height		\tab Height				\cr
    9   \tab curb.weight	\tab Curb weight			\cr
    10  \tab eng.size		\tab Engine size			\cr
    11  \tab horsepower		\tab Horsepower				\cr
    12  \tab peak.rpm		\tab Peak revolutions per minute	\cr
    13  \tab price		\tab Price in dollars			\cr
    14  \tab symbol		\tab Insurance risk rating		\cr
    15  \tab city.mpg		\tab Fuel consume in city		\cr
    16  \tab highway.mpg	\tab Fuel consume in highway		\cr
  }
}
\source{
  1) 1985 Model Import Car and Truck Specifications, 1985 Ward's Automotive Yearbook. \cr
  2) Personal Auto Manuals, Insurance Services Office, 160 Water Street, New York, NY 10038. \cr
  3) Insurance Collision Report, Insurance Institute for Highway Safety, Watergate 600, Washington, DC 20037. \cr

  Machine Learning Repository. \url{http://archive.ics.uci.edu/ml/datasets/Automobile}
}
\examples{
data(vehicles)
vehicles
}
\keyword{datasets}
