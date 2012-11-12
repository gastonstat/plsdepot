\name{nipals}
\alias{nipals}
\title{NIPALS: Non-linear Iterative Partial Least Squares}
\usage{
  nipals(Data, comps = 2, scaled = TRUE)
}
\arguments{
  \item{Data}{A numeric matrix or data frame (which may
  contain missing values).}

  \item{comps}{Number of components to be calculated (by
  default 2)}

  \item{scaled}{A logical value indicating whether to scale
  the data (\code{TRUE} by default).}
}
\value{
  An object of class \code{"nipals"}, basically a list with
  the following elements:

  When the analyzed data contain missing values, the help
  interpretation tools (e.g. \code{cor.xt, disto, contrib,
  cos, dmod}) may not be meaningful, that is to say, some
  of the results may not be coherent.

  \item{values}{The pseudo eigenvalues}

  \item{scores}{The extracted scores (i.e. components)}

  \item{loadings}{The loadings}

  \item{cor.xt}{Correlations between the variables and the
  scores}

  \item{disto}{Squared distance of the observations to the
  origin}

  \item{contrib}{Contributions of the observations (rows)}

  \item{cos}{Squared cosinus}

  \item{dmod}{Distance to the Model}
}
\description{
  Principal Components Analysis with NIPALS algorithm
}
\details{
  The function \code{nipals} performs Principal Components
  Analysis of a data matrix that may contain missing
  values.
}
\examples{
\dontrun{
 # load datasets carscomplete and carsmissing
 data(carscomplete) # complete data
 data(carsmissing)  # missing values

 # apply nipals
 my_nipals1 = nipals(carscomplete)
 my_nipals2 = nipals(carsmissing)

 # plot variables (circle of correlations)
 plot(my_nipals1, what="variables", main="Complete data")
 plot(my_nipals2, what="variables", main="Missing data")

 # plot observations with labels
 plot(my_nipals1, what="observations", show.names=TRUE, main="Complete data")
 plot(my_nipals2, what="observations", show.names=TRUE, main="Missing data")

 # compare results between my_nipals1 and my_nipals2
 plot(my_nipals1$scores[,1], my_nipals2$scores[,1], type="n")
 title("Scores comparison: my_nipals1  -vs-  my_nipals2", cex.main=0.9)
 abline(a=0, b=1, col="gray85", lwd=2)
 points(my_nipals1$scores[,1], my_nipals2$scores[,1], pch=21,
        col="#5592e3", bg = "#5b9cf277", lwd=1.5)
 }
}
\author{
  Gaston Sanchez
}
\references{
  Tenenhaus, M. (1998) \emph{La Regression PLS. Theorie et
  Pratique.} Paris: Editions TECHNIP.

  Tenenhaus, M. (2007) \emph{Statistique. Methodes pour
  decrire, expliquer et prevoir}. Paris: Dunod.
}
\seealso{
  \code{\link{plot.nipals}}, \code{\link{plsreg1}}
}

