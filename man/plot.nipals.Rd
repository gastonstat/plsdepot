\name{plot.nipals}
\alias{plot.nipals}
\title{Plot NIPALS basic results}
\usage{
  \method{plot}{nipals} (x, what = "variables",
    comps = c(1, 2), cex = 1, col.labels = "#5592e3",
    pos = NULL, offset = 0.1, col.arrows = "#5b9cf255",
    lwd = 3.5, length = 0, angle = 0,
    col.points = "#5592e3", pch = 21, pt.bg = "#5b9cf255",
    show.names = FALSE, xpd = TRUE, xlab = NULL,
    ylab = NULL, main = NULL, col.main = "gray35",
    cex.main = 1.2, col.axis = "gray40", show.grid = TRUE,
    col.grid = "gray95", ...)
}
\arguments{
  \item{x}{An object of class \code{"nipals"}.}

  \item{what}{What to plot. Options are \code{"variables"}
  and \code{"observations"}}

  \item{comps}{An integer vector of length two to indicate
  which components to plot}

  \item{cex}{Character expansion for labels and points.}

  \item{col.labels}{Color for labels of variables.}

  \item{pos}{Position for the labels text (see
  \code{\link{par}}).}

  \item{offset}{When \code{pos} is specified, this value
  gives the offset of the labels.}

  \item{col.arrows}{Color for the arrows when plotting
  variables.}

  \item{lwd}{The line width of arrows.}

  \item{length}{Length of the edges of the arrow head (in
  inches).}

  \item{angle}{Angle from the shaft of the arrow to the
  edge of the arrow head.}

  \item{col.points}{Color for the points when
  \code{what="observations"}.}

  \item{pch}{Plotting character symbol to use (see
  \code{\link{par}}).}

  \item{pt.bg}{Background (fill) color for the points given
  by \code{pch=21:25}.}

  \item{show.names}{Logical indicating whether to show
  names of points. Only used when
  \code{what="observations"}}

  \item{xpd}{Logical for controlling clipping region of
  labels and names.}

  \item{xlab}{Title for the x axis.}

  \item{ylab}{Title for the y axis.}

  \item{main}{Main title of the plot.}

  \item{col.main}{Color of main title.}

  \item{cex.main}{Character expansion of main title.}

  \item{col.axis}{Color of axis annotations (tick marks and
  labels).}

  \item{show.grid}{Logical indicating whether to show grid
  lines.}

  \item{col.grid}{Color of grid lines. Only used when
  \code{show.grid=TRUE}.}

  \item{\dots}{Further arguments are passed to labels or
  points.}
}
\description{
  Plot method for objects of class \code{"nipals"}. This
  function plots either the variables or the observations,
  on the selected components (i.e. scores). Variables are
  plotted inside the circle of correlations. Observations
  are plotted on a scatter plot.
}
\details{
  Variables are displayed using the correlations in
  \code{$cor.xt}.
}
\examples{
\dontrun{
 # load data climbing ropes
 data(ropes)

 # apply nipals with 3 components
 nip1 = nipals(ropes[,-1], comps=3)

 # plot variables (correlations)
 plot(nip1)

 # plot observations
 plot(nip1, what="obs")

 # plot observations with names
 plot(nip1, what="obs", show.names=TRUE)
 }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{nipals}}
}

