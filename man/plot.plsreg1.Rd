\name{plot.plsreg1}
\alias{plot.plsreg1}
\title{Plot PLS-R1 basic results}
\usage{
  \method{plot}{plsreg1} (x, what = "variables",
    comps = c(1, 2), where = c("t", "t"), cex = 1,
    col.xlabels = "#5592e3", col.ylabel = "#fe9429",
    yfont = 2, pos = NULL, offset = 0.1,
    col.xarrows = "#5b9cf255", col.yarrows = "#fe942955",
    lwd = 3, length = 0, angle = 0, col.points = "#5592e3",
    pch = 21, pt.bg = "#5b9cf255", show.names = FALSE,
    xpd = TRUE, xlab = NULL, ylab = NULL, main = NULL,
    col.main = "gray35", cex.main = 1.2,
    col.axis = "gray40", show.grid = TRUE,
    col.grid = "gray95", ...)
}
\arguments{
  \item{x}{An object of class \code{"plsreg1"}.}

  \item{what}{What to plot. Options are \code{"variables"}
  and \code{"observations"}.}

  \item{comps}{An integer vector of length two to indicate
  which components to plot.}

  \item{where}{Where to plot the observations. A character
  vector of length two to indicate which components to use
  when plotting observations. This parameter will take into
  account the values in \code{comps}. Possible options are:
  \code{c("t","u")} for using x-y components,
  \code{c("t","t")}, for using x components, and
  \code{c("u","u")} for using y components. Default
  \code{c("t","t")}.}

  \item{cex}{Character expansion for labels and points.}

  \item{col.xlabels}{Color for labels of X-block
  variables.}

  \item{col.ylabel}{Color for labels of Y-block variables.}

  \item{yfont}{Integer for specifying which font to use for
  Y-block labels. See \code{font} in graphical parameters
  \code{\link{par}}.}

  \item{pos}{Position for the text (see graphical
  paramaters \code{\link{par}}).}

  \item{offset}{When \code{pos} is specified, this value
  gives the offset of the labels.}

  \item{col.xarrows}{Color for the X-block arrows.}

  \item{col.yarrows}{Color for the Y-block arrows.}

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
  labels of points. Only used when
  \code{what="observations"}.}

  \item{xpd}{Logical for controlling clipping region of
  names and labels.}

  \item{xlab}{A title for the x axis.}

  \item{ylab}{A title for the y axis.}

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
  Plot method for objects of class \code{"plsreg1"}. This
  function plots either the variables or the observations,
  on the selected components (i.e. scores). Variables are
  plotted inside the circle of correlations. Observations
  are plotted on a scatter plot.
}
\details{
  Variables are displayed using the correlations in
  \code{$cor.xyt}.
}
\examples{
\dontrun{
 # load dataset cornell
 data(cornell)

 # apply plsreg1
 myplsr1 = plsreg1(cornell[,1:7], cornell[,8,drop=FALSE])

 # plot variables (circle of correlations)
 plot(myplsr1, what="variables")

 # plot observations (as points) using components (t1,u1)
 plot(myplsr1, what="observations", comps=c(1,1), where=c("t","u"))

 # plot observations with names using components (t1,u1)
 plot(myplsr1, what="observations", comps=c(1,1),
      where=c("t","u"), show.names=TRUE)

 # plot observations (as points) using components (t1,t2)
 plot(myplsr1, what="observations", comps=c(1,2), where=c("t","t"))

 # plot observations (as points) using components (u1,u2)
 plot(myplsr1, what="observations", comps=c(1,2), where=c("u","u"))
 }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{plsreg1}}
}

