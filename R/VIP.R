#' Variable Influence on Projection (VIP)
#' 
#' @param object \code{plsreg1} or \code{plsreg2} object.
#' @references
#' [1]. http://mevik.net/work/software/VIP.R
#' @export
VIP_plsreg <- function(object){
  SS     <- c(object$y.loads)^2 * colSums(object$x.scores^2)
  Wnorm2 <- colSums(object$raw.wgs^2)
  SSW    <- sweep(object$raw.wgs^2, 2, SS / Wnorm2, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}
