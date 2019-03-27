#' Predict method for Partial Least Squares Regression 1 (plsreg1)
#'
#' @param object plsreg1 object.
#' @param newdata An data frame in which to look for variables with which to predict.
#' @param ... other parameters will be ignore.
#'
#' @examples
#' \dontrun{
#' ## example of PLSR1 with the vehicles dataset
#' # predictand variable: price of vehicles
#' library(pls)
#' data(vehicles)
#'
#' d <- data.frame(y = vehicles[,13, drop = FALSE])
#' d$x = vehicles[,1:12] %>% as.matrix()
#'
#' m_pls <- plsr(price~x, data = d, scale = TRUE, validation = "CV")
#' # phenofit::GOF(d$price, m_pls$fitted.values[,,2])
#'
#' # apply plsreg1 extracting 2 components (no cross-validation)
#' pls1_one = plsreg1(vehicles[,1:12], vehicles[,13,drop=FALSE], comps=2, crosval=FALSE)
#' ysim <- predict.plsreg1(pls1_one)
#' # phenofit::GOF(d$price, ysim, include.r = T)
#' }
#' @import magrittr
#' @export
predict.plsreg1 <- function(object, newdata, ...){
    Xx <- object$INPUT$Xx
    Yy <- object$INPUT$Yy

    p  <- ncol(Xx)
    n  <- nrow(Xx)

    Bs <- object$std.coefs
    Br <- object$reg.coefs
    Th <- object$x.scores
    Ph <- object$x.loads

    mu.x <- attributes(Xx)$"scaled:center" #%>% as.matrix(ncol = 1)
    sd.x <- attributes(Xx)$"scaled:scale"  #%>% as.matrix(ncol = 1)
    mu.y <- attributes(Yy)$"scaled:center"
    sd.y <- attributes(Yy)$"scaled:scale"

    if (missing(newdata)){
        X <- Xx %*% diag(sd.x, p, p) + matrix(mu.x, n, p, byrow = T) # back to real X, x*sd + mu
    } else {
        X <- as.matrix(newdata)
    }

    slp   <- slope(X)
    y.hat <- as.vector(cbind(1, X) %*% object$reg.coefs)

    # add contribution analysis
    
    # na.miss <- ifelse(any(is.na(X)), TRUE, FALSE)

        # if (!na.miss) {
        # Br    = Bs * (rep(sd.y, p)/sd.x)                  # beta coeffs
        # cte   = as.vector(mu.y - Br %*% mu.x)             # intercept
        # y.hat = as.vector(X %*% Br + cte)                 # y predicted
    # } else {
    #     # error at this part
    #     X.hat = Th %*% t(Ph) %*% diag(sd.x, p, p) + matrix(mu.x, n, p, byrow = T)
    #
    #     Br = Bs * (rep(sd.y, p)/sd.x)          # beta coeffs (unstandardized)
    #     cte = as.vector(mu.y - Br %*% mu.x)    # intercept
    #     y.hat = as.vector(X.hat %*% Br + cte)
    # }
    y.hat # return
}
