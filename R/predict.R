#' Predict method for Partial Least Squares Regression 1 (plsreg1)
#' 
#' @param object plsreg1 object.
#' @param newdata An data frame in which to look for variables with which to predict.
#' @param ... other parameters will be ignore.
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
        X <- Xx %*% diag(sd.x, p, p) + matrix(mu.x, n, p, byrow = T) # back to real X
    } else {
        X <- as.matrix(newdata)    
    }
    
    na.miss <- ifelse(any(is.na(X)), TRUE, FALSE)

    if (!na.miss) {
        Br    = Bs * (rep(sd.y, p)/sd.x)                  # beta coeffs
        cte   = as.vector(mu.y - Br %*% mu.x)             # intercept
        y.hat = as.vector(X %*% Br + cte)                 # y predicted    
    } else {
        X.hat = Th %*% t(Ph) %*% diag(sd.x, p, p) + matrix(mu.x, n, p, byrow = T)
        
        Br = Bs * (rep(sd.y, p)/sd.x)          # beta coeffs (unstandardized)
        cte = as.vector(mu.y - Br %*% mu.x) # intercept
        y.hat = as.vector(X.hat %*% Br + cte)
    }
    y.hat # return
}
