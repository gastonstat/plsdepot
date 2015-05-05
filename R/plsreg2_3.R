plsreg2_3<-function (predictors, responses, comps = 2, crosval = TRUE) 
{
    X = as.matrix(predictors)
    if (any(is.na(X))) 
        stop("\nNo missing data are allowed")
    n = nrow(X)
    p = ncol(X)
    if (is.null(colnames(X))) 
        colnames(X) = paste(rep("X", p), 1:p, sep = "")
    if (is.null(rownames(X))) 
        rownames(X) = 1:n
    Y = as.matrix(responses)
    if (any(is.na(Y))) 
        stop("\nNo missing data are allowed")
    if (nrow(X) != nrow(Y)) 
        stop("\ndifferent number of rows in predictors and responses")
    q = ncol(Y)
    if (is.null(colnames(Y))) 
        colnames(Y) = paste(rep("Y", q), 1:q, sep = "")
    if (is.null(rownames(Y))) 
        rownames(Y) = 1:n
    if (p < 2 || q < 2) 
        stop("predictors and responses must have more than one column")
    if (!is.null(comps)) {
        nc = comps
        if (mode(nc) != "numeric" || length(nc) != 1 || nc <= 
            1 || (nc%%1) != 0 || nc > min(n, p)) 
            nc = min(n, p)
        if (nc == n) 
            nc = n - 1
    }
    if (!is.logical(crosval)) 
        crosval = FALSE
    else {
        if (n >= 10) {
            crosval = TRUE
            nc = min(n, p)
        }
        else {
            crosval = FALSE
            nc = 2
            message("\nSorry, no cross-validation with less than 10 observations")
        }
    }
    X.old = scale(X)
    Y.old = scale(Y)
    Wh = matrix(0, p, nc)
    Uh = matrix(0, n, nc)
    Th = matrix(0, n, nc)
    Ch = matrix(0, q, nc)
    Ph = matrix(0, p, nc)
    bh = rep(0, nc)
    if (crosval) {
        RSS = rbind(rep(n - 1, q), matrix(NA, nc, q))
        PRESS = matrix(NA, nc, q)
        Q2 = matrix(NA, nc, q)
        sets_size = c(rep(n%/%10, 9), n - 9 * (n%/%10))
        obs = sample(1:n, size = n)
        segments = vector("list", length = 10)
        ini = cumsum(sets_size) - sets_size + 1
        fin = cumsum(sets_size)
        for (k in 1:10) segments[[k]] = obs[ini[k]:fin[k]]
    }
    h = 1
    repeat {
        u.new = Y.old[, 1]
        w.old = rep(1, p)
        iter = 1
        repeat {
            w.new = t(X.old) %*% u.new/sum(u.new^2)
            w.new = w.new/sqrt(sum(w.new^2))
            t.new = X.old %*% w.new
            c.new = t(Y.old) %*% t.new/sum(t.new^2)
            u.new = Y.old %*% c.new/sum(c.new^2)
            w.dif = w.new - w.old
            w.old = w.new
            if (sum(w.dif^2) < 1e-06 || iter == 100) 
                break
            iter = iter + 1
        }
        p.new = t(X.old) %*% t.new/sum(t.new^2)
        if (crosval) {
            RSS[h + 1, ] = colSums((Y.old - t.new %*% t(c.new))^2)
            press = matrix(0, 10, q)
            for (i in 1:10) {
                aux = segments[[i]]
                uh.si = Y.old[-aux, 1]
                wh.siold = rep(1, p)
                itcv = 1
                repeat {
                    wh.si = t(X.old[-aux, ]) %*% uh.si/sum(uh.si^2)
                    wh.si = wh.si/sqrt(sum(wh.si^2))
                    th.si = X.old[-aux, ] %*% wh.si
                    ch.si = t(Y.old[-aux, ]) %*% th.si/sum(th.si^2)
                    uh.si = Y.old[-aux, ] %*% ch.si/sum(ch.si^2)
                    wsi.dif = wh.si - wh.siold
                    wh.siold = wh.si
                    if (sum(wsi.dif^2) < 1e-06 || itcv == 100) 
                        break
                    itcv = itcv + 1
                }
                Yhat.si = (X.old[aux, ] %*% wh.si) %*% t(ch.si)
                press[i, ] = colSums((Y.old[aux, ] - Yhat.si)^2)
            }
            PRESS[h, ] = colSums(press)
            Q2[h, ] = 1 - (PRESS[h, ]/RSS[h, ])
        }
        X.old = X.old - (t.new %*% t(p.new))
        Y.old = Y.old - (t.new %*% t(c.new))
        Wh[, h] = w.new
        Uh[, h] = u.new
        Th[, h] = t.new
        Ch[, h] = c.new
        Ph[, h] = p.new
        bh[h] = t(u.new) %*% t.new
        if (is.null(comps) && crosval) {
            if (sum(Q2[h, ] < 0.0975) == q || h == nc) 
                break
        }
        else {
            if (h == nc) 
                break
        }
        h = h + 1
    }
    Th = Th[, 1:h]
    Ph = Ph[, 1:h]
    Wh = Wh[, 1:h]
    Uh = Uh[, 1:h]
    Ch = Ch[, 1:h]
    Ph = Ph[, 1:h]
    Ws = Wh %*% solve(t(Ph) %*% Wh)
    Bs = Ws %*% t(Ch)
    Br = diag(1/apply(X, 2, sd)) %*% Bs %*% diag(apply(Y, 2, 
                                                       sd))
    cte = as.vector(apply(Y, 2, mean) - apply(X, 2, mean) %*% 
                        Br)
    Y.hat = X %*% Br + matrix(rep(cte, each = n), n, q)
    resids = Y - Y.hat
    cor.xt = cor(X, Th)
    cor.yt = cor(Y, Th)
    cor.tu = cor(Th, Uh)
    cor.xu = cor(X, Uh)
    cor.yu = cor(Y, Uh)
    R2x = cor(X, Th)^2
    R2y = cor(Y, Th)^2
    Rdx = colMeans(R2x)
    Rdy = colMeans(R2y)
    EV = cbind(Rdx, cumsum(Rdx), Rdy, cumsum(Rdy))
    Rd.mat = matrix(0, h, h)
    for (j in 1:h) Rd.mat[1:j, j] = Rdy[1:j]
    VIP = sqrt((Wh^2) %*% Rd.mat %*% diag(p/cumsum(Rdy), h, h))
    if (crosval) {
        PRESS = PRESS[1:h, ]
        RSS = RSS[1:(h + 1), ]
        Q2 = Q2[1:h, ]
        Q2G = 1 - (rowSums(PRESS)/rowSums(RSS[1:h, ]))
        Q2cum = Q2
        Q2cum[1, ] = 1 - (PRESS[1, ]/RSS[1, ])
        for (i in 2:h) Q2cum[i, ] = 1 - apply(PRESS[1:i, ]/RSS[1:i, 
                                                               ], 2, prod)
        Q2Gcum = Q2G
        for (i in 1:h) Q2Gcum[i] = 1 - prod((rowSums(PRESS)/rowSums(RSS[-h, 
                                                                        ]))[1:i])
        Q2T = cbind(Q2, Q2G)
        Q2TC = cbind(Q2cum, Q2Gcum)
        q2 = c(paste(rep("Q2", q), colnames(Y), sep = "."), "Q2")
        q2c = c(paste(rep("Q2cum", q), colnames(Y), sep = "."), 
                "Q2cum")
        dimnames(Q2T) = list(paste(rep("t", h), 1:h, sep = ""), 
                             q2)
        dimnames(Q2TC) = list(paste(rep("t", h), 1:h, sep = ""), 
                              q2c)
    }
    else {
        Q2T = NULL
        Q2TC = NULL
    }
    dimnames(Wh) = list(colnames(X), paste(rep("w", h), 1:h, 
                                           sep = ""))
    dimnames(Ws) = list(colnames(X), paste(rep("w*", h), 1:h, 
                                           sep = ""))
    dimnames(Uh) = list(rownames(Y), paste(rep("u", h), 1:h, 
                                           sep = ""))
    dimnames(Th) = list(rownames(X), paste(rep("t", h), 1:h, 
                                           sep = ""))
    dimnames(Ch) = list(colnames(Y), paste(rep("c", h), 1:h, 
                                           sep = ""))
    dimnames(Ph) = list(colnames(X), paste(rep("p", h), 1:h, 
                                           sep = ""))
    dimnames(Bs) = list(colnames(X), colnames(Y))
    dimnames(Br) = list(colnames(X), colnames(Y))
    dimnames(cor.xt) = list(colnames(X), paste(rep("t", h), 1:h, 
                                               sep = ""))
    dimnames(cor.yt) = list(colnames(Y), paste(rep("t", h), 1:h, 
                                               sep = ""))
    dimnames(cor.xu) = list(colnames(X), paste(rep("u", h), 1:h, 
                                               sep = ""))
    dimnames(cor.yu) = list(colnames(Y), paste(rep("u", h), 1:h, 
                                               sep = ""))
    dimnames(cor.tu) = list(paste(rep("t", h), 1:h, sep = ""), 
                            paste(rep("u", h), 1:h, sep = ""))
    dimnames(EV) = list(paste(rep("t", h), 1:h, sep = ""), c("R2X", 
                                                             "R2Xcum", "R2Y", "R2Ycum"))
    dimnames(Y.hat) = list(rownames(Y), colnames(Y))
    dimnames(resids) = list(rownames(Y), colnames(Y))
    dimnames(VIP) = list(colnames(X), paste(rep("t", h), 1:h, 
                                            sep = ""))
    coeffs = rbind(Br, INTERCEPT = cte)
    structure(list(x.scores = Th, x.loads = Ph, y.scores = Uh, 
                   y.loads = Ch, cor.xt = cor.xt, cor.yt = cor.yt, cor.xu = cor.xu, 
                   cor.yu = cor.yu, cor.tu = cor.tu, raw.wgs = Wh, mod.wgs = Ws, 
                   std.coefs = Bs, reg.coefs = coeffs, y.pred = Y.hat, resid = resids, 
                   expvar = EV, VIP = VIP, Q2 = Q2T, Q2cum = Q2TC), class = "plsreg2")
}