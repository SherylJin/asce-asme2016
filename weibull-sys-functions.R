###############################################################################
#       Functions to calculate and plot sets of reliability curves            #
#       for systems consisting of Weibull components with fixed shape         #
###############################################################################

# provides dinvgamma(x, shape, scale) where shape = alpha and scale = beta
library(actuar)
library(luck)
library(ReliabilityTheory)
library(ggplot2)
library(reshape2)

# translate lambda parameter of weibull to expected failure time
# lambda - scale parameter of weibull
# beta   - shape parameter of weibull
lambdatofailure <- function(lambda, beta = 2){
  lambda^(1/beta) * gamma(1 + 1/beta)
}

# translate failure time to lambda parameter of weibull
# ft   - expected failure time
# beta - shape parameter of weibull
failuretolambda <- function(ft, beta = 2){
  (ft/gamma(1 + 1/beta))^beta
}

# inverse gamma pdf and cdf with canonical parameters n0 and y0
cdinvgamma <- function(x, n0, y0, ...)
  dinvgamma(x, shape = n0+1, scale = n0*y0, ...)

cpinvgamma <- function(x, n0, y0, ...)
  pinvgamma(x, shape = n0+1, scale = n0*y0, ...)

cpigforoptim <- function(ny, t, ...)
  cpinvgamma(t, ny[1], ny[2], ...)

cpigfornoptim <- function(n, y, t, ...)
  cpinvgamma(t, n, y, ...)

# function to calculate P(C_t = l | n^(0), y^(0), data),
# the posterior predictive probability that l components function at time t
# in the system observed until t_now (notion "of type k" dropped here)
# this implements (20)
# n0y0    pair c(n0,y0) of prior parameters
# t       time t for which to calculate P(C_t), t > t_now
# l       number of functioning components, \in {0, 1, ..., n-e}
# n       number of components in the system
# tnow    time until the system is observed
# fts     vector of length e giving the observed failure times,
#         or NULL if no failures observed
# beta    fixed weibull shape parameter
# prior   whether the prior predictive should be calculated,
#         when FALSE, fts is ignored 
postpredC <- function(n0y0, beta, n, fts, tnow, t, l, prior = FALSE){
  if (t < tnow)
    stop("t must be larger than tnow")
  if(!prior){
    e <- length(fts)
    if (n < e)
      stop("there can be at most n failure times!")
  } else {
    e <- 0
  }
  if (l > n-e)
    stop("l can be at most n-e")
  nn <- n0y0[1] + e
  if(!prior)
    nnyn <- n0y0[1]*n0y0[2] + (n-e)*tnow + sum(fts^beta)
  else
    nnyn <- n0y0[1]*n0y0[2]
  j <- seq(0, n-e-l)
  choose(n-e, l) * sum( (-1)^j * choose(n-e-l, j) * (nnyn/(nnyn + (l+j)*(t^beta - tnow^beta)))^(nn + 1))
}

# calculates the probability mass function for C_t
postpredCpmf <- function(n0y0, beta, n, fts, tnow, t, prior = FALSE){
  l <- seq(0, n-length(fts))
  res <- numeric(length(l))
  for (i in l) res[i+1] <- postpredC(n0y0, beta, n, fts, tnow, t, i, prior = prior)
  res <- array(res)
  dimnames(res)[[1]] <- l
  res
}

# calculates the cumulative mass function for C_t
postpredCcmf <- function(n0y0, beta, n, fts, tnow, t, prior = FALSE){
  pmf <- postpredCpmf(n0y0, beta, n, fts, tnow, t, prior = prior)
  cmf <- cumsum(pmf)
  cmf
}

# plots the cumulative mass function for C_t
Ccmfplot <- function(cmf, add = FALSE, ylim = c(0,1), xlab = "l", ylab = "F(C = l)",...){
  if(add)
    lines(as.numeric(names(cmf)), cmf, type="s", ...)
  else
    plot(as.numeric(names(cmf)), cmf, type="s", ylim = ylim, xlab = xlab, ylab = ylab, ...)
}


# plots and prints the cmfs for the four 'corner' (n0,y0) pairs.
fourCornersCcmf <- function(luckobj, beta, n, fts, tnow, t, prior = FALSE){
  n0 <- n0(luckobj)
  y0 <- y0(luckobj)
  tl <- postpredCcmf(n0y0 = c(n0[1], y0[2]), beta = beta, n = n, fts = fts, tnow = tnow, t = t, prior = prior)
  tr <- postpredCcmf(n0y0 = c(n0[2], y0[2]), beta = beta, n = n, fts = fts, tnow = tnow, t = t, prior = prior)
  bl <- postpredCcmf(n0y0 = c(n0[1], y0[1]), beta = beta, n = n, fts = fts, tnow = tnow, t = t, prior = prior)
  br <- postpredCcmf(n0y0 = c(n0[2], y0[1]), beta = beta, n = n, fts = fts, tnow = tnow, t = t, prior = prior)
  Ccmfplot(tl, main = bquote(paste("n0 = [",.(n0[1]),",",.(n0[2]),"], y0 = [",.(round(y0[1],2)),",",.(round(y0[2],2)),"]")))
  Ccmfplot(tr, lty = 2, add = TRUE)
  Ccmfplot(bl, col = 2, add = TRUE)
  Ccmfplot(br, col = 2, lty = 2, add = TRUE)
  fail <- toString(fts)
  cens <- paste(toString(tnow),"+", sep="")
  cens <- paste(rep(cens, n-length(fts)), collapse=",")
  data <- paste(fail, cens, sep=",")
  mtext(paste("n = ",toString(n),", data = (",data,"), t = ",toString(t), sep=""), side = 3, line = 0.5)
  legend("topleft", legend=c("tl","tr","bl","br"), lty=c(1,2,1,2), col=c(1,1,2,2))
  cat("  ", paste(names(tl), collapse = "      "), "\n")
  cat("tl", paste(round(tl,4), collapse = " "), "\n")
  cat("tr", paste(round(tr,4), collapse = " "), "\n")
  cat("bl", paste(round(bl,4), collapse = " "), "\n")
  cat("br", paste(round(br,4), collapse = " "), "\n")
}

# calculates the system reliability / survival
# this implements (13) (last equation in Sec 4.2 using the posterior predictive from 4.3)
#
# n0y0     list of K prior parameter pairs c(n0,y0)
# survsign data frame with the survival signature as output by computeSystemSurvivalSignature()
# beta     vector of K fixed weibull shape parameters
# fts      list of K vectors giving the observed failure times for the compents of type 1,...,K;
#          the list element should be NULL if no failure has been observed for type k
# tnow     time until the system is observed
# t        time t for which to calculate P(T_sys > t), t > t_now
# table    if table of posterior predictive probabilities should be given along with the reliability
# nk       vector of length K giving the number of components of each type,
#          this is needed only when reduced survival signature is used
# prior    whether the prior system relibability should be calculated
sysrel <- function(n0y0, survsign, beta, fts, tnow, t, table = FALSE, nk = NULL, prior = FALSE){
  K <- length(fts)
  if(is.null(nk)){
    nk <- apply(survsign, 2, max)
    nk <- nk[-length(nk)]
  }
  if(!prior)
    ek <- unlist(lapply(fts, length))
  else
    ek <- rep(0, K)
  # sumarray <- array(data = NA, dim = nk-ek+1)
  # dimnames(sumarray) <- lapply(as.list(nk - ek), function(x) as.character(seq(0,x)))
  # signarray <- sumarray
  # only those rows of survsign which have up to nk-ek functioning
  survsign2 <- survsign
  for (k in 1:K)
    survsign2 <- survsign2[survsign2[,k] <= (nk-ek)[k],]
  if (dim(survsign2)[1] > 1)
    survsign2 <- survsign2[survsign2$Probability > 0,]
  for (k in 1:K){ # for loop is probably slow
    survsign2$tmp <- NA
    tmpname <- paste("PCt",k,sep="")
    ncols <- length(survsign2)
    names(survsign2) <- c(names(survsign2)[-ncols], tmpname)
    for (i in 1:dim(survsign2)[1]){
      survsign2[i,ncols] <- postpredC(n0y0 = n0y0[[k]], beta = beta[k], n = nk[k],
                                      fts = fts[[k]], tnow = tnow, t = t, l = survsign2[i,k], prior = prior)
    }
  }
  survsign2$summand <- apply(survsign2[,-(1:K)], 1, prod)
  res <- sum(survsign2$summand)
  if(table)
    return(list(rel = res, table = survsign2))
  else
    return(res)
}

# calculates the system reliability function for the 2*2^K corner priors
# (all combinations of K lower and upper n's, for both lower and upper y)
# luckobjlist list of K of prior luckmodel objetcs
# survsign    data frame with the survival signature as output by computeSystemSurvivalSignature()
# beta        vector of K fixed weibull shape parameters
# fts         list of K vectors giving the observed failure times for the compents of type 1,...,K;
#             the list element should be NULL if no failure has been observed for type k, 
# tnow        time until the system is observed
# tvec        vector of timepoints for which to calculate P(T_sys > t), t > t_now
# nk          vector of length K giving the number of components of each type,
#             this is needed only when reduced survival signature is used
# prior       whether the prior system relibability should be calculated
fourKcornersSysrel <- function(luckobjlist, survsign, beta, fts, tnow, tvec, nk = NULL, prior = FALSE){
  K <- length(luckobjlist)
  ncomb <- 2^K
  combs <- as.list(rep(NA,K))
  for (k in 1:K)
    combs[[k]] <- c(1,2)
  # with all y0[1], jede spalte eine kombination, darunter dann tvec 
  lowercorners <- t(expand.grid(combs)) # transposed!
  NAmat <- matrix(NA, nrow = length(tvec), ncol = ncomb)
  lowercorners <- rbind(lowercorners, NAmat)
  uppercorners <- lowercorners
  # all n0 combinations with lower y's
  for (i in 1:ncomb){ # over columns!
    comblist <- as.list(rep(NA, K))
    for (k in 1:K)    # over rows!
      comblist[[k]] <- c(n0(luckobjlist[[k]])[lowercorners[k,i]], y0(luckobjlist[[k]])[1])
    rvec <- sapply(tvec, FUN = sysrel, n0y0 = comblist,
                   survsign = survsign, beta=beta, fts = fts, tnow = tnow, nk = nk, prior = prior)
    lowercorners[(1:length(rvec))+K,i] <- rvec
  }
  # all n0 combinations with upper y's
  for (i in 1:ncomb){ # over columns!
    comblist <- as.list(rep(NA, K))
    for (k in 1:K)    # over rows!
      comblist[[k]] <- c(n0(luckobjlist[[k]])[lowercorners[k,i]], y0(luckobjlist[[k]])[2])
    rvec <- sapply(tvec, FUN = sysrel, n0y0 = comblist,
                   survsign = survsign, beta=beta, fts = fts, tnow = tnow, nk = nk, prior = prior)
    uppercorners[(1:length(rvec))+K,i] <- rvec
  }
  list(lower = lowercorners, upper = uppercorners)
}

# plots one of the result data frames produced by fourKcornersSysrel()
# tvec    vector of t values, used for fourKcornersSysrel()
# rframe  one of the two data frames produced by fourKcornersSysrel()
# legend  if to print a legend
# add     just adds lines if TRUE
fourKcornersSysrelPlot <- function(tvec, rframe, legend = FALSE, add = FALSE,
                                   col = NULL, lty = NULL, ylim = c(0,1), ...){
  plotmat <- rframe[dimnames(rframe)[[1]] == "",]
  combmat <- rframe[dimnames(rframe)[[1]] != "",]
  K <- dim(combmat)[1]
  ncomb <- dim(combmat)[2]
  if(is.null(col)) col <- 1
  if(is.null(lty)) lty <- 1
  if(!add)
    matplot(tvec, plotmat, type = "l", col = col, lty = lty, ylim = ylim, ...)
  else
    matlines(tvec, plotmat, col = col, lty = lty, ...)
  if(legend){
    #if(is.null(col) & is.null(lty))
    #  stop("supply at least one of col and lty to get a legend")
    #if(is.null(col)) col <- 1
    #if(is.null(lty)) lty <- 1      
    legend("topright", legend=apply(combmat, 2, function(x) paste(x,collapse="")),
           lty = lty, col = col, lwd = 2)
  }
}

# calculates the lower and upper system reliability for fixed t,
# together with parameter pairs which achieve these bounds,
# using optim over the K n0-parameters
# this implements (21) (last equation in Sec 4.4)
#
# luckobjlist list of K of prior luckmodel objetcs
# survsign    data frame with the survival signature as output by computeSystemSurvivalSignature()
# beta        vector of K fixed weibull shape parameters
# fts         list of K vectors giving the observed failure times for the compents of type 1,...,K;
#             the list element should be NULL if no failure has been observed for type k
# tnow        time until the system is observed
# t           timepoint for which to calculate P(T_sys > t), t > t_now
# returnasvec if output should be a vector (lower bound, upper bound, l.b. parameters, u.b. parameters)
# nk          vector of length K giving the number of components of each type,
#             this is needed only when reduced survival signature is used
# prior       whether the prior system relibability should be calculated
sysrelLuck <- function(luckobjlist, survsign, beta, fts, tnow, t, returnasvec = FALSE, nk = NULL, prior = FALSE){
  K <- length(luckobjlist)
  optimfu <- function(n0vec, y0vec, K, survsign, beta, fts, tnow, t, nk, prior = prior){
    n0y0 <- as.list(rep(NA, K))
    for (k in 1:K)
      n0y0[[k]] <- c(n0vec[k], y0vec[k])
    #cat("optimfu called with n0y0vec:\n")
    #print(n0y0)
    sysrel(n0y0 = n0y0, survsign = survsign, beta = beta, fts = fts, tnow = tnow, t = t, nk = nk, prior = prior)
  }
  parl <- numeric(K)
  paru <- numeric(K)
  y0l <- y0u <- numeric(K)
  for (k in 1:K){
    parl[k] <- n0(luckobjlist[[k]])[1]
    paru[k] <- n0(luckobjlist[[k]])[2]
    y0l[k] <- y0(luckobjlist[[k]])[1]
    y0u[k] <- y0(luckobjlist[[k]])[2]
  }
  optl <- optim(par = parl, fn = optimfu, method = "L-BFGS-B", lower = parl, upper = paru,
                y0vec = y0l, K = K, survsign = survsign, beta = beta, fts = fts, tnow = tnow, t = t, nk = nk, prior = prior)
  optu <- optim(par = paru, fn = optimfu, method = "L-BFGS-B", lower = parl, upper = paru,
                y0vec = y0u, K = K, survsign = survsign, beta = beta, fts = fts, tnow = tnow, t = t, nk = nk, prior = prior,
                control = list(fnscale = -1))
  if(returnasvec)
    return(c(optl$value, optu$value, optl$par, optu$par))
  else
    return(list(rel = c(optl$value, optu$value), lpar = optl$par, upar = optu$par))  
}


# calculates the the lower and upper system reliability function for vector of timepoints tvec
# luckobjlist list of K of prior luckmodel objetcs
# survsign    data frame with the survival signature as output by computeSystemSurvivalSignature()
# beta        vector of K fixed weibull shape parameters
# fts         list of K vectors giving the observed failure times for the compents of type 1,...,K;
#             the list element should be NULL if no failure has been observed for type k, 
# tnow        time until the system is observed
# tvec        vector of timepoints for which to calculate P(T_sys > t), t > t_now
# nk          vector of length K giving the number of components of each type,
#             this is needed only when reduced survival signature is used
# prior       whether the prior system relibability should be calculated
sysrelPbox <- function(luckobjlist, survsign, beta, fts, tnow, tvec, nk = NULL, prior = FALSE, ...){
  res <- sapply(tvec, FUN = sysrelLuck, luckobjlist = luckobjlist, survsign = survsign,
                beta = beta, fts = fts, tnow = tnow, nk = nk, prior = prior, returnasvec = TRUE)
  res <- cbind(tvec, t(res))
  K <- length(luckobjlist)
  colnames(res) <- c("tvec", "lower", "upper", paste("ln", 1:K, sep=""), paste("un", 1:K, sep=""))
  return(res)
}

# plots the the lower and upper system reliability function for vector of timepoints tvec
# in default graphics as grey area via polygon()
#
# res  table of lower and upper system reliability values as output by sysrelPbox()
plotSysrelPbox <- function(res, add = FALSE, ylim = c(0,1), xlab = "t", ylab = expression(R[sys](t)),
                           polygonBorderCol = NA, polygonFillCol = "grey", ...){
  tvec <- res[,1]
  if(!add){
    plot(c(min(tvec), max(tvec)), c(0,0), type = "n", ylim = ylim, xlab = "", ylab = "", ...)
    mtext(xlab, 1, 2)
    mtext(ylab, 2, 2)
  }
  tvecrev <- numeric(length(tvec))
  lower <- res[,2]
  upper <- numeric(length(tvec))
  for (t in 1:length(tvec)){
    tvecrev[length(tvec)-t+1] <- tvec[t]
    upper[length(tvec)-t+1] <- res[t,3]
  }
  polygon(c(tvec, tvecrev), c(lower, upper), border = polygonBorderCol, col = polygonFillCol)
}

# -------------------------------------

# prior predictive density
prpr <- function(t, n0, y0, beta = 2)
  beta * t^(beta - 1) * (n0 + 1) * (n0 * y0)^(n0 + 1) * (n0 * y0 + t^beta)^(-n0 - 2)

# prior predictive reliability function, works only for t vectors starting at 0
prprRold <- function(t, n0, y0, beta = 2){
  dens <- pmax(prpr(t = t, n0 = n0, y0 = y0, beta = beta), 0)
  1-cumsum(dens)/(length(t)/max(t)) # divide by evaluation points per unit
}

# prior predictive reliability function with numerical integration
prprR <- function(t, n0, y0, beta = 2){
  1 - sapply(t, function(sgt) integrate(f = prpr, lower = 1e-5, upper = sgt, n0 = n0, y0 = y0, beta = beta)$value)
}

# prior predictive reliability function with numerical integration, with loop to see where the problem is
prprRloop <- function(t, n0, y0, beta = 2){
  res <- numeric(length(t))
  for (i in 1:length(t)){
    sgt <- t[i]
    cat(i, ": t = ", sgt, "\n")
    res[i] <- 1 - integrate(f = prpr, lower = 1e-5, upper = sgt, n0 = n0, y0 = y0, beta = beta)$value
  }
  res
}

# functions to update prior to posterior parameters for Weibull likelihood with censored observations
# set fts = NULL when no failures occurred
nn <- function(n0, fts)
  n0 + length(fts)
yn <- function(n0, y0, fts, beta, n = length(fts), tnow = max(fts))
  (n0*y0 + (n - length(fts))*tnow^beta + sum(fts^beta))/(n0 + length(fts))

# prior predictive reliability function, works only for t vectors starting at 0
# going over n0 grid, default is only lower and upper n0
prprRluck <- function(tvec, luckobj, beta = 2, n0gridlen = 2){
  n0grid <- seq(n0(luckobj)[1], n0(luckobj)[2], length.out = n0gridlen)
  lmat <- sapply(n0grid, function(n0) prprR(tvec, n0, y0(luckobj)[1], beta))
  umat <- sapply(n0grid, function(n0) prprR(tvec, n0, y0(luckobj)[2], beta))
  data.frame(tvec = tvec, lower = apply(lmat, 1, min), upper = apply(umat, 1, max))
}


# posterior predictive reliability function, works only for t vectors starting at 0
# going over n0 grid, default is only lower and upper n0
# n and tnow used only for update with censored observations
poprRluck <- function(tvec, luckobj, beta = 2, fts, n, tnow, n0gridlen = 2){
  n0grid <- seq(n0(luckobj)[1], n0(luckobj)[2], length.out = n0gridlen)
  lmat <- sapply(n0grid, function(n0) prprR(tvec, nn(n0, fts),
                                            yn(n0, y0(luckobj)[1], fts, beta, n, tnow), beta))
  umat <- sapply(n0grid, function(n0) prprR(tvec, nn(n0, fts),
                                            yn(n0, y0(luckobj)[2], fts, beta, n, tnow), beta))
  data.frame(tvec = tvec, lower = apply(lmat, 1, min), upper = apply(umat, 1, max))
}


# checking only the four corners of the posterior parameter set
poprRluck2 <- function(tvec, luckobj, beta = 2, fts, n, tnow){
  l1 <- prprR(tvec, nn(n0(luckobj)[1], fts), yn(n0(luckobj)[1], y0(luckobj)[1], fts, beta, n, tnow), beta)
  print("l1 ok")
  l2 <- prprR(tvec, nn(n0(luckobj)[2], fts), yn(n0(luckobj)[2], y0(luckobj)[1], fts, beta, n, tnow), beta)
  print("l2 ok")
  u1 <- prprR(tvec, nn(n0(luckobj)[1], fts), yn(n0(luckobj)[1], y0(luckobj)[2], fts, beta, n, tnow), beta)
  print("u1 ok")
  u2 <- prprR(tvec, nn(n0(luckobj)[2], fts), yn(n0(luckobj)[2], y0(luckobj)[2], fts, beta, n, tnow), beta)
  print("u2 ok")
  data.frame(tvec = tvec, lower = pmin(l1, l2), upper = pmax(u1, u2))
}


# produces survival signature matrix for one component of type "name",
# for use in nonParBayesSystemInference()
oneCompSurvSign <- function(name){
  res <- data.frame(name=c(0,1), Probability=c(0,1))
  names(res)[1] <- name
  res
}

#