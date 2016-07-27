# --------------------------------------------------------------------------- #
# ASCE-ASME paper - prior predictive for t
# --------------------------------------------------------------------------- #

tvec <- seq(0, 10, length.out = 101)

pr1 <- prpr(tvec, 2, failuretolambda(5,2), 2)
pr2 <- prpr(tvec, 10, failuretolambda(5,2), 2)
pr3 <- prpr(tvec, 2, failuretolambda(2,2), 2)
#qplot(tvec, pr1, geom = "line")

plot(tvec, pr1, type = "l")
lines(tvec, pr2, col=2)
lines(tvec, pr3, col=3)

tvec <- seq(0, 10, length.out = 501)
pr1n0 <- 1
pr1y0 <- failuretolambda(5,2)
fts1 <- rep(5, 5)
pr1nn <- nn(pr1n0, fts1)
pr1yn <- yn(2, failuretolambda(5,2), fts1, 2)
pr1d <- prpr(tvec, pr1n0, pr1y0, 2)
pr1r <- prprR(tvec, pr1n0, pr1y0, 2)
pr1dp <- prpr(tvec, pr1nn, pr1yn, 2)
pr1rp <- prprR(tvec, pr1nn, pr1yn, 2)
plot(tvec, pr1r, type = "l", ylim=c(0,1), xlab="t", ylab="R(t)") # prior
lines(tvec, pr1rp, col=2) # posterior

# hazard
plot(tvec, pr1d/pr1r, type = "l", xlab="t", ylab="h(t)", ylim=c(0,1)) # prior
lines(tvec, pr1dp/pr1rp, col = 2) # posterior


# prior predictive not so useful...

# sysrel() and sysrelPbox work with one-component systems for prior and posterior
# but without observed failure time, makes no sense with failure time due to t >= tnow >= fts
ss1 <- oneCompSurvSign("C1")
test1 <- sysrel(n0y0 = list(c(2, failuretolambda(5,2))), survsign = ss1, beta = 2, fts = list(NULL), tnow = 0,
                t = 0, table = FALSE, nk = c(1), prior = TRUE)
test1 <- sysrel(n0y0 = list(c(2, failuretolambda(5,2))), survsign = ss1, beta = 2, fts = list(NULL), tnow = 0,
                t = 0, table = FALSE, nk = c(1), prior = FALSE)

el1lm <- LuckModel(n0 = c(1.99,2.01), y0 = c(failuretolambda(4.99,2), failuretolambda(5.01,2)))
el1 <- sysrelPbox(luckobjlist = list(el1lm), survsign = ss1, nk = c(1), beta = 2, fts = list(NULL), tnow = 0, 
                  tvec = tvec, prior = TRUE)
plotSysrelPbox(el1, polygonBorderCol = 1)
lines(tvec, 1-cumsum(pr1)/(length(tvec)/(max(tvec)-min(tvec))), col = 2)


#