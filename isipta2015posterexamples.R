###############################################################################
# the three poster examples
###############################################################################
posterprior1 <- LuckModel(n0 = c(2,10), y0 = c(failuretolambda(9,2), failuretolambda(11,2)))
posterprior2 <- LuckModel(n0 = c(8,16), y0 = c(failuretolambda(4,2), failuretolambda(5,2)))
posterprior3 <- LuckModel(n0 = c(1,5), y0 = c(failuretolambda(9,2), failuretolambda(11,2)))
posterprior <- list(posterprior1, posterprior2, posterprior3)

# prior system reliability at time 0
posterfts0 <- list(NULL, NULL, NULL)
# full system signature (for prior system reliability)
postersys <- graph.formula(s -- 1:2 -- 3:4:5 -- t)
V(postersys)$compType <- NA # This just creates the attribute compType
V(postersys)$compType[match(c("1","3"), V(postersys)$name)] <- "Type 1"
V(postersys)$compType[match(c("2","4"), V(postersys)$name)] <- "Type 2"
V(postersys)$compType[match(c("5"), V(postersys)$name)] <- "Type 3"
V(postersys)$compType[match(c("s","t"), V(postersys)$name)] <- NA
postersyssign <- computeSystemSurvivalSignature(postersys)

postertvec0 <- seq(0, 20, by=0.1)
postercorners0 <- fourKcornersSysrel(luckobjlist = posterprior, survsign = postersyssign, beta = rep(2,3),
                                     fts = posterfts0, tnow = 0, tvec = postertvec0, nk = c(2,2,1), prior = TRUE)
posterpbox0 <- sysrelPbox(luckobjlist = posterprior, survsign = postersyssign, nk = c(2,2,1),
                          beta = rep(2,3), fts = posterfts0, tnow = 0, tvec = postertvec0, prior = TRUE)
#pdf("poster0.pdf", width=9, height=4)
plotSysrelPbox(res = posterpbox0, polygonFillCol = rgb(255,154,0, max =255))
fourKcornersSysrelPlot(tvec = postertvec0, rframe = postercorners0$lower, add = TRUE)
fourKcornersSysrelPlot(tvec = postertvec0, rframe = postercorners0$upper, add = TRUE)
#dev.off()

# -----------------------------------------------------------------------------------

# example 1: failure times as expected: no failures type 1, failures type 2 c(4,5), no failure type 3
posterfts1 <- list(NULL, NULL)
# reduced signature (no surviving components of type 2)
postersys1 <- graph.formula(s -- 1 -- 2:3 -- t)
V(postersys1)$compType <- NA # This just creates the attribute compType
V(postersys1)$compType[match(c("1", "2"), V(postersys1)$name)] <- "Type 1"
V(postersys1)$compType[match(c("3"), V(postersys1)$name)] <- "Type 2"
#V(postersys1)$compType[match(c("3"), V(postersys1)$name)] <- "Type 3"
V(postersys1)$compType[match(c("s","t"), V(postersys1)$name)] <- NA
postersys1sign <- computeSystemSurvivalSignature(postersys1)

postertvec1 <- seq(5, 15, by=0.05)
priorpbox1 <- sysrelPbox(luckobjlist = posterprior, survsign = postersyssign, nk = c(2,2,1),
                         beta = rep(2,3), fts = posterfts0, tnow = 5, tvec = postertvec1, prior = TRUE)
postercorners1 <- fourKcornersSysrel(luckobjlist = list(posterprior1, posterprior3), survsign = postersys1sign,
                                     beta = rep(2,2), fts = posterfts1, tnow = 5, tvec = postertvec1, nk = c(2,1))
posterpbox1 <- sysrelPbox(luckobjlist = list(posterprior1, posterprior3), survsign = postersys1sign, nk = c(2,1),
                          beta = rep(2,2), fts = posterfts1, tnow = 5, tvec = postertvec1)

#pdf("poster1prior.pdf", width=5, height=4)
#par(mar=c(3,3,0,0)+0.1)
plotSysrelPbox(res = priorpbox1, polygonBorderCol = rgb(255,154,0,160, max =255),
               polygonFillCol = rgb(255,154,0, 80, max =255))
plotSysrelPbox(res = posterpbox1, add = TRUE, polygonBorderCol = NA,
               polygonFillCol = rgb(16,16,115, 80, max =255))
fourKcornersSysrelPlot(tvec = postertvec1, rframe = postercorners1$lower, add = TRUE, col = rgb(16,16,115, max =255))
fourKcornersSysrelPlot(tvec = postertvec1, rframe = postercorners1$upper, add = TRUE, col = rgb(16,16,115, max =255))
#dev.off()

# lower and upper median: region around 0.5
posterpbox1[posterpbox1[,2] < 0.51 & posterpbox1[,2] > 0.49,]
posterpbox1[posterpbox1[,3] < 0.51 & posterpbox1[,3] > 0.49,]
# closest to 0.5
posterpbox1[which.min(abs(posterpbox1[,2] - 0.5)),] # 8.10
posterpbox1[which.min(abs(posterpbox1[,3] - 0.5)),] # 9.95

#