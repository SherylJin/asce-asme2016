# --------------------------------------------------------------------------- #
# ASCE-ASME paper - code for Fig 2
# --------------------------------------------------------------------------- #

source("weibull-sys-functions.R")
source("plotdefs.R")

forfig2 <- list(n0 = c(2,5), y0 = c(failuretolambda(9,2), failuretolambda(11,2)), beta = 2)
data2 <- c(1,2)
data3 <- c(10,11)
lvec <- seq(0.1, 300, length.out = 300) # lambda vec

weidf <- function(prior, data, tvec){
  n0 <- prior$n0
  y0 <- prior$y0
  beta <- prior$beta
  n <- length(data)
  tau <- sum(data^beta)
  # prior set
  prlower <- sapply(tvec, FUN = function(t){
    optimize(f = cpigfornoptim, y = y0[2], t = t,
             lower = n0[1], upper = n0[2], maximum = FALSE)$objective
  })
  prupper <- sapply(tvec, FUN = function(t){
    optimize(f = cpigfornoptim, y = y0[1], t = t,
             lower = n0[1], upper = n0[2], maximum = TRUE)$objective
  })
  # posterior set
  polower <- sapply(tvec, FUN = function(t){
    optimize(f = function(n0f, y0f, t){ # here n0 and y0 are fixed values
      nn <- n0f + n
      yn <- (n0f*y0f + tau)/nn
      cpinvgamma(t, nn, yn)
    }, y0f = y0[2], t = t,
             lower = n0[1], upper = n0[2], maximum = FALSE)$objective
  })
  poupper <- sapply(tvec, FUN = function(t){
    optimize(f = function(n0f, y0f, t){ # here n0 and y0 are fixed values
      nn <- n0f + n
      yn <- (n0f*y0f + tau)/nn
      cpinvgamma(t, nn, yn)
    }, y0f = y0[1], t = t,
    lower = n0[1], upper = n0[2], maximum = TRUE)$objective
  })  
  data.frame(l = rep(tvec, 2), Fll = c(prlower, polower), Flu = c(prupper, poupper), 
             Item = factor(rep(c("Prior", "Posterior"), each = length(tvec)),
                           levels = c("Prior", "Posterior")))
}

fig2data2 <- weidf(forfig2, data2, tvec)
fig2data3 <- weidf(forfig2, data3, tvec)
fig2df <- rbind(data.frame(fig2data2, Panel = "Surprising Observations"),
                data.frame(fig2data3, Panel = "Unsurprising Observations"))

fig2 <- ggplot(fig2df, aes(x = l)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = Fll, ymax = Flu, group = Item, colour = Item, fill = Item), alpha  =0.5) +
  geom_line(aes(y = Fll, group = Item, colour = Item)) + 
  geom_line(aes(y = Flu, group = Item, colour = Item)) + 
  facet_wrap(~Panel, nrow=1) + xlab(expression(lambda)) + ylab(expression(F(lambda))) + rightlegend 

#setEPS()
#postscript("fig2.eps",width=8,height=3)
pdf("fig2.pdf", width=8, height=3)
fig2
dev.off()


### old stuff ###

#xseq <- seq(120, 140, length.out = 300) # for lower y0
#xseq <- seq(180, 200, length.out = 300) # for upper y0

#plot( xseq, cpinvgamma(xseq, forfig2$n0[1], forfig2$y0[1]), type="l", xlab="", ylab="")
#lines(xseq, cpinvgamma(xseq, forfig2$n0[2], forfig2$y0[1]))
#lines(xseq, cpinvgamma(xseq, forfig2$n0[1], forfig2$y0[2]))
#lines(xseq, cpinvgamma(xseq, forfig2$n0[2], forfig2$y0[2]))

# cpinvgamma(x, n0, y0)
# ny = c(n0, y0)

source("../esrel2015paper/00-06_cdfplotLuckModel.r")
source("../esrel2015paper/03-01_WeibullData.r")
source("../esrel2015paper/03-02_Weibull.r")

fig2luck <- WeibullLuckModel(n0 = c(2,5), y0 = c(failuretolambda(9,2), failuretolambda(11,2)),
                             data = WeibullData(1:2))
fig3luck <- WeibullLuckModel(n0 = c(2,5), y0 = c(failuretolambda(9,2), failuretolambda(11,2)),
                             data = WeibullData(10:11))

#setEPS()
#postscript("fig2.eps",width=5,height=3)
pdf("fig2.pdf",width=5,height=3)
par(mar=c(3,3,1,1)+0.1)
cdfplot(fig2luck, xvec = seq(0, 300, length.out = 301), vertdist = TRUE,
        control = controlList(posterior = FALSE, borderCol = rgb(1,0,0,0.4), polygonCol = rgb(1,0,0,0.1)))
cdfplot(fig2luck, xvec = seq(0, 300, length.out = 301), add = TRUE, vertdist = TRUE,
        control = controlList(posterior = TRUE, borderCol = "blue", polygonCol = rgb(0,0,1,0.3)))
mtext(expression(lambda), 1, 2)
mtext(expression(F(lambda)), 2, 2)
dev.off()

#setEPS()
#postscript("fig3.eps",width=5,height=3)
pdf("fig3.pdf",width=5,height=3)
par(mar=c(3,3,1,1)+0.1)
cdfplot(fig3luck, xvec = seq(0, 300, length.out = 301), vertdist = TRUE,
        control = controlList(posterior = FALSE, borderCol = rgb(1,0,0,0.4), polygonCol = rgb(1,0,0,0.1)))
cdfplot(fig3luck, xvec = seq(0, 300, length.out = 301), add = TRUE, vertdist = TRUE,
        control = controlList(posterior = TRUE, borderCol = "blue", polygonCol = rgb(0,0,1,0.3)))
mtext(expression(lambda), 1, 2)
mtext(expression(F(lambda)), 2, 2)
dev.off()
#