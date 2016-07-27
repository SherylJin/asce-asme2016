# --------------------------------------------------------------------------- #
# ASCE-ASME paper - code for example figs
# --------------------------------------------------------------------------- #

source("weibull-sys-functions.R")
source("plotdefs.R")

br <- graph.formula(s -- M -- C1:C2:C3:C4, P1:P2:P3:P4 -- t,
                    C1 -- P1, C2 -- P2, C3 -- P3, C4 -- P4, s -- H -- P3:P4)
br <- setCompTypes(br, list("M"=c("M"), "H"=c("H"), "C"=c("C1", "C2", "C3", "C4"), "P"=c("P1", "P2", "P3", "P4")))
brsysign <- computeSystemSurvivalSignature(br)
brMpr <- LuckModel(n0 = c(2,5),  y0 = c(failuretolambda(5,2.5), failuretolambda(8,2.5)))
brHpr <- LuckModel(n0 = c(1,10), y0 = c(failuretolambda(2,1),   failuretolambda(20,1)))
brCpr <- LuckModel(n0 = c(1,5),  y0 = c(failuretolambda(8,2),   failuretolambda(10,2)))
brPpr <- LuckModel(n0 = c(1,10), y0 = c(failuretolambda(3,1.5), failuretolambda(4,1.5)))
brpr <- list(brMpr, brHpr, brCpr, brPpr)
brprtvec <- seq(0, 15, length.out=201)
brprnk <- c(1, 1, 4, 4)
brbeta <- c(2.5, 1, 2, 1.5)
brfts0 <- list(NULL, NULL, NULL, NULL)
# components
brMprrel <- sysrelPbox(luckobjlist = list(brpr[[1]]), survsign = oneCompSurvSign("M"), nk = 1,
                       beta = brbeta[1], fts = list(NULL), tnow = 0, tvec = brprtvec, prior = TRUE)
brHprrel <- sysrelPbox(luckobjlist = list(brpr[[2]]), survsign = oneCompSurvSign("H"), nk = 1,
                       beta = brbeta[2], fts = list(NULL), tnow = 0, tvec = brprtvec, prior = TRUE)
brCprrel <- sysrelPbox(luckobjlist = list(brpr[[3]]), survsign = oneCompSurvSign("C"), nk = 1,
                       beta = brbeta[3], fts = list(NULL), tnow = 0, tvec = brprtvec, prior = TRUE)
brPprrel <- sysrelPbox(luckobjlist = list(brpr[[4]]), survsign = oneCompSurvSign("P"), nk = 1,
                       beta = brbeta[4], fts = list(NULL), tnow = 0, tvec = brprtvec, prior = TRUE)
brprdf <- rbind(data.frame(brMprrel, Part = "M", Item = "Prior"), data.frame(brHprrel, Part = "H", Item = "Prior"),
                data.frame(brCprrel, Part = "C", Item = "Prior"), data.frame(brPprrel, Part = "P", Item = "Prior"))
brprdf$Item <- ordered(brprdf$Item, levels=c("Prior", "Posterior"))
brprdf$Part <- ordered(brprdf$Part, levels=c("M", "H", "C", "P"))
fig3 <- ggplot(brprdf, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + 
  facet_wrap(~Part, nrow=2) + xlab(expression(t)) + ylab(expression(R(t))) + nolegend 
fig3

# using the prior predictive
prprtvec <- seq(0, 15, length.out=501)
prprM <- prprRluck(prprtvec, brpr[[1]], beta = brbeta[1])
prprH <- prprRluck(prprtvec, brpr[[2]], beta = brbeta[2])
prprC <- prprRluck(prprtvec, brpr[[3]], beta = brbeta[3])
prprP <- prprRluck(prprtvec, brpr[[4]], beta = brbeta[4])
prprdf <- rbind(data.frame(prprM, Part = "M", Item = "Prior"), data.frame(prprH, Part = "H", Item = "Prior"),
                data.frame(prprC, Part = "C", Item = "Prior"), data.frame(prprP, Part = "P", Item = "Prior"))
prprdf$Item <- ordered(prprdf$Item, levels=c("Prior", "Posterior"))
prprdf$Part <- ordered(prprdf$Part, levels=c("M", "H", "C", "P"))
prprdf$lower[prprdf$lower < 0] <- 0
fig4 <- ggplot(prprdf, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + 
  facet_wrap(~Part, nrow=2) + xlab(expression(t)) + ylab(expression(R(t))) + rightlegend #nolegend 
fig4

#setEPS()
#postscript("fig3.eps",width=8,height=6)
pdf("fig4.pdf", width=8, height=6)
fig4
dev.off()

# components posterior (test only for M)
poprM <- poprRluck(prprtvec, brpr[[1]], beta = brbeta[1], fts = NULL, n = 1, tnow = 10)
poprdf <- rbind(data.frame(prprM, Part = "M", Item = "Prior"), data.frame(poprM, Part = "M", Item = "Posterior"))
poprdf$Item <- ordered(poprdf$Item, levels=c("Prior", "Posterior"))
fig5 <- ggplot(poprdf, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + 
  xlab(expression(t)) + ylab(expression(R(t))) + rightlegend 
fig5


# system
brpr <- sysrelPbox(luckobjlist = posterprior, survsign = postersyssign, nk = c(2,2,1),
                          beta = rep(2,3), fts = posterfts0, tnow = 0, tvec = postertvec0, prior = TRUE)
#