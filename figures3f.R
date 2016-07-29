# --------------------------------------------------------------------------- #
# ASCE-ASME paper - code for example figs
# --------------------------------------------------------------------------- #

source("weibull-sys-functions.R")
source("plotdefs.R")

# full system for prior
br <- graph.formula(s -- M -- C1:C2:C3:C4, P1:P2:P3:P4 -- t,
                    C1 -- P1, C2 -- P2, C3 -- P3, C4 -- P4, s -- H -- P3:P4)
br <- setCompTypes(br, list("M"=c("M"), "H"=c("H"), "C"=c("C1", "C2", "C3", "C4"), "P"=c("P1", "P2", "P3", "P4")))
brsysign <- computeSystemSurvivalSignature(br)

# reduced system for posterior: we assume that C2, C3, P2 and P3 have failed
brpos <- graph.formula(s -- M -- C1:C4, P1:P4 -- t,
                       C1 -- P1, C4 -- P4, s -- H -- P4)
brpos <- setCompTypes(brpos, list("M"=c("M"), "H"=c("H"), "C"=c("C1", "C4"), "P"=c("P1", "P4")))
brpossysign <- computeSystemSurvivalSignature(brpos)

# prior param sets
brCpr <- LuckModel(n0 = c(1,5),  y0 = c(failuretolambda(8,2),   failuretolambda(10,2)))
brHpr <- LuckModel(n0 = c(1,10), y0 = c(failuretolambda(2,1.2), failuretolambda(20,1.2)))
brMpr <- LuckModel(n0 = c(2,5),  y0 = c(failuretolambda(5,2.5), failuretolambda(8,2.5)))
brPpr <- LuckModel(n0 = c(1,10), y0 = c(failuretolambda(3,1.5), failuretolambda(4,1.5)))
brpr <- list(brCpr, brHpr, brMpr, brPpr)
brprnk <- c(4, 1, 1, 4)
brbeta <- c(2, 1.2, 2.5, 1.5)
brfts0 <- list(NULL, NULL, NULL, NULL)
brprtvec <- seq(0, 20, length.out=101)

# prior system reliability function at tnow = 0
brprio <- sysrelPbox(luckobjlist = brpr, survsign = brsysign, nk = brprnk, beta = brbeta,
                     fts = brfts0, tnow = 0, tvec = brprtvec, prior = TRUE)

# component reliability functions via sysrelPbox, posterior does not work
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

# component reliability functions using the prior predictive
prprtvec <- seq(0, 20, length.out=301)
prprC <- prprRluck(prprtvec, brpr[[1]], beta = brbeta[1], n0gridlen = 2000)
prprH <- prprRluck(prprtvec, brpr[[2]], beta = brbeta[2], n0gridlen = 2000)
prprM <- prprRluck(prprtvec, brpr[[3]], beta = brbeta[3], n0gridlen = 2000)
prprP <- prprRluck(prprtvec, brpr[[4]], beta = brbeta[4], n0gridlen = 2000)
prprdf <- rbind(data.frame(prprM, Part = "M", Item = "Prior"), data.frame(prprH, Part = "H", Item = "Prior"),
                data.frame(prprC, Part = "C", Item = "Prior"), data.frame(prprP, Part = "P", Item = "Prior"))
prprdf$Item <- ordered(prprdf$Item, levels=c("Prior", "Posterior"))
prprdf$Part <- ordered(prprdf$Part, levels=c("M", "H", "C", "P"))
prprdf$lower[prprdf$lower < 0] <- 0
prprdf$upper[prprdf$upper > 1] <- 1
fig4 <- ggplot(prprdf, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + 
  facet_wrap(~Part, nrow=2) + xlab(expression(t)) + ylab(expression(R(t))) + rightlegend #nolegend 
fig4 # warnings refer to the four instances which have been rounded down to 1, bug in ggplot?

#setEPS()
#postscript("fig4.eps",width=8,height=6)
pdf("fig4.pdf", width=8, height=6)
fig4
dev.off()


# data 1: fitting failure times ---------------------------------------------------------------- #
brfts1 <- list(c(6, 7), NULL, NULL, c(3, 4))
tnow1 <- 8

# components posterior # CHMP
# n and tnow are used for the updating!
# prprtvec <- seq(0, 20, length.out=501)
#prprtvec[1] <- 1e-5
popr1C <- poprRluck(prprtvec, brpr[[1]], beta = brbeta[1], fts = brfts1[[1]], n = brprnk[1], tnow = tnow1, n0gridlen = 2000)
popr1H <- poprRluck(prprtvec, brpr[[2]], beta = brbeta[2], fts = brfts1[[2]], n = brprnk[2], tnow = tnow1, n0gridlen = 2000)
popr1M <- poprRluck(prprtvec, brpr[[3]], beta = brbeta[3], fts = brfts1[[3]], n = brprnk[3], tnow = tnow1, n0gridlen = 2000)
popr1P <- poprRluck(prprtvec, brpr[[4]], beta = brbeta[4], fts = brfts1[[4]], n = brprnk[4], tnow = tnow1, n0gridlen = 2000)
popr1df <- rbind(data.frame(prprC, Part = "C", Item = "Prior"), data.frame(popr1C, Part = "C", Item = "Posterior"),
                 data.frame(prprH, Part = "H", Item = "Prior"), data.frame(popr1H, Part = "H", Item = "Posterior"),
                 data.frame(prprM, Part = "M", Item = "Prior"), data.frame(popr1M, Part = "M", Item = "Posterior"),
                 data.frame(prprP, Part = "P", Item = "Prior"), data.frame(popr1P, Part = "P", Item = "Posterior"))
popr1df$Item <- ordered(popr1df$Item, levels=c("Prior", "Posterior"))
popr1df$Part <- ordered(popr1df$Part, levels=c("M", "H", "C", "P"))
popr1df$lower[popr1df$lower < 0] <- 0
popr1df$upper[popr1df$upper > 1] <- 1
fig5a <- ggplot(popr1df, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + 
  facet_wrap(~Part, nrow=2) + xlab(expression(t)) + ylab(expression(R(t))) + rightlegend 
fig5a # warnings refer to the instances which have been rounded down to 1, bug in ggplot?

pdf("fig5a.pdf", width=8, height=6)
fig5a
dev.off()

# posterior system reliability function at tnow = 8
brpos1tvec <- seq(tnow1, 20, length.out=61)
brpost1 <- sysrelPbox(luckobjlist = brpr, survsign = brpossysign, nk = brprnk, beta = brbeta,
                      fts = brfts1, tnow = tnow1, tvec = brpos1tvec, prior = FALSE)
br1df <- rbind(data.frame(brprio, Panel = "Prior", Item = "Prior"),
               data.frame(brprio, Panel = "Prior and Posterior 1", Item = "Prior"),
               data.frame(brpost1, Panel = "Prior and Posterior 1", Item = "Posterior"))
br1df$Item <- ordered(br1df$Item, levels=c("Prior", "Posterior"))
fig5 <- ggplot(br1df, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + facet_wrap(~Panel, nrow=1) + 
  xlab(expression(t)) + ylab(expression(R(t))) + rightlegend
fig5

#setEPS()
#postscript("fig5.eps",width=8,height=6)
pdf("fig5.pdf", width=8, height=3)
fig5
dev.off()


# data 2: early failure times ---------------------------------------------------------------- #
brfts2 <- list(c(1, 2), NULL, NULL, c(0.25, 0.5))
tnow2 <- 2

# components posterior # CHMP
popr2C <- poprRluck(prprtvec, brpr[[1]], beta = brbeta[1], fts = brfts2[[1]], n = brprnk[1], tnow = tnow2, n0gridlen = 2000)
popr2H <- poprRluck(prprtvec, brpr[[2]], beta = brbeta[2], fts = brfts2[[2]], n = brprnk[2], tnow = tnow2, n0gridlen = 2000)
popr2M <- poprRluck(prprtvec, brpr[[3]], beta = brbeta[3], fts = brfts2[[3]], n = brprnk[3], tnow = tnow2, n0gridlen = 2000)
popr2P <- poprRluck(prprtvec, brpr[[4]], beta = brbeta[4], fts = brfts2[[4]], n = brprnk[4], tnow = tnow2, n0gridlen = 2000)
popr2df <- rbind(data.frame(prprC, Part = "C", Item = "Prior"), data.frame(popr2C, Part = "C", Item = "Posterior"),
                 data.frame(prprH, Part = "H", Item = "Prior"), data.frame(popr2H, Part = "H", Item = "Posterior"),
                 data.frame(prprM, Part = "M", Item = "Prior"), data.frame(popr2M, Part = "M", Item = "Posterior"),
                 data.frame(prprP, Part = "P", Item = "Prior"), data.frame(popr2P, Part = "P", Item = "Posterior"))
popr2df$Item <- ordered(popr2df$Item, levels=c("Prior", "Posterior"))
popr2df$Part <- ordered(popr2df$Part, levels=c("M", "H", "C", "P"))
popr2df$lower[popr2df$lower < 0] <- 0
popr2df$upper[popr2df$upper > 1] <- 1
fig6a <- ggplot(popr2df, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + 
  facet_wrap(~Part, nrow=2) + xlab(expression(t)) + ylab(expression(R(t))) + rightlegend 
fig6a # warnings refer to the instances which have been rounded down to 1, bug in ggplot?
pdf("fig6a.pdf", width=8, height=6)
fig6a
dev.off()

# posterior system reliability function at tnow = 2
brpos2tvec <- seq(tnow2, 20, length.out=91)
brpost2 <- sysrelPbox(luckobjlist = brpr, survsign = brpossysign, nk = brprnk, beta = brbeta,
                      fts = brfts2, tnow = tnow2, tvec = brpos2tvec, prior = FALSE)
br2df <- rbind(data.frame(brprio, Panel = "Prior", Item = "Prior"),
               data.frame(brprio, Panel = "Prior and Posterior 2", Item = "Prior"),
               data.frame(brpost2, Panel = "Prior and Posterior 2", Item = "Posterior"))
br2df$Item <- ordered(br2df$Item, levels=c("Prior", "Posterior"))
fig6 <- ggplot(br2df, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + facet_wrap(~Panel, nrow=1) + 
  xlab(expression(t)) + ylab(expression(R(t))) + rightlegend
fig6

#setEPS()
#postscript("fig6.eps",width=8,height=3)
pdf("fig6.pdf", width=8, height=3)
fig6
dev.off()


# data 3: late failure times ---------------------------------------------------------------- #
brfts3 <- list(c(11, 12), NULL, NULL, c(8, 9))
tnow3 <- 12

# components posterior # CHMP
popr3C <- poprRluck(prprtvec, brpr[[1]], beta = brbeta[1], fts = brfts3[[1]], n = brprnk[1], tnow = tnow3, n0gridlen = 2000)
popr3H <- poprRluck(prprtvec, brpr[[2]], beta = brbeta[2], fts = brfts3[[2]], n = brprnk[2], tnow = tnow3, n0gridlen = 2000)
popr3M <- poprRluck(prprtvec, brpr[[3]], beta = brbeta[3], fts = brfts3[[3]], n = brprnk[3], tnow = tnow3, n0gridlen = 2000)
popr3P <- poprRluck(prprtvec, brpr[[4]], beta = brbeta[4], fts = brfts3[[4]], n = brprnk[4], tnow = tnow3, n0gridlen = 2000)
popr3df <- rbind(data.frame(prprC, Part = "C", Item = "Prior"), data.frame(popr3C, Part = "C", Item = "Posterior"),
                 data.frame(prprH, Part = "H", Item = "Prior"), data.frame(popr3H, Part = "H", Item = "Posterior"),
                 data.frame(prprM, Part = "M", Item = "Prior"), data.frame(popr3M, Part = "M", Item = "Posterior"),
                 data.frame(prprP, Part = "P", Item = "Prior"), data.frame(popr3P, Part = "P", Item = "Posterior"))
popr3df$Item <- ordered(popr3df$Item, levels=c("Prior", "Posterior"))
popr3df$Part <- ordered(popr3df$Part, levels=c("M", "H", "C", "P"))
popr3df$lower[popr3df$lower < 0] <- 0
popr3df$upper[popr3df$upper > 1] <- 1
fig7a <- ggplot(popr3df, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + 
  facet_wrap(~Part, nrow=2) + xlab(expression(t)) + ylab(expression(R(t))) + rightlegend 
fig7a # warnings refer to the instances which have been rounded down to 1, bug in ggplot?
pdf("fig7a.pdf", width=8, height=6)
fig7a
dev.off()

# posterior system reliability function at tnow = 12
brpos3tvec <- seq(tnow3, 20, length.out=41)
brpost3 <- sysrelPbox(luckobjlist = brpr, survsign = brpossysign, nk = brprnk, beta = brbeta,
                      fts = brfts3, tnow = tnow3, tvec = brpos3tvec, prior = FALSE)
br3df <- rbind(data.frame(brprio, Panel = "Prior", Item = "Prior"),
               data.frame(brprio, Panel = "Prior and Posterior 3", Item = "Prior"),
               data.frame(brpost3, Panel = "Prior and Posterior 3", Item = "Posterior"))
br3df$Item <- ordered(br3df$Item, levels=c("Prior", "Posterior"))
fig7 <- ggplot(br3df, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + facet_wrap(~Panel, nrow=1) + 
  xlab(expression(t)) + ylab(expression(R(t))) + rightlegend
fig7

#setEPS()
#postscript("fig7.eps",width=8,height=3)
pdf("fig7.pdf", width=8, height=3)
fig7
dev.off()

# figure with prior and the three posterior system reliabilties in four panels
br4df <- rbind(data.frame(brprio,  Panel = "Prior", Item = "Prior"),
               data.frame(brprio,  Panel = "Failures as expected", Item = "Prior"),
               data.frame(brpost1, Panel = "Failures as expected", Item = "Posterior"),
               data.frame(brprio,  Panel = "Surprisingly early failures", Item = "Prior"),
               data.frame(brpost2, Panel = "Surprisingly early failures", Item = "Posterior"),
               data.frame(brprio,  Panel = "Surprisingly late failures", Item = "Prior"),
               data.frame(brpost3, Panel = "Surprisingly late failures", Item = "Posterior"))
br4df$Item <- ordered(br4df$Item, levels=c("Prior", "Posterior"))
br3df$Panel <- ordered(br3df$Panel, levels=c("Prior", "Failures as expected", "Surprisingly early failures",
                                             "Surprisingly late failures"))
fig8 <- ggplot(br4df, aes(x = tvec)) + theme_bw() + ijarcol + ijarfill + ylim(0, 1) +  
  geom_ribbon(aes(ymin = lower, ymax = upper, group = Item, colour = Item, fill = Item), alpha = 0.5) +
  geom_line(aes(y = lower, group = Item, colour = Item)) + 
  geom_line(aes(y = upper, group = Item, colour = Item)) + facet_wrap(~Panel, nrow=2) + 
  xlab(expression(t)) + ylab(expression(R[sys](t))) + rightlegend
fig8

#setEPS()
#postscript("fig8.eps",width=8,height=6)
pdf("fig8.pdf", width=8, height=6)
fig8
dev.off()

# plot of sysrel prior / posterior imprecision
brpost1e <- brpost1
brpost1e[,1] <- brpost1e[,1] - tnow1
brpost2e <- brpost2
brpost2e[,1] <- brpost2e[,1] - tnow2
brpost3e <- brpost3
brpost3e[,1] <- brpost3e[,1] - tnow3
br5df <- rbind(data.frame(br4df, Panel2 = "Elapsed time"),
               data.frame(brprio,  Panel = "Prior", Item = "Prior", Panel2 = "Prospective time"),
               #data.frame(brprio,  Panel = "Failures as expected", Item = "Prior", Panel2 = "Prospective time"),
               data.frame(brpost1e, Panel = "Failures as expected", Item = "Posterior", Panel2 = "Prospective time"),
               #data.frame(brprio,  Panel = "Surprisingly early failures", Item = "Prior", Panel2 = "Prospective time"),
               data.frame(brpost2e, Panel = "Surprisingly early failures", Item = "Posterior", Panel2 = "Prospective time"),
               #data.frame(brprio,  Panel = "Surprisingly late failures", Item = "Prior", Panel2 = "Prospective time"),
               data.frame(brpost3e, Panel = "Surprisingly late failures", Item = "Posterior", Panel2 = "Prospective time"))
#br4dfe <- br4df
#br4dfe$tvec <- 
#br5df <- rbind(data.frame(br4df, Panel2 = "Elapsed time"), data.frame(br4dfe, Panel2 = "Elapsed time"))
               
fig9 <- ggplot(br5df, aes(x = tvec)) + theme_bw() + ylim(0, 1) + ijarcol + rightlegend + #ijarfill + 
  geom_line(aes(y = upper-lower, group = interaction(Item, Panel), colour = Item, lty = Panel)) +
  facet_wrap(~Panel2, nrow=1) + xlab(expression(t)) + ylab(bquote(bar(R)[sys](t) - ~underline(R)[sys](t)))
fig9

#setEPS()
#postscript("fig9.eps",width=8,height=6)
pdf("fig9.pdf", width=8, height=3)
fig9
dev.off()


#