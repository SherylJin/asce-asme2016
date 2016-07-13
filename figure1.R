# --------------------------------------------------------------------------- #
# ASCE-ASME paper - code for Fig 1
# --------------------------------------------------------------------------- #

source("weibull-sys-functions.R")
source("plotdefs.R")

# example for Fig 1: assume mean ft = 9
beta <- 2
y0 <- failuretolambda(9, beta)
n0 <- 2
t1 <- 1
t2 <- 2

n2 <- n0 + 2
y2 <- (n0*y0 + t1^beta + t2^beta)/n2
y2

eft2 <- lambdatofailure(y2, beta)
eft2

invgammavar <- function(n0, y0)
  y0^2/(1-1/n0)
invgammasd <- function(n0, y0)
  sqrt(y0^2/(1-1/n0))

invgammavar(n2, y2)
invgammasd(n2, y2)
invgammasd(n0, y0)

failuretolambda(7)
(2*failuretolambda(7) + 6^2 + 7^2)/4

# the plot
xseq <- seq(0.1, 200, length.out = 300)
fig1df <- data.frame(x = rep(xseq, 2),
                     y = c(cpinvgamma(xseq, n0, y0), cpinvgamma(xseq, n2, y2)),
                     p = factor(rep(c("Prior", "Posterior"), each = length(xseq)),
                                levels = c("Prior", "Posterior")))
fig1 <- ggplot(fig1df, aes(x=x)) + geom_line(aes(y=y, group=p, colour=p)) + ylim(0, 1) + 
  theme_bw() + ijarcol + rightlegend + xlab(expression(lambda)) + ylab(expression(F(lambda)))
#setEPS()
#postscript("fig1.eps",width=5,height=3)
pdf("fig1.pdf",width=6,height=3)
fig1
dev.off()

#