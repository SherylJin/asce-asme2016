# --------------------------------------------------------------------------- #
# ASCE-ASME paper - code for Fig 1
# --------------------------------------------------------------------------- #

source("weibull-sys-functions.R")
source("plotdefs.R")

# example for Fig 1: assume mean ft = 9
k <- 2
y0 <- failuretolambda(9,k)
n0 <- 2
t1 <- 1
t2 <- 2

n2 <- n0 + 2
y2 <- (n0*y0 + t1^k + t2^k)/n2
y2

eft2 <- lambdatofailure(y2,k)
eft2

invgammavar <- function(n0, y0)
  y0^2/(1-1/n0)
invgammasd <- function(n0, y0)
  sqrt(y0^2/(1-1/n0))

invgammavar(n2, y2)
invgammasd(n2, y2)
invgammasd(n0, y0)

(2*failuretolambda(7) + 6^2 + 7^2)/4

# inverse gamma pdf and cdf with canonical parameters n0 and y0
cdinvgamma <- function(x, n0, y0, ...)
  dinvgamma(x, shape = n0+1, scale = n0*y0, ...)

cpinvgamma <- function(x, n0, y0, ...)
  pinvgamma(x, shape = n0+1, scale = n0*y0, ...)

xseq <- seq(0.1, 200, length.out = 300)

par(mar=c(3,3,1,1)+0.1)
plot(xseq, cdinvgamma(xseq, n2, y2), type="l", xlab="", ylab="")
mtext(expression(lambda), 1, 2)
mtext(expression(p(lambda)), 2, 2)
lines(xseq, cdinvgamma(xseq, n0, y0), lty=2)
legend("topright", legend = c("prior", "posterior"), lty = c(2,1),
       inset = 0.02)
#title(main = "Prior and posterior densities")

#setEPS()
#postscript("fig1.eps",width=5,height=3)
pdf("fig1.pdf",width=5,height=3)
par(mar=c(3,3,1,1)+0.1)
plot(xseq, cpinvgamma(xseq, n2, y2), type="l", xlab="", ylab="", col = "blue")
mtext(expression(lambda), 1, 2)
mtext(expression(F(lambda)), 2, 2)
lines(xseq, cpinvgamma(xseq, n0, y0), col = rgb(1,0,0,0.3))
legend("bottomright", legend = c("prior", "posterior"), lty= 1, col = rgb(c(1,0),0,c(0,1),c(0.3,1)),  c("red", "blue"),
       inset = 0.02)
dev.off()

#