rm(list=ls())

# p.57
NilePoly <- dlmModPoly(order=1, dV=15100, dW=1468)
unlist(NilePoly)
NilePoly
NileFilt <- dlmFilter(y=Nile, mod=NilePoly)
NileFilt
str(NileFilt, 1)
n <- length(Nile)
class(NileFilt)
dlmSvd2var(u=NileFilt$U.C[[n+1]], d=NileFilt$D.C[[n+1]])

plot(Nile, type='o', col=c("darkgrey"))
mod1 <- dlmModPoly(order=1, dV=15100, dW=755)
NileFilt1 <- dlmFilter(y=Nile, mod=mod1)
lines(dropFirst(NileFilt1$m), lty = "longdash")
mod2 <- dlmModPoly(order=1, dV=15100, dW=7550)
NileFilt2 <- dlmFilter(y=Nile, mod=mod2)
lines(dropFirst(NileFilt2$m), lty = "dotdash", col="blue")
leg <- c("data", paste("filtered, W/V ="
                       , format(c(W(mod1) / V(mod1)
                                  ,W(mod2) / V(mod2)
                       )))
)
legend(x="bottomright", legend=leg, col=c("darkgrey", "black", "blue")
       , lty=c("solid", "longdash", "dotdash")
       , pch=c(1, NA, NA)
       , bty="n"
)
# # snakes' legs
# plot(Nile, col="grey")
# # points(NileFilt$y, col="blue")
# points(NileFilt$m[-1], col="black", type="l")
# # points(NileFilt$a, col="yellow", type="l")
# points(NileFilt$f, col="red", type="l")


# p.63
NileSmooth <- dlmSmooth(y=NileFilt)
str(NileSmooth, 1)
drop(dlmSvd2var(u=NileSmooth$U.S[[n+1]], d=NileSmooth$D.S[n+1,]))
drop(dlmSvd2var(NileFilt$U.C[[n+1]], NileFilt$D.C[n+1, ]))
drop(dlmSvd2var(NileSmooth$U.S[[n/2 + 1]], NileSmooth$D.S[n/2 + 1,]))
drop(dlmSvd2var(NileFilt$U.C[[n/2+1]], NileFilt$D.C[n/2+1,]))

hwid <- qnorm(p=0.025, lower.tail=F) *
  sqrt(unlist(dlmSvd2var(NileSmooth$U.S, NileSmooth$D.S)))
smooth <- cbind(NileSmooth$s, as.vector(x=NileSmooth$s) + hwid %o% c(-1,1))
plot(dropFirst(smooth), plot.type="s", type="l"
     , lty=c(1,5,5), ylab="Level", xlab=""
     , ylim=range(Nile))
lines(Nile, type="o", col="darkgrey")
legend("bottomleft", col=c("darkgrey", rep("black", 2))
       , lty=c(1,1,5), pch=c(1,NA,NA), bty="n"
       , legend=c("data", "smoothed level", "95% probability lines"))

# p.67
# expenditure
expd <- ts(read.table("inst//extdata/qconsum.dat",skip=4
                      , colClasses="numeric")[,1]
           , start=c(1957, 1)
           , frequency=4)
expd.dlm <- dlm(m0 = rep(0,4), C0 = 1e8 * diag(4)
                , FF = matrix(c(1,1,0,0), nrow=1)
                , V = 1e-3
                , GG = bdiag(matrix(1)
                             , matrix(c(-1,-1,-1,1,0,0,0,1,0), nrow=3, byrow=T))
                , W = diag(c(771.35, 86.48, 0, 0), nrow=4)
)
plot(expd, xlab = "", ylab="Expenditures", type="o", col="darkgrey")
### Filter
expdFilt <- dlmFilter(y=expd, mod=expd.dlm)
lines(dropFirst(x=expdFilt$m[,1]), lty="dotdash")
### Smooth
expdSmooth <- dlmSmooth(y=expdFilt)
lines(dropFirst(x=expdSmooth$s[,1]), lty="longdash")
legend("bottomright", col=c("darkgrey", rep("black", 2))
       , lty = c("solid", "dotdash", "longdash")
       , pch = c(1,NA,NA)
       , bty = "n"
       , legend=c("data", "filtered level", "smoothed level"))

### 寄り道: parameter estimation with using most likelyhood
# expdFiltによる予測
lines(expdFilt$f, lty="dotdash", col="green")
# ARMA付加(AR(3))
build <- function(param){
  expd.dlm + dlmModARMA(ar=c(0,param[1],param[2]), sigma2=exp(param[3]), dV=exp(param[4]))
}
fit <- dlmMLE(y=expd, parm=rep(0,4), build=build)
expdFiltWithARMA <- dlmFilter(y=expd, mod=build(param=fit$par))
lines(dropFirst(x=expdFiltWithARMA$m[,1]), lty="dashed", col="red")
# 予測 の つもり: expdFiltと一致してしまうように見える.かなり近いが,ARをつけたためか引きずられ気味になる.
# もともとのexpdFiltを前提にしているからだろう.
# expdFilt自体のパラメータ推定を含めて試行しておきたい.
lines(x=expdFiltWithARMA$f, lty="dashed", col="blue")

### Seasonal component
plot(dropFirst(expdSmooth$s[,2]), type="o", xlab = "", ylab="Expenditure - Seasonal component")
abline(h=0)

# p.68 prediction
# Nile
a <- window(x=cbind(Nile, NileFilt1$f, NileFilt2$f)
            , start= 1880, end=1920)
plot(a[,1], type="o", col="darkgrey", xlab="", ylab="Level")
lines(a[,2], lty="longdash")
lines(a[,3], lty="dotdash")
leg <- c("data", paste("one-step-ahead forecast, W/V = "
                       , format(c(W(mod1) / V(mod1),
                                  W(mod2) / V(mod2)))))
legend("bottomleft", legend=leg, col=c("darkgrey", rep("black",2))
       , lty=c("solid", "longdash", "dotdash")
       , pch=c(1,NA,NA)
       , bty="n")

# p.69 : time-variant variance
mod0 <- dlmModPoly(order=1, dV=15100, dW=1468)
X <- ts(matrix(mod0$W, ncol=1, nrow=length(Nile)), start=start(Nile))
window(X, 1898, 1899) <- 12 * mod0$W
modDam <- mod0
modDam$X <- X
modDam$JW <- matrix(data=1, nrow=1,ncol=1)
damFilt <- dlmFilter(y=Nile, mod=modDam)
mod0Filt <- dlmFilter(y=Nile, mod=mod0)
# most of all codes following this line are copied and pasted.
a <- window(x=cbind(Nile, mod0Filt$f, damFilt$f)
            , start= 1880, end=1920)
plot(a[,1], type="o", col="darkgrey", xlab="", ylab="Level")
lines(a[,2], lty="longdash")
lines(a[,3], lty="dotdash")
abline(v=1898, lty=2)
leg <- c("data", paste("one-step-ahead forecast, W/V = "
                       , c("mod0", "modDam")))
legend("bottomleft", legend=leg, col=c("darkgrey", rep("black",2))
       , lty=c("solid", "longdash", "dotdash")
       , pch=c(1,NA,NA)
       , bty="n")

# p.73 : k-step ahead forecasting simulation
set.seed(1)
expdFore <- dlmForecast(mod=expdFilt, nAhead=12, sampleNew=100)
plot(window(x=expd, start=c(1964,1)), type="o"
     , xlim=c(1964, 1971), ylim=c(350,850)
     , xlab="", ylab="Expenditures")
names(expdFore)
invisible(lapply(expdFore$newObs, function(x) lines(x, col = "darkgrey", type='o', pch=4)))
lines(expdFore$f, type='o', lwd=2, pch=16)
abline(v=mean(c(time(expdFore$f)[1], time(expd)[length(expd)])), lty="dashed")
