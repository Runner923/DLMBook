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

