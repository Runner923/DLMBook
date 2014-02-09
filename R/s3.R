s3 <- function(polydim=1){
  # p.95, 96
  lakeSup <- ts(read.table("inst//extdata/lakeSuperior.dat",skip=3
                           , colClasses="numeric")[,2]
                , start=1900)
  build <- function(para){
    dlmModPoly(order=polydim, dV=exp(para[1]), dW=exp(para[2:(polydim+1)]), m0=rep(lakeSup[1], polydim), C0=1e7 * diag(polydim))
  }
  mle.result <- dlmMLE(y=lakeSup, parm=rep(0, (polydim+1)), build=build)
  modLSup <- build(mle.result$par)
  str(modLSup,1)
  lSupFilt <- dlmFilter(y=lakeSup, mod=modLSup)
  res <- residuals(object=lSupFilt, sd=F)
  plot(lakeSup, type="o", col="darkgrey")
  lines(lSupFilt$f, col="red")
  shapiro.test(res)
  Box.test(x=res, lag=20, type="Ljung")
  sapply(X=1:20, function(i)
    Box.test(res, lag=i, type="Ljung-Box")$p.value
  )
  tsdiag(lSupFilt)
  build.poly <- build
  
  # p.96 HoltWinters function
  HWout <- HoltWinters(x=lakeSup, gamma=F, beta=F)
  plot(lakeSup, type="o", col="darkgrey")
  lines(dropFirst(lSupFilt$f), lty="dashed", xlab="", ylab="")
  lines(HWout$fitted[, "level"])
  leg <- c("Holt-Winters", "Local level DLM")
  legend("topleft", legend = leg, bty="n", lty=c("solid", "dashed"))
  
  # p.98, 99 : spain
  invSpain <- ts(read.table("inst//extdata/invest2.dat",skip=0
                           , colClasses="numeric")[,2]
                , start=1960)
  # local level
  modPolyAnalisys(invSpain, 1)
  # linear growth model
  modPolyAnalisys(invSpain, 2)
  print("integrated process")
  modPolyAnalisys(invSpain, 2, T)
  # quadratic growth ? model
  modPolyAnalisys(invSpain, 3)
  print("integrated process")
  modPolyAnalisys(invSpain, 3, T)
}

modPolyAnalisys <- function(indata, polydim = 1, integrated=F){
  if(integrated == T){
    init <- rep(0,2)
    build <- function(para){
      dlmModPoly(order=polydim, dV=exp(para[1]), dW=c(rep(0, polydim-1), exp(para[2]))
                 , m0=c(indata[1], rep(0, polydim-1)), C0=1e7 * diag(c(rep(0, polydim-1), 1)))
    }
  } else {
    init <- rep(0, (polydim+1))
    build <- function(para){
      dlmModPoly(order=polydim, dV=exp(para[1]), dW=exp(para[2:(polydim+1)])
                 , m0=c(indata[1], rep(0, polydim-1)), C0=1e7 * diag(polydim))
    }
  }  
  indata.mle <- dlmMLE(y=indata, parm=init, build=build)
  indata.dlm <- build(para=indata.mle$par) 
  str(indata.dlm, 1)
  indata.filt <- dlmFilter(y=indata, mod=indata.dlm)
  plot(indata, type="o", col="darkgrey")
  # first element of state vector
  if(polydim == 1){
    indata.filt$m <- matrix(indata.filt$m, ncol=1)
  }
  lines(dropFirst(indata.filt$m[,1]), col="red")
  lines(indata.filt$f, col="red", type="o", lty="longdash")
  res <- residuals(object=indata.filt,sd=F)
  qqnorm(res)
  qqline(res)
  tsdiag(indata.filt)
  shapiro.test(res)
  Box.test(x=res, lag=20, type="Ljung")
  sapply(X=1:20, function(i)
    Box.test(res, lag=i, type="Ljung-Box")$p.value
  )
}