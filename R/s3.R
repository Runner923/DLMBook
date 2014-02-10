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
  modPolyAnalisys(invSpain, polydim=1)
  # linear growth model
  modPolyAnalisys(invSpain, polydim=2)
  print("integrated process")
  modPolyAnalisys(invSpain, polydim=2, integrated=T)
  # quadratic growth ? model
  modPolyAnalisys(invSpain, polydim=3)
  print("integrated process")
  modPolyAnalisys(invSpain, polydim=3, integrated=T)
  # forcasting
  print("forcasting")
  modPolyAnalisys(indata=invSpain, ylim=c(0,4000) + range(invSpain), polydim=1, filt.start=c(1960,1), filt.end=c(1985,1))
  modPolyAnalisys(indata=invSpain, ylim=c(0,4000) + range(invSpain), polydim=2, filt.start=c(1960,1), filt.end=c(1985,1))
  modPolyAnalisys(indata=invSpain, ylim=c(0,4000) + range(invSpain), polydim=3, filt.start=c(1960,1), filt.end=c(1985,1))
}

modPolyAnalisys <- function(indata, ylim = range(indata), polydim = 1, integrated=F
                            , filt.start=start(indata), filt.end=end(indata)){
  plot(indata, type="o", col="black")
  indata <- window(x=indata, start=filt.start, end=filt.end)
  xlim <- c(start(indata)[1], end(indata)[1]) 
  if(integrated == T){
    init <- rep(3,2)
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
  # first element of state vector
  if(polydim == 1){
    m <- indata.filt$m
  } else {
    m <- indata.filt$m[,1]
  }
  lines(dropFirst(m), col="red")
  lines(indata.filt$f, col="red", type="o", lty="longdash")
  # forecasting
  set.seed(1)
  indata.forecast <- dlmForecast(mod=indata.filt, nAhead=10, sampleNew=100)
  invisible(lapply(indata.forecast$newObs, function(x) lines(x, col = "darkgrey", type='o', pch=4)))
  lines(indata.forecast$f, type='o', lwd=2, pch=16)
  abline(v=mean(c(time(indata.forecast$f)[1], time(indata)[length(indata)])), lty="dashed")
  print(paste(
    mean(abs(indata.filt$f - indata))
    , mean(abs(indata.filt$f - indata)^2)
    , mean(abs((indata.filt$f - indata) / indata))
    , sqrt(sum((indata.filt$f - indata)[-(1:5)]^2) / sum(diff(indata[-(1:4)])^2))
  ))
  # analysis of residuals
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