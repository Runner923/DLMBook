# p.124 CAPM
capm_1 <- function(file.path="inst/extdata/P.dat", target="IBM", counter.target="MARKET"){
  capm <- read.table(file.path)
  capm.ts <- ts(capm, start = c(1978,1), frequency=12)
  colnames(capm)
  plot(capm.ts)
  target <- capm.ts[,target] - capm.ts[,"RKFREE"]
  x <- capm.ts[,counter.target] - capm.ts[,"RKFREE"]
  outLM <- lm(target ~ x)
  print(outLM$coef)
  acf(outLM$residuals)
  qqnorm(outLM$residuals)
  
  mod <- dlmModReg(X=x, dV=0.00254, m0=c(0,1.5), C0=diag(c(1e+07,1)))
  outF <- dlmFilter(y=target,mod=mod)
  print(outF$m[1 + length(target), ])
  buildCapm <- function(u){
    dlmModReg(X=x, dV=exp(u[1]), dW=exp(u[2:3]))
  }
  outMLE <- dlmMLE(y=target, parm=rep(0,3), build=buildCapm)
  print(exp(outMLE$par))
  print(outMLE$value)
  mod <- buildCapm(outMLE$par)
  outS <- dlmSmooth(y=target, mod)
  plot(dropFirst(outS$s))
}