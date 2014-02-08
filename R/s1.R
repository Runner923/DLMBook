# p.44
s1 <- function(){
  rw <- dlm(m0 = 0, C0 = 10, FF=1, V=1.4, GG=1, W=0.2)
  unlist(rw)
  
  # linear growth model
  lg <- dlm(FF= matrix(c(1,0), nrow=1)
            , V=1.4
            , GG=matrix(c(1, 0, 1, 1), nrow=2, byrow=F)
            , W=diag(c(0,0.2))
            , m0=rep(0,2)
            , C0=10*diag(2)
  )
  unlist(lg)
  lg
  is.dlm(lg)
  
  # set and get
  V(lg) <- 0.8
  W(lg)[2,2] <- 0.5
  V(lg)
  W(lg)
  lg
  
  # p.46
  # time-varying DLM
  x <- matrix(rnorm(100), ncol=1) # covariates
  #x <- matrix(rnorm(100), ncol=2) # covariates
  dlr <- dlm(FF = matrix(c(1,0), nrow=1)
             , V=1.3
             , GG=diag(2)
             , W=diag(c(0.4,0.2))
             , m0=rep(0,2)
             , C0=10*diag(2)
             , JFF=matrix(c(0,1), nrow=1)
             , X=x
  )
  dlr
  
  # p.47
  rw
  JV(rw) <- 1
  is.dlm(rw)
  # dlm(rw) # error: rw's got no elements of X.
  X(rw) <- rep(c(0.75, 1.25), c(10, 20))
  rw <- dlm(rw)
  V(rw)
  rw
}