# exercise 2.5, 2.6
e2_5 <- function(){
  y <- c(17, 16.6, 16.3, 16.1, 17.1, 16.9, 16.8, 17.4, 17.1, 17)
  length(y)
  # (a): filtering 
  mod.ll <- dlmModPoly(order=1, dV=0.25, dW=25, m0=17.2, C0=1)
  mod.ll.filt <- dlmFilter(y=y, mod=mod.ll)
  # plot 
  plot(y, col="grey", pch=20, type="o")
  lines(dropFirst(mod.ll.filt$m), col="blue", lwd=0.5)
  # (b)
  lines(mod.ll.filt$f, pch=20, type="l", lwd=0.5)
  # グレーラインが実データ,黒が予測
  # グレーを1ステップ遅れて黒がフォローするようになっている.
  # 予測値は前期の実現値でほぼ決まっているということである。
  # 理想的な予測であれば、グレーラインを左にずらしたような線になる。
  
  # (c)
  print("(c)")
  pairs <- combn(x=c(0.01, 0.25, 1, 5, 25, 125), m=2)
  invisible(
    sapply(X=1:ncol(pairs), FUN=function(i){
      V(mod.ll) <- pairs[1,i]
      W(mod.ll) <- pairs[2,i]
      mod.ll.filt <- dlmFilter(y=y, mod=mod.ll)
      plot(y, col="darkgrey", pch=20, type="o")
      lines(dropFirst(mod.ll.filt$m), col="blue", lwd=0.5)
      lines(mod.ll.filt$f, pch=20, type="l", lwd=0.5, lty="longdash")
      leg <- c("data", paste("filtered, W/V ="
                             , format(c(W(mod.ll) / V(mod.ll))))
               , "forcast")
      legend(x="bottomright", legend=leg, col=c("darkgrey", "blue", "black")
             , lty=c("solid", "solid", "longdash")
             , pch=c(1, NA, NA)
             , bty="n"
      )
    })
  )
  # W/V比が大きいほど, 実績データに当てはまる.
  # (d)
  print("(d)")
  # 1点目のデータに近い点から始めることで,初期の 当てはまりを良くできる.
  mod.ll <- dlmModPoly(order=1, dV=0.25, dW=25, m0=1, C0=0.3)
  mod.ll.filt <- dlmFilter(y=y, mod=mod.ll)
  mod.ll2 <- dlmModPoly(order=1, dV=0.25, dW=25, m0=1, C0=30)
  mod.ll2.filt <- dlmFilter(y=y, mod=mod.ll2)
  # plot 
  plot(y, col="grey", pch=20, type="o")
  lines(dropFirst(mod.ll.filt$m), col="blue", lwd=0.5)
  lines(mod.ll.filt$f, pch=20, type="l", lwd=0.5)
  lines(dropFirst(mod.ll2.filt$m), col="blue", lwd=0.5, lty="longdash")
  lines(mod.ll2.filt$f, pch=20, type="l", lwd=0.5, lty="longdash")
  
  # (e)
  print("(e)")
  m0(mod.ll) <- 17
  C0(mod.ll) <- 1
#  smooth.ll <- dlmSmooth(mod.ll.filt)
  smooth.ll <- dlmSmooth(y=y, mod=mod.ll)
  plot(y, col="darkgrey", pch=20, type="o")
#  lines(dropFirst(mod.ll.filt$m), col="blue", lwd=0.5)
  lines(dropFirst(smooth.ll$s), col="black", lwd=0.5, lty="dotdash")
}
