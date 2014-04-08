#' \code{TStoDF}
#' @description TimeSeries オブジェクトをDataFrameに変換
#' for plotting with ggplot2
TStoDF <- function(obj.ts, target.columns=colnames(obj.ts), output.columns=(target.columns)){
  # tがtime-seriesだとmeltでまずいことになる
  temp.dt <- data.frame(t=as.numeric(time(obj.ts)), obj.ts)
  names(temp.dt)[-1] <- output.columns
  dt <- melt(data=temp.dt, id.vars="t", measure.vars=output.columns)
  dt
}

#' \code{TimeSeriesDFtoDFforggplot}
#' @description 素朴な時系列データフレーム
#' 
TimeSeriesDFtoDFforggplot <- function(dt, id="t", target.columns=colnames(dt)){
  dt <- melt(data=dt, id.vars=id, measure.vars=target.columns[target.columns != id])
  dt
}

DLMFilteredPredictionToDF <- function(dlmFiltered, exclude.indices=NULL, columnIndex=1, conf.interval=0.5){
  i <- columnIndex
  if("matrix" %in% class(dlmFiltered$y)){
    ts.y <- dlmFiltered$y[,i]
    mean.pred <-as.numeric(dlmFiltered$f[,i])
    f <- dlmFiltered$f [,i]
    sdev <- residuals(object=dlmFiltered)$sd[,i]
  } else {
    ts.y <- dlmFiltered$y
    mean.pred <-as.numeric(dlmFiltered$f)
    f <- dlmFiltered$f
    sdev <- residuals(object=dlmFiltered)$sd
  }
  original <- as.numeric(ts.y)
  t <- as.numeric(time(ts.y))
  dt <- data.frame(t=t, mean.pred = mean.pred, original=original)
  if(is.numeric(conf.interval) && abs(conf.interval) <= 1){
    interval.width <- qnorm(0.5 + 0.5 * conf.interval) * sdev
    upr <- as.numeric(f + interval.width)
    lwr <- as.numeric(f - interval.width)
    dt$upr = upr
    dt$lwr = lwr
  }
  if(is.integer(exclude.indices)){
    dt <- dt[-exclude.indices,]
  }
  tdt <- dt
  dt <- TimeSeriesDFtoDFforggplot(dt=dt)
  dt$colour <- as.character(dt$variable)
  dt$colour[dt$colour == "lwr" | dt$colour == "upr"] <- "conf.bound"
  return(list(dt=dt,TSDT=tdt))
}

#' 
#' @description 指定した値がupper, lowerの間にある割合を返す
CountInsideValuesRatio <- function(values, upper, lower){
  hits <- sum(values < upper & values > lower)
  return(hits / length(values))
}

#'
#' @param dfs DLMFilteredPredictionToDFの戻り値
PlotDLMFilteredPredictionDF <- function(dfs){
  g <- ggplot(data=dfs$dt,aes(x=t, y=value, colour=colour, group=variable))
  g <- g + geom_point() + geom_line()
  print(g)
  tdt <- dfs$TSDT
  predictionHitRate <- CountInsideValuesRatio(
    values=tdt$original, upper=tdt$upr, lower=tdt$lwr)
  predictionHitRate
}