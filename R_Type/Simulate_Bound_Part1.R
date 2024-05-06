# 模擬門檻函式：
# 模擬 fixed lag = optDelay
fixedLag_boot <- function(optDelay,timeleng,Xt_ori){
  # 將原始 Xt 丟入 arima 模型
  inf_X <- arima(Xt_ori,c(1,0,0))
  # 取得 AR(1) 之參數
  arBootPara <- inf_X$coef[[1]]
  # 以此參數建立 Xt_boot 之序列
  Xt <-  arima.sim(model = list(order=c(1,0,0),ar=arBootPara) , n=timeleng)
  
  Yt <- numeric(length = timeleng)
  
  for (i in (1+optDelay):timeleng) {
    Yt[i] <- Xt[i-optDelay] 
  }
  
  Yt[1:timeleng]    <- Yt[1:timeleng]    + rnorm(timeleng,0,1)  
  
  Yt <- ts(Yt)
  
  return(list(Xt,Yt))
}
