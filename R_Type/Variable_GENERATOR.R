variableLag_Case <- function(ARpara,  # 此為 AR(1) model 的參數
                             lagA,    # 混和的第一個 lag
                             lagB,    # 混和的第二個 lag
                             timeLeng,# 時間長度
                             probA = 0.5,
                             probB = 0.5){ 
  
  
  Xt <-  arima.sim(model = list(order=c(1,0,0),ar=ARpara) , n=timeLeng) 
  
  Yt <- numeric(length = timeLeng)
  
  # Xt 的 index
  ind_i <- 1
  # Yt 的 index
  ind_j <- 1
  
  # 安排 Yt 符合混和 lag 的規則
  repeat{
    # 抽取 0 ~ 1
    Pick <- runif(1,min = 0,max = 1)
    
    if(Pick <= probA){
      
      # lagA 的安排
      if((ind_i + lagA) > timeLeng){
        break
      } else {
        ind_j <- ind_i + lagA
        Yt[ind_j] <- Xt[ind_i]
        ind_i <- ind_j
      }
    } else {
      # lagB 的安排
      if((ind_i + lagB) > timeLeng){
        break
      } else {
        ind_j <- ind_i + lagB
        Yt[ind_j] <- Xt[ind_i]
        ind_i <- ind_j
      }
    }
  }
  
  Yt[1:timeLeng]    <- Yt[1:timeLeng]    + rnorm(timeLeng,0,1)
  Yt <- ts(Yt)
  return(list(Xt,Yt))
}