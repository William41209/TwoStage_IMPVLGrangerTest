# 實際程式(Separate Method)：
Separate_Method <- function(Yt,
                            Xt,maxLag,
                            timeLeng_Bound, # 設定時間長度
                            repTime = 200,  # 設定遞迴次數
                            alpha = 0.05,   # 設定 alpha
                            #block = 10,     # 設定 block
                            N_firstDO = 3)  # 原方法要做幾個 
{
  ### 計算並取得當前 case 的比例值
  ## Step 1. ~ 5.
  p_Original <- comP_func(Yt=Yt,Xt=Xt,maxLag)$p_Del
  optDelay   <- comP_func(Yt=Yt,Xt=Xt,maxLag)$optDelay
  
  IndexNote  <- {}
  
  ### 模擬實驗得到門檻值
  ## Step 6.
  
  
  # 設定時間長度
  timeLeng_Bound <- timeLeng_Bound
  # 設定遞迴次數
  repTime <- repTime
  # 設定 alpha
  alpha <- alpha
  
  # 取得分布 X = (p_1 , ... p_n)
  p_Distribution <- matrix(0,nrow = repTime,ncol = 1)
  for (j in 1:repTime) {
    if(1 <= j && j <= N_firstDO){
      # 取得 fixed lag = 1 之給定序列
      FixedCase_boot <- fixedLag_boot(optDelay = optDelay,timeleng = timeLeng_Bound,Xt_ori = Xt)
      Xt_boot <- FixedCase_boot[[1]]
      Yt_boot <- FixedCase_boot[[2]]
      
      # 紀錄於分布 X 中
      p_Distribution[j,1] <- comP_func(Yt=Yt_boot,Xt=Xt_boot)$p_Del
      IndexDiff <- comP_func(Yt=Yt_boot,Xt=Xt_boot)$IndexDiff
      IndexNote[[j]] <- IndexDiff
    } else {
      cho <- sample(1:N_firstDO,size = 1,replace = T)
      # 傳統 bootstrap
      Index_boot <- fixedLag_bootIndex(IndexNote[[cho]])
      
      # 紀錄於分布 X 中
      p_Distribution[j,1] <- sum(Index_boot != optDelay)/length(Index_boot)
    }
  }
  # 計算 p bar
  p_Mean <- mean(p_Distribution[,1])
  # 計算 p 的 sd
  p_SD   <- sd(p_Distribution[,1])
  # 計算門檻
  Bound  <- p_Mean + qnorm(1-alpha/2,0,1) * p_SD
  
  
  
  ### 利用門檻來判別
  ## p_Original >  Bound：variable lag
  ## p_Original <= Bound：fixed lag
  
  ## Step 7.
  
  if(p_Original >  Bound){
    print("所給定之 Xt 對於 Yt 具有 variable lag 之關係 .")
    en <- 2
  } else if(p_Original <=  Bound){
    print(paste("所給定之 Xt 對於 Yt 具有 fixed lag =",optDelay,"之關係",sep = " "))
    en <- 1
  }
  return(list(en=en,Bound = Bound))
}