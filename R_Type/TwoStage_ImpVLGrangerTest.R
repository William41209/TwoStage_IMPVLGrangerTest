### 總合併程式：
### 要執行此城市需要將前面所有函式都載入才可使用
SeparateImpVLGranger <- function(Yt,Xt,
                                 maxLag = 5,         # 最大 lag 之值
                                 alpha = 0.05,       # 設定 alpha
                                 gamma = 0.5,        # VLG 用
                                 autoLagflag = TRUE, # VLG 用
                                 family = gaussian,  # VLG 用
                                 #timeLeng_Bound = 1000,    # 設定模擬門檻之時間長度
                                 repTime = 200,  # 設定模擬門檻之遞迴次數
                                 #block = 10,     # 設定 block
                                 N_firstDO = 3)  # 原方法要做幾個   
{
  Infor <- VLGrangerFunc_Test(Yt,Xt,alpha,maxLag,gamma,autoLagflag,family)
  timeLeng_Origin <- as.numeric(length(Xt)) # 取得 Xt 時間長度作為後續 boot 使用
  if( (Infor$p.val < alpha) && (Infor$follOut$optDelay_YX > 0) ){
    inf_tmp <- Separate_Method(Yt,Xt,maxLag,timeLeng_Bound = timeLeng_Origin,repTime,
                               alpha,
                               #block,
                               N_firstDO)
    en <- inf_tmp$en
    Bound <- inf_tmp$Bound
    return(list(en=en,Bound=Bound))
  } 
  else {
    print("所給定之 Xt 對於 Yt 不具有 lag 之關係 .")
    en <- 0
    return(en)
  }
}