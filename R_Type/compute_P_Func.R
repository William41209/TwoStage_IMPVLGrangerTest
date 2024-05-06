# 重複執行部分函式：(已修正且優化)(o)
comP_func <- function(Yt,Xt,maxLag){
  
  Infor_VLG <- VLGrangerFunc_Test(Yt,Xt,maxLag=maxLag)
  
  if(Infor_VLG$p.val < 0.05){
    optDelay <- Infor_VLG$follOut$optDelay_RX
    lengthVL <- as.numeric(length(Infor_VLG$follOut$nX))
    #
    StdXt <- Infor_VLG$follOut$nX
    Rt <- Infor_VLG$follOut$rt
    
    if((length(Rt) != 0) && (optDelay > -1)){
      
      XY_Combine <- matrix(0,nrow = length(StdXt),ncol = 2)
      XY_Combine[,1] <- StdXt
      XY_Combine[,2] <- Rt
      #
      XtIndex <- Infor_VLG$follOut$Xt_index
      YtIndex <- Infor_VLG$follOut$Rt_index
      XYIndex_Combine <- matrix(0,nrow = length(XtIndex),ncol = 2)
      colnames(XYIndex_Combine) <- c("Xt","Yt")
      XYIndex_Combine[,1] <- XtIndex
      XYIndex_Combine[,2] <- YtIndex
      #
      XYIndex_DelPoint_Ori <- XYIndex_Combine
      #
      
      
      # 刪除 lag = 0 and lag < 0
      
      Pointer <- {}
      k <- 1
      for (i in 1:nrow(XYIndex_Combine)) {
        if(XYIndex_Combine[i,1] == XYIndex_Combine[i,2]){
          Pointer[k] <- i ; k <- k+1
        }
        if(XYIndex_Combine[i,1] > XYIndex_Combine[i,2]){
          Pointer[k] <- i ; k <- k+1
        }
      } 
      
      # DelPoint 法，直接刪除那列
      if(length(Pointer) != 0){
        XYIndex_DelPoint <- XYIndex_DelPoint_Ori[-Pointer,]
      } else {
        XYIndex_DelPoint <- XYIndex_DelPoint_Ori
      }
      RepeatRemove <- {} ; r <- 1
      # Xt重複，取lag最小者
      for (i in 2:nrow(XYIndex_DelPoint)) {
        if(XYIndex_DelPoint[i,1] == XYIndex_DelPoint[i-1,1]){
          RepeatRemove[r] <- i ; r <- r+1
        }
      }
      # 移除重複之欄位
      if(length(RepeatRemove) != 0){
        XYIndex_DelPoint_Final <- XYIndex_DelPoint[-RepeatRemove,]
      } else {
        XYIndex_DelPoint_Final <- XYIndex_DelPoint
      }
      # 超過Yt之長度的點去掉
      RecordRm_Del <- which(XYIndex_DelPoint_Final[,2] > lengthVL)
      
      if(length(RecordRm_Del) != 0){
        XYIndex_DelPoint_Final <- XYIndex_DelPoint_Final[-RecordRm_Del,]
      }
  
      # 多少個 lag != optDelay
      DiffLag_DelPoint <- sum((XYIndex_DelPoint_Final[,2]-XYIndex_DelPoint_Final[,1]) != optDelay)/nrow(XYIndex_DelPoint_Final)
      
    } # if的(Rt要有東西)
    
    # p.val < 0.05 但卻無 optDelay
    
  } # if的
  
  # 回傳 p 值 = 所占比例(刪去法)
  return(list(p_Del = DiffLag_DelPoint,optDelay = optDelay,IndexDiff = (XYIndex_DelPoint_Final[,2]-XYIndex_DelPoint_Final[,1])))
  
  
} # function 的