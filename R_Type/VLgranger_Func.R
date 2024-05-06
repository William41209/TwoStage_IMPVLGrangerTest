# VL-Granger function：(o)(o 已將畫圖部分移除)

# Part 1:
TSNANNearestNeighborPropagation<-function(X)
{
  if(sum(is.na(X) ) == 0)
    return(X)
  lengthL<-length(X)
  t<-1
  if(is.na(X[1]) )
  {
    for(k in seq(2,lengthL))
    {
      if(!is.na(X[k]))
      {
        X[1:(k-1)]<-X[k]
        t<-k
        break;
      }
    }
  }
  for(k in seq(t+1,lengthL))
  {
    if(is.na(X[k]) )
    {
      X[k]<-X[k-1]
    }
  }
  Xout<-X
  return(Xout)
}

# Part 2:

# Part 2:(followingRelation)
followingRelation_imp <- function(Y,X,timeLagWindow,lagWindow=0.2)
{
  library(dtw)
  Y <- as.numeric(Y)
  X <- as.numeric(X)
  T <- length(X)
  timeLeng <- length(X)
  follVal<-0
  
  Y <- TSNANNearestNeighborPropagation(Y) # filling NA
  X <- TSNANNearestNeighborPropagation(X) # filling NA
  
  if(missing(timeLagWindow))
  {
    timeLagWindow<-ceiling(lagWindow*T )
  }
  ccfout_YX <- ccf(Y,X,lag.max = timeLagWindow,plot=FALSE)
  optDelay_YX <- which.max( abs(ccfout_YX$acf) )-(timeLagWindow+1)
  optCor_YX <- ccfout_YX$acf[which.max( abs(ccfout_YX$acf) )]
  # 消除平移：
  nX <- X - mean(X)
  
  nX <- as.matrix(nX)
  Y  <- as.matrix(Y)
  if(optDelay_YX < 0)
  {
    follVal<- -1
    VLval<-0
    
    list(follVal=follVal,nX=nX,optDelay_YX=optDelay_YX,optCor_YX=optCor_YX,ccfout_YX=ccfout_YX)
  }
  else{
    
    if(optCor_YX<0)
      nX = -nX
    
    PastSub <- timeLeng - (optDelay_YX - 1)
    
    oriSub  <- optDelay_YX
    
    # yPast 留下 
    Y_Past  <- ts(Y[-(PastSub:timeLeng)])
    
    # yr 留下 
    Y_ori    <- ts(Y[-(1:oriSub)])
    
    
    Hypo <- glm( Y_ori ~ Y_Past , family = gaussian)
    
    rt <- residuals(Hypo)
    
    rt <- ts(rt)
    
    # 最後 rt 對 Xt 做 DTW
    
    alignment <- dtw(x = rt,y = nX[-(1:optDelay_YX)],keep.internals=TRUE
                     ,window.type = "sakoechiba"
                     ,window.size = timeLagWindow # 此處要設定 變動值(timeLagWindow/optDelay_YX) 不然有問題
                     #                 ,window.size = optDelay_YX
                     ,step.pattern = symmetric1)
    
    Rt_index <- alignment$index1
    
    Xt_index <- alignment$index2
    
    ccfout_RX <- ccf(rt,nX[-(1:optDelay_YX)],lag.max = timeLagWindow,plot=FALSE)
    optDelay_RX <- which.max( abs(ccfout_RX$acf) )-(timeLagWindow+1)
    
    nX <- as.numeric(nX[-(1:optDelay_YX)])
    list(follVal=follVal,nX=nX,optDelay_YX=optDelay_YX,optCor_YX=optCor_YX,ccfout_YX=ccfout_YX
         ,ccfout_RX=ccfout_RX,optDelay_RX=optDelay_RX
         ,rt=rt
         ,Rt_index=Rt_index,Xt_index=Xt_index)
  }
}

# Part 3:(本體部分)
# Part 3:(VL-Granger.v)
VLGrangerFunc_Test <-function(Y,X,alpha=0.05,maxLag,gamma=0.5, autoLagflag=TRUE
                              ,family = gaussian)
{
  library(stats)
  library(tseries)
  library(dtw)
  
  XgCsY_ftest <- FALSE
  if(missing(maxLag))
    maxLag <- 0.2*length(Y)
  if(autoLagflag == TRUE)
  {
    follOut <- followingRelation_imp(Y=Y,X=X,timeLagWindow=maxLag)
    maxLag <- max(1,follOut$optDelay_RX)
  }
  else
  {
    follOut <- followingRelation_imp(Y=Y,X=X,timeLagWindow=maxLag)
  }
  if(follOut$optDelay_YX == 0) # prevent X ~ Y
    X <- follOut$nX
  else
    X <- follOut$nX
  if(follOut$follVal == -1)
    rt <- Y
  else
    rt <- follOut$rt
  RX <- cbind(ts(rt),ts(X))
  D  <- RX
  
  # Create time-shift vesions of r and x (r(t),x(t),r(t-1),x(t-1),...)
  for(i in 1:maxLag)
    D <-ts.intersect(D, stats::lag(RX,  - i))
  
  R  <- D[, 1]
  n  <- length(R)
  xRPast <- D[,  - (1:2)] # delete two targted columns (leave only y past and x past)
  xPast <- D[, 4]
  RPast <- xRPast[, ((1:maxLag) * 2) - 1] # delete all x columns (leave only y past)
  #========
  # Regression
  H1 <- glm(R ~ xRPast,family=family)
  H0 <- glm(R ~ RPast ,family=family)
  S1 <- sum(H1$resid^2)
  S0 <- sum(H0$resid^2)
  
  
  ftest <- ((S0 - S1)/maxLag)/(S1/(n - 2 * maxLag - 1))
  pval  <- 1 - pf(ftest, maxLag, n - 2 * maxLag - 1)
  BIC_H0<-(S0/n)*n^( (maxLag+1)/n ) # less value is better
  BIC_H1<-(S1/n)*n^( (2*maxLag+1)/n ) # less value is better
  
  if( (pval<=alpha) )
    XgCsY_ftest=TRUE
  XgCsY_BIC<- ( (BIC_H1<BIC_H0) )
  
  # BICDiffRatio > gamma implies X Granger-causes Y (option 3)
  BICDiffRatio<-(BIC_H0-BIC_H1)/BIC_H0
  XgCsY<- ( (BICDiffRatio>=gamma) ) # Our main flag of X causes Y using BICDiffRatio
  
  
  res<-list(ftest = ftest, p.val = pval,BIC_H1=BIC_H1, BIC_H0=BIC_H0,
            XgCsY_ftest=XgCsY_ftest,XgCsY_BIC=XgCsY_BIC,follOut=follOut,
            maxLag=maxLag,H1=H1,H0=H0,BICDiffRatio=BICDiffRatio,XgCsY=XgCsY)
  return(res)
}
