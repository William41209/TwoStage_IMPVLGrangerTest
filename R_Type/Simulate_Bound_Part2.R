# Index bootstrap
# 傳統 bootstrap 用
fixedLag_bootIndex <- function(IndexDiff){
  
  
  Leng <- length(IndexDiff)
  
  IndexDiff_boot <- numeric(Leng)
  
  r <- sample(1:Leng,size = Leng,replace = T)
  
  for (i in 1:Leng) {
    IndexDiff_boot[i] <- IndexDiff[r[i]]
  }
  
  return(IndexDiff_boot=IndexDiff_boot)
}
