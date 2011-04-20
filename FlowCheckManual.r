#may need to use duplicated to find incorrectly routed segments

library(foreign)

swapn <- function(x,srows){
  x[srows,2:3] <- x[srows,3:2]
  return(x)
  }
swapr <- function(x,srows){
  x[srows,2:3] <- x[srows,3:2]
  return(x[srows,])
  }  

duperows <- function(x,y=dupes){
  x[x[,3]==y,]
  }
temp.dbf <- read.dbf( "C:\\temp\\rebuilt_Layer.dbf")
dim(temp.dbf)
temp2.dbf <- data.frame(temp.dbf[,6],temp.dbf)
temp2.dbf[,2:3] <- temp2.dbf[,3:2]
tempout <- temp2.dbf
dcheck1 <- duplicated(tempout[,3])
check.rows <- tempout[tempout[,3] %in% tempout[dcheck1,3],]
check.rows

#SWAP HERE
#tempswaps <- c(10,4,3,1,15,14)
#tempswaps <- c(1,16,60,57,21,23,44,43,31,28,27)
tempout[tempswaps,] <- swapr(tempout,tempswaps) #FUNCTION
dcheck1 <- duplicated(tempout[,3])
check.rows <- tempout[tempout[,3] %in% tempout[dcheck1,3],]
check.rows

tempout



rswaps <- c(11,9,1)