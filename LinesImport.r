#may need to use duplicated to find incorrectly routed segments

library(foreign)
library(stream.net)

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

#data(marys.aat,marys.elevslope, marys.lin, marys.pol)

#ander <- read.arcgenlin("G:\\StreamReach\\anderson")

temp.dbf <- read.dbf( "G:\\StreamReach\\arc_Layer2.dbf")

temp2.dbf <- data.frame(temp.dbf[,6],temp.dbf)

row.names(temp2.dbf) <- seq(1:dim(temp.dbf)[1])

dcheck1 <- duplicated(temp2.dbf[,3])

rswaps <- c(48,50)
temp2.dbf[rswaps,] <- swapr(temp2.dbf,rswaps)

#temp2.dbf[,2:3] <- temp2.dbf[,3:2]
names(temp2.dbf)[1:3] <- c("arcid","to","from")

temp2.dbf <-  temp2.dbf[order(temp2.dbf$arcid,decreasing=TRUE),]

write.csv(temp2.dbf[,1:3], "G:\\StreamReach\\seabeck.csv")



#seabeck <- read.arcgenlin("G:\\StreamReach\\seabeck.lin")

seabeck <- net.arcinput("G:\\StreamReach\\seabeck.csv", "G:\\StreamReach\\seabeck.lin", lineformat="R")

temp2.dbf[1,]

temp2.dbf



sea.dist <- net.dist(seabeck)

row.names(sea.dist) <- seabeck$segs$SEABECK_LI
seabeck$segs

net.orders(seabeck$links)
net.map(seabeck, linkatt=seabeck$links$lid)
net.map(seabeck, linkatt=seabeck$links$strahler)
