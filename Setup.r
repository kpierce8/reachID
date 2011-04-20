###Start SET-UP###################################
temp2.dbf <- data.frame(temp.dbf[,6],temp.dbf)
temp2.dbf[,2:3] <- temp2.dbf[,3:2]

#temp2.dbf[rswaps,] <- swapr(temp2.dbf,rswaps) #FUNCTION
#big beef 
#rswaps <- c(11,9,1)
#seabeck 
#rswaps <- c(48,50)

names(temp2.dbf)[1:3] <- c("arcid","to","from")
temp2.dbf <-  temp2.dbf[order(temp2.dbf$arcid,decreasing=TRUE),]
path.ids<- data.frame(temp2.dbf$arcid,temp2.dbf$from,matrix(NA,dim(temp2.dbf)[1],maxbranches))
names(path.ids)[1:3] <- c("arcid","from","ID Flag")
endnodes <- setdiff(temp2.dbf[,3],temp2.dbf[,2])
thepaths <- makepaths(endnodes,temp2.dbf) #FUNCTION
lengths <-  get.lengths(thepaths)  #FUNCTION
elengths <- data.frame(endnodes,lengths)
##############Assign 1st branches ids##############################################
pathid <- 1
path.lev <- 4 #4 is 4th column, i.e. first branches
path.nodes1 <- find.longest(endnodes,thepaths) #FUNCTION
path.arcs <- get.arcid(path.nodes1) #FUNCTION
path.ids <- path.assign(path.ids,path.arcs,path.lev,pathid) #FUNCTION

assign(paste("subpaths",path.lev,sep="."),thepaths)
assign(paste("path.arcs",path.lev,sep="."),path.arcs)
assign(paste("lev.index",path.lev,sep="."),1)

###END SET-UP###################################