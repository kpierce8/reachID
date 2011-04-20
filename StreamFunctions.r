#StreamFunctions
library(foreign)

#Make new path object from list of endnodes
get.subpaths<- function(rownum,pathobject,branches,root){
  subget<- c()
    for(i in 1:length(pathobject)){
    subget<-c(subget,match(branches[rownum,3],pathobject[[i]]))    }
  pathset<-pathobject[!is.na(subget)] }

#ASSIGN ids to longest path
path.assign <- function(path.ids,arclist,path.lev,pathid){
      print(c(arclist,"node arcids?"))

  ntemp <- path.ids[match(arclist,path.ids$arcid),3]
      print(c("assign 1",ntemp))
  arclist <- arclist[is.na(ntemp)]
   
      print(c("where path.lev is =", path.lev, length(arclist)))
      if(length(arclist) == 1){
      #path.lev<- path.lev+1
      print("singleton")
      path.ids[match(arclist,path.ids$arcid),path.lev] <-seq(length(arclist),1)
      print(c("assign 4, arclist =", arclist))
      print(c("next is match=", match(as.numeric(arclist),path.ids$arcid)))
  #print(path.ids$arcid)
  path.ids[match(arclist,path.ids$arcid),3] <- c(-1) 
  path.ids[match(arclist,path.ids$arcid),dim(path.ids)[2]] <- c(pathid) 
      print("assign 5")
  path.ids[match(arclist,path.ids$arcid),(path.lev + 1):(dim(path.ids)[2]-1)] <- 0
       path.lev<- path.lev-1
       temp<-(get(paste("lev.index",path.lev,sep=".")))
       path.ids[match(arclist,path.ids$arcid),path.lev] <- temp
       print(c("post singelton path.lev =", path.lev))
       }
  else {path.ids[match(arclist,path.ids$arcid),path.lev] <-seq(length(arclist),1)
      print(c("assign 2, arclist =", arclist))
      print(c("next is match=", match(as.numeric(arclist),path.ids$arcid)))
  #print(path.ids$arcid)
  path.ids[match(arclist,path.ids$arcid),3] <- c(-1) 
  path.ids[match(arclist,path.ids$arcid),dim(path.ids)[2]] <- c(pathid)
      print("assign 3")
  path.ids[match(arclist,path.ids$arcid),(path.lev + 1):(dim(path.ids)[2]-1)] <- 0 }
  
  return(path.ids)}

#ASSIGN/Tag Intermediate
tag.segs<-function(seglist,index,id.matrix){
  # index is passed as path.lev-1, ie which column to write to
  #print(c(seglist,"arcids seg?")) 
  ntemp <- path.ids[match(seglist,path.ids$arcid),3]
  seglist <- seglist[is.na(ntemp)]
  print(c(ntemp,"split",seglist))
  repset <- match(seglist,id.matrix[,1])
  print(c("start=",get(paste("lev.index",path.lev-1,sep=".")),path.lev-1))                                  
  id.matrix[repset[is.na(id.matrix[repset,3])],index] <- get(paste("lev.index",path.lev - 1,sep="."))
  return(id.matrix)}

  
# Finds longest path from list of endnodes  (NEED TO GET LONGEST WITH UNID'D)
find.longest <- function(endlist,pathobject){
    endone <- path.ids[match(endlist,path.ids$from),3]
    print("longest debug")
    print(endone)
    lpath <- elengths[elengths[,2]==max(elengths[match(endlist[is.na(endone)], 
            elengths[,1]),2]),1]
    print(lpath)
    for(i in 1:length(pathobject)){
             if(pathobject[[i]][1]==lpath) newpath <- pathobject[[i]]    }
    print(newpath)
  return(newpath) }

#is.ID identified function  
  is.nid <- function(rownum,branches){
  is.na(path.ids[match(branches[rownum,3],path.ids$from),3])
  }

#is.END function
  is.end<- function(rownum,branches){
    branches[rownum,3] %in% endnodes
    }

#Swap values in rows
swapr <- function(x,srows){
  x[srows,2:3] <- x[srows,3:2]
  return(x[srows,])  }

#convert from(endnodes values) to arcid (segment ids)
get.arcid<- function(x){
                 y<-temp2.dbf[match(x,temp2.dbf[,3]),1]
                 print("NAs removed")
                 return(y[!is.na(y)])                 }

#convert from(endnodes values) to arcid (segment ids)
get.endnodes<- function(x){
                 y<-c(temp2.dbf[match(x,temp2.dbf[,1]),3],1)
                 return(y)                 }


#Make path object from endnode list and topology set ("id","from","to" database)
makepaths <-  function(endnodes,temp2.dbf){
    thepaths <-list(seq(1:length(endnodes)))
    for(i in 1:length(endnodes)){
      startn <- endnodes[i]
      epath <- startn
      subnodes <- temp2.dbf[temp2.dbf[,3]==startn,]
        while(dim(subnodes)[1] > 0){
        addend <- temp2.dbf[temp2.dbf[,3]==startn,2]
        epath <- c(epath, addend)
        startn <- addend
        subnodes <- temp2.dbf[temp2.dbf[,3]==startn,]            }
      #print(epath)
      thepaths[[i]] <- epath    }
      return(thepaths)  }

#Using path object create vector of lengths
get.lengths <- function(thepaths){
  lengths<- c()
  for(i in 1:length(thepaths)){
  path.length<-sum(temp2.dbf[match(thepaths[[i]],temp2.dbf[,3]),6],na.rm=TRUE)
  lengths <- c(lengths,path.length)}
  }

#Find unique segments in a pathobject
find.unique <- function(pathobject){
  seg.unique <- c()
    for(i in 1:length(pathobject)){
            seg.unique <- c(seg.unique, as.vector(pathobject[[i]]))         }
  seg.unique <- unique(seg.unique)   }

get.nid <- function(newpath){
ntemp <- path.ids[match(newpath,path.ids$arcid),3]
    newpath <- newpath[is.na(ntemp)]  
    }  
    
#PATH ASSIGN 2    
path.assign2 <- function(path.ids,arclist,path.lev){
       
      print(c("where path.lev is =", path.lev))
  path.ids[match(arclist,path.ids$arcid),path.lev] <-  1
      print(c("assign 6, arclist =", arclist))
      print(c("next is match=", match(as.numeric(arclist),path.ids$arcid)))
  #print(path.ids$arcid)
  path.ids[match(arclist,path.ids$arcid),3] <- c(-1) 
      print("assign 7")
  path.ids[match(arclist,path.ids$arcid),(path.lev + 1):dim(path.ids)[2]] <- 0 
  return(path.ids)}
 
 
#Concatenate indices 
make.rid <- function(path.ids){
 RID <- data.frame(matrix(0,dim(path.ids)[1],2))
 for(i in 1:dim(RID)[1]){
 RID[i,1] <- match(0,path.ids[i,])
 RID[i,2] <- paste(path.ids[i,5:RID[i,1]-1],sep="",collapse=".")
 }
 return(RID)
}


#Updates
#1) Need to add path to code as an initiating variable
#2) Need to add a weighting system to alter longest paths for imposing hierarchy
#3) Replace route ID with initial path reach ID   
#4) Formalize route direction script