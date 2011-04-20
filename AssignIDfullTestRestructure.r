#recursion can cue off of longest path hierarchy,  fill column one, then column 2 etc
library(foreign)
temp.dbf <- read.dbf( "G:\\StreamReach\\arc_LayerANDER2.dbf")
maxbranches <- 10
source("G:/StreamReach/StreamFunctions.r")
source("G:/StreamReach/Setup.r")

loopcount <- c()
################## BRANCH PROCESSING ######################################
while(is.na(sum(path.ids[,3])))
#for(test in 1:5)
{      print("################################################################")
       loopcount = loopcount + 1
      print(c("loop =", loopcount))
now.path.object <- get(paste("subpaths",path.lev,sep="."))  #Rootnode iteration
now.step <- get(paste("lev.index",path.lev,sep="."))
now.lpath <- get(paste("path.arcs",path.lev,sep="."))

now.nodes<-get.endnodes(now.lpath)
      print("lpath =")
      print(now.lpath)      
      print("now.nodes =")
      print(now.nodes)
      print(c("branch match value =", now.nodes[length(now.nodes)-now.step]))
branches <- temp2.dbf[temp2.dbf$to == now.nodes[length(now.nodes)-now.step],]
branches <- branches[order(path.ids[match(branches[,3],path.ids$from),3]),]
print(branches)
if(dim(branches)[1] < 2) {print("yowza.................."); 
            print(branches); path.lev <- path.lev -1;
            temp.lev <- get(paste("lev.index",path.lev+1,sep="."));
            if(temp.lev > 1){
            temp<-(get(paste("lev.index",path.lev,sep=".")));
                            print(temp)
                            temp <- temp + 1;
            assign(paste("lev.index",path.lev,sep="."),temp)                
            } }  
#else if(if(sum(path.ids[match(branches$arcid,path.ids$arcid),3]) == -2 &  branches
else { for(rownum in 1:dim(branches)[1])
        { 
            print(c("current path.lev main=",path.lev))
            print(branches[rownum,])
            print(is.na(path.ids[match(branches[rownum,3],path.ids$from),3]))
         if(is.nid(rownum,branches))  
              {print("6 check");
               print(path.ids[66,]);
              print(c(branches[rownum,3])); 
               path.lev <- path.lev + 1; 
               source("G:/StreamReach/BranchRoutine.r")}                       
         else if(is.end(rownum,branches)) 
              {source("G:/StreamReach/EndRoutine.r")
              # path.lev is decremented, then lev.index incremented 
              }
         else print("hi") 
        }  
         ##NEED END BOTH BRANCH LOGIC HERE
                        if(sum(path.ids[match(branches$arcid,path.ids$arcid),3]) == -2) 
                        {if(is.end(1,branches) & is.end(2,branches)) {
                            print(path.lev)
                            path.lev <- path.lev -1;
                            temp<-(get(paste("lev.index",path.lev,sep=".")));
                            print(temp)
                            temp <- temp + 1;
                            print(temp)
                            print(c("woo hoo, path .lev =",path.lev));
                            assign(paste("lev.index",path.lev,sep="."),temp)
                            assign(paste("lev.index",path.lev+1,sep="."),1) #new change
                            } else
                            {if(is.end(2,branches)){ 
                            temp<-(get(paste("lev.index",path.lev,sep=".")));
                            print(temp)
                            temp <- temp + 1;
                            print(temp)
                            print(c("wee hee, path .lev =",path.lev));
                            assign(paste("lev.index",path.lev,sep="."),temp)
                            } else
                            {if(is.end(1,branches)){print(c("uh boy, path .lev =",path.lev))}
                            }
                            }
                            } #else (print("pass1")}
                        else print("pass")      
         }
         
            #assign(paste("lev.index",path.lev-1,sep="."),now.step+1)    
}
#rm(branches)
##################Clean-up and export########################################
#head(path.ids)
tail(path.ids,n=20)

#path.ids
write.table(path.ids,"c:/path_ids.txt",sep=",")
sum.test <- apply(path.ids,2,sum,na.rm=TRUE)
sum.test
first.zero <- match(0,sum.test)
branch.levels <- first.zero - 4
test.sum <- data.frame(path.ids$arcid,RID=((path.ids[,4]*100000)+(path.ids[,5]*1000)+(path.ids[,6]*100)+(path.ids[,7]*10)+path.ids[,8]))
#test.sum
#write.dbf(test.sum, "C:/testridBB")
#path.ids
dim(path.ids)

test<- make.rid(path.ids)
path.ids[,max(test[,1])+1] <- test[,2]
path.ids2<-data.frame(path.ids[,1:(max(test[,1])+1)],path.ids[,dim(path.ids)[2]])
names(path.ids2)[4:(dim(path.ids2)[2]-2)] <- paste("lev",seq((dim(path.ids2)[2])-5),sep="")
names(path.ids2)[dim(path.ids2)[2]-1] <- c("RID")
names(path.ids2)[dim(path.ids2)[2]] <- c("RTE_ID")
write.dbf(path.ids2[,1:(max(test[,1])+2)], "C:/testridANDER2")

path.ids2[1,]
