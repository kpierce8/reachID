#Find all subpaths that include the out flow reach
subpaths <-  get.subpaths(rownum,now.path.object,branches,branches[rownum,3])
assign(paste("subpaths",path.lev,sep="."),subpaths)
#Find all unique reaches within the subpaths
segs.uni <-  find.unique(subpaths)
segs.arc   <-  get.arcid(segs.uni)
segs.arc <- get.nid(segs.arc)
#Find which unique reaches are terminal reaches
subends <-  intersect(segs.uni,endnodes)
new.longest <-  find.longest(subends,subpaths)
#Find which subpath has the longest length
new.longest <- get.arcid(new.longest)
new.longest <- get.nid(new.longest)
#Tag all the paths not in the longest path with the current iterated paths id
  path.ids <- tag.segs(segs.arc,path.lev-1,path.ids)
  assign(paste("path.arcs",path.lev,sep="."),new.longest)
  assign(paste("lev.index",path.lev,sep="."),1)
        pathid <- pathid + 1
#Assign ids to the longest subpath, mark all of those reaches as id's and give a route number
  path.ids <- path.assign(path.ids,new.longest,path.lev,pathid)
#Decrement path level if current longest path is length 1   
  if(length(new.longest)==1) path.lev <- path.lev-1
