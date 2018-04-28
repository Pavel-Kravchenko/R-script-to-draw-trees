library(phytools)
tree <- read.tree(file = "/home/pavel/Desktop/Work/Project/Poster/tree_newick.txt")
library(readr)
for_tree_table <- read_csv("/home/pavel/Desktop/Work/Project/Poster/for_tree_table.csv", col_names = FALSE, trim_ws = FALSE)
for_tree_table
X<-data.frame(for_tree_table)
colnames(X)<-c("Uniparental maternal", "Uniparental paternal", "Biparental")
X

tree<-reorder(tree,"cladewise")
tree
tree$tip.label
length(tree$tip.label)
rownames(X)<-c(tree$tip.label)
X


plotTree(tree,plot=FALSE)
obj<-get("last_plot.phylo",envir=.PlotPhyloEnv)
plotTree(tree,lwd=1,ylim=c(0,obj$y.lim[2]),xlim=c(0,obj$x.lim[2]),
         ftype="off")
obj<-get("last_plot.phylo",envir=.PlotPhyloEnv)
h<-max(obj$xx)
fsize<-0.6
for(i in 1:Ntip(tree)){
  lines(c(obj$xx[i],h),rep(obj$yy[i],2),lty="dotted")
  text(h,obj$yy[i],tree$tip.label[i],cex=fsize,pos=4,font=3,offset=0.1)
}
s<-max(fsize*strwidth(tree$tip.label))
start.x<-1.05*h+s
cols<-setNames(c("red","blue","green"),c("Uniparental maternal", "Uniparental paternal", "Biparental"))
for(i in 1:ncol(X)){
  text(start.x,max(obj$yy),pos=4,srt=90,
       cex=0.7,offset=0)
  for(j in 1:nrow(X)){
    xy<-c(start.x,obj$yy[j])
    y<-c(xy[2]-0.5,xy[2]+0.5,xy[2]+0.5,xy[2]-0.5)
    asp<-(par()$usr[2]-par()$usr[1])/(par()$usr[4]-par()$usr[3])*
      par()$pin[2]/par()$pin[1]
    x<-c(xy[1]-0.5*asp,xy[1]-0.5*asp,xy[1]+0.5*asp,xy[1]+0.5*asp)
    polygon(x,y,col=cols[as.character(X[j,i])])
  }
  start.x<-start.x+asp
}

