# Returns dataframes ready for plotting in ggplot, lke so:
#
#   plot.data <- rd.ggplot.data(model.rd, bw.level=1)
#   p <- ggplot()
#   p + geom_point(data=plot.data$raw.data, aes(x=X, y=Y), alpha=0.2) + geom_vline(xintercept=0, size=2, colour="grey") +
#     geom_ribbon(aes(x=lval, ymin=llwr, ymax=lupr), data=plot.data$left, alpha=0.4, fill="red") + 
#     geom_ribbon(aes(x=rval, ymin=rlwr, ymax=rupr), data=plot.data$right, alpha=0.4, fill="darkgreen") + 
#     geom_line(aes(x=lval, y=lest), data=plot.data$left, size=2, colour="red") +
#     geom_line(aes(x=rval, y=rest), data=plot.data$right, size=2, colour="darkgreen") +
#     coord_cartesian(xlim=c(-0.5, 0.5), ylim=c(-1, 1))
#
# x is an RD model from the rdd package
#
# bw.level refers to the size of the bandwidth:
#   1 = actual bandwidth
#   2 = 50%
#   3 = 200%
#
# variable refers to the variable to plot:
#   1 = regular running variable
#   2 = assignment variable in fuzzy RD

rd.ggplot.data <- function(x, bw.level=1, variable=1, gran=400, range, ...) {
  frm<-FALSE
  if("frame" %in% names(x$call)) frm<-eval.parent(x$call$frame)
  if(!frm){
    x$call$frame<-TRUE
    x$call$verbose<-FALSE
    x<-eval.parent(x$call)
  }
  d<-as.data.frame(x$frame)
  
  if(length(x$na.action)>0)
    d<-d[-x$na.action,]
  
  if("kernel" %in% names(x$call)) 
    kern<-eval.parent(x$call$kernel)
  else 
    kern<-"triangular"
  
  if("cutpoint" %in% names(x$call)) 
    cut<-eval.parent(x$call$cutpoint)
  else
    cut<-0
  
  bw<-x$bw[bw.level]
  
  if(missing(range)) {
    range<-c(cut-10*bw,cut+10*bw)
    if(range[1]<min(d$X)) range[1]<-min(d$X)
    if(range[2]>max(d$X)) range[2]<-max(d$X)
  }
  
  if(range[1]=="min")
    range[1]<-min(d$X)
  if(range[2]=="max")
    range[2]<-max(d$X)
  range<-as.double(range)
  
  rdplot.ggplot<-function(d) {
    d.l<-data.frame(X=d$X[d$X<cut],Y=d$Y[d$X<cut])
    lval<-seq(range[1],cut,length.out=(gran%/%2))
    lest<-vector(length=(gran%/%2))
    llwr<-vector(length=(gran%/%2))
    lupr<-vector(length=(gran%/%2))
    for(i in 1:(gran%/%2)) {
      sub<-d.l$X>=(lval[i]-bw) & d.l$X<=(lval[i]+bw)
      w<-kernelwts(X=d.l$X[sub],center=lval[i],bw=bw,kernel=kern)
      ly<-d.l$Y[sub]
      lx<-d.l$X[sub]
      if(length(lx)<=2)
        pred<-rep(NA,3)
      else
        pred<-predict(lm(ly~lx,weights=w),interval="confidence",newdata=data.frame(lx=lval[i]))
      lest[i]<-pred[1]
      llwr[i]<-pred[2]
      lupr[i]<-pred[3]
    }
    
    d.r<-data.frame(X=d$X[d$X>=cut],Y=d$Y[d$X>=cut])
    rval<-seq(cut,range[2],length.out=(gran%/%2))
    rest<-vector(length=(gran%/%2))
    rlwr<-vector(length=(gran%/%2))
    rupr<-vector(length=(gran%/%2))
    for(i in 1:(gran%/%2)) {
      sub<-d.r$X>=(rval[i]-bw) & d.r$X<=(rval[i]+bw)
      w<-kernelwts(X=d.r$X[sub],center=rval[i],bw=bw,kernel=kern)
      ry<-d.r$Y[sub]
      rx<-d.r$X[sub]
      if(length(rx)<=2)
        pred<-rep(NA,3)
      else
        pred<-predict(lm(ry~rx,weights=w),interval="confidence",newdata=data.frame(rx=rval[i]))
      rest[i]<-pred[1]
      rlwr[i]<-pred[2]
      rupr[i]<-pred[3]
    }
    
    l.plot <- data.frame(lval, lest, llwr, lupr)
    r.plot <- data.frame(rval, rest, rlwr, rupr)
    return(list(raw.data=d, left=l.plot, right=r.plot))
  }
  
  if(x$type=="fuzzy" & variable==2){
    d$Y<-d$Z
    rdplot.ggplot(d)
  } else {
    rdplot.ggplot(d)
  }
}