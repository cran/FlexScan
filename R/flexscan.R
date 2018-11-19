flexscan=function(map,case,pop,nsim=999,k=10,alpha=0.05,isplot=TRUE,col=c("red","blue")){

  Srs=slot(map,"polygons")
  coords=t(sapply(Srs, function(i) slot(i, "labpt")))

  nb=poly2nb(map)
  listw=nb2listw(nb,style="B", zero.policy=TRUE)
  w=as(listw,"symmetricMatrix")
  w=as.matrix(w)

  map=map;cases=case;pop=pop;nsim=nsim;k=k;alpha=alpha;isplot=isplot;col=col

  fit=flex.test(coords=coords,cases=cases,pop=pop,w=w,k=k,type="poisson",nsim=nsim,alpha=alpha,lonlat=FALSE,parallel=TRUE)

  rst=matrix(rep(NA,length(fit$clusters)*8),ncol=8)
  dimnames(rst)=list(1:length(fit$clusters),c("Cluster Type","Region ID","Observed Cases","Expected Cases","SR","RR","LLR","P Value"))
  rst[,1]=c("Most Likely Cluster",rep("Secondary Cluster",length(fit$clusters)-1))
  rst[,2]=unlist(lapply(sapply(fit$clusters,"[",1),function(x) paste(x,collapse=", ")))
  rst[,3]=unlist(sapply(fit$clusters,"[",3))
  rst[,4]=sprintf("%.3f",unlist(sapply(fit$clusters,"[",4)))
  rst[,5]=sprintf("%.3f",unlist(sapply(fit$clusters,"[",5)))
  rst[,6]=sprintf("%.3f",unlist(sapply(fit$clusters,"[",6)))
  rst[,7]=sprintf("%.3f",unlist(sapply(fit$clusters,"[",7)))
  rst[,8]=sprintf("%.3f",unlist(sapply(fit$clusters,"[",8)))

  if(isplot){
    par(mai=c(0.2,0.2,0.2,0.2))
    plot(map)
    plot(map[sapply(fit$clusters,"[",1)[[1]],],add=TRUE,col=col[1])
    for(i in 2:length(fit$clusters)){
      plot(map[sapply(fit$clusters,"[",1)[[i]],],add=TRUE,col=col[2])
    }
    legend("bottomright",legend=c("Most likely cluster","Secondary cluster"),pch=15,col=c("red","blue"),bty="n")
  }

  return(rst)

}
