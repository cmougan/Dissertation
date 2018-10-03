dropAll <- function(mod,save=NULL,drop=NULL,trace=F,sort=F,full=T,
                    data=NULL){
  if(!is.null(save))
    save <- attr(terms(as.formula(paste('~',paste(save,collapse='+')))),
                 "term.labels")
  else if(!is.null(drop)){
    save <- attr(terms(as.formula(paste('~',paste(drop,collapse='+')))),
                 "term.labels")
    drop <- attr(mod$terms,'term.labels')
    save <- drop[is.na(match(drop,save))]
  }
  fn <- attr(mod$term,'term.labels')
  fn <- fn[is.na(match(fn,save))]
  nn <- length(fn)
  res <- data.frame(rank=rep(NA,nn),
                    Fvalue=NA,AIC=NA,Fmod=NA,DF=NA)
  rownames(res) <- fn
  li <- list()
  while(nn>0){
    dr <- drop1(mod,test='F')
    dn <- rownames(dr);
    df <- dr[,'F value'];
    df[match(save,dn)] <- NA
    li[[paste('df',nn,sep='')]] <-as.data.frame(dr)
    if(sum(!is.na(df))==0){
      res$rank[is.na(res$rank)] <- (nn+1)/2
      nn <- 0
    } else {
      dm <- min(df,na.rm=T)
      mn <- dn[df==dm]; mn <- mn[!is.na(mn)]
      if(trace) cat(nn,':',mn,'\n')
      res[mn,'rank'] <- nn-(length(mn)-1)/2
      res[mn,'Fvalue'] <- dm
      res[mn,'AIC'] <- dr[mn,'AIC']
      res[mn,c('Fmod','DF')] <- summary(mod)$fst[-2]
      nn <- nn-length(mn)
      nq <- paste('. ~ . ',paste(paste('-',mn,sep=''),collapse=' '))
      if(is.null(data)) mod <- update(mod,as.formula(nq))
      else mod <- update(mod,as.formula(nq),data=data)
    } 
  }
  if(sort) res <- res[order(res$rank),]
  if(full) {li[['ranking']] <- res; res <- li}
  res
}