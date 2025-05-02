#B.comb MLs
combML_auc=function(y_tr_lt,z_tr_lt,y_tr_rt,z_tr_rt,
                y_va_lt,z_va_lt,y_va_rt,z_va_rt,
                n_va,R,
                FUN1,FUN2,MLlist){

  #1. MLs for left child node
  fit_lt=y_hat_lt=list()
  #mse_tr_lt=rep(NA,R)
  mse_va_lt=rep(NA,R)
  for(r in 1:R){
    fit_lt[[r]]=FUN1[[r]](y_tr_lt,z_tr_lt)
    y_hat_lt[[r]]=FUN2[[r]](fit_lt[[r]],z_va_lt)
  }

  #2. MLs for left child node
  fit_rt=y_hat_rt=list()
  mse_tr_rt=rep(NA,R)
  mse_va_rt=rep(NA,R)
  for(r in 1:R){
    fit_rt[[r]]=FUN1[[r]](y_tr_rt,z_tr_rt)
    y_hat_rt[[r]]=FUN2[[r]](fit_rt[[r]],z_va_rt)
  }

  #3. Choose the best ML comb
  mse_va=matrix(NA,R,R)
  y_va=c(y_va_lt,y_va_rt)

  for(a in 1:R){
    if(sum(is.na(y_hat_lt[[a]])==0)){ #otherwise, auc is still computed with some missing data.
      for(b in 1:R){
        if(sum(is.na(y_hat_rt[[b]]))==0){
          y_hat=c(y_hat_lt[[a]],y_hat_rt[[b]])
          mse_va[a,b]=pROC::roc(y_va~y_hat,levels=c(0,1),direction="<")$auc #sum((as.numeric(y_va)-y_hat_root)^2)
        }
      }
    }
  }

  if(sum(!is.na(mse_va))==0)
    return(list(mse_va=NA,fit_lt=NA,fit_rt=NA,ML_lt=NA,ML_rt=NA))

  ab_hat=which(mse_va==max(mse_va,na.rm=TRUE), arr.ind = TRUE)
  if(nrow(ab_hat)>1) #if there are ties, randomly select
    ab_hat=ab_hat[sample(1:nrow(ab_hat),1),]

  a_hat=ab_hat[1]
  b_hat=ab_hat[2]

  fit_lt=fit_lt[[a_hat]]
  fit_rt=fit_rt[[b_hat]]

  mse_va=mse_va[a_hat,b_hat]

  ML_lt=MLlist[a_hat]
  ML_rt=MLlist[b_hat]

  return(list(mse_va=mse_va,fit_lt=fit_lt,fit_rt=fit_rt,ML_lt=ML_lt,ML_rt=ML_rt))
}
