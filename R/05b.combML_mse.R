#B.comb MLs
combML_mse=function(y_tr_lt,z_tr_lt,y_tr_rt,z_tr_rt,
                y_va_lt,z_va_lt,y_va_rt,z_va_rt,
                n_va,R,
                FUN1,FUN2,MLlist){

  #1. MLs for left child node
  fit_lt=list()
  #mse_tr_lt=rep(NA,R)
  mse_va_lt=rep(NA,R)
  for(r in 1:R){
    fit_lt_r=FUN1[[r]](y_tr_lt,z_tr_lt)
    if(!is.null(fit_lt_r)){
      fit_lt[[r]]=fit_lt_r
      y_hat_lt=FUN2[[r]](fit_lt[[r]],z_va_lt)

      #mse_tr_lt[r]=sum((y_tr_lt-y_hat_lt)^2)
      mse_va_lt[r]=sum((y_va_lt-y_hat_lt)^2)
    }
  }
  if(sum(!is.na(mse_va_lt))==0)
    return(list(mse_va=NA,fit_lt=NA,fit_rt=NA,ML_lt=NA,ML_rt=NA))

  #2. MLs for right child node
  fit_rt=list()
  #mse_tr_rt=rep(NA,R)
  mse_va_rt=rep(NA,R)
  for(r in 1:R){
    fit_rt_r=FUN1[[r]](y_tr_rt,z_tr_rt)
    if(!is.null(fit_rt_r)){
      fit_rt[[r]]=fit_rt_r
      y_hat_rt=FUN2[[r]](fit_rt[[r]],z_va_rt)

      #mse_tr_rt[r]=sum((y_tr_rt-y_hat_rt)^2)
      mse_va_rt[r]=sum((y_va_rt-y_hat_rt)^2)
    }
  }
  if(sum(!is.na(mse_va_rt))==0)
    return(list(mse_va=NA,fit_lt=NA,fit_rt=NA,ML_lt=NA,ML_rt=NA))

  #3. Choose the best ML comb
  mse_va_lt_min=min(mse_va_lt,na.rm=TRUE)
  a_hat=which(mse_va_lt==mse_va_lt_min)
  if(length(a_hat)>=2)
    a_hat=sample(a_hat,1)
  fit_lt=fit_lt[[a_hat]]
  ML_lt=MLlist[a_hat]

  mse_va_rt_min=min(mse_va_rt,na.rm=TRUE)
  b_hat=which(mse_va_rt==mse_va_rt_min)
  if(length(b_hat)>=2)
    b_hat=sample(b_hat,1)
  fit_rt=fit_rt[[b_hat]]
  ML_rt=MLlist[b_hat]

  mse_va=(mse_va_lt_min+mse_va_rt_min)/n_va

  return(list(mse_va=mse_va,fit_lt=fit_lt,fit_rt=fit_rt,ML_lt=ML_lt,ML_rt=ML_rt))
}
