#B.comb MLs
combML_bs=function(y_tr_lt,z_tr_lt,y_tr_rt,z_tr_rt,
                y_va_lt,z_va_lt,y_va_rt,z_va_rt,
                n_va,R,
                FUN1,FUN2,MLlist){

  #0 valdation data at left or right child node can be empty although train data is not empty
  if(min(c(length(y_tr_lt),length(y_tr_rt),length(y_va_lt),length(y_va_rt)))==0)
    return(list(mse_va=NA,fit_lt=NA,fit_rt=NA,ML_lt=NA,ML_rt=NA))

  #1. MLs for left child node
  fit_lt=list()
  #mse_tr_lt=rep(NA,R)
  mse_va_lt=rep(NA,R)
  for(r in 1:R){
    fit_lt[[r]]=FUN1[[r]](y_tr_lt,z_tr_lt)
    y_hat_lt=FUN2[[r]](fit_lt[[r]],z_va_lt)

    #mse_tr_lt[r]=sum((y_tr_lt-y_hat_lt)^2)
    y_va_lt_num=as.numeric(y_va_lt==1)
    mse_va_lt[r]=sum((y_va_lt_num-y_hat_lt)^2)
  }

  #2. MLs for left child node
  fit_rt=list()
  mse_tr_rt=rep(NA,R)
  mse_va_rt=rep(NA,R)
  for(r in 1:R){
    fit_rt[[r]]=FUN1[[r]](y_tr_rt,z_tr_rt)
    y_hat_rt=FUN2[[r]](fit_rt[[r]],z_va_rt)

    #mse_tr_rt[r]=sum((y_tr_rt-y_hat_rt)^2)
    y_va_rt_num=as.numeric(y_va_rt==1)
    mse_va_rt[r]=sum((y_va_rt_num-y_hat_rt)^2)
  }

  #3. Choose the best ML comb
  #mse_tr=outer(mse_tr_lt,mse_tr_rt,"+")/n_tr
  mse_va=outer(mse_va_lt,mse_va_rt,"+")/n_va
  if(sum(!is.na(mse_va))==0)
    return(list(mse_va=NA,fit_lt=NA,fit_rt=NA,ML_lt=NA,ML_rt=NA))

  ab_hat=which(mse_va==min(mse_va,na.rm=TRUE), arr.ind = TRUE)
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
