rootML_auc=function(df_tr,df_va,R,FUN1,FUN2,y_sel,z_sel,MLlist){
  y_tr=df_tr[,y_sel]; y_va=df_va[,y_sel];
  z_tr=df_tr[,z_sel]; z_va=df_va[,z_sel]
  fit_root=list()
  mse_va_root=rep(NA,R)
  for(r in 1:R){
    fit_root[[r]]=FUN1[[r]](y_tr,z_tr)
    y_hat_root=FUN2[[r]](fit_root[[r]],z_va)
    try_roc=try(roc_va_root<-pROC::roc(y_va~y_hat_root,levels=c(0,1),direction="<"),silent=TRUE)
    if(!inherits(try_roc,'try-error'))
      mse_va_root[r]=-c(roc_va_root$auc)
  }

  #4. Choose the best ML comb
  a_hat=which(mse_va_root==max(mse_va_root,na.rm=TRUE))[1] #choose the first one, if ties

  fit_root=fit_root[[a_hat]]
  mse_va_root=mse_va_root[a_hat]
  MLlist_root=MLlist[a_hat]
  return(list(fit_root=fit_root,mse_va_root=mse_va_root,MLlist_root=MLlist_root))
}
