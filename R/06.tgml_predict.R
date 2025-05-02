tgml_predict=function(y,x,z,fit){
  #1. set up
  terminal=fit$terminal
  internal=fit$internal
  splitVariable=fit$splitVariable
  cutoff=fit$cutoff

  fitML=fit$fitML
  selML=fit$selML

  #2. terminal node hat
  n=length(y)
  node_hat=rep(1,n)
  if(sum(terminal!=1)){ #does not need below if there is only root node
    for(s in internal){
      idx_s=which(node_hat==s) #sth internal node

      x.s=x[,splitVariable[s]] #left and right for all samples
      th_s=cutoff[s]
      lt=which(x.s<=th_s)
      rt=which(x.s>th_s)

      lt_s=intersect(lt,idx_s) #left and right at sth internal node
      rt_s=intersect(rt,idx_s)

      node_hat[lt_s]=2*s    #left
      node_hat[rt_s]=2*s+1  #right
    }
  }

  #3. fit ML per sth node terminal node
  y_hat=rep(NA,n)
  for(s in terminal){
    idx_s=which(node_hat==s)
    n_s=length(idx_s)
    if(n_s>0){ #there may be n=0 for new data
      y_s=y[idx_s]
      z_s=z[idx_s,]
      if(n_s==1)
        z_s=matrix(z_s,nrow=1)

      fitML_s=fitML[[s]]
      selML_s=selML[[s]]

      FUN2_s=match.fun(paste(selML_s,"_predict", sep = "")) #predicted

      y_hat[idx_s]=FUN2_s(fitML_s,z_s)
    }
  }
  return(list(y_hat=y_hat,node_hat=node_hat))
}
