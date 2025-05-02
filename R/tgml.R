tgml=function(y,x,z,ynew=NULL,xnew=NULL,znew=NULL,
             MLlist=c("lasso","rf","svm"),
             eval=NULL,
             cut=10,max_depth=4,min_sample=20){

  ###
  #1. setup
  ###
  #1.1. outcome type
  type="c"
  if(is.factor(y))
    type="b" #binary

  if(is.null(eval)){
    eval="mse"
    if(type=="b")
      eval="bs"
  }

  #1.2. Tailoring functions based on eval.
  if(eval=="auc"){
    rootML=match.fun(rootML_auc)
    growTree=match.fun(growTree_auc)
    combML=match.fun(combML_auc)
  }else{
    growTree=match.fun(growTree_mse) #mse & bs are the same
    if(eval=="mse"){
      rootML=match.fun(rootML_mse)
      combML=match.fun(combML_mse)
    }else if(eval=="bs"){
      rootML=match.fun(rootML_bs)
      combML=match.fun(combML_bs)
    }
  }

  #1.3. ML list
  MLlist=paste0(type,"_",MLlist) #add b./c. (binary/continuous) to MLlist

  #1.4. check min sample size condition in growTree
  if(eval=="mse"){
    FUN3=match.fun(c_n_min)
  }else{
    FUN3=match.fun(b_n_min)
  }

  #1.5 n,P,Q,R
  n=length(y)
  P=ncol(x)
  Q=ncol(z)
  R=length(MLlist)

  ###
  #2. potential cutoff
  ###
  th=list() #candidate threshold values for each pth variable
  prob=seq(1/cut,1,1/cut)
  for(p in 1:P){
    xp=sort(unique(x[,p]))
    n_xq=length(xp)
    if(n_xq<cut){ #discrete
      th[[p]]=(xp[-1]+xp[-n_xq])/2 #mid point
    }else{      #continuous
      th[[p]]=quantile(x[,p],prob=prob)
    }
  }
  th_num=unlist(lapply(th,length)) #number of candidate threshold values for eacth pth variable
  th_num_max=max(th_num) #n_th is the number of candidate threshold values for eact pth variable

  ###
  #3. training-validation split & initial settings for root node (or s=1 node)
  ###
  df=data.frame(y=y,x=x,z=z,node=1)
  y_sel=1
  x_sel=(1:P)+1
  z_sel=(1:Q)+1+P

  s=1 #sth node

  n_tr=NA
  n_va=NA
  n_tr[s]=round(n*0.7,0)
  tr_idx=sample(1:n,n_tr)
  df_tr=df[tr_idx,]
  df_va=df[-tr_idx,]
  n_va[s]=nrow(df_va)

  ###
  #4. Root node ML (or one-size-fits-all ML) & initial parmaeters
  ###
  #4.1. All possible fitted ML and predicted ML
  FUN1=FUN2=list()
  for(r in 1:R){
    FUN1[[r]]=match.fun(get(MLlist[r])) #fitted
    FUN2[[r]]=match.fun(get(paste(MLlist[r],"_predict", sep = ""))) #predicted
  }

  #4.2. root node ML
  splitVariable=cutoff=NA
  internal=NA
  terminal=1

  fit_root=rootML(df_tr=df_tr,df_va=df_va,R=R,FUN1=FUN1,FUN2=FUN2,y_sel=y_sel,z_sel=z_sel,MLlist=MLlist)
  mse_va=fit_root$mse_va_root
  fitML=list(); fitML[[s]]=fit_root$fit_root
  selML=NA;     selML[s]=fit_root$MLlist_root
  depth=floor(log2(max(terminal)))

  #overwrite
  fit_root=list(terminal=terminal,internal=internal,depth=depth,
                splitVariable=splitVariable,cutoff=cutoff,
                fitML=fitML,selML=selML,mse_va=mse_va,
                n_tr=n_tr,n_va=n_va)

  ###
  #5. recursive partitioning (grow)
  ###
  fit=fit_root #fitted tree
  MSE_VA=NULL
  depth.s=floor(log2(s)) #start from 1
  max_s=2^max_depth-1
  message("Recursive partitioning:")
  while(depth.s < max_depth){
    message(paste0("Node ",s, " of total ",max_s))
    depth.s

    lcn=2*s  #left and right child node
    rcn=2*s+1

    splitVariable[s]=NA #initial values
    cutoff[s]=NA
    fitML[[lcn]]=fitML[[rcn]]=NA #left & right
    selML[lcn]=selML[rcn]=NA     #left & right
    mse_va[lcn]=mse_va[rcn]=NA   #left & right #left & right have the same for mse_va only

    if(s %in% df_tr$node){
      res=growTree(s=s,
                   df_tr=df_tr, df_va=df_va, y_sel=y_sel, x_sel=x_sel, z_sel=z_sel,
                   P=P,Q=Q,R=R,
                   splitVariable=splitVariable,cutoff=cutoff,terminal=terminal,
                   th=th,th_num=th_num,th_num_max=th_num_max,min_sample=min_sample,
                   FUN1=FUN1,FUN2=FUN2,FUN3=FUN3,MLlist=MLlist,
                   combML=combML)

      if(res$grow==TRUE){ #1st condition to grow
        splitVariable[s]=res$p_hat
        cutoff[s]=res$th_hat

        internal=c(internal,s)
        internal=internal[complete.cases(internal)]

        terminal=c(terminal,lcn,rcn) #child node
        terminal=setdiff(terminal,internal)

        fitML[[lcn]]=res$fit_lt_hat
        fitML[[rcn]]=res$fit_rt_hat

        selML[lcn]=res$ML_lt_hat
        selML[rcn]=res$ML_rt_hat

        df_tr=res$df_tr #update node only
        df_va=res$df_va

        n_tr[lcn]=res$n_tr_lt; n_va[lcn]=res$n_va_lt
        n_tr[rcn]=res$n_tr_rt; n_va[rcn]=res$n_va_rt

        #save all trees up to the internal node s with two terminal nodes lcn & rcn
        depth=floor(log2(max(terminal)))

        if(eval%in%c("mse","bs")){
          if(res$mse_va_hat<fit$mse_va)
            fit=list(terminal=terminal,internal=internal,depth=depth,
                     splitVariable=splitVariable,cutoff=cutoff,
                     fitML=fitML,selML=selML,mse_va=res$mse_va_hat,
                     n_tr=n_tr,n_va=n_va)
        }else{
          if(res$mse_va_hat>fit$mse_va)
            fit=list(terminal=terminal,internal=internal,depth=depth,
                     splitVariable=splitVariable,cutoff=cutoff,
                     fitML=fitML,selML=selML,
                     mse_va=res$mse_va_hat,n_tr=n_tr,n_va=n_va)
        }
      }
    }
    s=s+1
    depth.s=floor(log2(s)) #theoretical depth regardless of whether tree grows or not
  }

  ###
  #7. yhat and node_hat for fitted & new
  ###
  fit_pred=tgml_predict(y,x,z,fit) #fitted
  fit$y_hat=fit_pred$y_hat
  fit$node_hat=fit_pred$node_hat
  if(type=="c"){
    fit$mse=mean((y-fit$y_hat)^2)
  }else if(type=="b") {
    y_num=as.numeric(y==1)
    fit$bs=mean((y_num-fit$y_hat)^2)

    fit$roc=fit$auc=NA
    try_roc=try(fit$roc<-pROC::roc(y~fit$y_hat,levels=c(0,1),direction="<"),silent=TRUE)
    if(!inherits(try_roc,'try-error'))
      fit$auc=fit$roc$auc
  }

  if(!is.null(ynew)){
    fit_pred_new=tgml_predict(ynew,xnew,znew,fit) #new
    fit$y_hat_new=fit_pred_new$y_hat
    fit$node_hat_new=fit_pred_new$node_hat
    if(type=="c"){
      fit$mse_new=mean((ynew-fit$y_hat_new)^2)
    }else if(type=="b"){
      y_num_new=as.numeric(ynew==1)
      fit$bs_new=mean((y_num_new-fit$y_hat_new)^2)

      fit$roc_new=fit$auc_new=NA
      try_roc=try(fit$roc_new<-pROC::roc(ynew~fit$y_hat_new,levels=c(0,1),direction="<"),silent=TRUE)
      if(!inherits(try_roc,'try-error'))
        fit$auc_new=fit$roc_new$auc
    }
  }

  ###
  #8. further summary
  ###
  fit$selML=sub('*..', '', fit$selML) #remove b./c. (binary / continuous)

  if(sum(fit$terminal!=1)){
    nt=max(fit$terminal,na.rm=TRUE)
    ntt=1:nt
    for(s in setdiff(ntt,fit$terminal)){ #e.g, internal nodes that had some values during the tree growing.
      fit$selML[s]=NA
      fit$fitML[[s]]=NA
      fit$n_tr[s]=NA
      fit$n_va[s]=NA
    }
  }

  fit$n_tr=fit$n_va=NULL
  fit$mse_va=NULL

  fit$grow=NULL
  fit$df_tr=fit$df_va=NULL
  fit$n_tr_lt=fit$n_tr_rt=fit$n_va_lt=fit$n_va_rt=NULL
  fit$mse_va_hat=NULL
  fit$p_hat=NULL
  fit$fit_lt_hat=fit$fit_rt_hat=NULL
  fit$ML_lt_hat=fit$ML_rt_hat=NULL

  ###
  #9 output
  ###
  message("Result")
  tgml_print(x,P,fit)

  return(fit)
}
