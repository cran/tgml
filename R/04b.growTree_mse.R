growTree_mse=function(s,
                   df_tr, df_va, y_sel, x_sel, z_sel,
                   P,Q,R,
                   splitVariable,cutoff,terminal,
                   th,th_num,th_num_max,min_sample,
                   FUN1,FUN2,FUN3,MLlist,
                   combML){

  #1. data set at sth node
  node_tr_s=which(df_tr$node==s)
  node_va_s=which(df_va$node==s)

  df_tr_s=df_tr[node_tr_s,]
  df_va_s=df_va[node_va_s,]

  y_tr=df_tr_s[,y_sel]; y_va=df_va_s[,y_sel];
  x_tr=df_tr_s[,x_sel]; x_va=df_va_s[,x_sel];
  z_tr=df_tr_s[,z_sel]; z_va=df_va_s[,z_sel];

  n_tr=length(y_tr)
  n_va=length(y_va)

  #1. initial values
  #mse_tr=matrix(NA,P,th_num_max) #matrix will work, i.e., NA if number of candidate th values < m_nt
  mse_va=matrix(NA,P,th_num_max)

  ML_lt=ML_rt=matrix(NA,P,th_num_max) #lt and rt: selected MLs for left and right child nodes.
  fit_lt=fit_rt=list()
  for(p in 1:P){
    fit_lt[[p]]=fit_rt[[p]]=list()
    for(c in 1:th_num_max){ #th_num_max, instead of unique cutoff
      fit_lt[[p]][[c]]=list()
      fit_rt[[p]][[c]]=list()
    }
  }

  #2. Fit all combs of MLs at each qth vairable and sth cutoff
  for(p in 1:P){ #pth variable
    x_tr_p=x_tr[,p]
    x_va_p=x_va[,p]

    for(c in 1:th_num[p]){ #cth threshold from pth variable
      th_pc=th[[p]][c]   #thresholds
      lt_tr=which(x_tr_p<=th_pc) #idx to left
      lt_va=which(x_va_p<=th_pc)

      y_tr_lt=y_tr[lt_tr];  y_va_lt=y_va[lt_va];
      y_tr_rt=y_tr[-lt_tr]; y_va_rt=y_va[-lt_va];

      z_tr_lt=z_tr[lt_tr,]; z_va_lt=z_va[lt_va,];
      z_tr_rt=z_tr[-lt_tr,];z_va_rt=z_va[-lt_va,];

      n_min_tr=FUN3(y_tr_lt,y_tr_rt)
      n_min_va=FUN3(y_va_lt,y_va_rt)
      if(n_min_tr>min_sample & n_min_va>=1){
        fit_pc=combML(y_tr_lt=y_tr_lt,z_tr_lt=z_tr_lt,y_tr_rt=y_tr_rt,z_tr_rt=z_tr_rt,
                      y_va_lt=y_va_lt,z_va_lt=z_va_lt,y_va_rt=y_va_rt,z_va_rt=z_va_rt,
                      n_va=n_va,R=R,
                      FUN1=FUN1,FUN2=FUN2,MLlist=MLlist)

          #mse_tr[p,c]=fit_pc$mse_tr
          mse_va[p,c]=fit_pc$mse_va

          fit_lt[[p]][[c]]=fit_pc$fit_lt
          fit_rt[[p]][[c]]=fit_pc$fit_rt

        ML_lt[p,c]=fit_pc$ML_lt
        ML_rt[p,c]=fit_pc$ML_rt
      }
    }
  }

  #3. if no MLs are performed.
  if(sum(!is.na(mse_va))==0)
    return(list(grow=FALSE))

  #4. Summary
  #4.1. minimum mse
  mse_va_hat=min(mse_va,na.rm=TRUE)

  #4.2. choose the best variable & cutoff for partitioning
  pc_hat=which(mse_va==mse_va_hat, arr.ind = TRUE)
  if(nrow(pc_hat)>1) #if there are ties, randomly select
    pc_hat=pc_hat[sample(1:nrow(pc_hat),1),]
  p_hat=pc_hat[1]
  c_hat=pc_hat[2]
  th_hat=th[[p_hat]][c_hat]

  #4.3. MLs hat
  fit_lt_hat=fit_lt[[p_hat]][[c_hat]]
  fit_rt_hat=fit_rt[[p_hat]][[c_hat]]
  ML_lt_hat=ML_lt[p_hat,c_hat]
  ML_rt_hat=ML_rt[p_hat,c_hat]

  #4.4. node hat
  #4.4.1. selected variable & node
  x_tr_p=df_tr[,x_sel[p_hat]];  x_va_p=df_va[,x_sel[p_hat]]; #overwrite
  node_tr=df_tr$node;           node_va=df_va$node;

  #4.4.2. idx to left & right
  lt_tr=which(x_tr_p<=th_hat); lt_va=which(x_va_p<=th_hat)
  rt_tr=which(x_tr_p>th_hat);  rt_va=which(x_va_p>th_hat)

  #4.4.3. idx to left & right AND sth node
  lt_tr_s=intersect(node_tr_s,lt_tr); lt_va_s=intersect(node_va_s,lt_va)
  rt_tr_s=intersect(node_tr_s,rt_tr); rt_va_s=intersect(node_va_s,rt_va)

  #4.4.4. node hat for sth node only
  node_tr[lt_tr_s]=2*node_tr[lt_tr_s];   node_va[lt_va_s]=2*node_va[lt_va_s];
  node_tr[rt_tr_s]=2*node_tr[rt_tr_s]+1; node_va[rt_va_s]=2*node_va[rt_va_s]+1;

  df_tr$node=node_tr
  df_va$node=node_va

  #4.5. n_tr & n_va
  n_tr_lt=length(lt_tr_s); n_va_lt=length(lt_va_s)
  n_tr_rt=length(rt_tr_s); n_va_rt=length(rt_va_s)

  return(list(grow=TRUE,
              df_tr=df_tr,df_va=df_va,
              #n_tr=n_tr,n_va=n_va,
              n_tr_lt=n_tr_lt,n_va_lt=n_va_lt,n_tr_rt=n_tr_rt,n_va_rt=n_va_rt,
              mse_va_hat=mse_va_hat,p_hat=p_hat,th_hat=th_hat,
              fit_lt_hat=fit_lt_hat,fit_rt_hat=fit_rt_hat,
              ML_lt_hat=ML_lt_hat,ML_rt_hat=ML_rt_hat
              ))
}
