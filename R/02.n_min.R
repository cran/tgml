#n_min: sampple size condition for two child nodes
c_n_min=function(y_tr_lt,y_tr_rt){ #for continuous
  n_min=min(length(y_tr_lt),length(y_tr_rt))
  return(n_min)
}

b_n_min=function(y_tr_lt,y_tr_rt){ #for binary
  n_min=min(c(sum(y_tr_lt==0),sum(y_tr_lt==1),sum(y_tr_rt==0),sum(y_tr_rt==1)))
  return(n_min)
}
