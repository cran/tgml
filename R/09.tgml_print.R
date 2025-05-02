tgml_print=function(x,P,fit){
  internal=fit$internal
  terminal=fit$terminal
  cutoff=fit$cutoff
  splitVariable=fit$splitVariable
  colX=colnames(x)
  if(is.null(colX[1]))
    colX=paste0("x",1:P)

  if(terminal[1]==1){
    message(paste0("Root node (or node 1) - ML model: ",fit$selML[1]))
  }else{
    for(s in terminal){
      depth=floor(log2(s))
      N1=paste0(rep("-",depth),collapse="")
      N2=paste0("Node ",s,collapse="")
      N3=fit$selML[s]

      N4=list()
      depth2=depth
      s1=s #terminal node
      for(j in depth:1){
        s2=floor(s1/2) #internal node above s1
        dir=">" #right
        if(s1%%2==0) #left
          dir="<="
        N4[[j]]=paste0(colX[splitVariable[s2]]," ",dir," ",cutoff[s2])
        s1=s2
      }
      if(depth>=2)
        N4=paste0(unlist(N4),collapse=" & ")
      N=paste0(c(N1,N2,": (",N4,") - ",N3),collapse="")
      message(N)
    }
  }
}
