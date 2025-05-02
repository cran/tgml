###
#B. ML with continuous outcomes (models with errors/warnings will not be considered)
#   y is scalar & x is data.frame
###
#B1 lasso
c_lasso=function(y,x){
  x=data.matrix(x)
  fit=NULL
  suppressWarnings(try(fit<-glmnet::cv.glmnet(x,y,alpha=1),silent=TRUE))
  return(fit)
}
c_lasso_predict=function(fit,newx){
  y.hat=rep(NA,nrow(newx))
  if(!is.null(fit)){
    #if(sum(abs(as.numeric(coef(fit)))>0)>1){ #at least one of the coef + int are not zero
      newx=data.matrix(newx)
      y.hat=as.numeric(predict(fit,newx=newx,s="lambda.min",type="response"))
    #}
  }
  return(y.hat)
}

#B2 random forest
c_rf=function(y,x){
  df=data.frame(x=x, y=y)
  colnames(df)=c(paste0("x",1:ncol(x)),"y")
  fit=NULL
  suppressWarnings(try(fit<-randomForest::randomForest(y~.,data=df),silent=TRUE))
  return(fit)
}
c_rf_predict=function(fit,newx){
  y.hat=rep(NA,nrow(newx))
  if(!is.null(fit)){
    df=data.frame(x=newx)
    colnames(df)=paste0("x",1:ncol(newx))
    y.hat=as.numeric(predict(fit, df, type = "response"))
  }
  return(y.hat)
}

#B3 svm-r
c_svm=function(y,x,kernel="radial") {
  df=data.frame(x=x, y=y)
  colnames(df)=c(paste0("x",1:ncol(x)),"y")
  fit=NULL
  suppressWarnings(try(fit<-e1071::svm(y~.,data=df, kernel=kernel),silent=TRUE))
  return(fit)
}
c_svm_predict=function(fit,newx) {
  y.hat=rep(NA,nrow(newx))
  if(!is.null(fit)){
    df=data.frame(x=newx)
    colnames(df)=paste0("x",1:ncol(newx))
    y.hat=as.numeric(predict(fit, newdata = df))
  }
  return(y.hat)
}
