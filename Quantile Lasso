# library(quantreg)

loss.qr.eqn<-function(y,l_t,p){
summ<-0
for(k in 1:length(y)){
summ<-(p-as.numeric(y[k]<l_t[k]))*(y[k]-l_t[k])+summ
}
return(loss=summ)
}

GACV.qr<-function(lamb,x,y,p){
n<-length(y)
fit<-rq.fit.lasso(x,y,tau=p,lambda=lamb)
D<-0
est<-x%*%fit$coefficients
L<-loss.qr.eqn(y,est,p)
D<-length(which(abs(y[i]-est[i])<0.000001))
return(loss=L/(n-D))
}
