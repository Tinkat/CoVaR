# Don't forget to add library(quantreg) and library(KernSmooth) in main before using this source code!
#### Author: Shih-Kang Chao #####

"lprq2" <- function(x, y, h, tau, x0) # modified from lprq, s.t. we can specify where to estimate quantiles
{       xx <- x0
        fv <- xx
        dv <- xx
        for(i in 1:length(xx)) {
                z <- x - xx[i]
                wx <- dnorm(z/h)
                r <- rq(y~z, weights=wx, tau=tau, ci=FALSE)
                fv[i] <- r$coef[1.]
                dv[i] <- r$coef[2.]
        }    
        list(xx = xx, fv = fv, dv = dv)}
"lprq3" <- function(x, y, h, x0) # modified from lprq, s.t. we can specify where to estimate quantiles, but "random quantiles"
{       xx <- x0
        fv <- xx
        dv <- xx
        for(i in 1:length(xx)) {
                z <- x - xx[i]
                wx <- dnorm(z/h)
                r <- rq(y~z, weights=wx, tau=runif(1), ci=FALSE)  # "random quantiles" is seen at tau=runif(1)
                fv[i] <- r$coef[1.]
                dv[i] <- r$coef[2.]
        }  
        list(xx = xx, fv = fv, dv = dv)}


conf=function(VAR1, VAR2, q = 0.05, gridn = 100, alpha=0.05, B=500){

n<-length(VAR1)
sor_VAR2 <- VAR2[order(VAR1)] # regressand

index<-seq(1/n, 1, length=n)

h2 <- dpill(index, sor_VAR2, gridsize = gridn)
qrh2 <- 2*h2*((q*(1-q)/(dnorm(qnorm(p=q))^2))^(1/5) )
fit <- lprq(index, sor_VAR2, h=qrh2, m=gridn, tau=q)

##### Asymptotic confidence band (begin) #####
cc = 1/4 # this is for normal kernel, if quartic kernel, value is  3/2
lambda= 1/2/sqrt(pi) # this is for Gaussian kernel, if quartic kernel, value is    5/7
delta=-log(qrh2)/log(n)
dd = sqrt(2*delta*log(n))+(2*delta*log(n))^(-1/2)*log(cc/2/pi)
h12=(0.5*(1-0.5)/dnorm(qnorm(0.5))^2)^0.2 *2.42*sd(index)*n^(-0.2)
if (h12<1) {b2=10*max(h12^5/((q*(1-q)/dnorm(qnorm(q))^2)^0.2 *2.42*sd(index)*n^(-0.2))^3, (q*(1-q)/dnorm(qnorm(q))^2)^0.2 *2.42*sd(index)*n^(-0.2)/10)} else
b2=10*h12^4/((q*(1-q)/dnorm(qnorm(q))^2)^0.2 *2.42*sd(index)*n^(-0.2))^3

# empirical pdf of x
fxd<-bkde(index, gridsize = gridn, range.x = c(fit$xx[1], fit$xx[gridn]),truncate=TRUE)

# estimate "conditional pdf f(epsilon|x)=f12bis" at the quantile point for the 100 grid points
fl2bis <- vector(length= gridn, mode="numeric")
kernelq <- function(u){dnorm(u,mean=0,sd=1)}    # or, quadratic kernel:    ){(15/16)*((1-u^2)^2)*(-1 <= u)*(u <= 1)}
for (k in 1: gridn){
    fl2bis[k]= sum ((kernelq((index- fit$xx[k])/qrh2)*kernelq((sor_VAR2 - fit$fv[k])/b2)/qrh2/b2))/sum(kernelq((index- fit$xx[k])/qrh2)/qrh2)
}
bandt2bis=(fxd$y)^(1/2)*fl2bis

cn=log(2)-log(abs(log(1-alpha)))
band2bis=(n*qrh2)^(-1/2)*sqrt(lambda*q*(1-q))*bandt2bis^(-1)*(dd+cn*(2*delta*log(n))^(-1/2))

##### Asymptotic confidence band (end) #####
############## Bootsrap ###########################
        
fitover<-lprq2(index,sor_VAR2,h=qrh2*n^(4/45),tau=q,x0=index)
fit_n <- lprq(index, sor_VAR2, h=qrh2, m=n, tau=q)

# Fit empirical f(x) and f(y|x) again with grid size n
fxd_n<-bkde(index, gridsize = n, range.x = c(fit_n$xx[1], fit_n$xx[n]),truncate=TRUE)

# estimate "conditional pdf f(epsilon|x)=f12bis"
fl2bis_n <- vector(length= n, mode="numeric")
for (k in 1: n){
    fl2bis_n[k]= sum ((kernelq((index- fit_n$xx[k])/qrh2)*kernelq((sor_VAR2 - fit_n$fv[k])/b2)/qrh2/b2))/sum(kernelq((index- fit_n$xx[k])/qrh2)/qrh2)
}
bandt2bis_n=(fxd_n$y)^(1/2)*fl2bis_n


d <- vector(length= B, mode="numeric") # initilize the bootstrap maximum #e <- vector(length= B, mode="numeric") # initilize the bootstrap maximum

for(jj in 1: B){
    fiterror <- lprq3(index,(sor_VAR2-fit_n$fv),h=qrh2,x0=index)
    ystar <- fitover$fv + fiterror$fv
    fitstar <- lprq(index, ystar,h= qrh2,tau=q, m=n) #    e[jj] <- max(abs(fitstar$fv - fitover2$fv))
    d[jj] <- max(abs(bandt2bis_n*(fitstar$fv - fitover$fv))) 
}
dstar <- quantile(d, probs = 1-alpha)
dstar <- dstar*bandt2bis^(-1)

##### Integrated Loss from Linear Specification (end) #############################
##### Output ##############
f<-approxfun(density(VAR1)$x,density(VAR1)$y, method="linear")
prob<-seq(1/gridn,1, length=gridn)
x0=quantile(VAR1, probs=prob,names=F)
y<- fit$fv*(sqrt(f(x0)))^(-1)
band_low<-y-band2bis*(sqrt(f(x0)))^(-1)
band_high<-y+band2bis*(sqrt(f(x0)))^(-1)
boot_band_low<-y-dstar*(sqrt(f(x0)))^(-1)
boot_band_high<-y+dstar*(sqrt(f(x0)))^(-1)

list(x0=x0, est=y, asy.l=band_low,asy.h=band_high,boot.l=boot_band_low,boot.h=boot_band_high)
}
