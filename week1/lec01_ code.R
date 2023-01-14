## code used to create plots in the slides for the first lecture
## this is auxiliary file 

## download files

library(kknn) ## knn library

# set seed
set.seed(4)

download.file("https://raw.githubusercontent.com/ChicagoBoothML/MLClassData/master/UsedCars/UsedCars_small.csv",destfile="UsedCars_small.csv")
download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R", "docv.R")


UsedCars <- read.csv(file="UsedCars_small.csv",head=TRUE,sep=",")

train_index = sample(1000, size = 500, replace = FALSE)

UsedCars_tr <- UsedCars[train_index,]
UsedCars_tst <- UsedCars[-train_index,]


## various plots

#

plot(UsedCars_tr$mileage,UsedCars_tr$price,cex.lab=2.0,col='darkgray',
     pch=19,cex=1.5,xlab="mileage",ylab="price")

#

plot(UsedCars_tr$mileage,UsedCars_tr$price,cex.lab=1.5,col='darkgray',
     pch=19,cex=1.5,xlab="mileage",ylab="price")
lmUC = lm(price~mileage, UsedCars)
abline(lmUC$coef,col='red',lwd=4)


#

train = data.frame(mileage=UsedCars_tr$mileage,price=UsedCars_tr$price) ## Fit first knn to mileage vs. price and plot.
test = train # test = train to get in-sample knn fit
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test =test[ind,] 

near = kknn(price~mileage,train,test,k=50,kernel = "rectangular") ##fit knn with k=50!!

plot(UsedCars_tr$mileage,UsedCars_tr$price,cex.lab=1.5,col='darkgray',pch=19,cex=1.5, main="k=50",xlab="mileage",ylab="price")
lines(test[,1],near$fitted,col=2,lwd=3)

# 

train = data.frame(mileage=UsedCars_tr$mileage,price=UsedCars_tr$price)
test = train # test = train to get in-sample knn fit
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test =test[ind,]

kval=50

near = kknn(price~mileage,train,test,k=kval,kernel = "rectangular")
iv = c(100, 300, 400, 470)
for(i in 1:length(iv)) {
  ii = near$C[iv[i],1:kval]
  plot(UsedCars_tr$mileage,UsedCars_tr$price,main=paste("k=",kval),
       pch=19,cex=1.5,col="darkgray",cex.main=2,cex.lab=2.0,xlab="mileage",ylab="price")
  lines(test[,1],near$fitted,col=2,lwd=3)
  abline(v=test[iv[i],1],col=2,lty=2)
  points(UsedCars_tr$mileage[ii],UsedCars_tr$price[ii],pch=19,col="blue")
  points(test[iv[i],1],mean(UsedCars_tr$price[ii]),pch=20,col='magenta',cex=3)
}


#

train = data.frame(mileage=UsedCars_tr$mileage,price=UsedCars_tr$price)
test = train # test = train to get in-sample knn fit
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test =test[ind,]

par(mfrow=c(2,3))

MSE = NULL
kk = c(2,10,50,100,200,500)

for(i in kk){
  
  near = kknn(price~mileage,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  MSE = c(MSE,aux)
  
  plot(UsedCars_tr$mileage,UsedCars_tr$price,main=paste("k=",i),
       pch=19,cex=0.8,col="darkgray",cex.main=2,cex.lab=2,xlab="mileage",ylab="price")
  lines(test[,1],near$fitted,col=2,lwd=2)
}

par(mfrow=c(1,1))
plot(log(1/kk),sqrt(MSE),type="b",xlab="Complexity (log(1/k))",col="blue",ylab="RMSE",lwd=2,cex.lab=1.8)
text(log(1/kk[1]),sqrt(MSE[1])+0.3,paste("k=",kk[1]),col=2,cex=1.2)
text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[10]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)
title(main="RMSE",cex.main=2)


# 

train = data.frame(mileage=UsedCars_tr$mileage,price=UsedCars_tr$price)
test = train # test = train to get in-sample knn fit
ind = order(test[,1]) #sorting test makes the plots below easier to make.
test =test[ind,]

par(mfrow=c(1,3))

MSE = NULL
kk = c(500,50,2)

for(i in kk){
  
  near = kknn(price~mileage,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  MSE = c(MSE,aux)
  
  plot(UsedCars_tr$mileage,UsedCars_tr$price,main=paste("k=",i),
       pch=19,cex=0.8,col="darkgray",cex.main=1.5,cex.lab=1,xlab="mileage",ylab="price")
  lines(test[,1],near$fitted,col=2,lwd=2)
}

# 

train = data.frame(mileage=UsedCars_tr$mileage,price=UsedCars_tr$price)
test = data.frame(mileage=UsedCars_tst$mileage,price=UsedCars_tst$price)

MSE = NULL
out_MSE = NULL

kk = seq(2,350,by=2)

for(i in kk){
  
  near = kknn(price~mileage,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  out_MSE = c(out_MSE,aux)
  
  near = kknn(price~mileage,train,train,k=i,kernel = "rectangular")
  aux = mean((train[,2]-near$fitted)^2)
  MSE = c(MSE,aux)
}

best = which.min(out_MSE)

par(mai=c(1.2,1.2,.5,.5))
plot(log(1/kk),sqrt(out_MSE),xlab="Complexity (log(1/k))", 
     ylab="RMSE", col=4, lwd=2,type="l", cex.lab=2.2, ylim=c(6000,14000))
text(log(1/kk[best]),sqrt(out_MSE[best])-0.3,paste("k=",kk[best]),col=2,cex=1.2)
text(log(1/2),sqrt(out_MSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/354)+0.4,sqrt(out_MSE[345]),paste("k=",345),col=2,cex=1.2)

lines(log(1/kk),sqrt(MSE),type="l",col="red",lwd=2,cex.lab=1.8)
title(main="RMSE",cex.main=2)

legend(x = "top", legend = c("Training error", "Test error"), lwd = rep(2, 2), 
       col = c("red", "blue"), cex=2)


#

set.seed(1)

par(mfrow=c(1,3))
kval=2

for(i in 1:3) {
  tr_ind = sample(500, size = 30, replace = FALSE)
  train = data.frame(mileage=UsedCars_tr$mileage[tr_ind],price=UsedCars_tr$price[tr_ind])
  test = data.frame(mileage=UsedCars_tr$mileage,price=UsedCars_tr$price)
  ind = order(test[,1]) #sorting test makes the plots below easier to make.
  test =test[ind,]
  
  near = kknn(price~mileage,train,test,k=kval,kernel = "rectangular")
  
  plot(test$mileage,test$price,main=paste("k=",kval),
       pch=19,cex=1.5,col="darkgray",cex.main=2,cex.lab=2.0,xlab="mileage",ylab="price")
  lines(test[,1],near$fitted,col=2,lwd=3)
  points(train$mileage,train$price,pch=19,col="blue")
}


#

set.seed(1)

par(mfrow=c(1,3))
kval=20

for(i in 1:3) {
  
  tr_ind = sample(500, size = 30, replace = FALSE)
  train = data.frame(mileage=UsedCars_tr$mileage[tr_ind],price=UsedCars_tr$price[tr_ind])
  test = data.frame(mileage=UsedCars_tr$mileage,price=UsedCars_tr$price)
  ind = order(test[,1]) #sorting test makes the plots below easier to make.
  test =test[ind,]
  
  near = kknn(price~mileage,train,test,k=kval,kernel = "rectangular")
  
  plot(test$mileage,test$price,main=paste("k=",kval),
       pch=19,cex=1.5,col="darkgray",cex.main=2,cex.lab=2.0,xlab="mileage",ylab="price")
  lines(test[,1],near$fitted,col=2,lwd=3)
  points(train$mileage,train$price,pch=19,col="blue")
}


#

set.seed(1)
source("docv.R")

kv = 2:100
locv = docvknn(matrix(UsedCars_tr$mileage,ncol=1),UsedCars_tr$price,kv,nfold=length(UsedCars_tr$mileage))

par(mfrow=c(1,2))
par(mai=c(.8,.8,.5,.5))
plot(log(1/kv),sqrt(locv/length(UsedCars_tr$price)),type='l',col='blue',lwd=2,
     xlab='complexity (log(1/k)',ylab='RMSE',cex.lab=1.5)
plot(kv,sqrt(locv/length(UsedCars_tr$price)),type='l',col='blue',lwd=2,
     xlab='k',ylab='RMSE',cex.lab=1.5)

#

par(mfrow=c(1,1))
bestk = kv[which.min(locv)]
train = data.frame(mileage=UsedCars_tr$mileage,price=UsedCars_tr$price)[order(UsedCars_tr$mileage),]
test=train
near = kknn(price~mileage,train,test,k=bestk,kernel = "rectangular")
plot(train$mileage,train$price,cex.lab=2,xlab='mileage',ylab='price',cex.lab=1.5)
lines(train$mileage,near$fitted,col='red',lwd=2)

#

set.seed(1)

source("docv.R")

kv = 2:100
cv1 = docvknn(matrix(UsedCars_tr$mileage,ncol=1),UsedCars_tr$price,kv,nfold=5)
cv2 = docvknn(matrix(UsedCars_tr$mileage,ncol=1),UsedCars_tr$price,kv,nfold=5)
cv3 = docvknn(matrix(UsedCars_tr$mileage,ncol=1),UsedCars_tr$price,kv,nfold=10)

cv1 = sqrt(cv1/length(UsedCars_tr$mileage)) 
cv2 = sqrt(cv2/length(UsedCars_tr$mileage))
cv3 = sqrt(cv3/length(UsedCars_tr$mileage))

par(mai=c(1.8,1.8,.5,.5))  ## plot LOOCV

par(mfrow=c(1,1))
rgy = range(c(cv1,cv2,cv3))
plot(log(1/kv),cv1,type='l',col='red',ylim=rgy,lwd=2,cex.lab=2.0,xlab='log(1/k)',ylab='RMSE')
lines(log(1/kv),cv2,col='blue',lwd=2)
lines(log(1/kv),cv3,col='green',lwd=2)
legend('topleft',legend=c('5-fold 1','5-fold 2','10 fold'),col=c('red','blue','green'),lwd=2,cex=1.5)


#

attach(UsedCars_tr)

par(mfrow=c(1,2))
plot(mileage,price,cex.lab=1.5)
plot(year,price,cex.lab=1.5)


#

attach(UsedCars_tr)

mmsc=function(x) {return((x-min(x))/(max(x)-min(x)))}
df = data.frame(mileage=mmsc(mileage),year=mmsc(year),price)
x = as.matrix(df[,1:2])
y = df$price

near = kknn(price~.,df,df,k=30,kernel = "rectangular")
lmf = lm(price~.,df)
fmat = cbind(y,near$fitted,lmf$fitted)
colnames(fmat) = c("y","knnfits30","linfits")
pairs(fmat,cex.lab=1.5)


#

plot(UsedCars_tr$mileage,UsedCars_tr$year,cex.lab=1.5,xlab="mileage",ylab="year")
xind = 2
ii = near$C[xind,]

points(UsedCars_tr$mileage[ii],UsedCars_tr$year[ii],col="red",pch=20,cex=2.5)
points(UsedCars_tr$mileage[xind],UsedCars_tr$year[xind],col="blue",pch=17,cex=3)


#

attach(UsedCars_tr)

mmsc=function(x) {return((x-min(x))/(max(x)-min(x)))}
df = data.frame(mileage=mmsc(mileage),year=mmsc(year),price)
x = as.matrix(df[,1:2])
y = df$price

n=length(y)
set.seed(99)
kv = 2:50
cv1 = docvknn(x,y,kv,nfold=5)
cv2 = docvknn(x,y,kv,nfold=5)
cv3 = docvknn(x,y,kv,nfold=10)
cv1 = sqrt(cv1/length(price)); cv2 = sqrt(cv2/length(price)); cv3 = sqrt(cv3/length(price))

par(mai=c(1.8,1.8,.5,.5))

par(mfrow=c(1,1))
rgy = range(c(cv1,cv2,cv3))
plot(log(1/kv),cv1,type='l',col='red',ylim=rgy,lwd=2,cex.lab=2.0,xlab='log(1/k)',ylab='RMSE')
lines(log(1/kv),cv2,col='blue',lwd=2)
lines(log(1/kv),cv3,col='green',lwd=2)
legend('topleft',legend=c('5-fold 1','5-fold 2','10 fold'),col=c('red','blue','green'),lwd=2,cex=1.5)


#

attach(UsedCars_tr)

mmsc=function(x) {return((x-min(x))/(max(x)-min(x)))}
df = data.frame(mileage=mmsc(mileage),year=mmsc(year),price)
x = as.matrix(df[,1:2])
y = df$price

n=length(y)
set.seed(99)

cvmean = rep(0,length(kv))
ndocv = 50
n=length(y)
cvmat = matrix(0,length(kv),ndocv)
for(i in 1:ndocv) {
  cvtemp = docvknn(x,y,kv,nfold=10)
  cvmean = cvmean + cvtemp
  cvmat[,i] = sqrt(cvtemp/n)
}
cvmean = cvmean/ndocv
cvmean = sqrt(cvmean/n)
plot(kv,cvmean,type="n",ylim=range(cvmat),xlab="k",cex.lab=1.5,ylab="mean RMSE")
for(i in 1:ndocv) lines(kv,cvmat[,i],col=i,lty=3)
lines(kv,cvmean,type="b",col="black",lwd=5)


#

attach(UsedCars_tr)

mmsc=function(x) {return((x-min(x))/(max(x)-min(x)))}
df = data.frame(mileage=mmsc(mileage),year=mmsc(year),price)
x = as.matrix(df[,1:2])
y = df$price

n=length(y)

nea16 = kknn(price~.,df,df,k=16,kernel = "rectangular")
lmf = lm(price~.,df)
fmat = cbind(y,near8$fitted,lmf$fitted)
colnames(fmat) = c("y","knn16fits","linfits")
pairs(fmat)
