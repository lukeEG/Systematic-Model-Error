library(truncnorm)
library(caret)
library(LKT)#see github.com/imrryr/LKT
library(WRS)
errordec <- function (v,d){
  w<-length(v)
  #  (cat(v,d,w,"\n"))
  sum((c(0,v[1:w]) * d^((w):0))/sum(d^((w+1):0)))}
slideerrordec <- function(x, d) {
  v <- c(rep(0, length(x)))
  for (i in 1:length(x) ) {
    v[i] <- errordec(x[1:i],d)  }
  return(c(0,v[1:length(x)-1]))}
restRange <-function(x){
  if(x>1){return(1)}
  if(x<0){return(0)}
  return(x)
}
nstu=500
ntrials=50
library(mvtnorm)

#Get correlation from fitting assistments with random slopes and intercepts

#correlation based off correlation among random suc/fail slopes in Assistments dataset
sigma=matrix(c(1,.75,0,.75,1,0,0,0,1), ncol=3);n=nstu;mbetas=c(1,1,-2)
coefs=rmvnorm(n*10, mean=mbetas, sigma=sigma)#Making extra so gets 100 with positive coefs
coefs = coefs[which(coefs[,1]>0 & coefs[,2]>0)[1:nstu],]

cor.test((coefs[,1]),coefs[,2])

stu.ints=coefs[,3]#rtruncnorm(nstu, a=-5, b=-1, mean = -3, sd = 3)#student intercepts


#!NOTE! N items tied to ntrials, assuming different item every practice !
item.ints=rtruncnorm(ntrials, a=-1, b=1, mean = 0, sd = .25)#item intercepts
students=matrix(nrow=nstu,ncol=ntrials)
pred1=matrix(nrow=nstu,ncol=ntrials)
pred2=matrix(nrow=nstu,ncol=ntrials)
pred3=matrix(nrow=nstu,ncol=ntrials)
b5.tmp=c()
#Student model of learning, change this to adjust how prior practices influence future performance etc
bias=rnorm(nstu,0,0)#rnorm(1,0,0.1)
true.PFA =   "plogis(coefs[i,1]*log(1+length(which(students[i,1:j]>=.5)-1))+coefs[i,2]*log(1+length(which(students[i,1:j]<.5)-1))+stu.ints[i]+item.ints[j])"
#estimated model of learning using mean learning rate and mean student intercepts. How well will it do?
recomp.model = "plogis(mean(b1)*log(1+j-1)+mean(stu.ints)+item.ints[j])"
pred.PFA =  "plogis((bias[i]+mean(coefs[i,1]))*log(1+length(which(students[i,1:j]>=.5)-1))+(bias[i]+mean(coefs[i,2]))*log(1+length(which(students[i,1:j]<.5)-1))+mean(stu.ints)+item.ints[j])"
pred2.PFA = "plogis((bias[i]+mean(coefs[i,1]))*log(1+length(which(students[i,1:j]>=.5)-1))+(bias[i]+mean(coefs[i,2]))*log(1+length(which(students[i,1:j]<.5)-1))+mean(stu.ints)+item.ints[j]) + -1*errordec(sign.err,.7)"

for(i in 1:(nstu)){
  item.ints=sample(item.ints)
for(j in 1:ntrials){
 students[i,j] = eval(parse(text=true.PFA))
 pred1[i,j] = restRange(eval(parse(text=pred.PFA)))
 sign.err = pred1[i,1:(j-1)]-students[i,1:(j-1)]
 #Deviance
 if(j==1){sign.err=0}
 pred2[i,j] = restRange(eval(parse(text=pred2.PFA)))

}
}

plot(colMeans(abs(pred1-students)),ylim=c(0,.2),type="l",lwd=3,ylab="Absolute Error from True Model",xlab="Trial")
lines(colMeans(abs(pred2-students)),ylim=c(0,.2),col="cornflowerblue",lwd=3)
text(40,.075,labels=c("ALM"))
text(40,.025,labels=c("ELM"),col="cornflowerblue")

plot(colSds((pred1-students)),ylim=c(-.2,.2),type="l",lwd=3,ylab="Absolute Error from True Model",xlab="Trial")
lines(colSds((pred2-students)),ylim=c(-.2,.2),col="cornflowerblue",lwd=3)
text(40,.075,labels=c("ALM"))
text(40,.025,labels=c("ELM"),col="cornflowerblue")


p1.rmse=(apply(pred1-students,1,function(x){sqrt(mean(x^2))}))
p2.rmse=(apply(pred2-students,1,function(x){sqrt(mean(x^2))}))
mean(p1.rmse)
mean(p2.rmse)
t.test(p1.rmse,p2.rmse,paired=TRUE)
prop.test(length(which((p1.rmse-p2.rmse)>0)),nstu)



midcut<-function(x,from,to,by){
  ## cut the data into bins...
  x=cut(x,seq(from,to,by),include.lowest=T)
  ## make a named vector of the midpoints, names=binnames
  vec=seq(from+by/2,to-by/2,by)
  names(vec)=levels(x)
  ## use the vector to map the names of the bins to the midpoint values
  unname(vec[x])
}


m1cuts=midcut(apply(pred1-students,1,function(x){(mean(x))}),-.4,.4,.005)
m2cuts=midcut(apply(pred2-students,1,function(x){(mean(x))}),-.4,.4,.005)
m1table=table(m1cuts)
m2table=table(m2cuts)
m2tablefill = m1table
m2tablefill[1:length(m2tablefill)]=0
m2tablefill[which(names(m2tablefill) %in% names(m2table))] = m2table
m2table=m2tablefill
range(as.numeric(names(table(m2table-m1table))))
plot(as.numeric(names(m1table)),as.numeric(m2table-m1table),ylim=c(-10,250),
     type="l",col="cornflowerblue",lwd=3,xlab="Signed Error",ylab="ELM - ALM Density Difference")
abline(h=0,col="gray",lty=5)

