#Simulating students with a simple model. Luke Eglington

# truncnorm ot required, just did this to make the data more consistent
#Change rtruncnorm(..) to rnorm(..) below if you don't want another package

#Open question: what is benefit of using residual deviance versus % error?
#they seem equivalent, except deviance goes inside plogis() and % goes outside
library(truncnorm)
library(tmvtnorm)
library(caret)
library(gmodels)

#make it 0 if <0, 1 if >1
#errordec, don't want ghost 1s
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
get.dev <- function(stu.dat,pred.dat){
  v = round(stu.dat)
  dev=rep(0,length(stu.dat))
  for(i in 1:length(stu.dat)){
    if(v[i]==1){
     dev[i] = sqrt(-log(pred.dat[i]))
    }else{
     dev[i] = -sqrt(-log(1-pred.dat[i]))
    }
  }
 return(dev)
}
recomp <- function(pastT,step.size,bz,mean.int,item.ints){
  v1=rep(0,length(pastT));v2=rep(0,length(pastT))
  stepdown = bz-step.size;stepup = bz+step.size
  #print(v1)
  for(zz in 1:length(pastT)){
    v1[zz] = plogis(stepdown*log(1+zz-1)+mean.int+item.ints[zz])
  }
  for(zz in 1:length(pastT)){
    v2[zz] = plogis(stepup*log(1+zz-1)+mean.int+item.ints[zz])
  }

  if(abs(RMSE(v1,pastT)-RMSE(v2,pastT))>.005){
    return(ifelse(RMSE(v1,pastT)<RMSE(v2,pastT),stepdown,stepup))
  }else{return(bz)}

}
nstu=1000
ntrials=100
library(mvtnorm)

#Get correlation from fitting assistments with random slopes and intercepts
rseed=sample(1:20000)[1]
set.seed(11514)
#correlation based off correlation among random suc/fail slopes in Assistments dataset
#old method
sigma=matrix(c(1,.8,0,.8,1,0,0,0,1), ncol=3);n=nstu;mbetas=c(1,1,-1)
coefs=rmvnorm(n*10, mean=mbetas, sigma=sigma)#Making extra so gets 100 with positive coefs
coefs = coefs[which(coefs[,1]>=0 & coefs[,2]>=0)[1:nstu],]
colSds(coefs)
colMeans(coefs)
cor.test((coefs[,1]),coefs[,2])


stu.ints=rtmvnorm(n=nstu, mean=c(-1))#coefs[,3]#rtruncnorm(nstu, a=-5, b=-1, mean = -3, sd = 3)#student intercepts

sigma <- matrix(c(1,0.81,0.81,1), ncol=2)
x <- rtmvnorm(n=nstu, mean=c(.81,.81),sigma=sigma, upper=c(4,4),lower=c(0,0))

colMeans(x)
colSds(x)
cor(x[,1],x[,2])
coefs[,1] = x[,1]
coefs[,2] = x[,2]

coefs_orig=coefs
stu.int_orig=stu.ints
#stu.ints = rep(-1,nstu)
#coefs[,1] = mean(coefs[,1])
#coefs[,2] = mean(coefs[,2])
#!NOTE! N items tied to ntrials, assuming different item every practice !
item.ints=rep(0,ntrials)#rtruncnorm(ntrials, a=-1, b=1, mean = 0, sd = .25)#item intercepts
students=matrix(nrow=nstu,ncol=ntrials)
students_prob=matrix(nrow=nstu,ncol=ntrials)
pred1=matrix(nrow=nstu,ncol=ntrials)
pred2=matrix(nrow=nstu,ncol=ntrials)
pred3=matrix(nrow=nstu,ncol=ntrials)
b5.tmp=c()
#Student model of learning, change this to adjust how prior practices influence future performance etc
bias=rnorm(nstu,0,0)#rnorm(1,0,0.1)
true.PFA =   "plogis(coefs[i,1]*log(1+length(which(students[i,1:j]>=.5)-1))+coefs[i,2]*log(1+length(which(students[i,1:j]<.5)-1))+stu.ints[i]+item.ints[j])"
#estimated model of learning using mean learning rate and mean student intercepts. How well will it do?
recomp.model = "plogis(mean(b1)*log(1+j-1)+mean(stu.ints)+item.ints[j])"
pred.PFA =  "plogis((bias[i]+mean(coefs[,1]))*log(1+length(which(students[i,1:j]>=.5)-1))+(bias[i]+mean(coefs[,2]))*log(1+length(which(students[i,1:j]<.5)-1))+mean(stu.ints)+item.ints[j])"
pred2.PFA = "plogis((bias[i]+mean(coefs[,1]))*log(1+length(which(students[i,1:j]>=.5)-1))+(bias[i]+mean(coefs[,2]))*log(1+length(which(students[i,1:j]<.5)-1))+mean(stu.ints)+item.ints[j]) + -1*(errordec(sign.err,.5))"
pred3.PFA = "plogis((bias[i]+mean(coefs[,1]))*log(1+length(which(students[i,1:j]>=.5)-1))+(bias[i]+mean(coefs[,2]))*log(1+length(which(students[i,1:j]<.5)-1))+mean(stu.ints)+item.ints[j]) + -1*((errordec(sign.err,.25))/sqrt(i))#/ifelse(sign(errordec(sign.err,.25))==0,1,sign(errordec(sign.err,.25))))"
  #"plogis((bias[i]+mean(coefs[i,1]))*log(1+length(which(students[i,1:j]>=.5)-1))+(bias[i]+mean(coefs[i,2]))*log(1+length(which(students[i,1:j]<.5)-1))+mean(stu.ints)+item.ints[j] + errordec(dev.err,.2))"

for(i in 1:(nstu)){
  item.ints=sample(item.ints)
for(j in 1:ntrials){
 students[i,j] = eval(parse(text=true.PFA))
 #students[i,j] = rbinom(1,1,students_prob[i,j])#eval(parse(text=true.PFA))
 pred1[i,j] = restRange(eval(parse(text=pred.PFA)))
 sign.err = pred1[i,1:(j-1)]-students[i,1:(j-1)]
 #Deviance
 if(j==1){sign.err=0}
 #Doesnt this need to be model deviance?
 #dev.err = get.dev(students[i,1:(j-1)],pred1[i,1:(j-1)])
 pred2[i,j] = restRange(eval(parse(text=pred2.PFA)))
 #pred3[i,j] = restRange(eval(parse(text=pred3.PFA)))

}
}

mastery = matrix(nrow=nstu,ncol=4)
for(i in 1:nstu){
  mastery[i,1] = ifelse(is.na(which(students_prob[i,]>=.95)[1]),ntrials,which(students[i,]>=.95)[1])
  mastery[i,2] = ifelse(is.na(which(pred1[i,]>=.95)[1]),ntrials,which(pred1[i,]>=.95)[1])
  mastery[i,3] = ifelse(is.na(which(pred2[i,]>=.95)[1]),ntrials,which(pred2[i,]>=.95)[1])
  mastery[i,4] = ifelse(is.na(which(pred3[i,]>=.95)[1]),ntrials,which(pred3[i,]>=.95)[1])
}

colMeans(mastery,na.rm=TRUE)
colSds(mastery)

#mean((abs(mastery[,1]-mastery[,2])))
#mean error when both slopes and intercepts vary (intercept not correlated):
#PFAp: 29.6.76 (33.2), EPFA: 1.09 (2.42)
#Mean error just slopes vary:
#PFAp: 30.44 (35.17), EPFA: .93(.78)
#Mean error just intercepts vary:
#PFAp: 2.78 (2.23), EPFA: 1.23 (.695)
#Figure
idx=c(1:nstu)#which(rowMaxs(students)>=.95)
par(mar=c(7.1,4.1,4.1,2.1))
hist((mastery[idx,1]-mastery[idx,2]),breaks=70,ylim=c(0,300),xlim=c(-50,50),main="",ylab="Count of Students",xlab="Predicted Mastery Trial - Actual",xaxt='n')
hist((mastery[idx,1]-mastery[idx,3]),breaks=25,add=TRUE,col="cornflowerblue")
text(-20,200,label=expression('PFA'),col="darkgray",pos=4)
text(-20,175,label=expression('PFA + PEV'),col="cornflowerblue",pos=4)
hdi1=hdi((mastery[,1]-mastery[,2]));segments(hdi1[1],-11,hdi1[2],-11,lwd=3,col="darkgray",xpd=TRUE)
hdi2=hdi((mastery[,1]-mastery[,3]));segments(hdi2[1],-14,hdi2[2],-14,lwd=3,col='cornflowerblue',xpd=TRUE)

axis(side=1,pos=-23)
text(-45,-12,labels=c("95% HDI"),xpd=TRUE)
arrows(-38,-12,-35,-12,xpd=TRUE,lwd=2,length=.1)
#title(xlab="Mean Signed Error",line = 5)

plot(colMeans(abs(pred1-students)),ylim=c(0,.3),type="l",lwd=3,ylab="Absolute Error from True Model",xlab="Trial")
lines(colMeans(abs(pred2-students)),ylim=c(0,.2),col="cornflowerblue",lwd=3)
lines(colMeans(abs(pred3-students)),ylim=c(0,.2),col="darkgreen",lwd=3)
text(40,.075,labels=c("PFA"))
text(40,.025,labels=c("EPFA"),col="cornflowerblue")



m1.abs = apply(abs(pred1-students),1,function(x){mean(x)})
m2.abs = apply(abs(pred2-students),1,function(x){mean(x)})
t.test(m1.abs,m2.abs)

par(mfrow=c(1,2))
plot(mastery[,1],mastery[,2],pch=16,ylab=expression('Predicted Mastery Trial'),
     xlab=expression('Actual Mastery Trial (PFA'['T']*')'),col="slategray4",ylim=c(0,100))
points(mastery[,1],mastery[,3],add=TRUE,col="cornflowerblue",pch=16)
text(100,115,"(a)",xpd=TRUE)
text(10,90,labels=c(expression('PFA'['P'])),col="slategray4",xpd=TRUE,pos=4,cex=1.5,font=2)
text(10,82,labels=c(expression('EPFA')),col="cornflowerblue",xpd=TRUE,cex=1.5,pos=4,font=2)
pfa_mastery = (abs(mastery[,1]-mastery[,2]))
pev_mastery = (abs(mastery[,1]-mastery[,3]))
propdec_mastery = ((mastery[,1]-mastery[,4]))
tmp=cbind((pfa_mastery),(pev_mastery))#c(mean(pfa_mastery),mean(pev_mastery))
barplot(colMeans(tmp),ylim=c(0,33),
        ylab=c("Predicted - Actual Mastery Trial (absolute value)"),
        col=c("slategray4","cornflowerblue"),
        names=c(expression('PFA'['P']),expression('EPFA')))
#error.bars(tmp,add=TRUE)
text(2.5,36,"(b)",xpd=TRUE)
segments(.7,gmodels::ci(pfa_mastery)[2],.7,gmodels::ci(pfa_mastery)[3])
segments(1.85,gmodels::ci(pev_mastery)[2],1.85,gmodels::ci(pev_mastery)[3])


#which students mastery content?
plot(mastery[which(mastery[,1]<100),1],mastery[which(mastery[,1]<100),2],pch=16,ylab=expression('Predicted Mastery Trial'),
     xlab=expression('Actual Mastery Trial (PFA'['T']*')'),col="slategray4",ylim=c(0,100))


#FIG 1
plot(0:20,plogis(3*log(1+(0:20))),col="blue",type="l",lwd=5,ylim=c(0.5,1))
plogis(2*log(1+(0:20)))
