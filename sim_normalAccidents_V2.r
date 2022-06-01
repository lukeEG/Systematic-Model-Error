#Old version. Simulating students with a simple model. Luke Eglington 9/28/2020

rmse.fun <- function(x){
  return(sqrt(mean(x^2)))
}
mat.ops <- function(m1,m2){
  v=matrix(nrow=length(m1[,1]),ncol=2)
  for(i in 1:length(m1[,1])){
    v[i,1] = sqrt(mean((m1[i,]-m2[i,])^2))
    v[i,2] = cor(m1[i,],m2[i,])
  }
  return(v)
}

library(truncnorm)

nstu=100
ntrials=50
bpd=1
b2 = rtruncnorm(nstu, a=1, b=3, mean = 1.5, sd = 1)#student learning slopes
stu.ints=rtruncnorm(nstu, a=-4, b=-2, mean = -3, sd = 2)#student intercepts

item.ints=rtruncnorm(ntrials, a=-1, b=1, mean = 0, sd = .5)#item intercepts
students=matrix(nrow=nstu,ncol=ntrials)
pred1=matrix(nrow=nstu,ncol=ntrials)
pred2=matrix(nrow=nstu,ncol=ntrials)

#Student model of learning, change this to adjust how prior practices influence future performance etc
true.model = "plogis(b2[i]*log(1+j-1)+stu.ints[i]+item.ints[j])"
#estimated model of learning using mean learning rate and mean student intercepts. How well will it do?
pred.model1 = "plogis(mean(b2)*log(1+j-1)+mean(stu.ints)+item.ints[j])"
#b.pdm*propdec(round(students[i,1:j]),.8)
pred.model2 = "plogis(5*propdec(round(students[i,1:j]),.7)+mean(stu.ints)+item.ints[j])"
for(i in 1:(nstu)){
  item.ints=sample(item.ints)
for(j in 1:ntrials){
 students[i,j] = eval(parse(text=true.model))
 pred1[i,j] = eval(parse(text=pred.model1))
 pred2[i,j] = eval(parse(text=pred.model2))
 }
}

#students = round(students)
#some plots to give you an idea of what the data looks like
matplot(t(students),xlab="Trial",ylab="p(correct)", type = "l",col="black",ylim=c(0,1),lwd=1.5,lty=1)#line per student
par(new = TRUE)
matplot(t(pred1),xlab="",ylab="", type = "l",col="darkred",ylim=c(0,1),lwd=1.5,lty=1)#line per prediction
par(new = TRUE)
matplot(t(pred2),xlab="",ylab="", type = "l",col="darkgreen",ylim=c(0,1),lwd=1.5,lty=1)#line per prediction


colMeans(mat.ops(pred1,students))
colMeans(mat.ops(pred2,students))
