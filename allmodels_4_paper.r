#Systematic errors models
library(LKT)
#assist = val
#chitone .808, .926, .902
#cloze .706, .952, .906
#KDD .715,.947,.827
#Andes .604, .76, .723
#MHE .998, .967, .992
#Assistments .576, .965, .936
#KC rpfa, errordec, student rpfa
#optim_params = c(.715,.947,.827) get defined in file for dataset
#source('C:/Users/lukee/Dropbox/NormalAccidents/final_paper_analysis/LKTfunctions.r')

#CV folds
unq = sample(unique(val$Anon.Student.Id))
sfold = rep(1:10,length.out=length(unq))
val$fold = rep(0,length(val[,1]))
for(i in 1:10){val$fold[which(val$Anon.Student.Id %in% unq[which(sfold==i)])]=i}


#PFA
componentl <- c("KC..Default.","KC..Default.","KC..Default.")
featl <- c("logsuc$","logfail$","intercept")
fixedl <- c(NA);offsetl <- c(NA);seedl<- c(NA);elastictest<- c("FALSE")
mod_pfa<-LKT::LKT(data=val,
             components=componentl,features=featl,
             fixedpars=fixedl,seedpars=seedl,
             dualfit=FALSE,interc=TRUE,elastic=elastictest,cv=TRUE)
mod_pfa$cv_res
colMeans(mod_pfa$cv_res)
auc(mod_pfa$newdata$CF..ansbin.,mod_pfa$prediction)
val$pred_ed = mod_pfa$prediction

#R-PFA
componentl <- c("KC..Default.","KC..Default.","KC..Default.")
featl <- c("propdec$","logfail$","intercept")
fixedl <- c(optim_params[1])
seedl<- c(NA)
elastictest<- c("FALSE")
mod_rpfa<-LKT::LKT(data=val,
             components=componentl,features=featl,
             fixedpars=fixedl,seedpars=seedl,
             dualfit=FALSE,interc=TRUE,elastic=elastictest,cv=TRUE)
mod_rpfa$cv_res
colMeans(mod_rpfa$cv_res)
auc(mod_rpfa$newdata$CF..ansbin.,mod_rpfa$prediction)


#PFA-PEV
#Using predictions from mod_pfa
val$pred_ed = mod_pfa$prediction
componentl <- c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.")
featl <- c("errordec","logsuc$","logfail$","intercept")
fixedl <- c(optim_params[2])
seedl<- c(NA)
elastictest<- c("FALSE")
mod_pfaerr<-LKT::LKT(data=val,
             components=componentl,features=featl,
             fixedpars=fixedl,seedpars=seedl,
             dualfit=FALSE,interc=TRUE,elastic=elastictest,cv=TRUE)
mod_pfaerr$cv_res
colMeans(mod_pfaerr$cv_res)
auc(mod_pfaerr$newdata$CF..ansbin.,mod_pfaerr$prediction)


#RPFA + student propdec
componentl <- c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.")
featl <- c("propdec","propdec$","logfail$","intercept")
fixedl <- c(optim_params[3],optim_params[1])
seedl<- c(NA,NA)
elastictest<- c("FALSE")
mod_rpfas<-LKT::LKT(data=val,
                components=componentl,features=featl,
                fixedpars=fixedl,seedpars=seedl,
                dualfit=FALSE,interc=TRUE,elastic=elastictest,cv=TRUE)
mod_rpfas$cv_res
colMeans(mod_rpfas$cv_res)
auc(mod_rpfas$newdata$CF..ansbin.,mod_rpfas$prediction)
#RPFA +student propdec+ errordec
#prediction from mod_pfa
componentl <- c("KC..Default.","Anon.Student.Id","Anon.Student.Id","KC..Default.","KC..Default.")
featl <- c("propdec$","errordec","propdec","logfail$","intercept")
fixedl <- optim_params
seedl<- c(NA)
elastictest<- c("FALSE")
mod_rpfas_pfaerr<-LKT::LKT(data=val,
                components=componentl,features=featl,
                fixedpars=fixedl,seedpars=seedl,
                dualfit=FALSE,interc=TRUE,elastic=elastictest,cv=TRUE)
mod_rpfas_pfaerr$cv_res
colMeans(mod_rpfas_pfaerr$cv_res)
auc(mod_rpfas_pfaerr$newdata$CF..ansbin.,mod_rpfas_pfaerr$prediction)

modm_rmse = c(mean(mod_pfa$cv_res$rmse),
mean(mod_rpfa$cv_res$rmse),
mean(mod_pfaerr$cv_res$rmse),
mean(mod_rpfas$cv_res$rmse),
mean(mod_rpfas_pfaerr$cv_res$rmse)
)
modsd_rmse = c(sd(mod_pfa$cv_res$rmse),
              sd(mod_rpfa$cv_res$rmse),
              sd(mod_pfaerr$cv_res$rmse),
              sd(mod_rpfas$cv_res$rmse),
              sd(mod_rpfas_pfaerr$cv_res$rmse)
)
