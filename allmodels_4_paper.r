#Systematic errors models

#assist = val

#chitone .808, .926, .902
#cloze .706, .952, .906
#KDD .715,.947,.827
#Andes .604, .76, .723
#MHE .998, .967, .992
#Assistments .576, .965, .936
#KC rpfa, errordec, student rpfa
optim_params = c(.576, .965, .936)
rlvl<-function(dat){
  if(dat$CF..ansbin.[1]==0){#find someone that starts with 1 and put it in front
    row1 = match(unique(dat$Anon.Student.Id), dat$Anon.Student.Id)
    temp_stu = dat$Anon.Student.Id[row1[which(dat$CF..ansbin.[row1]==1)[1]]]
    idx = which(dat$Anon.Student.Id %in% as.character(temp_stu))
    dat_new = rbind(dat[idx,],dat[-idx,])
    return(dat_new)
  }else{return(dat)}
}#end rlvl
val=rlvl(val)
#PFA
componentl <- c("KC..Default.","KC..Default.","KC..Default.")
featl <- c("logsuc$","logfail$","intercept")
fixedl <- c(NA)
offsetl <- c(NA)
seedl<- c(NA)
elastictest<- c("FALSE")
mod_pfa<-LKT(data=val,
             components=componentl,features=featl,offsetvals=offsetl,
             fixedpars=fixedl,seedpars=seedl,
             dualfit=FALSE,interc=TRUE,elastic=elastictest)
auc(mod_pfa$newdata$CF..ansbin.,mod_pfa$prediction)

#R-PFA
componentl <- c("KC..Default.","KC..Default.","KC..Default.","KC..Default.")
featl <- c("propdec$","logsuc$","logfail$","intercept")
fixedl <- c(optim_params[1])
offsetl <- c(NA)
seedl<- c(NA)
elastictest<- c("FALSE")
mod_rpfa<-LKT(data=val,
             components=componentl,features=featl,offsetvals=offsetl,
             fixedpars=fixedl,seedpars=seedl,
             dualfit=FALSE,interc=TRUE,elastic=elastictest)
auc(mod_rpfa$newdata$CF..ansbin.,mod_rpfa$prediction)


#PFA-PEV
val$pred_ed = mod_pfa$prediction
componentl <- c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.")
featl <- c("errordec","logsuc$","logfail$","intercept")
fixedl <- c(optim_params[2])
offsetl <- c(NA)
seedl<- c(NA)
elastictest<- c("FALSE")
mod_pfaerr<-LKT(data=val,
             components=componentl,features=featl,offsetvals=offsetl,
             fixedpars=fixedl,seedpars=seedl,
             dualfit=FALSE,interc=TRUE,elastic=elastictest)
auc(mod_pfaerr$newdata$CF..ansbin.,mod_pfaerr$prediction)


#RPFA + student propdec
componentl <- c("Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.","KC..Default.")
featl <- c("propdec","propdec$","logsuc$","logfail$","intercept")
fixedl <- c(optim_params[3],optim_params[1])
offsetl <- c(NA)
seedl<- c(NA,NA)
elastictest<- c("FALSE")
mod_rpfas<-LKT(data=val,
                components=componentl,features=featl,offsetvals=offsetl,
                fixedpars=fixedl,seedpars=seedl,
                dualfit=FALSE,interc=TRUE,elastic=elastictest)
auc(mod_rpfas$newdata$CF..ansbin.,mod_rpfas$prediction)
#RPFA +student propdec+ errordec
val$pred_ed = mod_pfa$prediction
componentl <- c("KC..Default.","Anon.Student.Id","Anon.Student.Id","KC..Default.","KC..Default.","KC..Default.")
featl <- c("propdec$","errordec","propdec","logsuc$","logfail$","intercept")
fixedl <- optim_params
offsetl <- c(NA)
seedl<- c(NA)
elastictest<- c("FALSE")
mod_rpfas_pfaerr<-LKT(data=val,
                components=componentl,features=featl,offsetvals=offsetl,
                fixedpars=fixedl,seedpars=seedl,
                dualfit=FALSE,interc=TRUE,elastic=elastictest)
auc(mod_rpfas_pfaerr$newdata$CF..ansbin.,mod_rpfas_pfaerr$prediction)
