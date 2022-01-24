library(dplyr)
library(rlang)
library(readr)
library(magrittr)
library(ggplot2)

load("data_last/train_features.Rda")
load("data_last/valid_features.Rda")
load("data_last/test_features.Rda")

load("data_last/train_targets.Rda")
load("data_last/valid_targets.Rda")
load("data_last/test_targets.Rda")

# pm = glm(`train_targets$GT` ~ ., family="poisson", data = cbind(train_normalized, train_targets$GT))
# pm = lm(1/`train_targets$Bb25` ~ ., data = cbind(train_normalized, train_targets$Bb25))
pm = glm(as.factor(`train_targets$GT` > 2.5) ~ ., data = cbind(train_normalized, train_targets$GT), family = "binomial")
summary(pm)

# VGT = predict(pm, valid_normalized, type="response")
VBb25 = predict(pm, valid_normalized, type="response")

hist(pm$fitted.values)
hist(valid_targets$GT - VGT)
plot(valid_targets$GT, VGT)


# ggplot(cbind(valid_targets, as.data.frame(VGT)), aes(x=GT, y=VGT)) + geom_point()
ggplot(cbind(valid_targets, as.data.frame(VBb25)), aes(x=1/Bb25, y=VBb25, color = sign(GT>2.5))) + geom_point()
ggplot(cbind(valid_targets, as.data.frame(VBb25)), aes(x=as.factor(GT>2.5), y=1/Bb25, color = sign(GT>2.5))) + geom_boxplot(notch=FALSE)

ggplot(cbind(valid_targets, as.data.frame(VBb25)), aes(x=as.factor(GT>2.5), y=VBb25, color = sign(GT>2.5))) + geom_boxplot(notch=FALSE)
ggplot(cbind(valid_targets, as.data.frame(VBb25)), aes(x=as.factor(GT>2.5), y=VBb25, color = sign(GT>2.5))) + geom_boxplot(notch=FALSE)





















## LINEAR REGRESSION
n=nrow(train)
K=100
BETA=NULL
rmse=NULL
for(i in c(1:K))
{
  sel=sort(sample(c(1:n),n-10,replace=FALSE))
  data0_bis=train[sel,]
  Y0=data0_bis$GT
  X0=as.matrix(data0_bis[,-ncol(train)])
  beta=solve(crossprod(X0))%*%t(X0)%*%Y0
  BETA=cbind(BETA,beta) 
  
  Y1=valid$GT
  X1=as.matrix(valid[,-ncol(valid)])
  reg.forecast=X1%*%beta
  rmse=c(rmse,sqrt(mean((Y1-reg.forecast)^2))) 
}

matplot(BETA,type='l')
plot(BETA[1,],type='l')
plot(BETA[2,],type='l')
boxplot(BETA[1,])
boxplot(as.data.frame(t(BETA)))
hist(BETA[1,],breaks=10)


plot(sort(rmse/sd(Y1)),type='l')
# norm=colSums(BETA^2)
# plot(norm,type='l')


### RIDGE REGRESSION
p=ncol(X0)
# p=5
Y0=train$GT
X0=as.matrix(train[,-ncol(train)])
# X0=as.matrix(train[,1:5])

lambda=50
betaRidge=solve(crossprod(X0)+lambda*diag(p))%*%t(X0)%*%Y0
HRidge=crossprod(t(X0),solve(crossprod(X0)+lambda*diag(p)))%*%t(X0)
ychap_ridge=X0%*%betaRidge
sum(diag(HRidge))


n=nrow(train)
K=100
BETAridge=NULL
rmseridge=NULL
for(i in c(1:K))
{
  sel=sort(sample(c(1:n),n-10,replace=FALSE))
  data0_bis=train[sel,]
  Y0=data0_bis$GT
  X0=as.matrix(data0_bis[,-ncol(train)])
  # X0=as.matrix(data0_bis[,1:5])
  beta=solve(crossprod(X0)+lambda*diag(p))%*%t(X0)%*%Y0
  BETAridge=cbind(BETAridge,beta) 
  
  Y1=valid$GT
  X1=as.matrix(valid[,-ncol(valid)])
  # X1=as.matrix(valid[,1:5])
  reg.forecast=X1%*%beta
  rmseridge=c(rmseridge,sqrt(mean((Y1-reg.forecast)^2))) 
}

par(mfrow=c(2,1))
matplot(BETA,type='l',ylim=range(BETA,BETAridge))
matplot(BETAridge,type='l',ylim=range(BETA,BETAridge))

par(mfrow=c(1,1))
plot(rmse/sd(Y0),type='l',ylim=range(rmse/sd(Y0),rmseridge/sd(Y0)))
lines(rmseridge/sd(Y0),col='red')
legend('top',col=c("black","red"),c("Reg. lineaire","ridge"),lty=1,bty='n',lwd=2)


plot(reg.forecast, valid$GT)

high_pred = valid$GT[reg.forecast>2.5]
score = sum(high_pred>2.5)/length(high_pred)
score

hist(valid$GT)
hist(reg.forecast)

pm = glm(GT ~ ., family="poisson", data=train)
pp = predict(pm, valid, type="response")
hist(pp)
plot(reg.forecast, valid$GT)
high_pred = valid$GT[pp>2.5]
score = sum(high_pred>2.5)/length(high_pred)
score





















### GAM
library(mgcv)
library(mgcViz)
library(gridExtra)
library(yarrr)
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}
attach(train)
plot(GT,type='l')
plot(HFG_mean, GT,pch=16,cex=0.5)
plot(HS_mean, GT,pch=16,cex=0.5)
acf(GT, lag.max=20)

Nblock<-10
borne_block<-seq(1, nrow(train), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))


gam1<-gam(GT~s(HFG_mean,k=10)+s(HS_mean,k=10)+s(AFG_mean,k=10)+s(AS_mean,k=10), data=train)
summary(gam1)  

blockRMSE<-function(equation, block)
{
  g<- gam(as.formula(equation), data=train[-block,])
  forecast<-predict(g, newdata=train[block,])
  return(train[block,]$Load-forecast)
} 

equation <- GT~s(HFG_mean,k=10)+s(HS_mean,k=10)+s(AFG_mean,k=10)+s(AS_mean,k=10)
Block_residuals<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc1<-rmse(Block_residuals)
boxplot(Block_residuals)
plot(Block_residuals, type='l')

plot(data0$Temp, Block_residuals, pch=16)
plot(data0$NumWeek, Block_residuals, pch=16)
plot(data0$Load1, Block_residuals, pch=16)

gam_prov <- gam(Block_residuals~s(Load1), data=data0)  
summary(gam_prov)
fit <- getViz(gam_prov, nsim = 50)
plot(sm(fit, 1), n = 400) + l_points() + l_fitLine() + l_ciLine()


equation <- Load~s(Time,k=3)+s(NumWeek,k=30)+s(Temp,k=5)+s(Load1, k=10)
gam2<-gam(equation, data=data0)
summary(gam2)
Block_residuals2<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc2<-rmse(Block_residuals2)
rmseBloc2

plot(data0$IPI, Block_residuals2, pch=16)
gam_prov <- gam(Block_residuals2~s(data0$IPI))   
summary(gam_prov)

plot(data0$IPI_CVS, Block_residuals2, pch=16)
gam_prov <- gam(Block_residuals2~s(data0$IPI_CVS))   
summary(gam_prov)
plot(gam_prov)

equation <- Load~s(Time,k=3)+s(NumWeek,k=30)+s(Temp,k=5)+s(Load1, k=10)+s(IPI_CVS)
gam3<-gam(equation, data=data0)
summary(gam3)
Block_residuals3<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc3<-rmse(Block_residuals3)
rmseBloc3

#####change the IPI_CVS in linear effect
equation <- Load~s(Time,k=3)+s(NumWeek,k=30)+s(Temp,k=5)+s(Load1, k=10)+IPI_CVS
gam4<-gam(equation, data=data0)
summary(gam4)
Block_residuals4<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc4<-rmse(Block_residuals4)
rmseBloc4


plot(data0$Temp1, Block_residuals4, pch=16)
gam_prov <- gam(Block_residuals4~s(data0$Temp1))   
summary(gam_prov)
plot(gam_prov)

equation <- Load~s(Time,k=3)+s(NumWeek,k=30)+s(Temp,k=5)+s(Load1, k=10)+IPI_CVS+s(Temp1)
gam5<-gam(equation, data=data0)
summary(gam5)
Block_residuals5<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc5<-rmse(Block_residuals5)
rmseBloc5

plot(gam5$residuals, type='l')

noel = which(abs(data0$Day - 24) <= 3 & data0$Month == 12)
consoNoel = vector("numeric", length(data0$Time))
consoNoel[noel] = 1
data0 <- data.frame(data0, consoNoel)

plot(data0$Time, gam5$residuals, type='l')
select<-which(data0$consoNoel==1)
points(data0$Time[select], gam5$residuals[select], col='red', pch=20)

equation <- Load~s(Time,k=3)+s(NumWeek,k=30, bs='cc')+s(Temp,k=5)+s(Load1, k=10)+IPI_CVS+s(Temp1)
+consoNoel
gam6<-gam(equation, data=data0)
summary(gam6)
Block_residuals6<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc6<-rmse(Block_residuals6)
rmseBloc6

plot(data0$Time, gam6$residuals, type='l')
select<-which(data0$consoNoel==1)
points(data0$Time[select], gam6$residuals[select], col='red', pch=20)

data0[which(abs(gam6$residuals)>3000), 1:3]

noel = which(abs(data1$Day - 24) <= 3 & data1$Month == 12)
consoNoel = vector("numeric", length(data1$Time))
consoNoel[noel] = 1
data1 <- data.frame(data1, consoNoel)


ychap6 <- predict(gam6, newdata=data1)
rmse(data1$Load-ychap)

equation <- Load~s(Time,k=3)+s(NumWeek,k=30, bs='cc')+te(Time, Temp, k=c(3, 5))+s(Load1, k=10)+IPI_CVS+s(Temp1)

gam7<-gam(equation, data=data0)
summary(gam7)
ychap <- predict(gam7, newdata=data1)
rmse(data1$Load-ychap)

par(mfrow=c(1,1))
plot(data1$Load, type='l')
lines(ychap, col='red')
lines(ychap6, col='blue')






# head(sort(all_data$Date, decreasing = TRUE))
# plot(FTHG ~ Date, all_data, xaxt = "n", type = "l")

colnames(all_data)
#GOALS
par(mfrow=c(1,3))
hist(all_data$FTHG)
hist(as.numeric(all_data$FTR))
hist(all_data$FTHG-as.numeric(all_data$FTR))
#CORNERS
par(mfrow=c(1,3))
hist(all_data$HC, breaks = 20)
hist(all_data$AC, breaks = 20)
hist(all_data$HC - all_data$AC, breaks = 20)
#FOULS
par(mfrow=c(1,3))
hist(all_data$HF, breaks = 20)
hist(all_data$AF, breaks = 20)
hist(all_data$HF - all_data$AF, breaks = 20)





# paste("Ali", "Baba", sep = "")
# matches <- read.csv(file = 'E0.csv')
# matches2 <- filter(matches$FTR, as.numeric(matches$B365H) < 3)
# res <- matches$FTR[as.numeric(matches$B365H) < 3 & as.numeric(matches$B365H) > 2.5 ]
# sum(res == "H" )/ length(res)
# length(res)
# 
# library(httr)
# 
# url <- "https://api-football-v1.p.rapidapi.com/v3/timezone"
# 
# response <- VERB("GET", url, add_headers(x_rapidapi_host = 'api-football-v1.p.rapidapi.com', x_rapidapi_key = 'f33ef3a56bmsh8afb532b2af65e3p156ca6jsn02d53038715e'), content_type("application/octet-stream"))
#                                          
# content(response, "text")
