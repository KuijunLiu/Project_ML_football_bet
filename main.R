library(dplyr)
library(rlang)
library(readr)
# all_data = read.csv(file = 'data/fulldata.csv')
all_data = list.files(path="../PJ/data", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows()
all_data$Date <- as.Date(all_data$Date, "%d/%m/%y")
all_data = all_data[order(all_data$Date),]

match_info = all_data[, c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "HTHG", "HTAG", "HS", "AS", "HST", "AST", "HC", "AC", "HF", "AF")]
match_info = match_info[complete.cases(match_info), ]
match_info$GT = match_info$FTHG + match_info$FTAG


hist(match_info$GT)



all_match_info = data.frame()
pb = txtProgressBar(min = 0, max = nrow(match_info), initial = 0) 
for (i in c(1:nrow(match_info))){
  this_home = match_info$HomeTeam[i]
  this_away = match_info$AwayTeam[i]
  this_part = match_info[1:(i-1),]
  this_part_home = this_part[this_part$HomeTeam == this_home | this_part$AwayTeam == this_home,]
  this_part_away = this_part[this_part$HomeTeam == this_away | this_part$AwayTeam == this_away,]
  if (nrow(this_part_home) < 5 | nrow(this_part_away) < 5){
    next
  }
  this_part_home = tail(this_part_home, n=5)
  this_part_away = tail(this_part_away, n=5)
  new_df_home = data.frame()
  new_df_away = data.frame()
  for (j in c(1:5)){
    if (this_part_home$HomeTeam[j] == this_home){
      this_row_home = data.frame("FG"=this_part_home$FTHG[j],
                                 "HG"=this_part_home$HTHG[j],
                                 "S"=this_part_home$HS[j],
                                 "ST"=this_part_home$HST[j],
                                 "C"=this_part_home$HC[j],
                                 "F"=this_part_home$HF[j])
    } else {
      this_row_home = data.frame("FG"=this_part_home$FTAG[j],
                                 "HG"=this_part_home$HTAG[j],
                                 "S"=this_part_home$AS[j],
                                 "ST"=this_part_home$AST[j],
                                 "C"=this_part_home$AC[j],
                                 "F"=this_part_home$AF[j])
    }
    new_df_home = rbind(new_df_home, this_row_home)
    
    
    if (this_part_away$HomeTeam[j] == this_away){
      this_row_away = data.frame("FG"=this_part_away$FTHG[j],
                                 "HG"=this_part_away$HTHG[j],
                                 "S"=this_part_away$HS[j],
                                 "ST"=this_part_away$HST[j],
                                 "C"=this_part_away$HC[j],
                                 "F"=this_part_away$HF[j])
    } else {
      this_row_away = data.frame("FG"=this_part_away$FTAG[j],
                                 "HG"=this_part_away$HTAG[j],
                                 "S"=this_part_away$AS[j],
                                 "ST"=this_part_away$AST[j],
                                 "C"=this_part_away$AC[j],
                                 "F"=this_part_away$AF[j])
    }
    new_df_away = rbind(new_df_away, this_row_away)
  }
  this_math_info = data.frame("HFG_min" = min(new_df_home$FG),
                              "HFG_mean" = mean(new_df_home$FG),
                              "HFG_max" = max(new_df_home$FG),
                              "HHG_min" = min(new_df_home$HG),
                              "HHG_mean" = mean(new_df_home$HG),
                              "HHG_max" = max(new_df_home$HG),
                              "HS_min" = min(new_df_home$S),
                              "HS_mean" = mean(new_df_home$S),
                              "HS_max" = max(new_df_home$S),
                              "HST_min" = min(new_df_home$ST),
                              "HST_mean" = mean(new_df_home$ST),
                              "HST_max" = max(new_df_home$ST),
                              "HC_min" = min(new_df_home$C),
                              "HC_mean" = mean(new_df_home$C),
                              "HC_max" = max(new_df_home$C),
                              "HF_min" = min(new_df_home$F),
                              "HF_mean" = mean(new_df_home$F),
                              "HF_max" = max(new_df_home$F),
                              "AFG_min" = min(new_df_away$FG),
                              "AFG_mean" = mean(new_df_away$FG),
                              "AFG_max" = max(new_df_away$FG),
                              "AHG_min" = min(new_df_away$HG),
                              "AHG_mean" = mean(new_df_away$HG),
                              "AHG_max" = max(new_df_away$HG),
                              "AS_min" = min(new_df_away$S),
                              "AS_mean" = mean(new_df_away$S),
                              "AS_max" = max(new_df_away$S),
                              "AST_min" = min(new_df_away$ST),
                              "AST_mean" = mean(new_df_away$ST),
                              "AST_max" = max(new_df_away$ST),
                              "AC_min" = min(new_df_away$C),
                              "AC_mean" = mean(new_df_away$C),
                              "AC_max" = max(new_df_away$C),
                              "AF_min" = min(new_df_away$F),
                              "AF_mean" = mean(new_df_away$F),
                              "AF_max" = max(new_df_away$F),
                              "GT" = match_info$GT[i])
  
  all_match_info = rbind(all_match_info, this_math_info)
  setTxtProgressBar(pb,i)
}
close(pb)

spec = c(train = .6, test = .2, validate = .2)

g = sample(cut(
  seq(nrow(all_match_info)), 
  nrow(all_match_info)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(all_match_info, g)

train = res$train
valid = res$valid
test = res$test


library(magrittr)


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
Y0=train$GT
X0=as.matrix(train[,-ncol(train)])

lambda=20
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
  beta=solve(crossprod(X0)+lambda*diag(p))%*%t(X0)%*%Y0
  BETAridge=cbind(BETAridge,beta) 
  
  Y1=valid$GT
  X1=as.matrix(valid[,-ncol(valid)])
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
