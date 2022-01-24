library(dplyr)
library(rlang)
library(readr)

all_data = list.files(path="data2", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows()
all_data$Date = as.Date(all_data$Date, "%d/%m/%y")
all_data = all_data[order(all_data$Date),]

match_info = all_data[, c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "HTHG", "HTAG", "HS", "AS", "HST", "AST", "HC", "AC", "HF", "AF", "BbMx>2.5")]
match_info = match_info[complete.cases(match_info), ]
match_info$GT = match_info$FTHG + match_info$FTAG
match_info$GD = match_info$FTHG - match_info$FTAG
match_info$FR = sign(match_info$FTHG - match_info$FTAG)

hist(match_info$GT)
hist(match_info$GD)

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
                                 "F"=this_part_home$HF[j],
                                 "FGC"=this_part_home$FTAG[j],
                                 "HGC"=this_part_home$HTAG[j],
                                 "SC"=this_part_home$AS[j],
                                 "STC"=this_part_home$AST[j],
                                 "CC"=this_part_home$AC[j],
                                 "FC"=this_part_home$AF[j])
    } else {
      this_row_home = data.frame("FG"=this_part_home$FTAG[j],
                                 "HG"=this_part_home$HTAG[j],
                                 "S"=this_part_home$AS[j],
                                 "ST"=this_part_home$AST[j],
                                 "C"=this_part_home$AC[j],
                                 "F"=this_part_home$AF[j],
                                 "FGC"=this_part_home$FTHG[j],
                                 "HGC"=this_part_home$HTHG[j],
                                 "SC"=this_part_home$HS[j],
                                 "STC"=this_part_home$HST[j],
                                 "CC"=this_part_home$HC[j],
                                 "FC"=this_part_home$HF[j])
    }
    new_df_home = rbind(new_df_home, this_row_home)
    
    
    if (this_part_away$HomeTeam[j] == this_away){
      this_row_away = data.frame("FG"=this_part_away$FTHG[j],
                                 "HG"=this_part_away$HTHG[j],
                                 "S"=this_part_away$HS[j],
                                 "ST"=this_part_away$HST[j],
                                 "C"=this_part_away$HC[j],
                                 "F"=this_part_away$HF[j],
                                 "FGC"=this_part_away$FTAG[j],
                                 "HGC"=this_part_away$HTAG[j],
                                 "SC"=this_part_away$AS[j],
                                 "STC"=this_part_away$AST[j],
                                 "CC"=this_part_away$AC[j],
                                 "FC"=this_part_away$AF[j])
    } else {
      this_row_away = data.frame("FG"=this_part_away$FTAG[j],
                                 "HG"=this_part_away$HTAG[j],
                                 "S"=this_part_away$AS[j],
                                 "ST"=this_part_away$AST[j],
                                 "C"=this_part_away$AC[j],
                                 "F"=this_part_away$AF[j],
                                 "FGC"=this_part_away$FTHG[j],
                                 "HGC"=this_part_away$HTHG[j],
                                 "SC"=this_part_away$HS[j],
                                 "STC"=this_part_away$HST[j],
                                 "CC"=this_part_away$HC[j],
                                 "FC"=this_part_away$HF[j])
    }
    new_df_away = rbind(new_df_away, this_row_away)
  }
  this_math_info = data.frame("HFGC" = mean(new_df_home$FGC),
                              "HFG" = mean(new_df_home$FG),
                              "HHGC" = mean(new_df_home$HGC),
                              "HHG" = mean(new_df_home$HG),
                              "HSC" = mean(new_df_home$SC),
                              "HS" = mean(new_df_home$S),
                              "HSTC" = mean(new_df_home$STC),
                              "HST" = mean(new_df_home$ST),
                              "HCC" = mean(new_df_home$CC),
                              "HC" = mean(new_df_home$C),
                              "HFC" = mean(new_df_home$FC),
                              "HF" = mean(new_df_home$F),
                              "AFGC" = mean(new_df_away$FGC),
                              "AFG" = mean(new_df_away$FG),
                              "AHGC" = mean(new_df_away$HGC),
                              "AHG" = mean(new_df_away$HG),
                              "ASC" = mean(new_df_away$SC),
                              "AS" = mean(new_df_away$S),
                              "ASTC" = mean(new_df_away$STC),
                              "AST" = mean(new_df_away$ST),
                              "ACC" = mean(new_df_away$CC),
                              "AC" = mean(new_df_away$C),
                              "AFC" = mean(new_df_away$FC),
                              "AF" = mean(new_df_away$F),
                              "FR" = match_info$FR[i],
                              "GT" = match_info$GT[i],
                              "GD" = match_info$GD[i],
                              "Bb25" = match_info$`BbMx>2.5`[i])
  
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

train_normalized = as.data.frame(scale(as.matrix(train[,c(1:(ncol(train)-4))])))
valid_normalized = as.data.frame(scale(valid[,c(1:(ncol(valid)-4))]))
test_normalized = as.data.frame(scale(test[,c(1:(ncol(test)-4))]))

train_targets = train[,c((ncol(train)-3):ncol(train))]
valid_targets = valid[,c((ncol(valid)-3):ncol(valid))]
test_targets = test[,c((ncol(test)-3):ncol(test))]

# save(train_normalized,file="data_last/train_features.Rda")
# save(valid_normalized,file="data_last/valid_features.Rda")
# save(test_normalized,file="data_last/test_features.Rda")

# save(train_targets, file = "data_last/train_targets.Rda")
# save(valid_targets, file = "data_last/valid_targets.Rda")
# save(test_targets, file = "data_last/test_targets.Rda")


hist(train$HFG)
hist(train_normalized$HFG)


