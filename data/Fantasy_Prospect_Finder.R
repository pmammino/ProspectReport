#Load All Applicable Packages ---------------------------------
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("Hmisc")
# install.packages("MASS")
# install.packages("WMDB")
# install.packages("teamcolors")
# install.packages("Lahman")
# library(teamcolors)
# library(Hmisc)
# library(SDMTools)
# library(MASS)
# library(tidyverse)
# library(readxl)
# library(Hmisc)
# library(WMDB)
# library(Lahman)
options(scipen = 999)

#Team Colors----

mlb_colors <- teamcolors %>%
  filter(league == "mlb") 

#Helper Functions -----
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Load In Data ----
minors_all <- read_excel('data/Online Championship Data.xlsx', sheet = "Minor_All")
wrc <- read.csv("data/wrc.csv",stringsAsFactors = FALSE)
minors_all <- minors_all[,-which(colnames(minors_all) %in% c("PA_MLB","BB%","K%","SB","CS","SB Att","SBA_PA","SBRate","Spd","wSB", "Pull%", "Cent%", "Oppo%", "ISO"))]

minors_all[minors_all$Name == "Ian Miller","PlayerId"] <- "15156"
minors_all[minors_all$Name == "Gavin Lux","PlayerId"] <- "19955"
minors_all[minors_all$Name == "Jaylin Davis","PlayerId"] <- "19552"
minors_all[minors_all$Name == "Danny Mendick","PlayerId"] <- "18889"
minors_all[minors_all$Name == "Sean Murphy","PlayerId"] <- "19352"
minors_all[minors_all$Name == "Sam Haggerty","PlayerId"] <- "18054"
minors_all[minors_all$Name == "Tres Barrera","PlayerId"] <- "19977"
minors_all[minors_all$Name == "Kyle Lewis","PlayerId"] <- "19508"
minors_all[minors_all$Name == "Sam Hilliard","PlayerId"] <- "17954"
minors_all[minors_all$Name == "Deivy Grullon","PlayerId"] <- "15988"
minors_all[minors_all$Name == "Sheldon Neuse","PlayerId"] <- "19635"
minors_all[minors_all$Name == "Ryan McBroom","PlayerId"] <- "16524"
minors_all[minors_all$Name == "Johnny Davis","PlayerId"] <- "15830"
minors_all[minors_all$Name == "Kean Wong","PlayerId"] <- "15994"
minors_all[minors_all$Name == "Erick Mejia","PlayerId"] <- "15817"
minors_all[minors_all$Name == "Donnie Walton","PlayerId"] <- "19314"
minors_all[minors_all$Name == "Tyler Heineman","PlayerId"] <- "13897"
minors_all[minors_all$Name == "Tyrone Taylor","PlayerId"] <- "13675"
minors_all[minors_all$Name == "Seth Mejias-Brean","PlayerId"] <- "13905"

minors_all <- left_join(minors_all,wrc[,c("PlayerId","Season","Team","Level","wRC.")], by = c("PlayerId", "Team", "Level","Season"))
minors_all <- minors_all[,c("Name",
                            "PlayerId",
                            "Season",
                            "Team",
                            "Level",
                            "Age",
                            "PA",
                            "BB/K",
                            "BABIP",
                            "SB_PA",
                            "GB/FB",
                            "GB%",
                            "LD%",
                            "FB%",
                            "IFFB%",
                            "SwStr%",
                            "wRC.",
                            "Total Val")]

miLB <- read_excel("data/MiLB 2019.xlsx")
miLB <- filter(miLB, Level != "R")

wrc2019 <- wrc %>%
  filter(Season == 2019)

miLB <- left_join(miLB,wrc2019[,c("PlayerId","Team","Level","wRC.")], by = c("PlayerId", "Team", "Level"))

miLB_names <- as.list(miLB %>%
  filter(grepl("sa",PlayerId)) %>%
  select(PlayerId))[[1]]

minors_all <- minors_all %>%
  filter(!PlayerId %in% miLB_names)

#Divide Sample/2019 Data Into Level Groups ----
AAA <- filter(minors_all, Level == "AAA")
AA <- filter(minors_all, Level == "AA")
A <- filter(minors_all, Level == "A")
A_Low <- filter(minors_all, Level == "A-")
A_High <- filter(minors_all, Level == "A+")
R <- filter(minors_all, Level == "R")


#Run Correlations For All Levels ----
AAA_Cor <- rcorr(as.matrix(AAA[,c(6,8:18)]))
AAA_Cor <- flattenCorrMatrix(AAA_Cor$r, AAA_Cor$P)
AAA_Cor <- filter(AAA_Cor,column == "Total Val")
age <- filter(AAA_Cor, row == "Age")
AAA_Cor <- filter(AAA_Cor,p < 0.005)
AAA_Cor <- filter(AAA_Cor, cor >= 0.06 | cor <= -0.06)
AAA_Cor <- rbind(age,AAA_Cor)
AAA_Cor <- AAA_Cor[-9,]

AA_Cor <- rcorr(as.matrix(AA[,c(6,8:18)]))
AA_Cor <- flattenCorrMatrix(AA_Cor$r, AA_Cor$P)
AA_Cor <- filter(AA_Cor,column == "Total Val")
AA_Cor <- filter(AA_Cor,p < 0.005)
AA_Cor <- filter(AA_Cor, cor >= 0.06 | cor <= -0.06)

A_Cor <- rcorr(as.matrix(A[,c(6,8:18)]))
A_Cor <- flattenCorrMatrix(A_Cor$r, A_Cor$P)
A_Cor <- filter(A_Cor,column == "Total Val")
A_Cor <- filter(A_Cor,p < 0.005)
A_Cor <- filter(A_Cor, cor >= 0.06 | cor <= -0.06)

A_High_Cor <- rcorr(as.matrix(A_High[,c(6,8:18)]))
A_High_Cor <- flattenCorrMatrix(A_High_Cor$r, A_High_Cor$P)
A_High_Cor <- filter(A_High_Cor,column == "Total Val")
A_High_Cor <- filter(A_High_Cor,p < 0.005)
A_High_Cor <- filter(A_High_Cor, cor >= 0.06 | cor <= -0.06)

A_Low_Cor <- rcorr(as.matrix(A_Low[,c(6,8:18)]))
A_Low_Cor <- flattenCorrMatrix(A_Low_Cor$r, A_Low_Cor$P)
A_Low_Cor <- filter(A_Low_Cor,column == "Total Val")
A_Low_Cor <- filter(A_Low_Cor,p < 0.005)
A_Low_Cor <- filter(A_Low_Cor, cor >= 0.06 | cor <= -0.06)

R_Cor <- rcorr(as.matrix(R[,c(6,8:18)]))
R_Cor <- flattenCorrMatrix(R_Cor$r, R_Cor$P)
R_Cor <- filter(R_Cor,column == "Total Val")
R_Cor <- filter(R_Cor,p < 0.005)
R_Cor <- filter(R_Cor, cor >= 0.06 | cor <= -0.06)


#Generate Samples For Each Level With Only Applicable Data -----
AAA_Cor$row <- as.character(AAA_Cor$row)
AAA_Sample <- AAA[,which(colnames(AAA) %in% c("Name",AAA_Cor$row, "Total Val"))]

AA_Cor$row <- as.character(AA_Cor$row)
AA_Sample <- AA[,which(colnames(AA) %in% c("Name",AA_Cor$row, "Total Val"))]

A_Cor$row <- as.character(A_Cor$row)
A_Sample <- A[,which(colnames(A) %in% c("Name",A_Cor$row, "Total Val"))]

A_High_Cor$row <- as.character(A_High_Cor$row)
A_High_Sample <- A_High[,which(colnames(A_High) %in% c("Name",A_High_Cor$row,"Total Val"))]

A_Low_Cor$row <- as.character(A_Low_Cor$row)
A_Low_Sample <- A_Low[,which(colnames(A_Low) %in% c("Name",A_Low_Cor$row,"Total Val"))]

R_Cor$row <- as.character(R_Cor$row)
R_Sample <- R[,which(colnames(R) %in% c("Name",R_Cor$row, "Total Val"))]


#Write Function to Create and Calculater xVal -----
Minors_calc <- function(key)
{
    player <- filter(miLB, Key == key)
    if(player$Level == "AAA")
    {
      player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(AAA_Cor$row)))]
      Sample <- AAA_Sample
      weight <- diag(abs(AAA_Cor$cor)/abs(AAA_Cor[1,'cor'])/(sum(abs(AAA_Cor$cor)/abs(AAA_Cor[1,'cor']))))
    }else if(player$Level == "AA")
    {
      player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(AA_Cor$row)))]
      Sample <- AA_Sample
      weight <- diag(abs(AA_Cor$cor)/abs(AA_Cor[1,'cor'])/(sum(abs(AA_Cor$cor)/abs(AA_Cor[1,'cor']))))
    }else if(player$Level == "A+")
    {
      player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_High_Cor$row)))]
      Sample <- A_High_Sample
      weight <- diag(abs(A_High_Cor$cor)/abs(A_High_Cor[1,'cor'])/(sum(abs(A_High_Cor$cor)/abs(A_High_Cor[1,'cor']))))
    }else if(player$Level == "A-")
    {
      player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_Low_Cor$row)))]
      Sample <- A_Low_Sample
      weight <- diag(abs(A_Low_Cor$cor)/abs(A_Low_Cor[1,'cor'])/(sum(abs(A_Low_Cor$cor)/abs(A_Low_Cor[1,'cor']))))
    }else if(player$Level == "A")
    {
      player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_Cor$row)))]
      Sample <- A_Sample
      weight <- diag(abs(A_Cor$cor)/abs(A_Cor[1,'cor'])/(sum(abs(A_Cor$cor)/abs(A_Cor[1,'cor']))))
    }else
    {
      player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(R_Cor$row)))]
      Sample <- R_Sample
      weight <- diag(abs(R_Cor$cor)/abs(R_Cor[1,'cor'])/(sum(abs(R_Cor$cor)/abs(R_Cor[1,'cor']))))
    }
    
    #Dist <- mahalanobis(x = Sample[,2:7], as.numeric(player[,-c(1:5)]), cov(Sample[,2:7]),tol=1e-20)
    Dist <- wmahalanobis(x = Sample[,2:(length(weight[1,])+1)], as.numeric(player[,-c(1:5)]), cov(Sample[,2:(length(weight[1,])+1)]),weight)
    Sample$Dist <- Dist
    Sample$Dist <- round(Sample$Dist,05)
    Sample$W <- 1/Sample$Dist
    Sample <- Sample[order(Sample$Dist, decreasing=FALSE),]
    Sample <- Sample[1:100,]
    weighted.mean(Sample$`Total Val`, Sample$W)
}

# #Level By Level Calc ------
#  AAA_19 <- filter(miLB, Level == "AAA")
#  List_all <- as.list(AAA_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_calc)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  xVal <- data.frame(unlist(comps_all))
#  colnames(xVal) <- "xVal"
#  AAA_19 <- cbind(AAA_19,xVal)
#  AAA_19 <- AAA_19[,c(1:7,28)]
# 
#  AA_19 <- filter(miLB, Level == "AA")
#  List_all <- as.list(AA_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_calc)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  xVal <- data.frame(unlist(comps_all))
#  colnames(xVal) <- "xVal"
#  AA_19 <- cbind(AA_19,xVal)
#  AA_19 <- AA_19[,c(1:7,28)]
# 
#  A_High_19 <- filter(miLB, Level == "A+")
#  List_all <- as.list(A_High_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_calc)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  xVal <- data.frame(unlist(comps_all))
#  colnames(xVal) <- "xVal"
#  A_High_19 <- cbind(A_High_19,xVal)
#  A_High_19 <- A_High_19[,c(1:7,28)]
# 
#  A_Low_19 <- filter(miLB, Level == "A-")
#  List_all <- as.list(A_Low_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_calc)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  xVal <- data.frame(unlist(comps_all))
#  colnames(xVal) <- "xVal"
#  A_Low_19 <- cbind(A_Low_19,xVal)
#  A_Low_19 <- A_Low_19[,c(1:7,28)]
# 
#  A_19 <- filter(miLB, Level == "A")
#  List_all <- as.list(A_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_calc)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  xVal <- data.frame(unlist(comps_all))
#  colnames(xVal) <- "xVal"
#  A_19 <- cbind(A_19,xVal)
#  A_19 <- A_19[,c(1:7,28)]
# 
#  milb_2019 <- rbind(AAA_19,AA_19,A_High_19,A_Low_19,A_19)
# 
# 
# # Build Table 2019 Value at All Levels -----
#  all_players_AAA <- milb_2019 %>%
#    filter(Level == "AAA") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(AAA.PA = sum(PA),
#              AAAxVal = wt.mean(xVal,PA))
# 
#  all_players_AA <- milb_2019 %>%
#    filter(Level == "AA") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(AA.PA = sum(PA),
#              AAxVal = wt.mean(xVal,PA))
# 
#  all_players_A_High <- milb_2019 %>%
#    filter(Level == "A+") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(A_High.PA = sum(PA),
#              A_HighxVal = wt.mean(xVal,PA))
# 
#  all_players_A <- milb_2019 %>%
#    filter(Level == "A") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(A.PA = sum(PA),
#              AxVal = wt.mean(xVal,PA))
# 
#  all_players_A_Low <- milb_2019 %>%
#    filter(Level == "A-") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(A_Low.PA = sum(PA),
#              A_LowxVal = wt.mean(xVal,PA))
# 
#  all_players <- merge(milb_2019[,c(1,2,6)],all_players_AAA, by = intersect(names(milb_2019), names(all_players_AAA)),all = TRUE)
#  all_players <- merge(all_players,all_players_AA, by = intersect(names(all_players), names(all_players_AA)),all = TRUE)
#  all_players <- merge(all_players,all_players_A_High, by = intersect(names(all_players), names(all_players_A_High)),all = TRUE)
#  all_players <- merge(all_players,all_players_A, by = intersect(names(all_players), names(all_players_A)),all = TRUE)
#  all_players <- merge(all_players,all_players_A_Low, by = intersect(names(all_players), names(all_players_A_Low)),all = TRUE)
#  all_players <- distinct(all_players)
#  all_players[is.na(all_players)] <- 0
# 
#  all_players$TotalxVal <- rowSums(all_players[,c(5,7,9,11,13)] *
#                                   all_players[,c(4,6,8,10,12)])/
#    rowSums(all_players[,c(4,6,8,10,12)])
# 
# 
# 
# # Adjust For League Context----
#  all_players_adjust <- all_players
#  all_players_adjust$AAAxVal <- all_players_adjust$AAAxVal-wt.mean(all_players_adjust$AAAxVal,all_players_adjust$AAA.PA)
#  all_players_adjust$AAxVal <- all_players_adjust$AAxVal-wt.mean(all_players_adjust$AAxVal,all_players_adjust$AA.PA)
#  all_players_adjust$A_HighxVal <- all_players_adjust$A_HighxVal-wt.mean(all_players_adjust$A_HighxVal,all_players_adjust$A_High.PA)
#  all_players_adjust$AxVal <- all_players_adjust$AxVal-wt.mean(all_players_adjust$AxVal,all_players_adjust$A.PA)
#  all_players_adjust$A_LowxVal <- all_players_adjust$A_LowxVal-wt.mean(all_players_adjust$A_LowxVal,all_players_adjust$A_Low.PA)
#  all_players_adjust$TotalxVal <- rowSums(all_players_adjust[,c(5,7,9,11,13)] *
#                                                all_players_adjust[,c(4,6,8,10,12)])/
#    rowSums(all_players_adjust[,c(4,6,8,10,12)])
# 
# 
#  all_players_adjust <- all_players_adjust %>%
#    filter(Age <=29) %>%
#    arrange(desc(TotalxVal))
# 
#  all_players_adjust$TotalxVal <- round(all_players_adjust$TotalxVal,3)
#  all_players_2019 <- all_players_adjust[,c(1,2,3,14)]
# 
#  all_players_trim <- all_players[,c(1,2,3,14)]
#  all_players_adjust_trim <- all_players_adjust[,c(1,2,3,14)]
#  all_players_merge <- merge(all_players_trim,all_players_adjust_trim, by = "PlayerId",all = TRUE)
#  all_players_merge <- all_players_merge[,c(1,2,3,4,7)]
#  colnames(all_players_merge)<- c("PlayerId","Name","Age", "xVal", "xVal_Adj")
# 
#  all_players_merge <- all_players_merge %>%
#    filter(Age <=29)
# 
# all_players_merge$Avg <- (all_players_merge$xVal * .5) + (all_players_merge$xVal_Adj * .5)
# all_players_merge$Avg <- round(all_players_merge$Avg,3)
# all_players_merge$xVal <- round(all_players_merge$xVal,3)
# all_players_merge <- all_players_merge %>% arrange(desc(Avg))
# saveRDS(all_players_merge, file = "data/all_players_2019_V2.rds")

all_players_2019 <- readRDS("data/all_players_2019_V2.rds")
# 
# version_comp <- left_join(all_players_merge,all_players_2019_V1,by = "PlayerId")
# version_comp$ValRank_V1 <- rank(-version_comp$xVal.y)
# version_comp$ValRank_V2 <- rank(-version_comp$xVal.x)
# version_comp$AdjRank_V1 <- rank(-version_comp$xVal_Adj.y)
# version_comp$AdjRank_V2 <- rank(-version_comp$xVal_Adj.x)
# version_comp$AvgRank_V1 <- rank(-version_comp$Avg.y)
# version_comp$AvgRank_V2 <- rank(-version_comp$Avg.x)
# 
# version_comp$Val_Diff <- version_comp$ValRank_V2 - version_comp$ValRank_V1
# version_comp$Adj_Diff <- version_comp$AdjRank_V2 - version_comp$AdjRank_V1
# version_comp$Avg_Diff <- version_comp$AvgRank_V2 - version_comp$AvgRank_V1
# 

#Function to Show Top 100 Comps----
Minors_comp <- function(key)
{
  player <- filter(miLB, Key == key)
  if(player$Level == "AAA")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(AAA_Cor$row)))]
    Sample <- AAA_Sample
    weight <- diag(abs(AAA_Cor$cor)/abs(AAA_Cor[1,'cor'])/(sum(abs(AAA_Cor$cor)/abs(AAA_Cor[1,'cor']))))
  }else if(player$Level == "AA")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(AA_Cor$row)))]
    Sample <- AA_Sample
    weight <- diag(abs(AA_Cor$cor)/abs(AA_Cor[1,'cor'])/(sum(abs(AA_Cor$cor)/abs(AA_Cor[1,'cor']))))
  }else if(player$Level == "A+")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_High_Cor$row)))]
    Sample <- A_High_Sample
    weight <- diag(abs(A_High_Cor$cor)/abs(A_High_Cor[1,'cor'])/(sum(abs(A_High_Cor$cor)/abs(A_High_Cor[1,'cor']))))
  }else if(player$Level == "A-")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_Low_Cor$row)))]
    Sample <- A_Low_Sample
    weight <- diag(abs(A_Low_Cor$cor)/abs(A_Low_Cor[1,'cor'])/(sum(abs(A_Low_Cor$cor)/abs(A_Low_Cor[1,'cor']))))
  }else if(player$Level == "A")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_Cor$row)))]
    Sample <- A_Sample
    weight <- diag(abs(A_Cor$cor)/abs(A_Cor[1,'cor'])/(sum(abs(A_Cor$cor)/abs(A_Cor[1,'cor']))))
  }else
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(R_Cor$row)))]
    Sample <- R_Sample
    weight <- diag(abs(R_Cor$cor)/abs(R_Cor[1,'cor'])/(sum(abs(R_Cor$cor)/abs(R_Cor[1,'cor']))))
  }
  
  #Dist <- mahalanobis(x = Sample[,2:7], as.numeric(player[,-c(1:5)]), cov(Sample[,2:7]),tol=1e-20)
  Dist <- wmahalanobis(x = Sample[,2:(length(weight[1,])+1)], as.numeric(player[,-c(1:5)]), cov(Sample[,2:(length(weight[1,])+1)]),weight)
  Sample$Dist <- Dist
  Sample$W <- 1/Sample$Dist
  Sample <- Sample[order(Sample$Dist, decreasing=FALSE),]
  Sample <- Sample[1:100,c("Name", "Total Val", "Dist","W")]
  Sample$`Total Val` <- round(Sample$`Total Val`,2)
  Sample$Dist <- round(Sample$Dist,2)
  Sample
}

Minors_chart <- function(key)
{
  player <- Minors_comp(key)
  team <- filter(miLB, Key == key)$Team
  player$prob <- player$W/sum(player$W)
  ggplot(player, aes(x = `Total Val`, y = ..density.., weight = prob))+ 
    geom_histogram(binwidth = 1, color = filter(mlb_colors,mascot == team)$primary, fill = filter(mlb_colors,mascot == team)$secondary) +
    theme_bw() +
    labs(x = "Expected Value",
       y = "Probabilty",
       caption = "@pmamminofantasy",
       title = "Comp Based Expected Fantasy Value",
       subtitle = filter(miLB, Key == key)$Name) +
    theme_bw() +
    theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10))
}

##Prob Elite
Minors_Elite <- function(key)
{
  player <- filter(miLB, Key == key)
  if(player$Level == "AAA")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(AAA_Cor$row)))]
    Sample <- AAA_Sample
    weight <- diag(abs(AAA_Cor$cor)/abs(AAA_Cor[1,'cor'])/(sum(abs(AAA_Cor$cor)/abs(AAA_Cor[1,'cor']))))
  }else if(player$Level == "AA")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(AA_Cor$row)))]
    Sample <- AA_Sample
    weight <- diag(abs(AA_Cor$cor)/abs(AA_Cor[1,'cor'])/(sum(abs(AA_Cor$cor)/abs(AA_Cor[1,'cor']))))
  }else if(player$Level == "A+")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_High_Cor$row)))]
    Sample <- A_High_Sample
    weight <- diag(abs(A_High_Cor$cor)/abs(A_High_Cor[1,'cor'])/(sum(abs(A_High_Cor$cor)/abs(A_High_Cor[1,'cor']))))
  }else if(player$Level == "A-")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_Low_Cor$row)))]
    Sample <- A_Low_Sample
    weight <- diag(abs(A_Low_Cor$cor)/abs(A_Low_Cor[1,'cor'])/(sum(abs(A_Low_Cor$cor)/abs(A_Low_Cor[1,'cor']))))
  }else if(player$Level == "A")
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(A_Cor$row)))]
    Sample <- A_Sample
    weight <- diag(abs(A_Cor$cor)/abs(A_Cor[1,'cor'])/(sum(abs(A_Cor$cor)/abs(A_Cor[1,'cor']))))
  }else
  {
    player <- player[,which(colnames(player) %in% c("Name","PlayerId","Team","Level","Key",as.character(R_Cor$row)))]
    Sample <- R_Sample
    weight <- diag(abs(R_Cor$cor)/abs(R_Cor[1,'cor'])/(sum(abs(R_Cor$cor)/abs(R_Cor[1,'cor']))))
  }
  
  #Dist <- mahalanobis(x = Sample[,2:7], as.numeric(player[,-c(1:5)]), cov(Sample[,2:7]),tol=1e-20)
  Dist <- wmahalanobis(x = Sample[,2:(length(weight[1,])+1)], as.numeric(player[,-c(1:5)]), cov(Sample[,2:(length(weight[1,])+1)]),weight)
  Sample$Dist <- Dist
  Sample$Dist <- round(Sample$Dist,05)
  Sample$W <- 1/Sample$Dist
  Sample <- Sample[order(Sample$Dist, decreasing=FALSE),]
  Sample <- Sample[1:100,]
  sum <- sum(Sample$W)
  Sample <- Sample %>%
    filter(`Total Val` >= 11)
  round((sum(Sample$W)/sum) * 100,1)
}

# #Elite Level Calc
#  AAA_19 <- filter(miLB, Level == "AAA")
#  List_all <- as.list(AAA_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_Elite)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  Elite <- data.frame(unlist(comps_all))
#  colnames(Elite) <- "Elite_Rate"
#  AAA_19 <- cbind(AAA_19,Elite)
#  AAA_19 <- AAA_19[,c(1:7,28)]
# 
#  AA_19 <- filter(miLB, Level == "AA")
#  List_all <- as.list(AA_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_Elite)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  Elite <- data.frame(unlist(comps_all))
#  colnames(Elite) <- "Elite_Rate"
#  AA_19 <- cbind(AA_19,Elite)
#  AA_19 <- AA_19[,c(1:7,28)]
# 
#  A_High_19 <- filter(miLB, Level == "A+")
#  List_all <- as.list(A_High_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_Elite)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  Elite <- data.frame(unlist(comps_all))
#  colnames(Elite) <- "Elite_Rate"
#  A_High_19 <- cbind(A_High_19,Elite)
#  A_High_19 <- A_High_19[,c(1:7,28)]
# 
#  A_Low_19 <- filter(miLB, Level == "A-")
#  List_all <- as.list(A_Low_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_Elite)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  Elite <- data.frame(unlist(comps_all))
#  colnames(Elite) <- "Elite_Rate"
#  A_Low_19 <- cbind(A_Low_19,Elite)
#  A_Low_19 <- A_Low_19[,c(1:7,28)]
# 
#  A_19 <- filter(miLB, Level == "A")
#  List_all <- as.list(A_19$Key)
#  start_time <- Sys.time()
#  comps_all <- lapply(List_all, Minors_Elite)
#  end_time <- Sys.time()
#  end_time - start_time
# 
#  Elite <- data.frame(unlist(comps_all))
#  colnames(Elite) <- "Elite_Rate"
#  A_19 <- cbind(A_19,Elite)
#  A_19 <- A_19[,c(1:7,28)]
# 
#  milb_2019 <- rbind(AAA_19,AA_19,A_High_19,A_Low_19,A_19)
# 
# #Build Table 2019 Elite Rate at All Levels -----
#  all_players_AAA <- milb_2019 %>%
#    filter(Level == "AAA") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(AAA.PA = sum(PA),
#              AAAER = wt.mean(Elite_Rate,PA))
# 
#  all_players_AA <- milb_2019 %>%
#    filter(Level == "AA") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(AA.PA = sum(PA),
#              AAER = wt.mean(Elite_Rate,PA))
# 
#  all_players_A_High <- milb_2019 %>%
#    filter(Level == "A+") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(A_High.PA = sum(PA),
#              A_HighER = wt.mean(Elite_Rate,PA))
# 
#  all_players_A <- milb_2019 %>%
#    filter(Level == "A") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(A.PA = sum(PA),
#              AER = wt.mean(Elite_Rate,PA))
# 
#  all_players_A_Low <- milb_2019 %>%
#    filter(Level == "A-") %>%
#    group_by(Name, PlayerId) %>%
#    arrange(Name, PlayerId) %>%
#    summarise(A_Low.PA = sum(PA),
#              A_LowER = wt.mean(Elite_Rate,PA))
# 
#  all_players <- merge(milb_2019[,c(1,2,6)],all_players_AAA, by = intersect(names(milb_2019), names(all_players_AAA)),all = TRUE)
#  all_players <- merge(all_players,all_players_AA, by = intersect(names(all_players), names(all_players_AA)),all = TRUE)
#  all_players <- merge(all_players,all_players_A_High, by = intersect(names(all_players), names(all_players_A_High)),all = TRUE)
#  all_players <- merge(all_players,all_players_A, by = intersect(names(all_players), names(all_players_A)),all = TRUE)
#  all_players <- merge(all_players,all_players_A_Low, by = intersect(names(all_players), names(all_players_A_Low)),all = TRUE)
#  all_players <- distinct(all_players)
#  all_players[is.na(all_players)] <- 0
#  all_players$TotalER <- rowSums(all_players[,c(5,7,9,11,13)] *
#                                   all_players[,c(4,6,8,10,12)])/
#    rowSums(all_players[,c(4,6,8,10,12)])
# 
#  all_players_adjust <- all_players
#  all_players_adjust$AAAER <- all_players_adjust$AAAER-wt.mean(all_players_adjust$AAAER,all_players_adjust$AAA.PA)
#  all_players_adjust$AAER <- all_players_adjust$AAER-wt.mean(all_players_adjust$AAER,all_players_adjust$AA.PA)
#  all_players_adjust$A_HighER <- all_players_adjust$A_HighER-wt.mean(all_players_adjust$A_HighER,all_players_adjust$A_High.PA)
#  all_players_adjust$AER <- all_players_adjust$AER-wt.mean(all_players_adjust$AER,all_players_adjust$A.PA)
#  all_players_adjust$A_LowER <- all_players_adjust$A_LowER-wt.mean(all_players_adjust$A_LowER,all_players_adjust$A_LowER)
#  all_players_adjust$TotalER <- rowSums(all_players_adjust[,c(5,7,9,11,13)] *
#                                            all_players_adjust[,c(4,6,8,10,12)])/
#    rowSums(all_players_adjust[,c(4,6,8,10,12)])
# 
# write_rds(all_players,"data/upside_rates.rds")

all_players_elite <- readRDS("data/upside_rates.rds")

all_players_elite <- all_players_elite[,c("PlayerId","TotalER")]

all_players_2019 <- left_join(all_players_2019,all_players_elite,by = "PlayerId")
all_players_2019$TotalER <- round(all_players_2019$TotalER,1)

all_players_2019 <- all_players_2019[,c("Name",
                                        "PlayerId",
                                        "Age",
                                        "xVal",
                                        "xVal_Adj",
                                        "TotalER" )]

colnames(all_players_2019) <- c("Name",
                                "PlayerID",
                                "Age",
                                "Value",
                                "Adjusted Value",
                                "Elite Rate")

all_players_2019 <- all_players_2019 %>%
      filter(Age <= 29) %>%
      arrange(desc(`Adjusted Value`))

updates <- tibble(
  Version = c("1.0",
              "1.1",
              "2.0",
              "2.1",
              "2.2"),
  Description = c("Initial Upload",
                  "Download Handler",
                  "Removed ISO Replaced With wRC+",
                  "Elite Rate Added",
                  "Adjustment To Player Sample"),
  Date = c("1-13-2020",
           "2-4-2020",
           "5-8-2020",
           "5-19-2020",
           "5-30-2020")
)

update <- gt(updates,
             groupname_col = "Version") %>%
  tab_options(heading.background.color = "#074d8b") %>%
  tab_header(
    title = "Update Log") %>%
  tab_style(style = list(
    cell_fill(color = "#46e82d"),
    cell_text(weight = "bold", color = "white")
  ),
  locations = cells_row_groups(groups = unique(updates$Version)))

# all_players_table <- all_players %>%
#   filter(grepl('sa', PlayerId)) %>%
#   select(Name, TotalER) %>%
#   arrange(desc(TotalER))
# 
# all_players_table$TotalER <- round(all_players_table$TotalER,1)
# 
# 
# all_players_table <- all_players_table[1:10,]
# 
# top_upside <- gt(all_players_table) %>%
#   tab_options(
#               heading.background.color = "black",
#               column_labels.background.color = "gray") %>%
#   tab_header(
#     title = "Top Fantasy Upside Prospects",
#     subtitle = "@pmamminofantasy") %>%
#   cols_label(
#     TotalER = "Elite Outcome Chance"
#   )  %>%
#   tab_footnote(
#     footnote = "Based On Statitical Comps",
#     locations = cells_column_labels(
#       columns = vars(TotalER)))

all_players_2019_nz <- all_players_2019 %>%
  filter(`Elite Rate` > 0)

colfunc <- colorRampPalette(c("blue","white", "red"))
