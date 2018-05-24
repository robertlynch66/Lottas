#load packages
library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)

# make original data data frames
path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
# birth year must be earlier than 1920 for Katiha table and select females
p <- p %>% filter (birthyear<1920 & sex==0)
p$age <- 1944-p$birthyear

# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# get all childless women in 1940 and those who never gave birth 
#(adding kids =0 make this analysis more conservative)
p <- p %>% filter (first_child_yob>1939 | ( is.na(first_child_yob) & kids==0 ))


#  run the dredge code to predict lotta service and a seperate model to predict LRS
# select variables
p <- p %>% select("lotta","age","brothers","sisters","agriculture","never_married","education")

model_1<- p[complete.cases(p),]


#model 2
path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
# replace NAs with 0's for unmarrieds in husband served and husband injured cats
p$servedduringwar_husband[p$never_married==1]<- 0
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$injuredinwar_husband[p$never_married==1]<- 0
p$outbred2 <- ifelse(p$outbred==0 | is.na(p$outbred), 0, 1)
# add age in 1944
p$age <- 1944-p$birthyear
# kids over 18 by 1944
p$emancipated_kids <- ifelse(p$last_child_yob>1927 | is.na(p$last_child_yob),0,1)
p <- p %>% filter (sex==0)
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# Women who married before 1945 or never married - exclude all the ones who got married after 1940
p <- p %>% filter (weddingyear<1940 |  never_married==1 | first_child_yob<1940 )
p <- p %>% filter (age>18)
# select complete cases for models
p <- p %>% select("lotta","age","sons","daughters","agriculture","returnedkarelia","outbred2",
                  "education","servedduringwar_husband","injuredinwar_husband","never_married")

model_2<- p[complete.cases(p),]

# Model 3
path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$lotta<- as.numeric(p$lotta)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
# replace NAs with 0's for unmarrieds in husband served and husband injured cats
p$servedduringwar_husband[p$never_married==1]<- 0
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$injuredinwar_husband[p$never_married==1]<- 0
# add age in 1944
p$age <- 1944-p$birthyear
# kids over 18 by 1944
p$emancipated_kids <- ifelse(p$last_child_yob>1927 | is.na(p$last_child_yob),0,1)
p <- p %>% filter (sex==0)
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# Women who married before 1945 or never married - exclude all the ones who got married after 1940
p <- p %>% filter (weddingyear<1940 |  never_married==1 | first_child_yob<1940 )
p <- p %>% filter (age>18 & emancipated_kids==1)

p <- p %>% select("lotta","age","sons","daughters","agriculture",
                  "education","servedduringwar_husband","injuredinwar_husband","never_married")

model_3<- p[complete.cases(p),]

########### dataframes used for models are model_1, model_2 and model_3

# My pi values
my_PI <- function(x) {
  return(PI(x, prob=0.89))
}
library(rethinking)
#load models from desktop
mod_1 <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas/Models/Model_1.rds")
mod_2 <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas/Models/Model_2.rds")
mod_3 <- readRDS("C:/Users/rofrly/Dropbox/Github/Lottas/Models/Model_3.rds")

# get 1000 simulations for each row (observation) of the model
simsmod_1<- sim(mod_1)
simsmod_2<- sim(mod_2)
simsmod_3<- sim(mod_3)

# get the mean of the simulations for each row/ observation
mu_m1 <- apply(simsmod_1, 2, mean)
# get the actual outcome (lotta) from the original dataframe used to construct the model
lm1 <- model_1$lotta
# get the mean of the simulations for each row/ observation
mu_m2 <- apply(simsmod_2, 2, mean)
# get the actual outcome (lotta) from the original dataframe used to construct the model
lm2 <-model_2$lotta
# get the mean of the simulations for each row/ observation
mu_m3 <- apply(simsmod_3, 2, mean)
# get the actual outcome (lotta) from the original dataframe used to construct the model
lm3 <- model_3$lotta



# combine the real observations with the model predicted simulations into a single data frame
df1<- cbind(mu_m1,lm1) %>% as.data.frame()
df2<- cbind(mu_m2,lm2) %>% as.data.frame()
df3 <- cbind(mu_m3,lm3) %>% as.data.frame()


big_data <- as.data.frame(mapply(c,df1,df2,df3))
big_data$condition <- c(rep(1,5784),rep(2,23191),
                        rep(3,2085))
                                                                                 
names(big_data)<- c("predicted","observed","model")
# first make data frame with all conditions

big_data1 <- big_data %>% filter (model==1)
big_data2 <- big_data %>% filter (model==2)
big_data3 <- big_data %>% filter (model==3)


#get means and sd of observed for each model- put in geom segment and geom point below
mean(big_data1$observed)
sd(big_data1$observed)

m1 <- big_data1 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.604,y=1,yend=1, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
 
  
  geom_point(aes(x=0.202, y=1),position=position_nudge(y=0.1), colour="black",size=2)+
  
  scale_x_discrete(name="",limits=c(0,1), labels=c("",""))+
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c("Model 1\nMarried before the war"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        legend.title=element_text(size=12, face="bold"))
m1

#get means and sd of observed for each model- put in geom segment and geom point below
mean(big_data2$observed)
sd(big_data2$observed)

m2 <- big_data2 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.435,y=2,yend=2, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  geom_point(aes(x=0.115, y=2),position=position_nudge(y=0.1), colour="black",size=2)+

  scale_x_discrete(name="",limits=c(0,1), labels=c("",""))+
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_discrete(name="",limits=c(2),breaks=c("2"),
                   labels=c("Model 2\nMarried after the war"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        legend.title=element_text(size=12, face="bold"))
m2


#get means and sd of observed for each model- put in geom segment and geom point below
mean(big_data3$observed)
sd(big_data3$observed)

m3 <- big_data3 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.33,y=3,yend=3, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
 

  
  geom_point(aes(x=0.073, y=3),position=position_nudge(y=0.1), colour="black",size=2)+
  scale_x_discrete(name="",limits=c(0,1), labels=c("Not a Lotta","Lotta"))+
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_discrete(name="",limits=c(3),breaks=c("3"),
                   labels=c("Model 3\nAll children over age 17\nby 1944"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        legend.title=element_text(size=12, face="bold"))
m3


#######Extend x axis####

# join plots
library(ggpubr)
  PPC<- ggarrange(m1, m2, m3,nrow=3,ncol=1,
                                labels=c("",""),
                                 common.legend=TRUE)
PPC
  

  #  Posterior predictive check figure
PPC_final <-annotate_figure(PPC,
                  top = text_grob("Posterior Predictive check", color = "black",
                                  face = "bold", size = 14),
  fig.lab = "Figure 1", fig.lab.face = "bold")



