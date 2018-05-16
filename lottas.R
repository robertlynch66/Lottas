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
p1 <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# get all childless women in 1940 and those who never gave birth 
#(adding kids =0 make this analysis more conservative)
p2 <- p1 %>% filter (first_child_yob>1939 | ( is.na(first_child_yob) & kids==0 ))


#  run the dredge code to predict lotta service and a seperate model to predict LRS
# select variables
brothers <- p2 %>% select("lotta","age","brothers","sisters","agriculture","never_married","education")

brothers<- brothers[complete.cases(brothers),]


#  Married and had kids after the war
################## MODEL RANK ####################
options(na.action = "na.fail") 

model<-glm(lotta ~  age +
            education + agriculture  + brothers+sisters+never_married, data=brothers,
           family = binomial)
modelset<-dredge(model, rank = AICc, trace=FALSE) 
summary(modelset)


################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")



#### Women who got married before war or never married#############################################################################
# all lottas
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

p <- p %>% filter (sex==0)
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p1 <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# Women who married before 1945 or never married - exclude all the ones who got married after 1940
p2 <- p1 %>% filter (weddingyear<1940 |  never_married==1 )
p2 <- p2 %>% filter (age>18)
# select complete cases for models
model_1 <- p2 %>% select("lotta","age","sons","daughters","agriculture","brothers","sisters",
                         "education","servedduringwar_husband","injuredinwar_husband","never_married")

model_1<- model_1[complete.cases(model_1),]

options(na.action = "na.fail") 

model<-glm(lotta ~  age +sons +daughters+ agriculture+education+brothers+sisters+
             servedduringwar_husband+injuredinwar_husband+never_married, data=model_4,
           family = binomial)
modelset<-dredge(model, rank = AICc, trace=FALSE)
summary(modelset)




################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")


