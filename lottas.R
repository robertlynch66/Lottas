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


# birth year must be earlier than 1920 for Katiha table and select females
p <- p %>% filter (birthyear<1920 & sex==0)
p$age <- 1944-p$birthyear


# Filter out those who were childless in 1944
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p1 <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# get all childless women in 1940 and those who never gave birth 
#(adding kids =0 make this analysis more conservative)
p2 <- p1 %>% filter (first_child_yob>1940 | ( is.na(first_child_yob) & kids==0 ))


#  run the dredge code to predict lotta service and a seperate model to predict LRS
# select variables
brothers <- p2 %>% select("lotta","age","brothers","sisters","agriculture","education")

brothers<- brothers[complete.cases(brothers),]


#  Married and had kids after the war
################## MODEL RANK ####################DV's InAlt", "HostOut", "ParAlt###
options(na.action = "na.fail") 
#model<-glm(HostOut ~ M1F2+RFA96+RFA2006+RelDev+IntRB+ExtRB+Christian+Church+baptize, data=RL_Data, family = binomial)
model<-glm(lotta ~  age +
            education + agriculture  + brothers+sisters, data=brothers,
           family = binomial)
modelset<-dredge(model, rank = AICc, trace=FALSE) #subset = (!RelDev | !IntRB))# find out from Dylan how to exclude more correlated  variables from being entered into the same model
#modelset
summary(modelset)

##Provide an output path for AICc table - NOTE not sorted
#write.table(modelset,"C:/Users/rofrly/Desktop/AICc_Table.csv",sep=",")


################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")


###### Sons ########################################################################
# take home - having children of either sex before the war substantially reduces
#likelihood of volunteering
path <- "C:/Users/rofrly/Dropbox/Working papers/R data files/"
file2<- "person_data.rds"
p <- readRDS(paste0(path, file2))
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$lotta<- as.numeric(p$lotta)
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
p$offspring_sex_ratio <- p$sons/p$kids
p$age <- 1944-p$birthyear
# select cases where sons are of draft age (17 or over)
p1 <- p %>% filter (sex==0 & last_child_yob<1920)

p1 <- p %>% filter (sex==0 & first_child_yob<1940 & age < 40)
sons <- p1 %>% select("lotta","age","sons","daughters","agriculture","education")

sons<- sons[complete.cases(sons),]


options(na.action = "na.fail") 
#model<-glm(HostOut ~ M1F2+RFA96+RFA2006+RelDev+IntRB+ExtRB+Christian+Church+baptize, data=RL_Data, family = binomial)
model<-glm(lotta ~  age +
             education + agriculture  +sons + daughters, data=sons,
           family = binomial)
modelset<-dredge(model, rank = AICc, trace=FALSE) #subset = (!RelDev | !IntRB))# find out from Dylan how to exclude more correlated  variables from being entered into the same model
#modelset
summary(modelset)

##Provide an output path for AICc table - NOTE not sorted
#write.table(modelset,"C:/Users/rofrly/Desktop/AICc_Table.csv",sep=",")


################## AVERAGE MODELS WITHIN 2 AICC POINTS ##################
avgmodel<-model.avg(modelset, subset = delta < 2 )
summary(avgmodel)
summary(model)
topmodel<-get.models(modelset, subset = 1) [[1]]
summary(topmodel, type = "response")


#### Husbands #############################################################################
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
# make never_married category
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
# make husband served and husband injured 0 for unmarrieds
p$servedduringwar_husband_complete<- p$servedduringwar_husband
p$servedduringwar_husband_complete[p$never_married==1]<- 0

p$injuredinwar_husband_complete<- p$injuredinwar_husband
p$injuredinwar_husband_complete[p$never_married==1]<- 0