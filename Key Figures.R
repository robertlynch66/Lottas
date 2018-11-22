### HDI figures 1 amd 2
husband_served_model <- readRDS("Models/Model_2.rds")
brothers_model <- readRDS("Models/Model_1.rds")
all_emancipated_kids <- readRDS("Models/Model_3.rds")
#new model with smaller sample
model_2_new <- readRDS("Models/Model_2_new.rds")

library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)

# get posteriors and select columns
post_model1 <- extract.samples(brothers_model) %>% as.data.frame()
post_model1 <- post_model1 %>% select (1:5)
post_model1 <- post_model1 [c(1,4,5,2,3)]

post_model2 <- extract.samples(new_husband_served_model) %>% as.data.frame()
post_model2 <- post_model2 [c(1,8,4,5,2,3,6,7)]


post_model3 <- extract.samples(all_emancipated_kids) %>% as.data.frame()
post_model3 <- post_model3 [c(1,4,5,2,3,6)]


#rename the variables
post_model1 <- post_model1 %>% rename(Age = ba, Agriculture= bag, Educated=bed, Sisters=bs, Brothers=bb)
post_model2 <- post_model2 %>% rename(Age = ba, Single = bsing, Agriculture= bag, Educated=bed, Sons=bs, Daughters=bd,
                                      "Husband served" = bserv, "Husband injured" = binj)

post_model3 <- post_model3 %>% rename(Age = ba,  Agriculture= bag, Educated=bed, Sons=bs, Daughters=bd,
                                      "Husband served" = bserv)
# make figures of posteriors
color_scheme_set("blue")
p1 <- mcmc_areas(post_model1,prob = 0.8, prob_outer = 1)
color_scheme_set("blue")
p2 <- mcmc_areas(post_model2,prob = 0.8, prob_outer = 1)
color_scheme_set("blue")
p3 <- mcmc_areas(post_model3,prob = 0.8, prob_outer = 1)



# make titles and plot graphics 


p1 <- p1 +
  scale_x_continuous(name="Odds ratio",limits=c(-0.5,1.1), labels=c("0.75","1","1.5","2", "3"),
                     breaks=c(-0.29,0,0.41, 0.7, 1.1)) +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="bold"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="black",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="bold"))

p1

ggsave(p1, filename = "Figure 1.png", width = 4, height = 4, device = "png", dpi = 600,units = "in")


p2<- p2 + scale_y_discrete(limits=c("Husband injured",
                                        "Husband served",
                                        "Daughters","Sons",
                                        
                                        "Educated",
                                        "Agriculture","Single","Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-0.7,1.62), labels=c("0.5","1","1.5","2", "3","5"),
                     breaks=c(-0.69,0,0.41, 0.7, 1.1,1.61)) +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="bold"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="bold"))
p2
ggsave(p2, filename = "Figure 2.png", width = 4, height = 6, device = "png", dpi = 600,units = "in")

# Figure 3
p3<- p3 +  scale_y_discrete(limits=c("Husband served",
                                         "Daughters","Sons",
                                         "Educated",
                                         "Agriculture","Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-1.2,2.5), labels=c("0.5","1","1.5","2", "3","5","9.5"),
                     breaks=c(-0.69,0,0.41, 0.7, 1.1,1.61,2.25)) +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="bold"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="bold"))
p3
ggsave(p3, filename = "Figure S1.png", width = 4, height = 6, device = "png", dpi = 600,units = "in")


library(dplyr)
library(rethinking)

library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
################################################################################################################
######################Model 1####################################################################
################################################################################################
################Lottas who never married or were childless or single in 1939###########
######################################################################################

# go up a directory and read in from the data files folder

p <- readRDS("../data files/person_data_old.rds")
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$martta<- as.numeric(p$martta)
p$lotta<- as.numeric(p$lotta)
#p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
# birth year must be earlier than 1920 for Katiha table and select females
p <- p %>% filter (birthyear<1920 & sex==0)
p$age <- 1944-p$birthyear
p$age <-1940-p$birthyear

# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))

# get all childless women in 1940 and those who never gave birth 
#(adding kids =0 make this analysis more conservative)
p <- p %>% filter (first_child_yob>1939 | ( is.na(first_child_yob) & kids==0 ))


#  run the dredge code to predict lotta service and a seperate model to predict LRS
# select variables
p <- p %>% select("lotta","age","agriculture","education","brothers","sisters")

p1<- p[complete.cases(p),]


p <- readRDS("../data files/person_data_old.rds")
library(dplyr)
library("lme4")
library("MuMIn")
library("lsmeans")
# convert booleans to numeric
p$lotta<- as.numeric(p$lotta)
#make a single in 1945 category
p$single_in_45 <- ifelse(p$weddingyear>1945 |is.na(p$spouse_id) , 1, 0)


p$never_married <- ifelse(is.na(p$spouse_id),1,0)
# replace NAs with 0's for unmarrieds in husband served and husband injured cats
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
p$servedduringwar_husband[p$never_married==1]<- 0
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$injuredinwar_husband[p$never_married==1]<- 0
p$outbred2 <- ifelse(p$outbred==0 | is.na(p$outbred), 0, 1)
# add age in 1944
p$age <- 1940-p$birthyear
# kids over 18 by 1944
p$emancipated_kids <- ifelse(p$last_child_yob>1927 | is.na(p$last_child_yob),0,1)
p <- p %>% filter (sex==0)
# filter age at first birth between 13 and 50 - i.e. get rid of rows with impossible values
p <- p %>% filter (age_at_first_birth > 12 & age_at_first_birth < 51 | is.na(age_at_first_birth))


# Women who married before 1940 or never married - exclude all the ones who got married after 1940
p <- p %>% filter (weddingyear<1940 |  single_in_45==1 | first_child_yob<1940)# | single_in_45==1)
p <- p %>% filter (age>18)
# select complete cases for models
p <- p %>% select("lotta","age","sons","daughters","agriculture","returnedkarelia","outbred2",
                  "education","servedduringwar_husband","injuredinwar_husband",
                  "single_in_45","never_married")

p2<- p[complete.cases(p),]
## Fig 3####################################

# read in models
# husband served model for married women
husband_served_model <- readRDS("Models/Model_2.rds")
brothers_model <- readRDS("Models/Model_1.rds")
all_emancipated_kids <- readRDS("Models/Model_3.rds")
#new model with smaller sample
new_husband_served_model <- readRDS("Models/Model_2_new.rds")


##### START HERE for figures 3 and 4
### Brothers figure
attach(p1)
bros <- tidyr::crossing(
  age = mean(age),
  brothers = c(0L,1L,2L,3L,4L,5L),
  education=mean(education),
  never_married=0L,
  agriculture = mean(agriculture),
  sisters = mean(sisters)) %>%
  as.data.frame()
detach(p1)
library(tidybayes.rethinking)

df_bros <- tidy_link(bros, model_1) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for lotta likelihood
# make raw data frame for all siblings


std <- function(x) sd(x)/sqrt(length(x))
# subset to only through 5 brothers
#rawdata_sibs <- p1[which(p1$brothers<6 & p1$sisters<6),]
#data <- data[which(data$lambda >min(int_2) & data$lambda < max(int_2)), ]
rawdata_bros <- aggregate(lotta ~ brothers, data=p1, FUN= function(x) c(mean_lot=mean(x),sd_lot=sd(x),
se_lot=std(x))) 

z <- unlist(rawdata_bros$lotta)

rawdata_bros <- cbind(rawdata_bros,z)
rawdata_bros$lotta <-  NULL

#rawdata_bros[6,2] <- 0.2231
data <- df_bros%>% left_join (rawdata_bros, by =c("brothers"="brothers"))
data$sibcat <- "brothers"
data$siblings <- data$brothers

data$brothers<-NULL
data$sisters<-NULL
# repeat for sisters
attach(p1)
sists <- tidyr::crossing(
  age = mean(age),
  sisters = c(0L,1L,2L,3L,4L,5L),
  education=mean(education),
  never_married=0L,
  agriculture = mean(agriculture),
  brothers = 2.46) %>%
  as.data.frame()
detach(p1)
library(tidybayes.rethinking)

df_sists <- tidy_link(sists, model_1) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for lotta likelihood
# make raw data frame for all siblings


std <- function(x) sd(x)/sqrt(length(x))
# subset to only through 5 brothers
#rawdata_sibs <- p1[which(p1$brothers<6 & p1$sisters<6),]
#data <- data[which(data$lambda >min(int_2) & data$lambda < max(int_2)), ]
rawdata_sists <- aggregate(lotta ~ sisters, data=p1, FUN= function(x) c(mean_lot=mean(x),sd_lot=sd(x),
                                                                        se_lot=std(x))) 

z <- unlist(rawdata_sists$lotta)

rawdata_sists <- cbind(rawdata_sists,z)
rawdata_sists$lotta <-  NULL

data2 <- df_sists%>% left_join (rawdata_sists, by =c("sisters"="sisters"))
data2$siblings <- data2$sisters
data2$sibcat <- "sisters"
data2$brothers<-NULL
data2$sisters<-NULL
# stack the brothers and sister data frames
data_all_sibs <- rbind(data, data2)


## set up color vector
cols <- c("brothers" = "black", "sisters" = "grey")
sibs_fig <- ggplot(data=data_all_sibs,aes(x=siblings,y=p)) +
  stat_lineribbon(aes(colour=factor(sibcat)),
                      .width = c(.8, .5),show.legend = TRUE,geom="lineribbon") +
  scale_fill_brewer(name="Model Predictions\n Credibility Intervals",
                    palette="Greens")+
  #scale_colour_brewer(name="Credibility interval",
    #                palette="Greens")+
  
  geom_errorbar(position=position_dodge(width=0.3),size=0.6,width=0.3,
                aes(group=factor(sibcat),color=factor(sibcat),
x=siblings,ymin=(mean_lot-se_lot), ymax=(mean_lot+se_lot))) +
  

  #scale_colour_brewer(name="Credibility interval",
       #             palette="Blues")
  
  geom_point(position=position_dodge(width=0.3),
             alpha=1,size=2,aes(group=factor(sibcat),color=factor(sibcat) ,
      x=siblings,y=mean_lot))+
  scale_shape_identity()+
  
 
scale_colour_manual(
  name="Siblings",
  values = cols,
  breaks = c("brothers", "sisters"),
  labels = c("brothers\n(observed) SE",
             "sisters\n(observed) SE")
)    +

  scale_x_continuous(breaks=c(0,1,2,3,4,5),labels=c("0","1","2","3", "4",
                                                                ">5")) +
  scale_y_continuous(limits=c(0.16,0.245),breaks=c(0.18,0.21,0.24),labels=c("18%","21%","24%")) +
  xlab("Brothers or sisters with mean\nnumber of opposite sex siblings") + ylab("Probability of volunteering") + 
  #ggtitle("Unmarried women with more brothers\nare more likely to volunteer") + 
  theme(plot.title = element_text(hjust = 0.5, size=10,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=11,angle=0,hjust=.5,vjust=0,face="bold"),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10),
        axis.title.y = element_text(colour="black",size=11,angle=90,hjust=.5,vjust=.5,face="bold"))
  sibs_fig
  
ggsave(sibs_fig, filename = "Figure 3.png", width = 6, height = 4, device = "png", dpi = 600,units = "in")
#########################

  
#Figure 4 Violin ploys for Husband served  
  
  
  
  
attach(p2)
husband_served <- tidyr::crossing(
  age = mean(age),
  sons = mean(sons),
  daughters = mean(daughters),
  agriculture = mean(agriculture),
  education=mean(education),
  single_in_45 = mean(single_in_45),
  injured = mean(injuredinwar_husband),
  served = c(0L,1L),
  outbred = mean(outbred2),
  returnedkarelia = mean(returnedkarelia)) %>%
  as.data.frame()
detach(p2)
library(tidybayes.rethinking)

df_husband <- tidy_link(husband_served, model_2_new) %>% as.data.frame()

library(plotrix)
# make new dataframe with raw data for husband served
# # make standard error function


std <- function(x) sd(x)/sqrt(length(x))
rawdata_husband <- aggregate(lotta ~ servedduringwar_husband, data=p2, FUN= function(x) c(mean_lot=mean(x),sd_lot=sd(x),
                                                                                                   se_lot=std(x))) 

z <- unlist(rawdata_husband$lotta)

rawdata_husband <- cbind(rawdata_husband,z)
rawdata_husband$lotta <-  NULL



data <- df_husband %>% left_join (rawdata_husband, by =c("served"="servedduringwar_husband"))



# Violin plots

int_2 <- quantile(data$p, c(0.1,0.9))
library(HDInterval)
#int_2<- hdi(data$p,credMass = 0.95)
# yields a range of 2.79 to 3.13
# subset data
data <- data[which(data$p >min(int_2) & data$p < max(int_2)), ]


#males returned/ no rt, females tr/no rt, males inbred, females inbred

scaleFUN <- function(x) sprintf("%.2f", x)





fig4 <- ggplot(data = data, aes(x = factor(served), y = p, position="dodge")) +
  geom_violin( fill="#B47846",alpha=0.66)+
  geom_boxplot(width=.1,fill='#A4A4A4', col="darkred") +
  geom_errorbar(size=0.6,width=0.3,aes(x=factor(served),
                                       ymin=(mean_lot-se_lot),
                                       ymax=(mean_lot+se_lot))) +
  geom_point(alpha=1,size=1,aes(x = factor(served), y = mean_lot)) +
  scale_shape_identity() +


 scale_x_discrete(breaks=c("0","1"),limits = c("0","skip","1"),labels=c("Did not serve",
                                                                                    "Served")) +
  scale_y_continuous(name="",breaks=c(0.08,0.10,0.12,0.14,0.16,0.18),
                     limits = c(0.08,0.18),
                     labels=c("","10%","12%","14%","16%","")) +
  
  
  
  xlab("Husband served in war") + 
  
  scale_y_continuous(name="Probability of volunteering",labels=scaleFUN)+
  #ggtitle("Women with husbands who serve in\nthe military are more likely to volunteer") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, "in")) +
  theme(plot.title = element_text(hjust = 0.5, size=10,face="bold"),
        axis.text.x = element_text(colour="grey20",size=10,angle=0,face="bold"),
        axis.text.y = element_text(colour="grey20",size=10,angle=0,hjust=0,vjust=0,face="bold"),  
        axis.title.x = element_text(colour="black",size=11,angle=0,hjust=.5,vjust=0,face="bold"),
        axis.title.y = element_text(colour="black",size=11,angle=90,hjust=.5,vjust=.5,face="bold"))  



fig4




ggsave(fig4, filename = "Figure 4.png", width = 4, height = 4, device = "png", dpi = 600,units = "in")

### Supp Pos pred check figure 2a-c
#load packages
library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)

# make original data data frames

p <- readRDS("../data files/person_data_old.rds")
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
p <- readRDS("../data files/person_data_old.rds")
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
p <- readRDS("../data files/person_data_old.rds")
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
  stat_intervalh(aes(x=predicted), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.604,y=1,yend=1, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  
  
  geom_point(aes(x=0.202, y=1),position=position_nudge(y=0.1), colour="black",size=2)+
  
  scale_x_discrete(name="",limits=c(0,1), labels=c("Not a Lotta","Lotta"))+
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c("The impact\n of brothers"))+ 
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
  stat_intervalh(aes(x=predicted), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.435,y=2,yend=2, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  geom_point(aes(x=0.115, y=2),position=position_nudge(y=0.1), colour="black",size=2)+
  
  scale_x_discrete(name="",limits=c(0,1), labels=c("Not a Lotta","Lotta"))+
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_discrete(name="",limits=c(2),breaks=c("2"),
                   labels=c("The impact of\nhaving a husband who\nserved in military"))+ 
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
  stat_intervalh(aes(x=predicted), .width = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.33,y=3,yend=3, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  
  
  
  geom_point(aes(x=0.073, y=3),position=position_nudge(y=0.1), colour="black",size=2)+
  scale_x_discrete(name="",limits=c(0,1), labels=c("Not a Lotta","Lotta"))+
  coord_cartesian(xlim = c(0, 1)) +
  scale_y_discrete(name="",limits=c(3),breaks=c("3"),
                   labels=c("All children\nover age 17\nby 1944"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        legend.title=element_text(size=12, face="bold"))
m3

library(ggpubr)
######### panel plots for supp figure S3a-c and S4a-c
# make panel plots for reproduction before and after war fig 2a 
panel_plot_s3 <- ggarrange(m1,m2,m3, labels=c("", 
                                              ""),
                           vjust=2.5, hjust= -2,ncol=3, nrow=1, common.legend=TRUE)
figureS3 <- annotate_figure(panel_plot_s3,
                            top = text_grob("Posterior predictive checks for models Factors affecting Intermarriage", color = "black", face = "bold", size = 14),
                            fig.lab = "", fig.lab.face = "bold"
)
ggsave(figureS3, filename = "Figure S2a-c.png", width = 14, height = 4, device = "png", dpi = 600,units = "in")
