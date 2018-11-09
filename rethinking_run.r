######################Model 2####################################################################
#########################################################################################
#### Women who got married before war or never married#############################################################################
##########################################################################################
library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "person_data.rds"
p <- readRDS(paste0(path, file))
# convert booleans to numeric
p$lotta<- as.numeric(p$lotta)
#make a single in 1945 category
p$single_in_45 <- ifelse(p$weddingyear>1945 , 1, 0)
p$never_married <- ifelse(is.na(p$spouse_id), 1, 0)
# replace NAs with 0's for unmarrieds in husband served and husband injured cats
p$servedduringwar_husband<- as.numeric(p$servedduringwar_husband)
p$servedduringwar_husband[p$single_in_45==1]<- 0
p$injuredinwar_husband<- as.numeric(p$injuredinwar_husband)
p$injuredinwar_husband[p$single_in_45==1]<- 0
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
                  "single_in_45")

p2<- p[complete.cases(p),]


#####################################################################################
## Rethinking Bayes code for model 2 #################################
##################################################################
data_list <- list(
  lotta  = p2$lotta,
  age = p2$age,
  sons = p2$sons,
  daughters = p2$daughters,
  agriculture = p2$agriculture,
  education = p2$education,
  served = p2$servedduringwar_husband,
  injured = p2$injuredinwar_husband,
  returnedkarelia =p2$returnedkarelia,
  outbred = p2$outbred2,
  single_in_45 = p2$single_in_45)

model <- map2stan(
  alist(
    lotta ~ dbinom (1,p),
    # Here is the model with all the predictors
    logit(p) <- a +
      ba*age +
      bs*sons +
      bd*daughters +
      bag*agriculture +
      bed*education +
      bserv*served+
      binj*injured+
      bo*outbred +
      brk*returnedkarelia +
      bsing*single_in_45,
    
    sigma ~ dcauchy(0,1),
    a ~ dnorm (0,10),
    # priors for all slopes (b terms) in main model
    c(ba,bs,bd,bag,bed,bserv,binj,bsing,bo,brk) ~ dnorm(0,1)
  ),
  data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(ba=0,bs=0,bd=0,bag=0,bed=0,bserv=0,binj=0,bsing=0,bo=0,brk=0), chains =4, cores=4)

path<- (paste0("results/"))
filename <- "model_2_new.rds"

saveRDS(model, paste0(path, filename))