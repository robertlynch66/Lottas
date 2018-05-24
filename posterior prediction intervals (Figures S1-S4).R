#load packages
library(dplyr)
library(rethinking)
library(tidybayes)
library(bayesplot)

# make original data data frames
path <- (paste0("~/Dropbox/Working papers/R data files/"))
# Or from desktop
path <- (paste0("C:/Users/rofrly/Dropbox/Working papers/R data files/"))
# get the file that the models were run on (old file)
file<- "person_data_old.rds"
m <- readRDS(paste0(path, file))
# the m2 is for the returned models
m2 <- m %>% filter(birthregion == "karelia" & primaryperson == 1)
# m is for the kids and marriage models
m <- m %>% filter(birthregion == "karelia" & primaryperson == 1 & birthyear<1939)

m$hypergamy <- m$social_class-m$social_class_spouse

m$log_pop <- log(m$birthpopulation)
m$log_pop <- m$log_pop-min(m$log_pop, na.rm=TRUE)
m$log_pop <- m$log_pop / max(m$log_pop, na.rm=TRUE)
m$log_fdf_pop <- log(m$fdf_population)
m$log_fdf_pop <- m$log_fdf_pop-min(m$log_fdf_pop, na.rm=TRUE)
m$log_fdf_pop <- m$log_fdf_pop / max(m$log_fdf_pop, na.rm=TRUE)

m$age <- 1970- m$birthyear
m$age <- m$age - min (m$age, na.rm =TRUE)
m$age <- m$age / max(m$age, na.rm = TRUE)
m$census_1950 <- as.factor(m$'1950_census')

m$technical<- ifelse(m$census_1950==0, 1, 0)
m$office<- ifelse(m$census_1950==1, 1, 0)
m$business<- ifelse(m$census_1950==2, 1, 0)
m$agricult<- ifelse(m$census_1950==3, 1, 0)
m$transport<- ifelse(m$census_1950==5, 1, 0)
m$factory<- ifelse(m$census_1950==6, 1, 0)
m$service<- ifelse(m$census_1950==8, 1, 0)
m$outbred <- ifelse(m$birthregion_spouse=="karelia" & !is.na(m$birthregion_spouse), 0, 
                    ifelse(m$birthregion_spouse=="other" & !is.na(m$birthregion_spouse), 1, NA))
# do the m2's
m2$log_pop <- log(m2$birthpopulation)
m2$log_pop <- m2$log_pop-min(m2$log_pop, na.rm=TRUE)
m2$log_pop <- m2$log_pop / max(m2$log_pop, na.rm=TRUE)
m2$log_fdf_pop <- log(m2$fdf_population)
m2$log_fdf_pop <- m2$log_fdf_pop-min(m2$log_fdf_pop, na.rm=TRUE)
m2$log_fdf_pop <- m2$log_fdf_pop / max(m2$log_fdf_pop, na.rm=TRUE)

m2$age <- 1970- m2$birthyear
m2$age <- m2$age - min (m2$age, na.rm =TRUE)
m2$age <- m2$age / max(m2$age, na.rm = TRUE)
m2$census_1950 <- as.factor(m2$'1950_census')

m2$agricult<- ifelse(m2$census_1950==3, 1, 0)

m2$factory<- ifelse(m2$census_1950==6, 1, 0)
m2$service<- ifelse(m2$census_1950==8, 1, 0)

m2$single <- ifelse(is.na(m2$spouse_id) & (m2$kids==0 | is.na(m2$kids)), 1, 0)
m2$marr_status <- ifelse(m2$birthregion_spouse=="karelia" & !is.na(m2$birthregion_spouse), 0,
                        ifelse(m2$birthregion_spouse=="other" & !is.na(m2$birthregion_spouse), 1, 0))
mrt <- m2 %>% select(birthplaceid,sex,age,education,log_pop,log_fdf_pop,agricult,
                     factory,service,returnedkarelia,marr_status,single)
print(nrow(mrt))
# should be 32277 cases -yes
mrt <- mrt [complete.cases(mrt),]
mrt <- mrt %>% arrange(birthplaceid)
mrt$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(mrt$birthplaceid)) !=0))

# split data
born_before_00 <- m %>% filter(!is.na(birthyear)) %>% filter(birthyear<1905)
wed_before_40 <- m %>% filter(!is.na(weddingyear)) %>% filter(weddingyear<1940)
mb <- unique(rbind(born_before_00, wed_before_40)) 

#married after
born_after_23 <- m %>% filter(!is.na(birthyear)) %>% filter(birthyear>1923)
wed_after_39 <- m %>% filter(!is.na(weddingyear)) %>% filter(weddingyear>1939)
ma <- unique(rbind(born_after_23, wed_after_39)) 

ma <- ma %>% select(birthplaceid,sex,outbred,sex,age,hypergamy,log_pop,log_fdf_pop,
                    agricult,
                     education,
                     returnedkarelia,transport)
#should be 8252 cases - yes
ma <- ma [complete.cases(ma),]
ma <- ma %>% arrange(birthplaceid)
ma$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(ma$birthplaceid)) !=0))

#before
mb <- mb %>% select(birthplaceid,outbred,log_pop,age,agricult,education,office,
                    technical,
                    hypergamy,returnedkarelia,sex)
#should be 6775 cases -yes
mb <- mb [complete.cases(mb),]
mb <- mb %>% arrange(birthplaceid)
mb$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(mb$birthplaceid)) !=0))

mk  <- m %>% select(kids,birthplaceid,sex,age,agricult,technical,office,business,
                   transport,factory,service,outbred,returnedkarelia,birthyear)
print(nrow(m))
# should be 41155 cases - yes
mk <- mk [complete.cases(mk),]
mk <- mk %>% arrange(birthplaceid)
mk$birthplace_id_seq <- cumsum(c(1,as.numeric(diff(mk$birthplaceid)) !=0))
# should be 41155


########### dataframes used for models are mk, mrt, ma and mb

# My pi values
my_PI <- function(x) {
  return(PI(x, prob=0.89))
}

#load models from desktop
marriage_before_war_dredge <- readRDS("C:/Users/rofrly/Dropbox/model results/marriage_before_war_dredge.rds")
marriage_after_war_dredge <- readRDS("C:/Users/rofrly/Dropbox/model results/marriage_after_war_dredge.rds")
Final_return <- readRDS("C:/Users/rofrly/Dropbox/model results/Final_return.rds")
Final_kids <- readRDS("C:/Users/rofrly/Dropbox/model results/Final_kids.rds")
# add simulations
simsmb<- sim(marriage_before_war_dredge)
simsma<- sim(marriage_after_war_dredge)
simsrt<- sim(Final_return)
simsk<- sim(Final_kids)

mu_mb <- apply(simsmb, 2, mean)
ob_b <- mb$outbred
mu_ma <- apply(simsma, 2, mean)
ob_ma <-ma$outbred
mu_rt <- apply(simsrt, 2, mean)
rt <- mrt$returnedkarelia
mu_k <- apply(simsk, 2, mean)
kids <- mk$kids



df1<- cbind(mu_mb,ob_b) %>% as.data.frame()
df2<- cbind(mu_ma,ob_ma) %>% as.data.frame()
df3 <- cbind(mu_rt,rt) %>% as.data.frame()
df4 <- cbind(mu_k,kids) %>% as.data.frame()

big_data <- as.data.frame(mapply(c,df1,df2,df3,df4))
big_data$condition <- c(rep(1,6775),rep(2,8252),
                        rep(3,32277),rep(4,41155))
                                                                                 
names(big_data)<- c("predicted","observed","model")
# first make data frame with all conditions

big_data1 <- big_data %>% filter (model==1)
big_data2 <- big_data %>% filter (model==2)
big_data3 <- big_data %>% filter (model==3)
big_data4 <- big_data %>% filter (model==4)

big_data1 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer() +
  stat_pointintervalh(aes(x=observed),  show.legend = FALSE,
                      position=position_nudge(y=-0.2))+
  scale_x_discrete(name="Predicted by model",limits=c(0,1), labels=c("No","Yes"))+
scale_y_discrete(name="",limits=c(1,2,3),breaks=c("1","2","3"),
labels=c("Married a resident Finn\n before the war","Married a resident Finn\n after the war",
"Returned to Karelia\n between the wars"))#+

#+
  # if you want to add the raw data
  #geom_point(aes(x=observed))

models_1_to_3 <- big_data1 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.58,y=1,yend=1, linetype='observations'), 
                      position=position_nudge(y=0.1))+
  geom_segment(aes(x=0.35, xend=1,y=2,yend=2), 
               position=position_nudge(y=0.1))+
  geom_segment(aes(x=0.16, xend=1,y=3,yend=3), 
               position=position_nudge(y=0.1))+
  
  geom_point(aes(x=0.194, y=1),position=position_nudge(y=0.1), colour="black",size=2)+
  geom_point(aes(x=0.77, y=2),position=position_nudge(y=0.1), colour="black",size=2)+
  geom_point(aes(x=0.64, y=3),position=position_nudge(y=0.1), colour="black",size=2)+
  scale_x_discrete(name="",limits=c(0,1), labels=c("No","Yes"))+
  scale_y_discrete(name="",limits=c(1,2,3),breaks=c("1","2","3"),
                   labels=c("Married a resident Finn\n before the war","Married a resident Finn\n after the war",
                            "Returned to Karelia\n between the wars"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        legend.title=element_text(size=12, face="bold"))
models_1_to_3

model_1 <- big_data1 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.58,y=1,yend=1, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
 
  
  geom_point(aes(x=0.194, y=1),position=position_nudge(y=0.1), colour="black",size=2)+
  
  scale_x_discrete(name="",limits=c(0,1), labels=c("No","Yes"))+
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c("Married a resident Finn\n before the war"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        legend.title=element_text(size=12, face="bold"))
model_1
model_2 <- big_data2 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0.35, xend=1,y=2,yend=2, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  geom_point(aes(x=0.77, y=2),position=position_nudge(y=0.1), colour="black",size=2)+

  scale_x_discrete(name="",limits=c(0,1), labels=c("No","Yes"))+
  scale_y_discrete(name="",limits=c(2),breaks=c("2"),
                   labels=c("Married a resident Finn\n after the war"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        legend.title=element_text(size=12, face="bold"))
model_2

model_3 <- big_data3 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0.16, xend=1,y=3,yend=3, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
 
  
  
  geom_point(aes(x=0.64, y=3),position=position_nudge(y=0.1), colour="black",size=2)+
  scale_x_discrete(name="",limits=c(0,1), labels=c("Remained\n in Finland","Returned\n to Karelia"))+
  scale_y_discrete(name="",limits=c(3),breaks=c("3"),
                   labels=c("Returned to Karelia\n between the wars"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.title.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        legend.title=element_text(size=12, face="bold"))
model_3

# kids model
model_4 <- big_data4 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction interval") +

  
  geom_segment(aes(x=0.68, xend=5,y=4,yend=4, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
 
  
  geom_point(aes(x=2.84, y=4),position=position_nudge(y=0.1), colour="black",size=2)+

  scale_x_continuous(name="Number of children",limits=c(0,7))+
  scale_y_discrete(name="",limits=c(4),breaks=c(4),labels=c("Number of children"))+ 
  #coord_cartesian(ylim=c(0.9,1.1))+
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold", hjust=0),
        axis.title.x=element_text(size=12, face="bold"),
        legend.title=element_text(size=12, face="bold"))
        #plot.margin=grid::unit(c(0,0,0,0), "mm"))
model_4 
# join plots
library(ggpubr)
  PPC<- ggarrange(model_1, model_2, model_3,model_4,
                                labels=c("",""),
                                vjust=2.5, hjust= -2,ncol=2, nrow=3, common.legend=TRUE)
PPC
  

  #  Posterior predictive check figure
PPC_final <-annotate_figure(PPC,
                  top = text_grob("Posterior Predictive check", color = "black",
                                  face = "bold", size = 14),
  fig.lab = "Figure 1", fig.lab.face = "bold")

# Or make them individually
model_1 <- big_data1 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0, xend=0.58,y=1,yend=1, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  
  
  geom_point(aes(x=0.194, y=1),position=position_nudge(y=0.1), colour="black",size=2)+
  coord_fixed(ratio=1)+
  scale_x_discrete(name="",limits=c(0,1), labels=c("No","Yes"))+
  scale_y_discrete(name="",limits=c(1),breaks=c("1"),
                   labels=c("Married a resident Finn\n before the war"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.title=element_text(size=12, face="bold"))
model_1


model_2 <- big_data2 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0.35, xend=1,y=2,yend=2, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  geom_point(aes(x=0.77, y=2),position=position_nudge(y=0.1), colour="black",size=2)+
  coord_fixed(ratio=1)+
  
  scale_x_discrete(name="",limits=c(0,1), labels=c("No","Yes"))+
  scale_y_discrete(name="",limits=c(2),breaks=c("2"),
                   labels=c("Married a resident Finn\n after the war"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.title=element_text(size=12, face="bold"))
model_2

model_3 <- big_data3 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction intervals") +
  geom_vline(xintercept=c(0,1), linetype="dotted") +
  
  geom_segment(aes(x=0.16, xend=1,y=3,yend=3, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  
  
  
  geom_point(aes(x=0.64, y=3),position=position_nudge(y=0.1), colour="black",size=2)+
  coord_fixed(ratio=1)+
  scale_x_discrete(name="",limits=c(0,1), labels=c("Remained\n in Finland","Returned\n to Karelia"))+
  scale_y_discrete(name="",limits=c(3),breaks=c(3),
                   labels=c("Returned to Karelia\n between the wars"))+ 
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.title=element_text(size=12, face="bold"))
model_3

# kids model
model_4 <- big_data4 %>% ggplot (aes(y= model)) +
  stat_intervalh(aes(x=predicted), .prob = c(.5,.8,.95),show.legend = NA) +
  scale_color_brewer(palette = "Greens",name="Prediction interval") +
  
  
  geom_segment(aes(x=0.68, xend=5,y=4,yend=4, linetype='observations\n mean and sd'), 
               position=position_nudge(y=0.1))+
  
  
  geom_point(aes(x=2.84, y=4),position=position_nudge(y=0.1), colour="black",size=2)+
  coord_fixed(ratio=3)+
  scale_x_continuous(name="Number of children",limits=c(0,7))+
  scale_y_discrete(name="",limits=c(4),breaks=c(4),labels=c("Number of children"))+ 
  #coord_cartesian(ylim=c(0.9,1.1))+
  #ggtitle("Posterior Predictive Check")+
  scale_linetype_manual("", values=c("observations\n mean and sd"=1))+
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.title=element_text(size=12, face="bold"))
#plot.margin=grid::unit(c(0,0,0,0), "mm"))
model_4 

theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
      axis.ticks.x = element_line(size = 1),
      axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
      axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
      axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))
