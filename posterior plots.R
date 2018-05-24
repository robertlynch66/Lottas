model_1 <- lottas_unmarried_and_childless_in_1940_new2
model_2 <- Model
model_3 <- lottas_emancipated_kids
# load packages
library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)

# get posteriors and select columns
post_model1 <- extract.samples(model_1) %>% as.data.frame()
post_model1 <- post_model1 %>% select (1:6,8)
post_model1 <- post_model1 [c(1,4,5,6,2,3)]

post_model2 <- extract.samples(model_2) %>% as.data.frame()
post_model2 <- post_model2 [c(1,4,5,10,2,3,8,9,6,7)]


post_model3 <- extract.samples(model_3) %>% as.data.frame()
post_model3 <- post_model3 [c(1,4,5,8,2,3,6,7)]



# make figures of posteriors
color_scheme_set("purple")
p1 <- mcmc_areas(post_model1,prob = 0.8, prob_outer = 1) 
color_scheme_set("green")
p2 <- mcmc_areas(post_model2,prob = 0.8, prob_outer = 1)
color_scheme_set("blue")
p3 <- mcmc_areas(post_model3,prob = 0.8, prob_outer = 1)


# make titles and plot graphics 
plot_1<- p1 + scale_y_discrete(limits=c("Brothers",
                                        "Sisters","Never married",
                                              "Educated",
                                              "Agriculture","Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-0.5,1.2), labels=c("0.75","1","1.5","2", "3"),
                     breaks=c(-0.29,0,0.41, 0.7, 1.1)) +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))

plot_1
plot_2<- p2 + scale_y_discrete(limits=c("Returned to \nKarelia","Married a \nresident Finn","Husband injured \nin war",
                                        "Husband served \nin military",
                                        "Daughters","Sons",
                                        "Never married",
                                        "Educated",
                                        "Agriculture","Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-0.7,1.62), labels=c("0.5","1","1.5","2", "3","5"),
                     breaks=c(-0.69,0,0.41, 0.7, 1.1,1.61)) +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))
plot_2

# Figure 3
plot_3<- p3 +  scale_y_discrete(limits=c("Husband injured \nin War","Husband served \nin Military",
                                         "Daughters","Sons",
                                         "Never married",
                                         "Educated",
                                         "Agriculture","Age")) +
  scale_x_continuous(name="Odds ratio",limits=c(-2,2.8), labels=c("0.12","0.25","0.5","1","1.5","2", "3","5","9.5"),
                     breaks=c(-1.98,-1.39,-0.69,0,0.41, 0.7, 1.1,1.61,2.25)) +
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))
plot_3
# Figure 4
plot_4<- p4 + scale_y_discrete(limits=c("Returned to Karelia \nX Age","Married out \nX Age",
                                       "Returned to Karelia",
                                       "Married out","Service",
                                       "Factory","Transportation","Business",
                                       "Office",
                                       "Agriculture",
                                       "Age","Sex")) +
  scale_x_continuous(name="Number of children",limits=c(-0.75,0.55), labels=c("1.4","1.8","2.3","3.0",
                                                                              "3.8"),
                     breaks=c(-0.5, -0.25,0, 0.25, 0.5)) +
 
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))
plot_4
# sup materials graph
plot_5 <- p + scale_y_discrete(limits=c("Married out",
                                        "Returned to Karelia",
                                        "Agriculture",
                                        "Factory","Business",
                                        "Office",
                                        "Technical",
                                        "Age","Sex")) +
  scale_x_continuous(name="Number of children",limits=c(-0.75,0.55), labels=c("1.4","1.8","2.3","3.0",
                                                                              "3.8"),
                     breaks=c(-0.5, -0.25,0, 0.25, 0.5)) +
  
  theme(axis.text.x = element_text(colour="black",size=10,angle=0,face="plain"),
        axis.ticks.x = element_line(size = 1),
        axis.text.y = element_text(colour="grey8",size=10,angle=0,hjust=0,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=11,angle=90,hjust=.5,vjust=.5,face="plain"))
plot_5
