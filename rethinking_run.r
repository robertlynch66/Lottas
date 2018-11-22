library(dplyr)
library(rethinking)
# path to the folder with the R data files
path<- (paste0("~/r_files/"))
file<- "full_pop_data.rds.rds"
p <- readRDS(paste0(path, file))

# make total votes vars for weights
p$total_votes_16_gen <- p$trump_16+p$clinton_16
p$total_votes_16_08_gen <- p$trump_16+p$mccain_08

p <- data %>% select (state_id,trump_16, soc_cap_indx_14, suicides_16_scaled, drug_16_scaled,
                      pop_density_2014,median_hh_income,perc_white,perc_hisp,diversity_idx_2016,
                      total_votes_16_gen)

p<- p[complete.cases(p),]

# put state ids in format for STAN
p <- p %>% arrange(state_id)
p$state_id_seq <- cumsum(c(1,as.numeric(diff(p$state_id)) != 0))
#####################################################################################
## Rethinking Bayes code for model 2 #################################
##################################################################
#make dv integers
p$trump_16<- as.integer(p$trump_16)
p$total_votes_16_gen <- as.integer(p$total_votes_16_gen)


data_list <- list(
  trump_16 = p$trump_16,
  soc_cap_indx_14 = p$soc_cap_indx_14,
  suicides_16_scaled = p$suicides_16_scaled,
  drug_16_scaled = p$drug_16_scaled,
  pop_density_2014 = p$pop_density_2014,
  median_hh_income = p$median_hh_income,
  perc_white = p$perc_white,
  perc_hisp = p$perc_hisp,
  diversity_idx_2016 =p$diversity_idx_2016,
  total_votes_16_gen = p$total_votes_16_gen,
  state_id_seq = p$state_id_seq)





model <- map2stan(
  alist(
    # success ~ dbinom (total trials, p),
    trump_16 ~ dbinom(total_votes_16_gen, p),
    logit(p) <- a + a_state_id[state_id_seq]+
      bscap*soc_cap_indx_14 +
      bdrug*drug_16_scaled +
      bsuic*suicides_16_scaled +
      bdens*pop_density_2014 +
      bmedinc*median_hh_income +
      bwhite*perc_white +
      bhisp*perc_hisp +
      bdiversity*diversity_idx_2016,
    a ~ dnorm(0,1),
    a_state_id[state_id_seq] ~ dnorm(0, sigma),
    sigma ~ dcauchy(0,1),
    bscap ~ dnorm(0,1),
    bdrug ~ dnorm(0,1),
    bsuic ~ dnorm(0,1),
    bdens ~ dnorm(0,1),
    bmedinc ~ dnorm(0,1),
    bwhite ~ dnorm(0,1),
    bhisp ~ dnorm(0,1),
    bdiversity ~ dnorm(0,1)
  ), data=data_list, iter=8000, warmup=2000, control=list(max_treedepth=20),
  start=list(bscap=0,bdrug=0,bsuic=0,bdens=0,bmedinc=0,bwhite=0,bhisp=0,bdiversity=0), chains =4, cores=4)


path<- (paste0("results/"))
filename <- "Trump_16_model.rds"

saveRDS(model, paste0(path, filename))