
###########################################################
##########----- The Temperature of Emotions -----##########
##########------------ Experiment 2B ------------##########
###########################################################


# Prepare workspace #
rm(list = ls())
opar = par()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



# Libraries #
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(rlang)
library(reshape2)
library(doBy)




########################
##### Prepare data #####
########################

# Read data
iatdata_raw <- read.csv("iatdata_raw_arousal.csv", header=TRUE)

### Filter data
# Retain combined blocks
# Delete trials with negative RTs
# Delete trials with RTs greater than 10,000 ms

iatdata_filt <- subset(iatdata_raw,
                      pstatus == "complete" &
                      attempt == 1 &
                        block %in% c(3,4,6,7) &
                        rt >= 0)

# Coalesce keys variables
iatdata_filt$keys <- with(iatdata_filt, ifelse(is.na(key1), key2, key1))

# Add block category
iatdata_filt <- mutate(iatdata_filt,
                      block_cat = if_else(block %in% c(3,6), "p",
                                          ifelse(block %in% c(4,7), "e","o")))

# Add congruency of blocks
iatdata_filt <- mutate(iatdata_filt,
                      congruency = if_else(block %in% c(3,4), "congruent",
                                           ifelse(block %in% c(6,7), "incongruent","error")))

# Convert Response Time to numeric
iatdata_filt$rt <- as.numeric(iatdata_filt$rt)



######################
##### Clean data #####
######################

### Delete participants for whom P10 RT was lower than 300 ms
# Identify participants
subs_rt300 <- iatdata_filt %>%
  group_by(pid) %>%
  summarise(
    p10_rt = quantile(rt, .10)) %>%
  ungroup() %>%
  filter(p10_rt < 300) %>%
  .$pid

# Filter out participants
iatdata_filt <- subset(iatdata_filt,
                       !pid %in% subs_rt300)



########################
##### Process Data #####
########################

# Compute means and standard deviation per block
iat_means <- iatdata_filt %>%
  filter(correct == 1) %>%
  group_by(pid, block) %>%
  summarise(
    n_rt_correct = n(),
    mean_rt = mean(rt),
    sd_rt = sd(rt)) %>%
  ungroup()


# Compute pooled standard deviations for practice and experimental trials
iat_pooled <- iatdata_filt %>%
  group_by(pid, block_cat) %>%
  summarise(
    sd_rt_pooled = sd(rt)) %>%
  ungroup()


# Replace error RTs
iat_merrors <- full_join(iatdata_filt, iat_means) %>%
  mutate(n_err = if_else(correct == 0, mean_rt + 2*sd_rt, rt)) %>%
  group_by(pid, block, sd_rt) %>%
  summarise(
    n_rt_all = n(),
    m_n_err = mean(n_err)
  ) %>%
  ungroup() %>%
  arrange()


# Get DFs with unique IDs
pooled <- iat_merrors %>% select(pid) %>% unique()
dd <- iat_merrors %>% select(pid) %>% unique()


# Split measures by blocks and merge data frames
for (i in c(3, 4, 6, 7)) {
  iat_b <- iat_merrors %>%
    filter(block == i) %>%
    select(-c(block))
  names(iat_b) <- c("pid", paste0("sd_rt_b", i), paste0("n_rt_all_b", i), paste0("m_n_err_b", i))
  dd <- full_join(dd, iat_b, by = c("pid"))
}

# Split pooled SDs
for (i in c("p", "e")) {
  iat_p <- iat_pooled %>%
    filter(block_cat == i) %>%
    select(-c(block_cat))
  names(iat_p) <- c("pid", paste0("sd_rt_pooled_", i))
  pooled <- full_join(pooled, iat_p, by = c("pid"))
}


# Merge data frames
dd_pooled <- full_join(dd, pooled, by = c("pid"))


# Compute D scores
iat_out <- dd_pooled %>%
  mutate(
    # D Practice trials
    D_p = (m_n_err_b6 - m_n_err_b3) / sd_rt_pooled_p,
    # D Experimental trials
    D_e = (m_n_err_b7 - m_n_err_b4) / sd_rt_pooled_e,
    # D practice and test
    D_overall = round((D_p + D_e) / 2, 3)) %>%
  select(pid, D3 = D_overall)





##############################
#####----- Analyses -----#####
##############################


###################
##### D Score #####
###################

# Test normality
library(ggpubr)

ggqqplot(iat_out$D3)
shapiro.test(iat_out$D3)

# Mean
mean(iat_out$D3)

# SD
sd(iat_out$D3)

### T-test ###
t.test(iat_out$D3, mu = 0, alternative = "two.sided")



##########################
##### Response times #####
##########################

# Extract correct responses
iatdata_correct <- subset(iatdata_filt, correct==1)

# Mean response times per participant and congruency condition
mean_rt_correct <- summaryBy(rt ~ pid + congruency, data=iatdata_correct, FUN = c(mean), keep.names = T)

# Split data frames by congruency conditions
rt_congruent <- subset(mean_rt_correct, congruency=="congruent")
rt_incongruent <- subset(mean_rt_correct, congruency=="incongruent")

# Response time Descriptive Stats
rt_stats <- ddply(mean_rt_correct, c("congruency"), summarise,
                  mean = mean(rt), sd = sd(rt),
                  sem = sd(rt)/sqrt(length(rt)))


# Test normality
ggqqplot(rt_incongruent$rt)
shapiro.test(rt_incongruent$rt)


### ATS ###

library(nparLD)

# Compute ATS
rt_ats <- with(mean_rt_correct, ld.f1(y=rt, time=congruency, subject=pid,
                                              time.name='congruency'))

# ATS ANOVA
rt_ats_anova <- rt_ats[["ANOVA.test"]]
rt_ats_anova


### Wilcoxon Signed Ranked Test ###

library(multcompView)

# Compute WSRT
rt_wsrt <- pairwise.wilcox.test(mean_rt_correct$rt, mean_rt_correct$congruency,
                                        p.adj = "holm", paired= TRUE)

# WSRT p-value
rt_wsrt_p <- as.data.frame(rt_wsrt[["p.value"]])
rt_wsrt_p


### Cliff's Delta ###

library(effsize)

#Compute CD
rt_cliff <- cliff.delta(rt_incongruent$rt, rt_congruent$rt, return.dm=TRUE)
rt_cliff



##########################
##### Response times #####
##########################

# Attempted trials
attempted <- summaryBy(attempt ~ pid + congruency, data=iatdata_filt, FUN = c(sum), keep.names = T)

# Correct trials
correct <- summaryBy(correct ~ pid + congruency, data=iatdata_filt, FUN = c(sum), keep.names = T)

# Compute error rate
error_rate <- merge(correct, attempted, by=c("pid", "congruency"))

error_rate <- mutate(error_rate, erate=1-(correct/attempt))

# Split data frames by congruency conditions
erate_congruent <- subset(error_rate, congruency=="congruent")
erate_incongruent <- subset(error_rate, congruency=="incongruent")

# Error rates Descriptive Stats
er_stats <- ddply(error_rate, c("congruency"), summarise,
                          mean = mean(erate), sd = sd(erate),
                          sem = sd(erate)/sqrt(length(erate)))


# Test normality
ggqqplot(erate_congruent$erate)
shapiro.test(erate_congruent$erate)


### ATS ###

# Compute ATS
er_ats <- with(error_rate, ld.f1(y=erate, time=congruency, subject=pid,
                                         time.name='congruency'))
# ATS ANOVA
er_ats_anova <- er_ats[["ANOVA.test"]]
er_ats_anova


### Wilcoxon Signed Ranked Test ###

# Compute WSRT
erate_wsrt <- pairwise.wilcox.test(error_rate$erate, error_rate$congruency,
                                           p.adj = "holm", paired= TRUE)
# WSRT p-value
erate_wsrt_p <- as.data.frame(erate_wsrt[["p.value"]])
erate_wsrt_p


### Cliff's Delta ###

# Compute CS
erate_cliff <- cliff.delta(erate_congruent$erate, erate_incongruent$erate, return.dm=TRUE)
erate_cliff





##########################
##### Response times #####
##########################

iatdata_demo <- read.csv("iatdata_demo_arousal.csv", header=TRUE)



### Descriptive statistics
mean(iatdata_demo$age)

sd(iatdata_demo$age)

min(iatdata_demo$age)
max(iatdata_demo$age)

table(iatdata_demo$gender)


