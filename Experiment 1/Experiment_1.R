###########################################################
##########----- The Temperature of Emotions -----##########
##########------------- Experiment 1 ------------##########
###########################################################



# Prepare workspace #
rm(list = ls())
opar = par()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Libraries #
library(dplyr)
library(doBy)
library(tidyr)
library(data.table)




########################
##### Prepare data #####
########################

# Read data 
dte_exp1 <- read.csv("dte_exp1_v4.csv", header=TRUE)


# Change order of emotions starting from second quadrant in Jaeger et al. (2020)
dte_exp1$emotion <- factor(dte_exp1$emotion, 
                           levels = c("Tense, Bothered",
                                      "Jittery, Nervous", "Active, Alert", "Energetic, Excited",
                                      "Enthusiastic, Inspired", "Happy, Satisfied", "Secure, At ease",
                                      "Relaxed, Calm", "Passive, Quiet","Dull, Bored", "Blue, Uninspired", "Unhappy, Dissatisfied"))
levels(dte_exp1$emotion)




######################
##### Clean data #####
######################

# Filter data by duration
d_duration <- dplyr::select(dte_exp1, c(rid, duration))
d_duration <- distinct(d_duration, rid, .keep_all = TRUE)

mean_duration <- mean(d_duration$duration)
mean_duration

sd_duration <- sd(d_duration$duration)
sd_duration


# Remove observations above and below 2SD from the mean duration
d_duration <- subset(d_duration,
                     d_duration$duration >= mean_duration - 2*sd_duration &
                       d_duration$duration <= mean_duration + 2*(sd_duration))

d_duration <- select(d_duration, -c(duration))

dte_exp1 <- merge(d_duration, dte_exp1, by.x="rid", by.y="rid")


# Retain participants who completed the questionnaire in their native language
dte_exp1$UserLanguage <- as.character(dte_exp1$UserLanguage)

dte_filt <- subset(dte_exp1,
                   firstlang == UserLanguage)




###############################################
##### Participants descriptive Statistics #####
###############################################

# Number of data points per language
count_userlang <- plyr::count(dte_filt, 'UserLanguage')
count_userlang

# Summary statistics per Language group and country
count_UserLang <- plyr::count(dte_filt, 'UserLanguage')
count_country <- plyr::count(dte_filt, 'country')
table_lang_country <- table(dte_filt$country, dte_filt$UserLanguage)

count_UserLang
count_country
table_lang_country/60


# Age summary statistics
summ_stats_age <- summaryBy(age ~ UserLanguage + country, data=dte_filt,
                            FUN = c(length, min, max, mean, sd), keep.names = T)

summ_stats_age_language <- summaryBy(age ~ UserLanguage, data=dte_filt,
                                     FUN = c(length, min, max, mean, sd), keep.names = T)

summ_stats_age_gender <- summaryBy(age ~ gender, data=dte_filt,
                                   FUN = c(length, min, max, mean, sd), keep.names = T)

mean(dte_filt$age)
sd(dte_filt$age)




##############################################
##### Temperature descriptive Statistics #####
##############################################

stats_temp_all <- summaryBy(response ~ temp_cat + UserLanguage + emotion, data=dte_filt,
                            FUN = c(length, mean, sd), keep.names = T)

stats_temp_all$response.se <- stats_temp_all$response.sd / sqrt(stats_temp_all$response.length)
stats_temp_all <- mutate(stats_temp_all, lower=response.mean-2*response.se, upper=response.mean+2*response.se)


### Overall
stats_temp_overall <- summaryBy(response ~ temp_cat + emotion, data=dte_filt,
                                FUN = c(length, mean, sd), keep.names = T)
stats_temp_overall$response.se <- stats_temp_overall$response.sd / sqrt(stats_temp_overall$response.length)
stats_temp_overall <- mutate(stats_temp_overall, lower=response.mean-2*response.se, upper=response.mean+2*response.se)





##############################
#####----- Analyses -----#####
##############################


########################################
##### Principal component analysis #####
########################################

library(factoextra)
library(FactoMineR)
library(cluster)


### Prepare data ###

# Summary stats
SUMM <- summaryBy(response ~ temp_cat + emotion + UserLanguage, data=dte_filt,
                  FUN = c(mean), keep.names = T)

# Convert to wide
emo_pca <- spread(SUMM, temp_cat, response)

# Add interaction emotion x language
emo_pca$emo_lang <- interaction(emo_pca$emotion, emo_pca$UserLanguage)


# Reorder variables
emo_pca <- emo_pca[,c(3:7,1:2,8)]

# Add unique names to rows
rownames(emo_pca) <- emo_pca[,8]



old_vars <- c('temp0','temp10','temp20','temp30','temp40')
new_vars <- c('0°C','10°C','20°C','30°C','40°C')
setnames(emo_pca, old = old_vars, new = new_vars)



### PCA ###

# PCA Summary
res.pca <- PCA(emo_pca, scale.unit=TRUE, ncp=2, quali.sup=6:8, graph=TRUE) 
summary(res.pca)

# PCA Plots
barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(res.pca, col.var = "black")

# Biplot
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "black",
                habillage = "emotion")

res.pca <- prcomp(emo_pca[, 1:5], scale=TRUE)
res.pca





#####################################################
##### Associations similarity across languages #####
#####################################################

library(CCA)


# Association matrices per language
for (lang in c("English", "Spanish", "Chinese", "Japanese")) {
  m_lang <- subset(stats_temp_all, UserLanguage == lang)
  m_lang <- select(m_lang, temp_cat, emotion, response.mean)
  m_lang <- spread(m_lang, temp_cat, response.mean)
  rownames(m_lang) <- m_lang[,1]
  m_lang <- select(m_lang, -c(emotion))
  assign(paste0("m_", lang), data.frame(m_lang))
}


# Correlation tests
cor_en_sp <- cor.test(c(as.matrix(m_English)), c(as.matrix(m_Spanish)))   # English - Spanish
cor_en_jp <- cor.test(c(as.matrix(m_English)), c(as.matrix(m_Japanese)))  # English - Japanese
cor_en_cn <- cor.test(c(as.matrix(m_English)), c(as.matrix(m_Chinese)))   # English - Chinese

cor_sp_jp <- cor.test(c(as.matrix(m_Spanish)), c(as.matrix(m_Japanese)))  # Spanish - Japanese
cor_sp_cn <- cor.test(c(as.matrix(m_Spanish)), c(as.matrix(m_Chinese)))   # Spanish - Chinese

cor_jp_cn <- cor.test(c(as.matrix(m_Japanese)), c(as.matrix(m_Chinese)))  # Japanese - Chinese





#################################
##### ANOVA-Type Statistics #####
#################################

library(WRS2)
library(nparLD)
library(nortest)
library(nparcomp)


### ATS ###

# Placeholder data frame for results
ANOVA_test_all2 <- data.frame(matrix(ncol=5, nrow=0))
colnames(ANOVA_test_all2) <- c("rm", "Statistic", "df", "p-value", "temp")

# Compute ATS
for (temp in c("temp0", "temp10", "temp20", "temp30", "temp40")) {
  dats2 <- subset(dte_filt, temp_cat==temp)
  ats2 <- with(dats2, f1.ld.f1(y=response, time=emotion, group=UserLanguage, subject=rid,
                               time.name='emotion', group.name='language'))
  print(ats2)
  ANOVA_test2 <- as.data.frame(ats2$ANOVA.test)
  ANOVA_test2 <- setDT(ANOVA_test2, keep.rownames = TRUE[])
  ANOVA_test2 <- mutate(ANOVA_test2, temp=paste0(temp))
  ANOVA_test_all2 <- rbind(ANOVA_test_all2, ANOVA_test2)
}


# View ATS results
print(ANOVA_test_all2)





#####################################
##### Wilcoxon Signed Rank Test #####
#####################################

library(multcompView)


# Placeholder data frame for results
WSRT_cld_full <- data.frame(matrix(ncol=16, nrow=0))
WSRT_cld_full_summ <- data.frame(matrix(ncol=4, nrow=0))


### Run WSRT per language and temperature
for (lang in c("English", "Spanish", "Chinese", "Japanese")) {
  for (temp in c("temp0", "temp10", "temp20", "temp30", "temp40")) {
    # Filter data by temperature and language
    dwsrt <- subset(dte_filt, UserLanguage==lang & temp_cat==temp)
    # Run Wilcoxon Signed Rank Test
    wsrt <- pairwise.wilcox.test(dwsrt$response, dwsrt$emotion, p.adj = "holm", paired= TRUE)
    pw_wsrt <- as.data.frame(wsrt[["p.value"]])
    pw_wsrt_m <- as.matrix(pw_wsrt)
    # Add first row and column to run Compact Letter Display
    r1 <- c(NA)
    c12 <- c(NA)
    pw_wsrt_m <- rbind(r1, pw_wsrt_m)
    pw_wsrt_m <- cbind(pw_wsrt_m, c12)
    rownames(pw_wsrt_m)[1] <- c("Tense, Bothered")
    colnames(pw_wsrt_m)[12] <- c("Unhappy, Dissatisfied")
    # Run Compact Letter Display
    mcl <- multcompLetters(pw_wsrt_m, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
    cld <- mcl[["Letters"]]
    # Compile full matrix results
    emotion <- row.names(pw_wsrt_m)
    pw_wsrt_m <- cbind(emotion,pw_wsrt_m,cld)
    pw_wsrt_m <- as.data.frame(pw_wsrt_m)
    pw_wsrt_m <- mutate(pw_wsrt_m, UserLanguage=paste0(lang), temp_cat=paste0(temp))
    WSRT_cld_full <- rbind(WSRT_cld_full, pw_wsrt_m)
    # Compile summarized reults
    summ_cld <- pw_wsrt_m
    summ_cld <- dplyr::select(summ_cld, c(emotion, temp_cat, UserLanguage, cld))
    WSRT_cld_full_summ <- rbind(WSRT_cld_full_summ, summ_cld)
  }}


### View WSRT results ###

#view(WSRT_cld_full_summ)


#####----- Full WSRT pairwise comparisons per language and temperature -----#####
#view(WSRT_cld_full)




# Placeholder data frame for results
WSRT_cld_overall <- data.frame(matrix(ncol=15, nrow=0))
WSRT_cld_overall_summ <- data.frame(matrix(ncol=3, nrow=0))

### Run overall WSRT with full dataset
for (temp in c("temp0", "temp10", "temp20", "temp30", "temp40")) {
  dwsrt2 <- subset(dte_filt, temp_cat==temp)
  wsrt <- pairwise.wilcox.test(dwsrt2$response, dwsrt2$emotion, p.adj = "holm", paired= TRUE)
  pw_wsrt <- as.data.frame(wsrt[["p.value"]])
  assign(paste0("WSRT_", "all_lang_", temp), data.frame(pw_wsrt))
  pw_wsrt_m <- as.matrix(pw_wsrt)
  r1 <- c(NA)
  c12 <- c(NA)
  pw_wsrt_m <- rbind(r1, pw_wsrt_m)
  pw_wsrt_m <- cbind(pw_wsrt_m, c12)
  rownames(pw_wsrt_m)[1] <- c("Tense, Bothered")
  colnames(pw_wsrt_m)[12] <- c("Unhappy, Dissatisfied")
  mcl <- multcompLetters(pw_wsrt_m, compare="<", threshold=0.05, Letters=letters, reversed = FALSE)
  cld <- mcl[["Letters"]]
  emotion <- row.names(pw_wsrt_m)
  pw_wsrt_m <- cbind(emotion,pw_wsrt_m,cld)
  pw_wsrt_m <- as.data.frame(pw_wsrt_m)
  pw_wsrt_m <- mutate(pw_wsrt_m, temp_cat=paste0(temp))
  WSRT_cld_overall <- rbind(WSRT_cld_overall, pw_wsrt_m)
  summ_cld <- pw_wsrt_m
  summ_cld <- dplyr::select(summ_cld, c(emotion, temp_cat, cld))
  WSRT_cld_overall_summ <- rbind(WSRT_cld_overall_summ, summ_cld)
}


### View WSRT results ###

#view(WSRT_cld_overall_summ)


#####----- Full WSRT pairwise comparisons per temperature including all languages -----#####
#view(WSRT_cld_overall)




##############################
##### Linear Mixed Model #####
##############################


#####----- Convert tempeature associations to continous variable
dte_filt <- dte_filt %>%
  mutate(temp_converted = (4-0)*(response-1)/(5-1) + 0)


library(reshape2)
data_wide <- as.data.frame(reshape2::dcast(dte_filt, rid + country + age + gender + firstlang + platform + emotion  ~ temp_cat, value.var="temp_converted"))

data_wide <- data_wide %>%
  mutate(temp_sum = rowSums(.[8:12]))


# First transformation
te_gather <- gather(data_wide, temp_cat, temp_converted, temp0:temp40, factor_key=TRUE)

te_gather <- mutate(te_gather, category = ifelse(temp_cat == "temp0", 0,
                                                 ifelse(temp_cat == "temp10", 10,
                                                        ifelse(temp_cat == "temp20", 20,
                                                               ifelse(temp_cat == "temp30", 30,
                                                                      ifelse(temp_cat == "temp40", 40, NA))))))

te_gather <- te_gather %>%
  mutate(temp_weighted = (temp_converted/temp_sum)*category)


te_gather_wide <- as.data.frame(reshape2::dcast(te_gather, rid + country + age + gender + firstlang + platform + emotion  ~ temp_cat, value.var="temp_weighted"))


te_gather_wide <- te_gather_wide %>%
  mutate(temp_cont = rowSums(.[8:12]))

te_gather_wide$firstlang <- factor(te_gather_wide$firstlang, 
                                   levels = c("English", "Spanish", "Japanese", "Chinese"))


##### Descriptive stats
stats_temp_cont <- summaryBy(temp_cont ~ emotion + firstlang, data=subset(te_gather_wide, !is.na(temp_cont)),
                             FUN = c(length, min, max, mean, sd), keep.names = T)

library(lmerTest)
library(pbkrtest)


##### Models

# Model 0
m0 <- lmer(temp_cont ~ (1|rid) , data = te_gather_wide)
summary(m0)
anova(m0)

# Model 1
m1 <- lmer(temp_cont ~ emotion*firstlang + (1|rid) , data = te_gather_wide)
summary(m1)
anova(m1)

# Model 2
m2 <- lmer(temp_cont ~ emotion*firstlang + age + gender + (1|rid) , data = te_gather_wide)
summary(m2)
anova(m2)

# Model 3
m3 <- lmer(temp_cont ~ emotion*firstlang + age + gender + platform + (1|rid), data = te_gather_wide)
summary(m3)
anova(m3)

### LRT
anova(m0, m1)
anova(m1, m2)
anova(m1, m3)
