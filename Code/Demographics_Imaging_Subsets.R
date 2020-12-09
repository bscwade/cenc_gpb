require(gdata)
require(plyr)
require(knitr)

# Clinical & Demographics
gpb <- read.csv('~/Ben/Projects/CENC/GPB/Data/DavidTateData.csv')
gpb_red <- gpb[, c('Subject', 'GPBDOMHANDTIME', 'STUDYGROUP', 'TBI_FLAG', 'GENDERTYP_STD', 'DEMOGAGEYEARS', 'GPBRELCD')]
gpb_red <- gpb_red[gpb_red$GPBRELCD<3, ]
gpb_red <- gpb_red[!is.na(gpb_red$GPBDOMHANDTIME), ]
tsi <- read.xls('~/Ben/Projects/CENC/GPB/Data/TimeSinceIndex PLus Other TimeSince Variables_Baseline_07-09.xlsx')
tsi <- plyr::rename(tsi, replace = c('SubjectID'='Subject'))
tsi <- tsi[, c('Subject', 'TimeSinceIndexYears')]
dat <- merge(gpb_red, tsi, by='Subject')

# Volumetrics
vol <- read.csv('~/Ben/Projects/CENC/GPB/Data/Imaging/aseg-volume-stats.csv')
vol <- plyr::rename(vol, replace = c('Measure.volume'='Subject'))
vol <- vol[!grepl('ses-2$', vol$Subject), ]
vol$Subject <- substr(vol$Subject, 5, 11)

# Cortical thickness
ct_lh <- read.csv('~/Ben/Projects/CENC/GPB/Data/Imaging/aparc-thickness-lh-stats.csv')
ct_rh <- read.csv('~/Ben/Projects/CENC/GPB/Data/Imaging/aparc-thickness-rh-stats.csv')

ct_lh <- plyr::rename(ct_lh, replace = c('lh.aparc.thickness'='Subject'))
ct_rh <- plyr::rename(ct_rh, replace = c('rh.aparc.thickness'='Subject'))

ct_lh <- ct_lh[!grepl('ses-2$', ct_lh$Subject), ]
ct_rh <- ct_rh[!grepl('ses-2$', ct_rh$Subject), ]

ct_lh$Subject <- substr(ct_lh$Subject, 5, 11)
ct_rh$Subject <- substr(ct_rh$Subject, 5, 11)

# diffusion
diffusion <- read.xls('~/Ben/Projects/CENC/GPB/Data/Imaging/CENC_8Mar20-Diffusion_No_Harmonization.xlsx', sheet='FA')
diffusion <- plyr::rename(diffusion, replace = c('subjectID'='Subject'))
diffusion <- diffusion[!grepl('ses-2$', diffusion$Subject), ]
diffusion$Subject <- substr(diffusion$Subject, 5, 11)

# Merge
df_volume <- Reduce(function(...) merge(..., by='Subject'), list(dat, vol, ct_lh, ct_rh))
df_diffusion <- merge(dat, diffusion, by='Subject')

setdiff(df_volume$Subject, df_diffusion$Subject)
setdiff(df_diffusion$Subject, df_volume$Subject)

# For demographics reporting, let's take the union of subjects with diffusion and volume data since there is a disjoint set
sid_keep <- dplyr::union(df_volume$Subject, df_diffusion$Subject)
dem <- dat[dat$Subject %in% sid_keep, ]
dim(dem)

# TBI Status
table(dem$TBI_FLAG)

# Sex Frequency
table(dem$GENDERTYP_STD)

# Sex Frequency by TBI Status
table(dem$TBI_FLAG, dem$GENDERTYP_STD)
chisq.test(table(dem$TBI_FLAG, dem$GENDERTYP_STD))

# Age
summary(dem$DEMOGAGEYEARS)
sd(dem$DEMOGAGEYEARS, na.rm=T)

# Age by TBI Status
t.test(dem$DEMOGAGEYEARS ~ dem$TBI_FLAG)

# GPB Dominant hand time
summary(dem$GPBDOMHANDTIME)
sd(dem$GPBDOMHANDTIME)

# GPB Time by TBI Status
t.test(dem$GPBDOMHANDTIME ~ dem$TBI_FLAG)

# GPB Time by TBI Status with adjustments
summary(lm(GPBDOMHANDTIME ~ TBI_FLAG + GENDERTYP_STD + DEMOGAGEYEARS + TimeSinceIndexYears, data = dem))





## Demographics tables
tab <- ddply(dem, .(TBI_FLAG), summarise, n=length(GPBDOMHANDTIME), mean_age=mean(DEMOGAGEYEARS), 
             sd_age=sd(DEMOGAGEYEARS), males=sum(GENDERTYP_STD==1))



tab <- ddply(df, .(group), summarise, n=length(cont_sex), mean_age=mean(cont_age), sd_age=sd(cont_age), males=sum(cont_sex), females=sum(cont_sex==0), sx_baseline=mean(outcome_baseline), sx_baseline_sd=sd(outcome_baseline),
             sx_percent_change=mean(outcome/outcome_baseline), sx_percent_change_sd=sd(outcome/outcome_baseline), depression_duration=mean(dabst_duration, na.rm=T), depression_duration_sd=sd(dabst_duration, na.rm=T), number_of_episodes=mean(dabst_num_episodes, na.rm=T), number_of_episodes_sd=sd(dabst_num_episodes, na.rm=T))











