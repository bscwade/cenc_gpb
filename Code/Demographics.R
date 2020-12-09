require(gdata)
#gpb <- read.xls('~/Ben/Projects/CENC/GPB/Data/DavidTateData.xlsx', sheet = 'SubjectLevelDataset')
gpb <- read.csv('~/Ben/Projects/CENC/GPB/Data/DavidTateData.csv')
gpb_red <- gpb[, c('Subject', 'GPBDOMHANDTIME', 'STUDYGROUP', 'TBI_FLAG', 'GENDERTYP_STD', 'DEMOGAGEYEARS', 'GPBRELCD')]
gpb_red <- gpb_red[gpb_red$GPBRELCD<3, ]
gpb_red <- gpb_red[!is.na(gpb_red$GPBDOMHANDTIME), ]
tsi <- read.xls('~/Ben/Projects/CENC/GPB/Data/TimeSinceIndex PLus Other TimeSince Variables_Baseline_07-09.xlsx')
tsi <- plyr::rename(tsi, replace = c('SubjectID'='Subject'))
tsi <- tsi[, c('Subject', 'TimeSinceIndexYears')]
dat <- merge(gpb_red, tsi, by='Subject')


# 
chisq.test(table(dat$TBI_FLAG, dat$GENDERTYP_STD))
















