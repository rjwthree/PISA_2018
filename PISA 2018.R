######################################################################################
######################################################################################
############### SEX DIFFERENCES IN THE PISA 2018 MATH AND READING DATA ###############
######################################################################################
######################################################################################


# [1] Read and Format Data
# select country
# objects needed throughout the script

# [2] Weighted Functions
# functions needed throughout the script, with descriptions

# [3] Means and Medians, Tail Proportion Ratios (TPRs), U3 Ratios (U3Rs)
# Means and medians of total group, females, males, as well as mean and median differences
# TPRs and LTPRs above mean and percentiles
# LTPR tail-center differences
# U3Rs and LU3Rs above percentiles
# LU3R tail-center differences

# [4] Other Effect Sizes
# Cohen's d
# U3
# Probability of superiority (PS)
# Variance ratio (VR), Log-transformed VR (LVR)
# Left and right VR and LVR (VR_L, VR_R, LVR_L, LVR_R)
# Mean absolute deviation (from the median) ratio (MADR), Log-transformed MADR (LMADR)
# Left and right MADR and LMADR (MADR_L, MADR_R, LMADR_L, LMADR_R)
# Gini's mean difference ratio (GMDR), Log-transformed GMDR (LGMDR)

# [5] Other Effect Sizes Adjusted for Age
# dataframe with scores linearly corrected for age
# age-corrected effect sizes

# [6] Standard Errors
# bootstrap resampling variance
# imputation variance
# total variance and standard error
# for all of the following
# Means and medians of total group, females, males, as well as mean and median differences
# LTPRs and LTPR tail-center differences***
# LU3Rs and LU3R tail-center differences**
# d, U3, LVR, LVR_L, LVR_R, LMADR, LMADR_L, LMADR_R, LGMDR*
# Probability of superiority****

# [7] 95% Confidence Intervals
# 95% CIs for Means and Medians, TPRs, U3Rs, other effect sizes

# [8] Output
# write dataframe with all needed variables

# ratios are log-transformed to place them on a linear scale

# *nontrivial runtime (more than a few seconds on a personal computer)
# a higher number of asterisks indicates longer runtime
# updates are printed while long loops are running




##############################
#### Read and Format Data ####
##############################

#### Select Country ####

library(haven) # read SPSS
library(data.table) # efficient reading

PISA18 <- read_spss('PISA/CY07_MSU_STU_QQQ.sav') # read data
write.csv(x = PISA18, file = 'PISA input/2018.csv') # store it as a csv

PISA18 <- fread('PISA input/2018.csv')[,-1] # load all PISA 2018 with data.table

Country <- 'Australia' # manually select country

# subset a country
CT <- 'ALB' # Albania
CT <- 'ARG' # Argentina
CT <- 'AUS' # Australia
CT <- 'AUT' # Austria
CT <- 'QAZ' # Baku, Azerbaijan
CT <- 'QCI' # B-S-J-Z, China (Beijing, Shanghai, Jiangsu, Zhejiang)
CT <- 'BLR' # Belarus
CT <- 'BEL' # Belgium
CT <- 'BIH' # Bosnia and Herzegovina
CT <- 'BRA' # Brazil
CT <- 'BRN' # Brunei Darussalam
CT <- 'BGR' # Bulgaria
CT <- 'CAN' # Canada
CT <- 'CHL' # Chile
CT <- 'COL' # Colombia
CT <- 'CRI' # Costa Rica
CT <- 'HRV' # Croatia
CT <- 'CZE' # Czech Republic
CT <- 'DNK' # Denmark
CT <- 'DOM' # Dominican Republic
CT <- 'EST' # Estonia
CT <- 'FIN' # Finland
CT <- 'FRA' # France
CT <- 'GEO' # Georgia
CT <- 'DEU' # Germany
CT <- 'GRC' # Greece
CT <- 'HKG' # Hong Kong, China
CT <- 'HUN' # Hungary
CT <- 'ISL' # Iceland
CT <- 'IDN' # Indonesia
CT <- 'IRL' # Ireland
CT <- 'ISR' # Israel
CT <- 'ITA' # Italy
CT <- 'JPN' # Japan
CT <- 'JOR' # Jordan
CT <- 'KAZ' # Kazakhstan
CT <- 'KOR' # Korea
CT <- 'KSV' # Kosovo
CT <- 'LVA' # Latvia
CT <- 'LBN' # Lebanon
CT <- 'LTU' # Lithuania
CT <- 'LUX' # Luxembourg
CT <- 'MAC' # Macao, China
CT <- 'MKD' # Macedonia
CT <- 'MYS' # Malaysia
CT <- 'MLT' # Malta
CT <- 'MEX' # Mexico
CT <- 'MDA' # Moldova
CT <- 'MNE' # Montenegro
CT <- 'MAR' # Morocco
CT <- 'NLD' # Netherlands
CT <- 'NZL' # New Zealand
CT <- 'NOR' # Norway
CT <- 'PAN' # Panama
CT <- 'PER' # Peru
CT <- 'PHL' # Phillipines
CT <- 'POL' # Poland
CT <- 'PRT' # Portugal
CT <- 'QAT' # Qatar
CT <- 'QMR' # ???
CT <- 'QRT' # ???
CT <- 'ROU' # Romania
CT <- 'RUS' # Russia
CT <- 'SAU' # Saudi Arabia
CT <- 'SRB' # Serbia
CT <- 'SGP' # Singapore
CT <- 'SVK' # Slovakia
CT <- 'SVN' # Slovenia
CT <- 'ESP' # Spain
CT <- 'SWE' # Sweden
CT <- 'CHE' # Switzerland
CT <- 'TAP' # Taiwan
CT <- 'THA' # Thailand
CT <- 'TUR' # Turkey
CT <- 'UKR' # Ukraine
CT <- 'ARE' # United Arab Emirates
CT <- 'GBR' # United Kingdom
CT <- 'USA' # United States
CT <- 'URY' # Uruguay
CT <- 'VNM' # Vietnam



#### Create Objects ####

P.18 <- data.frame(PISA18[CNT == CT])
P18 <- P.18[,c(4, 29, 727, 641, 810:829, 728:807)] # subset columns

remove(PISA18, P.18) # remove unneeded objects

colnms <- numeric(80)
for (i in 1:80) {colnms[i] <- paste0('BW',i)} # bootstrap replicate weight column names: BW1-BW80

names(P18) <- c('Student', 'Sex', 'HWt', 'Age',
                'MV1', 'MV2', 'MV3', 'MV4', 'MV5', 'MV6', 'MV7', 'MV8', 'MV9', 'MVX',
                'RV1', 'RV2', 'RV3', 'RV4', 'RV5', 'RV6', 'RV7', 'RV8', 'RV9', 'RVX', colnms)

Size <- nrow(P18) # sample size
P18$HWt <- Size/sum(P18$HWt)*P18$HWt # equalize sample size and sum of HWt

P18_F <- P18[which(P18$Sex == 1),] # female subset of P18
P18_M <- P18[which(P18$Sex == 2),] # male subset of P18

FSize <- nrow(P18_F) # female sample size
MSize <- nrow(P18_M) # male sample size




##############################
##### Weighted Functions #####
##############################

# arithmetic mean
wt.mn <- function(x, w) {mean(x*(length(x)/sum(w)*w))}


# variance
wt.var <- function(x, w) {
  mn <- wt.mn(x, w)
  n <- length(x)
  wt <- n/sum(w)*w
  return(sum(wt*(x-mn)^2)/(n-sum(wt^2)/n))
}
# mean squared deviation from the mean, with the weighted variant of Bessel's correction


# quantile
wt.qnt <- function(x, w, q) {
  k <- sum(w)*q
  if (q <= .5) {
    ascend <- order(x)
    a <- x[ascend]
    b <- w[ascend]
    wt <- 0
    for (i in 1:length(a)) {
      if (wt < k) {
        wt <- wt + b[i]
      } else {
        R <- b[i-1]
        S <- a[i-2]
        V <- a[i-1]
        break
      }
    }
    return(V-(wt-k)/R*(V-S))
  }
  else {
    descend <- order(x, decreasing = T)
    a <- x[descend]
    b <- w[descend]
    wt <- sum(w)
    for (i in 1:length(a)) {
      if (wt > k) {
        wt <- wt - b[i]
      } else {
        R <- b[i-1]
        S <- a[i-1]
        V <- a[i]
        break
      }
    }
    return(V+(k-wt)/R*(S-V))
  }
}
# sort the scores and weights in ascending order of scores
# add weights until the fraction of total weight exceeds q
# perform linear interpolation between the closest scores (S and V)
# for efficiency, reverse the process if q exceeds .5


# log-transformed tail proportion ratio (LTPR)
LTPRfn <- function(d1, d2, q, w, v) {
  tango1 <- d1[which(d1[,v] >= max(d1[,v][which(d1[,v] < q)])),]
  tango2 <- d2[which(d2[,v] >= max(d2[,v][which(d2[,v] < q)])),]
  mango1 <- tango1[order(tango1[,v]),]
  mango2 <- tango2[order(tango2[,v]),]
  
  if (mango1[1,v] == mango1[2,v]) {
    excess <- nrow(mango1[which(mango1[,v] == mango1[1,v]),])-1
    mango1 <- mango1[-c(1:excess),]
  }
  if (mango2[1,v] == mango2[2,v]) {
    excess <- nrow(mango2[which(mango2[,v] == mango2[1,v]),])-1
    mango2 <- mango2[-c(1:excess),]
  }
  
  slice1 <- (mango1[2,v]-q)/(mango1[2,v]-mango1[1,v])*mango1[2,w]
  slice2 <- (mango2[2,v]-q)/(mango2[2,v]-mango2[1,v])*mango2[2,w]
  
  return(log((sum(mango1[-c(1:2),w])+slice1)/sum(d1[,w])/
               ((sum(mango2[-c(1:2),w])+slice2)/sum(d2[,w]))))
}
# M/F ratio of weight above a threshold q divided by the subgroup weight (see U3 description)
# log-transform the ratio to produce unbiased means


# log-transformed U3 ratio (LU3R)
LU3Rfn <- function(d1, d2, q, w, v) {
  k <- wt.qnt(x = d2[,v], w = d2[,w], q = q)
  tango <- d1[which(d1[,v] >= max(d1[,v][which(d1[,v] < k)])),]
  mango <- tango[order(tango[,v]),]
  if (mango[1,v] == mango[2,v]) {
    excess <- nrow(mango[which(mango[,v] == mango[1,v]),])-1
    mango <- mango[-c(1:excess),]
  }
  slice <- (mango[2,v]-k)/(mango[2,v]-mango[1,v])*mango[2,w]
  return(log((sum(mango[-c(1:2),w])+slice)/sum(d1[,w])/(1-q)))
}
# share of male weight above a female subgroup quantile (see U3 description)
# divided by the natural share of female weight above that quantile (1-q)
# log-transform the ratio to produce unbiased means


# Cohen's d
dfn <- function(d1, d2, w, v) {
  (wt.mn(x = d1[,v], w = d1[,w])-wt.mn(x = d2[,v], w = d2[,w]))/
    sqrt((wt.var(x = d1[,v], w = d1[,w])+wt.var(x = d2[,v], w = d2[,w]))/2)
}
# raw mean difference divided by quadratic mean of standard deviations


# U3
U3fn <- function(d1, d2, w, v) {
  q <- wt.qnt(x = d2[,v], w = d2[,w], q = .5)
  tango <- d1[which(d1[,v] >= max(d1[,v][which(d1[,v] < q)])),]
  mango <- tango[order(tango[,v]),]
  if (mango[1,v] == mango[2,v]) {
    excess <- nrow(mango[which(mango[,v] == mango[1,v]),])-1
    mango <- mango[-c(1:excess),]
  }
  slice <- (mango[2,v]-q)/(mango[2,v]-mango[1,v])*mango[2,w]
  return((sum(mango[-c(1:2),w])+slice)/sum(d1[,w]))
}
# compute female median (q)
# store the male subset with scores higher than q,
# plus the row with the score immediately below q (tango)
# sort tango in ascending order (mango)
# discard excess in the rare case that more than one student has the score immediately below q
# divide the distance between the score immediately above q and q
# by the distance between the score immediately above q and the score immediately below q
# then multiply this proportion by the weight associated with the score above q (slice)
# add slice to all weight above the score immediately above q, then divide by total male weight
# this gives the precise share of male weight above the female median


# Probability of superiority (PS)
PSfn <- function(d1, d2, w, v) {
  D1 <- d1[order(d1[,v]),]
  
  SuperM <- numeric(FSize)
  for (i in 1:FSize) {SuperM[i] <- sum(D1[which(D1[,v] > d2[i,v]),w])}
  
  equal <- intersect(D1[,v], d2[,v])
  EWtsM <- EWtsF <- numeric(length(equal))
  for (i in 1:length(equal)) {
    EWtsM[i] <- sum(D1[which(D1[,v] %in% equal[i]),w])
    EWtsF[i] <- sum(d2[which(d2[,v] %in% equal[i]),w])
  }
  
  return(sum(c(SuperM*d2[,w], .5*EWtsM*EWtsF))/(sum(D1[,w])*sum(d2[,w])))
}
# for efficiency, sort male scores and weights in order of scores (D1)
# compute the male weight above each female score (SuperM)
# store the small subset of scores that are equal to a score in the other subgroup (equal)
# find the associated weights in males and females (EWtsM, EWtsF)
# sum weights of superior males multiplied by associated female weights
# and half of the sum of equal male and female weights multiplied
# then divide by the total weight of male-female pairs
# this gives the probability that a random male has a higher score than a random female


# log-transformed variance ratio (LVR)
LVRfn <- function(d1, d2, w, v) {
  log(wt.var(x = d1[,v], w = d1[,w])/wt.var(x = d2[,v], w = d2[,w]))
}
# M/F ratio of variance
# log-transform the ratio to produce unbiased means


# log-transformed tail VR (LVR_T)
LVR_Tfn <- function(d1, d2, w, v, t) {
  q1 <- wt.mn(x = d1[,v], w = d1[,w])
  q2 <- wt.mn(x = d2[,v], w = d2[,w])
  if (t == 'L') {
    MBMn <- d1[which(d1[,v] < q1),]
    FBMn <- d2[which(d2[,v] < q2),]
  } else if (t == 'R') {
    MBMn <- d1[which(d1[,v] > q1),]
    FBMn <- d2[which(d2[,v] > q2),]
  }
  return(log(sum(MBMn[,w]*(MBMn[,v]-q1)^2)/sum(MBMn[,w])/
               (sum(FBMn[,w]*(FBMn[,v]-q2)^2)/sum(FBMn[,w]))))
}
# subset the males and females below (t = 'L') or above (t = 'R') the subgroup mean
# compute the M/F ratio of mean squared deviation from the mean in the left or right tail
# Bessel's correction not applicable because in this case the sample mean's deviation
# from the population mean causes random not systematic error
# log-transform the ratio to produce unbiased means


# log-transformed mean absolute deviation (from the median) ratio (LMADR)
LMADRfn <- function(d1, d2, w, v) {
  log(wt.mn(x = abs(d1[,v]-wt.qnt(x = d1[,v], w = d1[,w], q = .5)), w = d1[,w])/
        wt.mn(x = abs(d2[,v]-wt.qnt(x = d2[,v], w = d2[,w], q = .5)), w = d2[,w]))
}
# M/F ratio of mean absolute deviation from the median
# log-transform for unbiased means


# log-transformed tail MADR (LMADR_T)
LMADR_Tfn <- function(d1, d2, w, v, t) {
  q1 <- wt.qnt(x = d1[,v], w = d1[,w], q = .5)
  q2 <- wt.qnt(x = d2[,v], w = d2[,w], q = .5)
  if (t == 'L') {
    MBMd <- d1[which(d1[,v] < q1),]
    FBMd <- d2[which(d2[,v] < q2),]
    return(log(wt.mn(x = q1-MBMd[,v], w = MBMd[,w])/
                 wt.mn(x = q2-FBMd[,v], w = FBMd[,w])))
  } else if (t == 'R') {
    MBMd <- d1[which(d1[,v] > q1),]
    FBMd <- d2[which(d2[,v] > q2),]
    return(log(wt.mn(x = MBMd[,v]-q1, w = MBMd[,w])/
                 wt.mn(x = FBMd[,v]-q2, w = FBMd[,w])))
  }
}
# subset the males and females below (t = 'L') or above (t = 'R') the subgroup median
# compute the M/F ratio of mean absolute deviation from the median in the left or right tail
# log-transform the ratio to produce unbiased means


# log-transformed Gini's mean difference ratio (LGMDR)
LGMDRfn <- function(d1, d2, w, v) {
  a1 <- sort(d1[,v])
  a2 <- sort(d2[,v])
  b1 <- d1[order(d1[,v]),w]/sum(d1[,w])
  b2 <- d2[order(d2[,v]),w]/sum(d2[,w])
  s1 <- cumsum(b1)
  s2 <- cumsum(b2)
  y1 <- cumsum(a1*b1)
  y2 <- cumsum(a2*b2)
  z1 <- y1/y1[MSize]
  z2 <- y2/y2[FSize]
  Gini1 <- as.numeric(t(z1[-1]) %*% s1[-MSize] - t(z1[-MSize]) %*% s1[-1])
  Gini2 <- as.numeric(t(z2[-1]) %*% s2[-FSize] - t(z2[-FSize]) %*% s2[-1])
  return(log(Gini1*wt.mn(x = d1[,v], w = d1[,w])/(Gini2*wt.mn(x = d2[,v], w = d2[,w]))))
}
# adapted from acid::weighted.gini by Alexander Sohn
# order scores in ascending order (a1/a2)
# order weights in ascending order of scores and divide by total subgroup weight (b1/b2)
# store cumul sum of b1/b2, expressing cumul wt as a fraction of total wt (s1/s2; Lorenz curve)
# store cumul sum of ordered scores multiplied by ordered weights (y1/y2)
# and divide by the last index of y1/y2, which is the sum of a1*b1/a2*b2 (z1/z2)
# compute the Gini index (Gini1/Gini2)
# multiply by two times the mean for the mean absolute difference (but the twos cancel)
# this produces the ratio of mean abs diff between students selected randomly with replacement
# log-transform the ratio to produce unbiased means




#############################
##### Means and Medians #####
#############################

### total group
MnMs <- c(wt.mn(x = P18$MV1, w = P18$HWt), # MV1 mean
          wt.mn(x = P18$MV2, w = P18$HWt), # MV2
          wt.mn(x = P18$MV3, w = P18$HWt), # MV3
          wt.mn(x = P18$MV4, w = P18$HWt), # MV4
          wt.mn(x = P18$MV5, w = P18$HWt), # MV5
          wt.mn(x = P18$MV6, w = P18$HWt), # MV6
          wt.mn(x = P18$MV7, w = P18$HWt), # MV7
          wt.mn(x = P18$MV8, w = P18$HWt), # MV8
          wt.mn(x = P18$MV9, w = P18$HWt), # MV9
          wt.mn(x = P18$MVX, w = P18$HWt)) # MVX

MnM <- mean(MnMs)

MnRs <- c(wt.mn(x = P18$RV1, w = P18$HWt), # RV1 mean
          wt.mn(x = P18$RV2, w = P18$HWt), # RV2
          wt.mn(x = P18$RV3, w = P18$HWt), # RV3
          wt.mn(x = P18$RV4, w = P18$HWt), # RV4
          wt.mn(x = P18$RV5, w = P18$HWt), # RV5
          wt.mn(x = P18$RV6, w = P18$HWt), # RV6
          wt.mn(x = P18$RV7, w = P18$HWt), # RV7
          wt.mn(x = P18$RV8, w = P18$HWt), # RV8
          wt.mn(x = P18$RV9, w = P18$HWt), # RV9
          wt.mn(x = P18$RVX, w = P18$HWt)) # RVX

MnR <- mean(MnRs)

MdMs <- c(wt.qnt(x = P18$MV1, w = P18$HWt, q = .5), # MV1 median
          wt.qnt(x = P18$MV2, w = P18$HWt, q = .5), # MV2
          wt.qnt(x = P18$MV3, w = P18$HWt, q = .5), # MV3
          wt.qnt(x = P18$MV4, w = P18$HWt, q = .5), # MV4
          wt.qnt(x = P18$MV5, w = P18$HWt, q = .5), # MV5
          wt.qnt(x = P18$MV6, w = P18$HWt, q = .5), # MV6
          wt.qnt(x = P18$MV7, w = P18$HWt, q = .5), # MV7
          wt.qnt(x = P18$MV8, w = P18$HWt, q = .5), # MV8
          wt.qnt(x = P18$MV9, w = P18$HWt, q = .5), # MV9
          wt.qnt(x = P18$MVX, w = P18$HWt, q = .5)) # MVX

MdM <- mean(MdMs)

MdRs <- c(wt.qnt(x = P18$RV1, w = P18$HWt, q = .5), # RV1 median
          wt.qnt(x = P18$RV2, w = P18$HWt, q = .5), # RV2
          wt.qnt(x = P18$RV3, w = P18$HWt, q = .5), # RV3
          wt.qnt(x = P18$RV4, w = P18$HWt, q = .5), # RV4
          wt.qnt(x = P18$RV5, w = P18$HWt, q = .5), # RV5
          wt.qnt(x = P18$RV6, w = P18$HWt, q = .5), # RV6
          wt.qnt(x = P18$RV7, w = P18$HWt, q = .5), # RV7
          wt.qnt(x = P18$RV8, w = P18$HWt, q = .5), # RV8
          wt.qnt(x = P18$RV9, w = P18$HWt, q = .5), # RV9
          wt.qnt(x = P18$RVX, w = P18$HWt, q = .5)) # RVX

MdR <- mean(MdRs)


### females
MnMs_F <- c(wt.mn(x = P18_F$MV1, w = P18_F$HWt), # MV1 mean
            wt.mn(x = P18_F$MV2, w = P18_F$HWt), # MV2
            wt.mn(x = P18_F$MV3, w = P18_F$HWt), # MV3
            wt.mn(x = P18_F$MV4, w = P18_F$HWt), # MV4
            wt.mn(x = P18_F$MV5, w = P18_F$HWt), # MV5
            wt.mn(x = P18_F$MV6, w = P18_F$HWt), # MV6
            wt.mn(x = P18_F$MV7, w = P18_F$HWt), # MV7
            wt.mn(x = P18_F$MV8, w = P18_F$HWt), # MV8
            wt.mn(x = P18_F$MV9, w = P18_F$HWt), # MV9
            wt.mn(x = P18_F$MVX, w = P18_F$HWt)) # MVX

MnM_F <- mean(MnMs_F)

MnRs_F <- c(wt.mn(x = P18_F$RV1, w = P18_F$HWt), # RV1 mean
            wt.mn(x = P18_F$RV2, w = P18_F$HWt), # RV2
            wt.mn(x = P18_F$RV3, w = P18_F$HWt), # RV3
            wt.mn(x = P18_F$RV4, w = P18_F$HWt), # RV4
            wt.mn(x = P18_F$RV5, w = P18_F$HWt), # RV5
            wt.mn(x = P18_F$RV6, w = P18_F$HWt), # RV6
            wt.mn(x = P18_F$RV7, w = P18_F$HWt), # RV7
            wt.mn(x = P18_F$RV8, w = P18_F$HWt), # RV8
            wt.mn(x = P18_F$RV9, w = P18_F$HWt), # RV9
            wt.mn(x = P18_F$RVX, w = P18_F$HWt)) # RVX

MnR_F <- mean(MnRs_F)

MdMs_F <- c(wt.qnt(x = P18_F$MV1, w = P18_F$HWt, q = .5), # MV1 median
            wt.qnt(x = P18_F$MV2, w = P18_F$HWt, q = .5), # MV2
            wt.qnt(x = P18_F$MV3, w = P18_F$HWt, q = .5), # MV3
            wt.qnt(x = P18_F$MV4, w = P18_F$HWt, q = .5), # MV4
            wt.qnt(x = P18_F$MV5, w = P18_F$HWt, q = .5), # MV5
            wt.qnt(x = P18_F$MV6, w = P18_F$HWt, q = .5), # MV6
            wt.qnt(x = P18_F$MV7, w = P18_F$HWt, q = .5), # MV7
            wt.qnt(x = P18_F$MV8, w = P18_F$HWt, q = .5), # MV8
            wt.qnt(x = P18_F$MV9, w = P18_F$HWt, q = .5), # MV9
            wt.qnt(x = P18_F$MVX, w = P18_F$HWt, q = .5)) # MVX

MdM_F <- mean(MdMs_F)

MdRs_F <- c(wt.qnt(x = P18_F$RV1, w = P18_F$HWt, q = .5), # RV1 median
            wt.qnt(x = P18_F$RV2, w = P18_F$HWt, q = .5), # RV2
            wt.qnt(x = P18_F$RV3, w = P18_F$HWt, q = .5), # RV3
            wt.qnt(x = P18_F$RV4, w = P18_F$HWt, q = .5), # RV4
            wt.qnt(x = P18_F$RV5, w = P18_F$HWt, q = .5), # RV5
            wt.qnt(x = P18_F$RV6, w = P18_F$HWt, q = .5), # RV6
            wt.qnt(x = P18_F$RV7, w = P18_F$HWt, q = .5), # RV7
            wt.qnt(x = P18_F$RV8, w = P18_F$HWt, q = .5), # RV8
            wt.qnt(x = P18_F$RV9, w = P18_F$HWt, q = .5), # RV9
            wt.qnt(x = P18_F$RVX, w = P18_F$HWt, q = .5)) # RVX

MdR_F <- mean(MdRs_F)


### males
MnMs_M <- c(wt.mn(x = P18_M$MV1, w = P18_M$HWt), # MV1 mean
            wt.mn(x = P18_M$MV2, w = P18_M$HWt), # MV2
            wt.mn(x = P18_M$MV3, w = P18_M$HWt), # MV3
            wt.mn(x = P18_M$MV4, w = P18_M$HWt), # MV4
            wt.mn(x = P18_M$MV5, w = P18_M$HWt), # MV5
            wt.mn(x = P18_M$MV6, w = P18_M$HWt), # MV6
            wt.mn(x = P18_M$MV7, w = P18_M$HWt), # MV7
            wt.mn(x = P18_M$MV8, w = P18_M$HWt), # MV8
            wt.mn(x = P18_M$MV9, w = P18_M$HWt), # MV9
            wt.mn(x = P18_M$MVX, w = P18_M$HWt)) # MVX

MnM_M <- mean(MnMs_M)

MnRs_M <- c(wt.mn(x = P18_M$RV1, w = P18_M$HWt), # RV1 mean
            wt.mn(x = P18_M$RV2, w = P18_M$HWt), # RV2
            wt.mn(x = P18_M$RV3, w = P18_M$HWt), # RV3
            wt.mn(x = P18_M$RV4, w = P18_M$HWt), # RV4
            wt.mn(x = P18_M$RV5, w = P18_M$HWt), # RV5
            wt.mn(x = P18_M$RV6, w = P18_M$HWt), # RV6
            wt.mn(x = P18_M$RV7, w = P18_M$HWt), # RV7
            wt.mn(x = P18_M$RV8, w = P18_M$HWt), # RV8
            wt.mn(x = P18_M$RV9, w = P18_M$HWt), # RV9
            wt.mn(x = P18_M$RVX, w = P18_M$HWt)) # RVX

MnR_M <- mean(MnRs_M)

MdMs_M <- c(wt.qnt(x = P18_M$MV1, w = P18_M$HWt, q = .5), # MV1 median
            wt.qnt(x = P18_M$MV2, w = P18_M$HWt, q = .5), # MV2
            wt.qnt(x = P18_M$MV3, w = P18_M$HWt, q = .5), # MV3
            wt.qnt(x = P18_M$MV4, w = P18_M$HWt, q = .5), # MV4
            wt.qnt(x = P18_M$MV5, w = P18_M$HWt, q = .5), # MV5
            wt.qnt(x = P18_M$MV6, w = P18_M$HWt, q = .5), # MV6
            wt.qnt(x = P18_M$MV7, w = P18_M$HWt, q = .5), # MV7
            wt.qnt(x = P18_M$MV8, w = P18_M$HWt, q = .5), # MV8
            wt.qnt(x = P18_M$MV9, w = P18_M$HWt, q = .5), # MV9
            wt.qnt(x = P18_M$MVX, w = P18_M$HWt, q = .5)) # MVX

MdM_M <- mean(MdMs_M)

MdRs_M <- c(wt.qnt(x = P18_M$RV1, w = P18_M$HWt, q = .5), # RV1 median
            wt.qnt(x = P18_M$RV2, w = P18_M$HWt, q = .5), # RV2
            wt.qnt(x = P18_M$RV3, w = P18_M$HWt, q = .5), # RV3
            wt.qnt(x = P18_M$RV4, w = P18_M$HWt, q = .5), # RV4
            wt.qnt(x = P18_M$RV5, w = P18_M$HWt, q = .5), # RV5
            wt.qnt(x = P18_M$RV6, w = P18_M$HWt, q = .5), # RV6
            wt.qnt(x = P18_M$RV7, w = P18_M$HWt, q = .5), # RV7
            wt.qnt(x = P18_M$RV8, w = P18_M$HWt, q = .5), # RV8
            wt.qnt(x = P18_M$RV9, w = P18_M$HWt, q = .5), # RV9
            wt.qnt(x = P18_M$RVX, w = P18_M$HWt, q = .5)) # RVX

MdR_M <- mean(MdRs_M)


### differences

MnDfMs <- MnMs_M-MnMs_F # mean difference (math)
MnDfRs <- MnRs_M-MnRs_F # mean difference (reading)
MdDfMs <- MdMs_M-MdMs_F # median difference (math)
MdDfRs <- MdRs_M-MdRs_F # median difference (reading)

MnDfM <- mean(MnDfMs)
MnDfR <- mean(MnDfRs)
MdDfM <- mean(MdDfMs)
MdDfR <- mean(MdDfRs)




#########################################
##### Tail Proportion Ratios (TPRs) #####
#########################################

### Math TPRs and LTPRs: mean and every 5th percentile from 5 to 95
LTPRMnMs <- c(LTPRfn(P18_M, P18_F, MnMs[1], 'HWt', 'MV1'), # MV1 mean
              LTPRfn(P18_M, P18_F, MnMs[2], 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, MnMs[3], 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, MnMs[4], 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, MnMs[5], 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, MnMs[6], 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, MnMs[7], 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, MnMs[8], 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, MnMs[9], 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, MnMs[10],'HWt', 'MVX')) # MVX
LTPR05Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .05), 'HWt', 'MV1'), # MV1 5th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .05), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .05), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .05), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .05), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .05), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .05), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .05), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .05), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .05), 'HWt', 'MVX')) # MVX
LTPR10Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .10), 'HWt', 'MV1'), # MV1 10th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .10), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .10), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .10), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .10), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .10), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .10), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .10), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .10), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .10), 'HWt', 'MVX')) # MVX
LTPR15Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .15), 'HWt', 'MV1'), # MV1 15th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .15), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .15), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .15), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .15), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .15), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .15), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .15), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .15), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .15), 'HWt', 'MVX')) # MVX
LTPR20Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .20), 'HWt', 'MV1'), # MV1 20th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .20), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .20), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .20), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .20), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .20), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .20), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .20), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .20), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .20), 'HWt', 'MVX')) # MVX
LTPR25Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .25), 'HWt', 'MV1'), # MV1 25th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .25), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .25), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .25), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .25), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .25), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .25), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .25), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .25), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .25), 'HWt', 'MVX')) # MVX
LTPR30Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .30), 'HWt', 'MV1'), # MV1 30th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .30), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .30), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .30), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .30), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .30), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .30), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .30), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .30), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .30), 'HWt', 'MVX')) # MVX
LTPR35Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .35), 'HWt', 'MV1'), # MV1 35th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .35), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .35), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .35), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .35), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .35), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .35), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .35), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .35), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .35), 'HWt', 'MVX')) # MVX
LTPR40Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .40), 'HWt', 'MV1'), # MV1 40th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .40), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .40), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .40), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .40), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .40), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .40), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .40), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .40), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .40), 'HWt', 'MVX')) # MVX
LTPR45Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .45), 'HWt', 'MV1'), # MV1 45th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .45), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .45), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .45), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .45), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .45), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .45), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .45), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .45), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .45), 'HWt', 'MVX')) # MVX
LTPR50Ms <- c(LTPRfn(P18_M, P18_F, MdMs[1], 'HWt', 'MV1'), # MV1 50th
              LTPRfn(P18_M, P18_F, MdMs[2], 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, MdMs[3], 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, MdMs[4], 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, MdMs[5], 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, MdMs[6], 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, MdMs[7], 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, MdMs[8], 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, MdMs[9], 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, MdMs[10],'HWt', 'MVX')) # MVX
LTPR55Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .55), 'HWt', 'MV1'), # MV1 55th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .55), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .55), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .55), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .55), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .55), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .55), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .55), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .55), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .55), 'HWt', 'MVX')) # MVX
LTPR60Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .60), 'HWt', 'MV1'), # MV1 60th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .60), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .60), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .60), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .60), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .60), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .60), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .60), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .60), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .60), 'HWt', 'MVX')) # MVX
LTPR65Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .65), 'HWt', 'MV1'), # MV1 65th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .65), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .65), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .65), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .65), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .65), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .65), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .65), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .65), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .65), 'HWt', 'MVX')) # MVX
LTPR70Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .70), 'HWt', 'MV1'), # MV1 70th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .70), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .70), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .70), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .70), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .70), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .70), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .70), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .70), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .70), 'HWt', 'MVX')) # MVX
LTPR75Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .75), 'HWt', 'MV1'), # MV1 75th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .75), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .75), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .75), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .75), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .75), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .75), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .75), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .75), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .75), 'HWt', 'MVX')) # MVX
LTPR80Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .80), 'HWt', 'MV1'), # MV1 80th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .80), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .80), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .80), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .80), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .80), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .80), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .80), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .80), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .80), 'HWt', 'MVX')) # MVX
LTPR85Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .85), 'HWt', 'MV1'), # MV1 85th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .85), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .85), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .85), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .85), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .85), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .85), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .85), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .85), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .85), 'HWt', 'MVX')) # MVX
LTPR90Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .90), 'HWt', 'MV1'), # MV1 90th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .90), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .90), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .90), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .90), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .90), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .90), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .90), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .90), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .90), 'HWt', 'MVX')) # MVX
LTPR95Ms <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18$HWt, .95), 'HWt', 'MV1'), # MV1 95th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18$HWt, .95), 'HWt', 'MV2'), # MV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18$HWt, .95), 'HWt', 'MV3'), # MV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18$HWt, .95), 'HWt', 'MV4'), # MV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18$HWt, .95), 'HWt', 'MV5'), # MV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18$HWt, .95), 'HWt', 'MV6'), # MV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18$HWt, .95), 'HWt', 'MV7'), # MV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18$HWt, .95), 'HWt', 'MV8'), # MV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18$HWt, .95), 'HWt', 'MV9'), # MV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18$HWt, .95), 'HWt', 'MVX')) # MVX

LTPRMnM <- mean(LTPRMnMs) # mean
LTPR05M <- mean(LTPR05Ms) # 5th
LTPR10M <- mean(LTPR10Ms) # 10th
LTPR15M <- mean(LTPR15Ms) # 15th
LTPR20M <- mean(LTPR20Ms) # 20th
LTPR25M <- mean(LTPR25Ms) # 25th
LTPR30M <- mean(LTPR30Ms) # 30th
LTPR35M <- mean(LTPR35Ms) # 35th
LTPR40M <- mean(LTPR40Ms) # 40th
LTPR45M <- mean(LTPR45Ms) # 45th
LTPR50M <- mean(LTPR50Ms) # 50th
LTPR55M <- mean(LTPR55Ms) # 55th
LTPR60M <- mean(LTPR60Ms) # 60th
LTPR65M <- mean(LTPR65Ms) # 65th
LTPR70M <- mean(LTPR70Ms) # 70th
LTPR75M <- mean(LTPR75Ms) # 75th
LTPR80M <- mean(LTPR80Ms) # 80th
LTPR85M <- mean(LTPR85Ms) # 85th
LTPR90M <- mean(LTPR90Ms) # 90th
LTPR95M <- mean(LTPR95Ms) # 95th

TPRMnM <- exp(LTPRMnM) # mean
TPR05M <- exp(LTPR05M) # 5th
TPR10M <- exp(LTPR10M) # 10th
TPR15M <- exp(LTPR15M) # 15th
TPR20M <- exp(LTPR20M) # 20th
TPR25M <- exp(LTPR25M) # 25th
TPR30M <- exp(LTPR30M) # 30th
TPR35M <- exp(LTPR35M) # 35th
TPR40M <- exp(LTPR40M) # 40th
TPR45M <- exp(LTPR45M) # 45th
TPR50M <- exp(LTPR50M) # 50th
TPR55M <- exp(LTPR55M) # 55th
TPR60M <- exp(LTPR60M) # 60th
TPR65M <- exp(LTPR65M) # 65th
TPR70M <- exp(LTPR70M) # 70th
TPR75M <- exp(LTPR75M) # 75th
TPR80M <- exp(LTPR80M) # 80th
TPR85M <- exp(LTPR85M) # 85th
TPR90M <- exp(LTPR90M) # 90th
TPR95M <- exp(LTPR95M) # 95th


### Reading TPRs and LTPRs: mean and every 5th percentile from 5 to 95
LTPRMnRs <- c(LTPRfn(P18_M, P18_F, MnRs[1], 'HWt', 'RV1'), # RV1 mean
              LTPRfn(P18_M, P18_F, MnRs[2], 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, MnRs[3], 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, MnRs[4], 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, MnRs[5], 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, MnRs[6], 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, MnRs[7], 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, MnRs[8], 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, MnRs[9], 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, MnRs[10],'HWt', 'RVX')) # RVX
LTPR05Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .05), 'HWt', 'RV1'), # RV1 5th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .05), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .05), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .05), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .05), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .05), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .05), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .05), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .05), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .05), 'HWt', 'RVX')) # RVX
LTPR10Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .10), 'HWt', 'RV1'), # RV1 10th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .10), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .10), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .10), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .10), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .10), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .10), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .10), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .10), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .10), 'HWt', 'RVX')) # RVX
LTPR15Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .15), 'HWt', 'RV1'), # RV1 15th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .15), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .15), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .15), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .15), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .15), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .15), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .15), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .15), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .15), 'HWt', 'RVX')) # RVX
LTPR20Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .20), 'HWt', 'RV1'), # RV1 20th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .20), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .20), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .20), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .20), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .20), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .20), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .20), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .20), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .20), 'HWt', 'RVX')) # RVX
LTPR25Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .25), 'HWt', 'RV1'), # RV1 25th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .25), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .25), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .25), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .25), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .25), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .25), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .25), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .25), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .25), 'HWt', 'RVX')) # RVX
LTPR30Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .30), 'HWt', 'RV1'), # RV1 30th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .30), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .30), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .30), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .30), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .30), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .30), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .30), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .30), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .30), 'HWt', 'RVX')) # RVX
LTPR35Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .35), 'HWt', 'RV1'), # RV1 35th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .35), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .35), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .35), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .35), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .35), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .35), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .35), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .35), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .35), 'HWt', 'RVX')) # RVX
LTPR40Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .40), 'HWt', 'RV1'), # RV1 40th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .40), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .40), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .40), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .40), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .40), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .40), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .40), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .40), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .40), 'HWt', 'RVX')) # RVX
LTPR45Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .45), 'HWt', 'RV1'), # RV1 45th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .45), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .45), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .45), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .45), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .45), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .45), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .45), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .45), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .45), 'HWt', 'RVX')) # RVX
LTPR50Rs <- c(LTPRfn(P18_M, P18_F, MdRs[1], 'HWt', 'RV1'), # RV1 50th
              LTPRfn(P18_M, P18_F, MdRs[2], 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, MdRs[3], 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, MdRs[4], 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, MdRs[5], 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, MdRs[6], 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, MdRs[7], 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, MdRs[8], 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, MdRs[9], 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, MdRs[10],'HWt', 'RVX')) # RVX
LTPR55Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .55), 'HWt', 'RV1'), # RV1 55th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .55), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .55), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .55), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .55), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .55), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .55), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .55), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .55), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .55), 'HWt', 'RVX')) # RVX
LTPR60Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .60), 'HWt', 'RV1'), # RV1 60th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .60), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .60), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .60), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .60), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .60), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .60), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .60), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .60), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .60), 'HWt', 'RVX')) # RVX
LTPR65Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .65), 'HWt', 'RV1'), # RV1 65th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .65), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .65), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .65), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .65), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .65), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .65), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .65), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .65), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .65), 'HWt', 'RVX')) # RVX
LTPR70Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .70), 'HWt', 'RV1'), # RV1 70th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .70), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .70), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .70), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .70), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .70), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .70), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .70), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .70), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .70), 'HWt', 'RVX')) # RVX
LTPR75Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .75), 'HWt', 'RV1'), # RV1 75th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .75), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .75), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .75), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .75), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .75), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .75), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .75), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .75), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .75), 'HWt', 'RVX')) # RVX
LTPR80Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .80), 'HWt', 'RV1'), # RV1 80th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .80), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .80), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .80), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .80), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .80), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .80), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .80), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .80), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .80), 'HWt', 'RVX')) # RVX
LTPR85Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .85), 'HWt', 'RV1'), # RV1 85th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .85), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .85), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .85), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .85), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .85), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .85), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .85), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .85), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .85), 'HWt', 'RVX')) # RVX
LTPR90Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .90), 'HWt', 'RV1'), # RV1 90th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .90), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .90), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .90), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .90), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .90), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .90), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .90), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .90), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .90), 'HWt', 'RVX')) # RVX
LTPR95Rs <- c(LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18$HWt, .95), 'HWt', 'RV1'), # RV1 95th
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18$HWt, .95), 'HWt', 'RV2'), # RV2
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18$HWt, .95), 'HWt', 'RV3'), # RV3
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18$HWt, .95), 'HWt', 'RV4'), # RV4
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18$HWt, .95), 'HWt', 'RV5'), # RV5
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18$HWt, .95), 'HWt', 'RV6'), # RV6
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18$HWt, .95), 'HWt', 'RV7'), # RV7
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18$HWt, .95), 'HWt', 'RV8'), # RV8
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18$HWt, .95), 'HWt', 'RV9'), # RV9
              LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18$HWt, .95), 'HWt', 'RVX')) # RVX

LTPRMnR <- mean(LTPRMnRs) # mean
LTPR05R <- mean(LTPR05Rs) # 5th
LTPR10R <- mean(LTPR10Rs) # 10th
LTPR15R <- mean(LTPR15Rs) # 15th
LTPR20R <- mean(LTPR20Rs) # 20th
LTPR25R <- mean(LTPR25Rs) # 25th
LTPR30R <- mean(LTPR30Rs) # 30th
LTPR35R <- mean(LTPR35Rs) # 35th
LTPR40R <- mean(LTPR40Rs) # 40th
LTPR45R <- mean(LTPR45Rs) # 45th
LTPR50R <- mean(LTPR50Rs) # 50th
LTPR55R <- mean(LTPR55Rs) # 55th
LTPR60R <- mean(LTPR60Rs) # 60th
LTPR65R <- mean(LTPR65Rs) # 65th
LTPR70R <- mean(LTPR70Rs) # 70th
LTPR75R <- mean(LTPR75Rs) # 75th
LTPR80R <- mean(LTPR80Rs) # 80th
LTPR85R <- mean(LTPR85Rs) # 85th
LTPR90R <- mean(LTPR90Rs) # 90th
LTPR95R <- mean(LTPR95Rs) # 95th

TPRMnR <- exp(LTPRMnR) # mean
TPR05R <- exp(LTPR05R) # 5th
TPR10R <- exp(LTPR10R) # 10th
TPR15R <- exp(LTPR15R) # 15th
TPR20R <- exp(LTPR20R) # 20th
TPR25R <- exp(LTPR25R) # 25th
TPR30R <- exp(LTPR30R) # 30th
TPR35R <- exp(LTPR35R) # 35th
TPR40R <- exp(LTPR40R) # 40th
TPR45R <- exp(LTPR45R) # 45th
TPR50R <- exp(LTPR50R) # 50th
TPR55R <- exp(LTPR55R) # 55th
TPR60R <- exp(LTPR60R) # 60th
TPR65R <- exp(LTPR65R) # 65th
TPR70R <- exp(LTPR70R) # 70th
TPR75R <- exp(LTPR75R) # 75th
TPR80R <- exp(LTPR80R) # 80th
TPR85R <- exp(LTPR85R) # 85th
TPR90R <- exp(LTPR90R) # 90th
TPR95R <- exp(LTPR95R) # 95th



### LTPR tail-center differences

Md95TMs <- LTPR95Ms-LTPR50Ms # 95th percentile and median (math)
Md95TRs <- LTPR95Rs-LTPR50Rs # 95th percentile and median (reading)
Md90TMs <- LTPR90Ms-LTPR50Ms # 90th percentile and median (math)
Md90TRs <- LTPR90Rs-LTPR50Rs # 90th percentile and median (reading)
Md10TMs <- LTPR50Ms-LTPR10Ms # median and 10th percentile (math)
Md10TRs <- LTPR50Rs-LTPR10Rs # median and 10th percentile (reading)
Md05TMs <- LTPR50Ms-LTPR05Ms # median and 5th percentile (math)
Md05TRs <- LTPR50Rs-LTPR05Rs # median and 5th percentile (reading)
Mn95TMs <- LTPR95Ms-LTPRMnMs # 95th percentile and mean (math)
Mn95TRs <- LTPR95Rs-LTPRMnRs # 95th percentile and mean (reading)
Mn90TMs <- LTPR90Ms-LTPRMnMs # 90th percentile and mean (math)
Mn90TRs <- LTPR90Rs-LTPRMnRs # 90th percentile and mean (reading)
Mn10TMs <- LTPRMnMs-LTPR10Ms # mean and 10th percentile (math)
Mn10TRs <- LTPRMnRs-LTPR10Rs # mean and 10th percentile (reading)
Mn05TMs <- LTPRMnMs-LTPR05Ms # mean and 5th percentile (math)
Mn05TRs <- LTPRMnRs-LTPR05Rs # mean and 5th percentile (reading)

Md95TM <- mean(Md95TMs) # 95th percentile and median (math)
Md95TR <- mean(Md95TRs) # 95th percentile and median (reading)
Md90TM <- mean(Md90TMs) # 90th percentile and median (math)
Md90TR <- mean(Md90TRs) # 90th percentile and median (reading)
Md10TM <- mean(Md10TMs) # median and 10th percentile (math)
Md10TR <- mean(Md10TRs) # median and 10th percentile (reading)
Md05TM <- mean(Md05TMs) # median and 5th percentile (math)
Md05TR <- mean(Md05TRs) # median and 5th percentile (reading)
Mn95TM <- mean(Mn95TMs) # 95th percentile and mean (math)
Mn95TR <- mean(Mn95TRs) # 95th percentile and mean (reading)
Mn90TM <- mean(Mn90TMs) # 90th percentile and mean (math)
Mn90TR <- mean(Mn90TRs) # 90th percentile and mean (reading)
Mn10TM <- mean(Mn10TMs) # mean and 10th percentile (math)
Mn10TR <- mean(Mn10TRs) # mean and 10th percentile (reading)
Mn05TM <- mean(Mn05TMs) # mean and 5th percentile (math)
Mn05TR <- mean(Mn05TRs) # mean and 5th percentile (reading)




############################
##### U3 Ratios (U3Rs) #####
############################

### Math U3Rs and LU3Rs: every 5th percentile from 5 to 95
LU3R05Ms <- c(LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV1'), # MV1 5th
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'MVX')) # MVX
LU3R10Ms <- c(LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV1'), # MV1 10th
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'MVX')) # MVX
LU3R15Ms <- c(LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV1'), # MV1 15th
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'MVX')) # MVX
LU3R20Ms <- c(LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV1'), # MV1 20th
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'MVX')) # MVX
LU3R25Ms <- c(LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV1'), # MV1 25th
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'MVX')) # MVX
LU3R30Ms <- c(LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV1'), # MV1 30th
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'MVX')) # MVX
LU3R35Ms <- c(LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV1'), # MV1 35th
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'MVX')) # MVX
LU3R40Ms <- c(LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV1'), # MV1 40th
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'MVX')) # MVX
LU3R45Ms <- c(LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV1'), # MV1 45th
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'MVX')) # MVX
LU3R50Ms <- c(LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV1'), # MV1 50th
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'MVX')) # MVX
LU3R55Ms <- c(LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV1'), # MV1 55th
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'MVX')) # MVX
LU3R60Ms <- c(LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV1'), # MV1 60th
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'MVX')) # MVX
LU3R65Ms <- c(LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV1'), # MV1 65th
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'MVX')) # MVX
LU3R70Ms <- c(LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV1'), # MV1 70th
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'MVX')) # MVX
LU3R75Ms <- c(LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV1'), # MV1 75th
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'MVX')) # MVX
LU3R80Ms <- c(LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV1'), # MV1 80th
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'MVX')) # MVX
LU3R85Ms <- c(LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV1'), # MV1 85th
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'MVX')) # MVX
LU3R90Ms <- c(LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV1'), # MV1 90th
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'MVX')) # MVX
LU3R95Ms <- c(LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV1'), # MV1 95th
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV2'), # MV2
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV3'), # MV3
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV4'), # MV4
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV5'), # MV5
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV6'), # MV6
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV7'), # MV7
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV8'), # MV8
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MV9'), # MV9
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'MVX')) # MVX

LU3R05M <- mean(LU3R05Ms) # 5th
LU3R10M <- mean(LU3R10Ms) # 10th
LU3R15M <- mean(LU3R15Ms) # 15th
LU3R20M <- mean(LU3R20Ms) # 20th
LU3R25M <- mean(LU3R25Ms) # 25th
LU3R30M <- mean(LU3R30Ms) # 30th
LU3R35M <- mean(LU3R35Ms) # 35th
LU3R40M <- mean(LU3R40Ms) # 40th
LU3R45M <- mean(LU3R45Ms) # 45th
LU3R50M <- mean(LU3R50Ms) # 50th
LU3R55M <- mean(LU3R55Ms) # 55th
LU3R60M <- mean(LU3R60Ms) # 60th
LU3R65M <- mean(LU3R65Ms) # 65th
LU3R70M <- mean(LU3R70Ms) # 70th
LU3R75M <- mean(LU3R75Ms) # 75th
LU3R80M <- mean(LU3R80Ms) # 80th
LU3R85M <- mean(LU3R85Ms) # 85th
LU3R90M <- mean(LU3R90Ms) # 90th
LU3R95M <- mean(LU3R95Ms) # 95th

U3R05M <- exp(LU3R05M) # 5th
U3R10M <- exp(LU3R10M) # 10th
U3R15M <- exp(LU3R15M) # 15th
U3R20M <- exp(LU3R20M) # 20th
U3R25M <- exp(LU3R25M) # 25th
U3R30M <- exp(LU3R30M) # 30th
U3R35M <- exp(LU3R35M) # 35th
U3R40M <- exp(LU3R40M) # 40th
U3R45M <- exp(LU3R45M) # 45th
U3R50M <- exp(LU3R50M) # 50th
U3R55M <- exp(LU3R55M) # 55th
U3R60M <- exp(LU3R60M) # 60th
U3R65M <- exp(LU3R65M) # 65th
U3R70M <- exp(LU3R70M) # 70th
U3R75M <- exp(LU3R75M) # 75th
U3R80M <- exp(LU3R80M) # 80th
U3R85M <- exp(LU3R85M) # 85th
U3R90M <- exp(LU3R90M) # 90th
U3R95M <- exp(LU3R95M) # 95th


### Reading U3Rs and LU3Rs: every 5th percentile from 5 to 95
LU3R05Rs <- c(LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV1'), # RV1 5th
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .05, 'HWt', 'RVX')) # RVX
LU3R10Rs <- c(LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV1'), # RV1 10th
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .10, 'HWt', 'RVX')) # RVX
LU3R15Rs <- c(LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV1'), # RV1 15th
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .15, 'HWt', 'RVX')) # RVX
LU3R20Rs <- c(LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV1'), # RV1 20th
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .20, 'HWt', 'RVX')) # RVX
LU3R25Rs <- c(LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV1'), # RV1 25th
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .25, 'HWt', 'RVX')) # RVX
LU3R30Rs <- c(LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV1'), # RV1 30th
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .30, 'HWt', 'RVX')) # RVX
LU3R35Rs <- c(LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV1'), # RV1 35th
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .35, 'HWt', 'RVX')) # RVX
LU3R40Rs <- c(LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV1'), # RV1 40th
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .40, 'HWt', 'RVX')) # RVX
LU3R45Rs <- c(LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV1'), # RV1 45th
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .45, 'HWt', 'RVX')) # RVX
LU3R50Rs <- c(LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV1'), # RV1 50th
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .50, 'HWt', 'RVX')) # RVX
LU3R55Rs <- c(LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV1'), # RV1 55th
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .55, 'HWt', 'RVX')) # RVX
LU3R60Rs <- c(LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV1'), # RV1 60th
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .60, 'HWt', 'RVX')) # RVX
LU3R65Rs <- c(LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV1'), # RV1 65th
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .65, 'HWt', 'RVX')) # RVX
LU3R70Rs <- c(LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV1'), # RV1 70th
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .70, 'HWt', 'RVX')) # RVX
LU3R75Rs <- c(LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV1'), # RV1 75th
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .75, 'HWt', 'RVX')) # RVX
LU3R80Rs <- c(LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV1'), # RV1 80th
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .80, 'HWt', 'RVX')) # RVX
LU3R85Rs <- c(LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV1'), # RV1 85th
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .85, 'HWt', 'RVX')) # RVX
LU3R90Rs <- c(LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV1'), # RV1 90th
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .90, 'HWt', 'RVX')) # RVX
LU3R95Rs <- c(LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV1'), # RV1 95th
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV2'), # RV2
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV3'), # RV3
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV4'), # RV4
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV5'), # RV5
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV6'), # RV6
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV7'), # RV7
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV8'), # RV8
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RV9'), # RV9
              LU3Rfn(P18_M, P18_F, .95, 'HWt', 'RVX')) # RVX

LU3R05R <- mean(LU3R05Rs) # 5th
LU3R10R <- mean(LU3R10Rs) # 10th
LU3R15R <- mean(LU3R15Rs) # 15th
LU3R20R <- mean(LU3R20Rs) # 20th
LU3R25R <- mean(LU3R25Rs) # 25th
LU3R30R <- mean(LU3R30Rs) # 30th
LU3R35R <- mean(LU3R35Rs) # 35th
LU3R40R <- mean(LU3R40Rs) # 40th
LU3R45R <- mean(LU3R45Rs) # 45th
LU3R50R <- mean(LU3R50Rs) # 50th
LU3R55R <- mean(LU3R55Rs) # 55th
LU3R60R <- mean(LU3R60Rs) # 60th
LU3R65R <- mean(LU3R65Rs) # 65th
LU3R70R <- mean(LU3R70Rs) # 70th
LU3R75R <- mean(LU3R75Rs) # 75th
LU3R80R <- mean(LU3R80Rs) # 80th
LU3R85R <- mean(LU3R85Rs) # 85th
LU3R90R <- mean(LU3R90Rs) # 90th
LU3R95R <- mean(LU3R95Rs) # 95th

U3R05R <- exp(LU3R05R) # 5th
U3R10R <- exp(LU3R10R) # 10th
U3R15R <- exp(LU3R15R) # 15th
U3R20R <- exp(LU3R20R) # 20th
U3R25R <- exp(LU3R25R) # 25th
U3R30R <- exp(LU3R30R) # 30th
U3R35R <- exp(LU3R35R) # 35th
U3R40R <- exp(LU3R40R) # 40th
U3R45R <- exp(LU3R45R) # 45th
U3R50R <- exp(LU3R50R) # 50th
U3R55R <- exp(LU3R55R) # 55th
U3R60R <- exp(LU3R60R) # 60th
U3R65R <- exp(LU3R65R) # 65th
U3R70R <- exp(LU3R70R) # 70th
U3R75R <- exp(LU3R75R) # 75th
U3R80R <- exp(LU3R80R) # 80th
U3R85R <- exp(LU3R85R) # 85th
U3R90R <- exp(LU3R90R) # 90th
U3R95R <- exp(LU3R95R) # 95th



### LU3R tail-center differences

Md95UMs <- LU3R95Ms-LU3R50Ms # 95th percentile and median (math)
Md95URs <- LU3R95Rs-LU3R50Rs # 95th percentile and median (reading)
Md90UMs <- LU3R90Ms-LU3R50Ms # 90th percentile and median (math)
Md90URs <- LU3R90Rs-LU3R50Rs # 90th percentile and median (reading)
Md10UMs <- LU3R50Ms-LU3R10Ms # median and 10th percentile (math)
Md10URs <- LU3R50Rs-LU3R10Rs # median and 10th percentile (reading)
Md05UMs <- LU3R50Ms-LU3R05Ms # median and 5th percentile (math)
Md05URs <- LU3R50Rs-LU3R05Rs # median and 5th percentile (reading)

Md95UM <- mean(Md95UMs) # 95th percentile and median (math)
Md95UR <- mean(Md95URs) # 95th percentile and median (reading)
Md90UM <- mean(Md90UMs) # 90th percentile and median (math)
Md90UR <- mean(Md90URs) # 90th percentile and median (reading)
Md10UM <- mean(Md10UMs) # median and 10th percentile (math)
Md10UR <- mean(Md10URs) # median and 10th percentile (reading)
Md05UM <- mean(Md05UMs) # median and 5th percentile (math)
Md05UR <- mean(Md05URs) # median and 5th percentile (reading)




##############################
##### Other Effect Sizes #####
##############################

### Cohen's d

dMs <- c(dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1'), # MV1
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2'), # MV2
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3'), # MV3
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4'), # MV4
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5'), # MV5
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6'), # MV6
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7'), # MV7
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8'), # MV8
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9'), # MV9
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX')) # MVX

dM <- mean(dMs)

dRs <- c(dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1'), # RV1
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2'), # RV2
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3'), # RV3
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4'), # RV4
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5'), # RV5
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6'), # RV6
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7'), # RV7
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8'), # RV8
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9'), # RV9
         dfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX')) # RVX

dR <- mean(dRs)


### U3

U3Ms <- c(U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1'), # MV1
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2'), # MV2
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3'), # MV3
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4'), # MV4
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5'), # MV5
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6'), # MV6
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7'), # MV7
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8'), # MV8
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9'), # MV9
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX')) # MVX

U3M <- mean(U3Ms)

U3Rs <- c(U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1'), # RV1
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2'), # RV2
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3'), # RV3
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4'), # RV4
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5'), # RV5
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6'), # RV6
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7'), # RV7
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8'), # RV8
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9'), # RV9
          U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX')) # RVX

U3R <- mean(U3Rs)


### Probability of superiority (PS)

PSMs <- c(PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1'), # MV1
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2'), # MV2
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3'), # MV3
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4'), # MV4
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5'), # MV5
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6'), # MV6
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7'), # MV7
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8'), # MV8
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9'), # MV9
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX')) # MVX

PSM <- mean(PSMs)

PSRs <- c(PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1'), # RV1
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2'), # RV2
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3'), # RV3
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4'), # RV4
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5'), # RV5
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6'), # RV6
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7'), # RV7
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8'), # RV8
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9'), # RV9
          PSfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX')) # RVX

PSR <- mean(PSRs)


### Variance ratio (VR), Log-transformed VR (LVR)

LVRMs <- c(LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1'), # MV1
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2'), # MV2
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3'), # MV3
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4'), # MV4
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5'), # MV5
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6'), # MV6
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7'), # MV7
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8'), # MV8
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9'), # MV9
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX')) # MVX

LVRM <- mean(LVRMs)
VRM <- exp(LVRM)

LVRRs <- c(LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1'), # RV1
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2'), # RV2
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3'), # RV3
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4'), # RV4
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5'), # RV5
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6'), # RV6
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7'), # RV7
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8'), # RV8
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9'), # RV9
           LVRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX')) # RVX

LVRR <- mean(LVRRs)
VRR <- exp(LVRR)


### Left and right VR and LVR (VR_L, VR_R, LVR_L, LVR_R)

LVRM_Ls <- c(LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1', t = 'L'), # MV1
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2', t = 'L'), # MV2
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3', t = 'L'), # MV3
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4', t = 'L'), # MV4
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5', t = 'L'), # MV5
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6', t = 'L'), # MV6
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7', t = 'L'), # MV7
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8', t = 'L'), # MV8
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9', t = 'L'), # MV9
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX', t = 'L')) # MVX

LVRM_L <- mean(LVRM_Ls)
VRM_L <- exp(LVRM_L)

LVRR_Ls <- c(LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1', t = 'L'), # RV1
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2', t = 'L'), # RV2
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3', t = 'L'), # RV3
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4', t = 'L'), # RV4
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5', t = 'L'), # RV5
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6', t = 'L'), # RV6
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7', t = 'L'), # RV7
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8', t = 'L'), # RV8
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9', t = 'L'), # RV9
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX', t = 'L')) # RVX

LVRR_L <- mean(LVRR_Ls)
VRR_L <- exp(LVRR_L)

LVRM_Rs <- c(LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1', t = 'R'), # MV1
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2', t = 'R'), # MV2
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3', t = 'R'), # MV3
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4', t = 'R'), # MV4
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5', t = 'R'), # MV5
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6', t = 'R'), # MV6
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7', t = 'R'), # MV7
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8', t = 'R'), # MV8
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9', t = 'R'), # MV9
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX', t = 'R')) # MVX

LVRM_R <- mean(LVRM_Rs)
VRM_R <- exp(LVRM_R)

LVRR_Rs <- c(LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1', t = 'R'), # RV1
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2', t = 'R'), # RV2
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3', t = 'R'), # RV3
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4', t = 'R'), # RV4
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5', t = 'R'), # RV5
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6', t = 'R'), # RV6
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7', t = 'R'), # RV7
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8', t = 'R'), # RV8
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9', t = 'R'), # RV9
             LVR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX', t = 'R')) # RVX

LVRR_R <- mean(LVRR_Rs)
VRR_R <- exp(LVRR_R)


### Mean absolute deviation (from the median) ratio (MADR), Log-transformed MADR (LMADR)

LMADRMs <- c(LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1'), # MV1
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2'), # MV2
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3'), # MV3
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4'), # MV4
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5'), # MV5
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6'), # MV6
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7'), # MV7
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8'), # MV8
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9'), # MV9
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX')) # MVX

LMADRM <- mean(LMADRMs)
MADRM <- exp(LMADRM)

LMADRRs <- c(LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1'), # RV1
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2'), # RV2
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3'), # RV3
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4'), # RV4
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5'), # RV5
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6'), # RV6
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7'), # RV7
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8'), # RV8
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9'), # RV9
             LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX')) # RVX

LMADRR <- mean(LMADRRs)
MADRR <- exp(LMADRR)


### Left and right MADR and LMADR (MADR_L, MADR_R, LMADR_L, LMADR_R)

LMADRM_Ls <- c(LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1', t = 'L'), # MV1
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2', t = 'L'), # MV2
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3', t = 'L'), # MV3
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4', t = 'L'), # MV4
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5', t = 'L'), # MV5
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6', t = 'L'), # MV6
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7', t = 'L'), # MV7
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8', t = 'L'), # MV8
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9', t = 'L'), # MV9
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX', t = 'L')) # MVX

LMADRM_L <- mean(LMADRM_Ls)
MADRM_L <- exp(LMADRM_L)

LMADRR_Ls <- c(LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1', t = 'L'), # RV1
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2', t = 'L'), # RV2
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3', t = 'L'), # RV3
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4', t = 'L'), # RV4
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5', t = 'L'), # RV5
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6', t = 'L'), # RV6
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7', t = 'L'), # RV7
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8', t = 'L'), # RV8
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9', t = 'L'), # RV9
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX', t = 'L')) # RVX

LMADRR_L <- mean(LMADRR_Ls)
MADRR_L <- exp(LMADRR_L)

LMADRM_Rs <- c(LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1', t = 'R'), # MV1
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2', t = 'R'), # MV2
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3', t = 'R'), # MV3
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4', t = 'R'), # MV4
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5', t = 'R'), # MV5
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6', t = 'R'), # MV6
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7', t = 'R'), # MV7
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8', t = 'R'), # MV8
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9', t = 'R'), # MV9
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX', t = 'R')) # MVX

LMADRM_R <- mean(LMADRM_Rs)
MADRM_R <- exp(LMADRM_R)

LMADRR_Rs <- c(LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1', t = 'R'), # RV1
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2', t = 'R'), # RV2
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3', t = 'R'), # RV3
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4', t = 'R'), # RV4
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5', t = 'R'), # RV5
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6', t = 'R'), # RV6
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7', t = 'R'), # RV7
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8', t = 'R'), # RV8
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9', t = 'R'), # RV9
               LMADR_Tfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX', t = 'R')) # RVX

LMADRR_R <- mean(LMADRR_Rs)
MADRR_R <- exp(LMADRR_R)


### Gini's mean difference ratio (GMDR), Log-transformed GMDR (LGMDR)

LGMDRMs <- c(LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV1'), # MV1
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV2'), # MV2
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV3'), # MV3
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV4'), # MV4
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV5'), # MV5
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV6'), # MV6
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV7'), # MV7
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV8'), # MV8
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MV9'), # MV9
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'MVX')) # MVX

LGMDRM <- mean(LGMDRMs)
GMDRM <- exp(LGMDRM)

LGMDRRs <- c(LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV1'), # RV1
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV2'), # RV2
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV3'), # RV3
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV4'), # RV4
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV5'), # RV5
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV6'), # RV6
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV7'), # RV7
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV8'), # RV8
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RV9'), # RV9
             LGMDRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'RVX')) # RVX

LGMDRR <- mean(LGMDRRs)
GMDRR <- exp(LGMDRR)




###############################################
##### Other Effect Sizes Adjusted for Age #####
###############################################

A18 <- P18[!is.na(P18$Age),] # P18 with Age NAs (if any) excluded

A18_F <- A18[which(A18$Sex == 1),] # female subset
A18_M <- A18[which(A18$Sex == 2),] # male subset

# sex differences in age distribution
AgeU3 <- U3fn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'Age')
AgeLMADR <- LMADRfn(d1 = P18_M, d2 = P18_F, w = 'HWt', v = 'Age')
AgeMADR <- exp(AgeLMADR)

# control for the correlation between age and score
SlopeM1 <- lm(formula = MV1 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV1
SlopeM2 <- lm(formula = MV2 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV2
SlopeM3 <- lm(formula = MV3 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV3
SlopeM4 <- lm(formula = MV4 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV4
SlopeM5 <- lm(formula = MV5 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV5
SlopeM6 <- lm(formula = MV6 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV6
SlopeM7 <- lm(formula = MV7 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV7
SlopeM8 <- lm(formula = MV8 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV8
SlopeM9 <- lm(formula = MV9 ~ Age, data = A18, weights = HWt)$coefficients[2] # MV9
SlopeMX <- lm(formula = MVX ~ Age, data = A18, weights = HWt)$coefficients[2] # MVX
SlopeR1 <- lm(formula = RV1 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV1
SlopeR2 <- lm(formula = RV2 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV2
SlopeR3 <- lm(formula = RV3 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV3
SlopeR4 <- lm(formula = RV4 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV4
SlopeR5 <- lm(formula = RV5 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV5
SlopeR6 <- lm(formula = RV6 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV6
SlopeR7 <- lm(formula = RV7 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV7
SlopeR8 <- lm(formula = RV8 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV8
SlopeR9 <- lm(formula = RV9 ~ Age, data = A18, weights = HWt)$coefficients[2] # RV9
SlopeRX <- lm(formula = RVX ~ Age, data = A18, weights = HWt)$coefficients[2] # RVX

AgeMn <- wt.mn(x = A18$Age, w = A18$HWt) # mean age

# if there are Age NAs, replace with mean age so their nominally age-corrected scores do not change
if (length(P18[is.na(P18$Age),'Age']) != 0) {P18[is.na(P18$Age),'Age'] <- AgeMn}

# new scores linearly corrected for age
MV1A <- P18$MV1+(AgeMn-P18$Age)*SlopeM1 # MV1
MV2A <- P18$MV2+(AgeMn-P18$Age)*SlopeM2 # MV2
MV3A <- P18$MV3+(AgeMn-P18$Age)*SlopeM3 # MV3
MV4A <- P18$MV4+(AgeMn-P18$Age)*SlopeM4 # MV4
MV5A <- P18$MV5+(AgeMn-P18$Age)*SlopeM5 # MV5
MV6A <- P18$MV6+(AgeMn-P18$Age)*SlopeM6 # MV6
MV7A <- P18$MV7+(AgeMn-P18$Age)*SlopeM7 # MV7
MV8A <- P18$MV8+(AgeMn-P18$Age)*SlopeM8 # MV8
MV9A <- P18$MV9+(AgeMn-P18$Age)*SlopeM9 # MV9
MVXA <- P18$MVX+(AgeMn-P18$Age)*SlopeMX # MVX
RV1A <- P18$RV1+(AgeMn-P18$Age)*SlopeR1 # RV1
RV2A <- P18$RV2+(AgeMn-P18$Age)*SlopeR2 # RV2
RV3A <- P18$RV3+(AgeMn-P18$Age)*SlopeR3 # RV3
RV4A <- P18$RV4+(AgeMn-P18$Age)*SlopeR4 # RV4
RV5A <- P18$RV5+(AgeMn-P18$Age)*SlopeR5 # RV5
RV6A <- P18$RV6+(AgeMn-P18$Age)*SlopeR6 # RV6
RV7A <- P18$RV7+(AgeMn-P18$Age)*SlopeR7 # RV7
RV8A <- P18$RV8+(AgeMn-P18$Age)*SlopeR8 # RV8
RV9A <- P18$RV9+(AgeMn-P18$Age)*SlopeR9 # RV9
RVXA <- P18$RVX+(AgeMn-P18$Age)*SlopeRX # RVX

# P18 with age-corrected scores
A18 <- data.frame(P18, MV1A, MV2A, MV3A, MV4A, MV5A, MV6A, MV7A, MV8A, MV9A, MVXA,
                  RV1A, RV2A, RV3A, RV4A, RV5A, RV6A, RV7A, RV8A, RV9A, RVXA)
A18_F <- A18[which(A18$Sex == 1),] # female subset
A18_M <- A18[which(A18$Sex == 2),] # male subset


### Age-corrected Cohen's d (d_A)

dM_A <- mean(c(dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A'),  # MV1
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A'),  # MV2
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A'),  # MV3
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A'),  # MV4
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A'),  # MV5
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A'),  # MV6
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A'),  # MV7
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A'),  # MV8
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A'),  # MV9
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA'))) # MVX

dR_A <- mean(c(dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A'),  # RV1
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A'),  # RV2
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A'),  # RV3
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A'),  # RV4
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A'),  # RV5
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A'),  # RV6
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A'),  # RV7
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A'),  # RV8
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A'),  # RV9
               dfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA'))) # RVX


### Age-corrected U3 (U3_A)

U3M_A <- mean(c(U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A'),  # MV1
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A'),  # MV2
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A'),  # MV3
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A'),  # MV4
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A'),  # MV5
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A'),  # MV6
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A'),  # MV7
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A'),  # MV8
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A'),  # MV9
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA'))) # MVX

U3R_A <- mean(c(U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A'),  # RV1
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A'),  # RV2
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A'),  # RV3
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A'),  # RV4
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A'),  # RV5
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A'),  # RV6
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A'),  # RV7
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A'),  # RV8
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A'),  # RV9
                U3fn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA'))) # RVX


### Age-corrected Probability of superiority (PS_A)

PSM_A <- mean(c(PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A'),  # MV1
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A'),  # MV2
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A'),  # MV3
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A'),  # MV4
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A'),  # MV5
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A'),  # MV6
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A'),  # MV7
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A'),  # MV8
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A'),  # MV9
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA'))) # MVX

PSR_A <- mean(c(PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A'),  # RV1
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A'),  # RV2
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A'),  # RV3
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A'),  # RV4
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A'),  # RV5
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A'),  # RV6
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A'),  # RV7
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A'),  # RV8
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A'),  # RV9
                PSfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA'))) # RVX


### Age-corrected Variance ratio (VR_A), Log-transformed VR (LVR_A)

LVRM_A <- mean(c(LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A'),  # MV1
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A'),  # MV2
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A'),  # MV3
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A'),  # MV4
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A'),  # MV5
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A'),  # MV6
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A'),  # MV7
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A'),  # MV8
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A'),  # MV9
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA'))) # MVX

VRM_A <- exp(LVRM_A)

LVRR_A <- mean(c(LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A'),  # RV1
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A'),  # RV2
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A'),  # RV3
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A'),  # RV4
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A'),  # RV5
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A'),  # RV6
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A'),  # RV7
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A'),  # RV8
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A'),  # RV9
                 LVRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA'))) # RVX

VRR_A <- exp(LVRR_A)


### Age-corrected Left and right VR and LVR (VR_LA, VR_RA, LVR_LA, LVR_RA)

LVRM_LA <- mean(c(LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A', t = 'L'),  # MV1
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A', t = 'L'),  # MV2
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A', t = 'L'),  # MV3
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A', t = 'L'),  # MV4
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A', t = 'L'),  # MV5
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A', t = 'L'),  # MV6
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A', t = 'L'),  # MV7
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A', t = 'L'),  # MV8
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A', t = 'L'),  # MV9
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA', t = 'L'))) # MVX

VRM_LA <- exp(LVRM_LA)

LVRR_LA <- mean(c(LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A', t = 'L'),  # RV1
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A', t = 'L'),  # RV2
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A', t = 'L'),  # RV3
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A', t = 'L'),  # RV4
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A', t = 'L'),  # RV5
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A', t = 'L'),  # RV6
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A', t = 'L'),  # RV7
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A', t = 'L'),  # RV8
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A', t = 'L'),  # RV9
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA', t = 'L'))) # RVX

VRR_LA <- exp(LVRR_LA)

LVRM_RA <- mean(c(LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A', t = 'R'),  # MV1
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A', t = 'R'),  # MV2
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A', t = 'R'),  # MV3
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A', t = 'R'),  # MV4
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A', t = 'R'),  # MV5
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A', t = 'R'),  # MV6
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A', t = 'R'),  # MV7
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A', t = 'R'),  # MV8
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A', t = 'R'),  # MV9
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA', t = 'R'))) # MVX

VRM_RA <- exp(LVRM_RA)

LVRR_RA <- mean(c(LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A', t = 'R'),  # RV1
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A', t = 'R'),  # RV2
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A', t = 'R'),  # RV3
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A', t = 'R'),  # RV4
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A', t = 'R'),  # RV5
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A', t = 'R'),  # RV6
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A', t = 'R'),  # RV7
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A', t = 'R'),  # RV8
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A', t = 'R'),  # RV9
                  LVR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA', t = 'R'))) # RVX

VRR_RA <- exp(LVRR_RA)


### Age-corrected Mean absolute deviation ratio (MADR_A), Log-transformed MADR (LMADR_A)

LMADRM_A <- mean(c(LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A'),  # MV1
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A'),  # MV2
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A'),  # MV3
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A'),  # MV4
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A'),  # MV5
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A'),  # MV6
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A'),  # MV7
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A'),  # MV8
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A'),  # MV9
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA'))) # MVX

MADRM_A <- exp(LMADRM_A)

LMADRR_A <- mean(c(LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A'),  # RV1
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A'),  # RV2
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A'),  # RV3
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A'),  # RV4
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A'),  # RV5
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A'),  # RV6
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A'),  # RV7
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A'),  # RV8
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A'),  # RV9
                   LMADRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA'))) # RVX

MADRR_A <- exp(LMADRR_A)


### Age-corrected Left and right MADR and LMADR (MADR_LA, MADR_RA, LMADR_LA, LMADR_RA)

LMADRM_LA <- mean(c(LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A', t = 'L'),  # MV1
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A', t = 'L'),  # MV2
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A', t = 'L'),  # MV3
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A', t = 'L'),  # MV4
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A', t = 'L'),  # MV5
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A', t = 'L'),  # MV6
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A', t = 'L'),  # MV7
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A', t = 'L'),  # MV8
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A', t = 'L'),  # MV9
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA', t = 'L'))) # MVX

MADRM_LA <- exp(LMADRM_LA)

LMADRR_LA <- mean(c(LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A', t = 'L'),  # RV1
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A', t = 'L'),  # RV2
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A', t = 'L'),  # RV3
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A', t = 'L'),  # RV4
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A', t = 'L'),  # RV5
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A', t = 'L'),  # RV6
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A', t = 'L'),  # RV7
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A', t = 'L'),  # RV8
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A', t = 'L'),  # RV9
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA', t = 'L'))) # RVX

MADRR_LA <- exp(LMADRR_LA)

LMADRM_RA <- mean(c(LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A', t = 'R'),  # MV1
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A', t = 'R'),  # MV2
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A', t = 'R'),  # MV3
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A', t = 'R'),  # MV4
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A', t = 'R'),  # MV5
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A', t = 'R'),  # MV6
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A', t = 'R'),  # MV7
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A', t = 'R'),  # MV8
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A', t = 'R'),  # MV9
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA', t = 'R'))) # MVX

MADRM_RA <- exp(LMADRM_RA)

LMADRR_RA <- mean(c(LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A', t = 'R'),  # RV1
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A', t = 'R'),  # RV2
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A', t = 'R'),  # RV3
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A', t = 'R'),  # RV4
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A', t = 'R'),  # RV5
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A', t = 'R'),  # RV6
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A', t = 'R'),  # RV7
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A', t = 'R'),  # RV8
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A', t = 'R'),  # RV9
                    LMADR_Tfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA', t = 'R'))) # RVX

MADRR_RA <- exp(LMADRR_RA)


### Age-corrected Gini's mean difference ratio (GMDR), Log-transformed GMDR (LGMDR)

LGMDRM_A <- mean(c(LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV1A'),  # MV1
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV2A'),  # MV2
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV3A'),  # MV3
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV4A'),  # MV4
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV5A'),  # MV5
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV6A'),  # MV6
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV7A'),  # MV7
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV8A'),  # MV8
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MV9A'),  # MV9
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'MVXA'))) # MVX

GMDRM_A <- exp(LGMDRM_A)

LGMDRR_A <- mean(c(LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV1A'),  # RV1
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV2A'),  # RV2
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV3A'),  # RV3
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV4A'),  # RV4
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV5A'),  # RV5
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV6A'),  # RV6
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV7A'),  # RV7
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV8A'),  # RV8
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RV9A'),  # RV9
                   LGMDRfn(d1 = A18_M, d2 = A18_F, w = 'HWt', v = 'RVXA'))) # RVX

GMDRR_A <- exp(LGMDRR_A)




###########################
##### Standard Errors #####
###########################

#### SEs: Means and Medians ####

A1 <- A2 <- A3 <- A4 <- A5 <- A6 <- A7 <- A8 <- A9 <- AX <- numeric(80) # empty containers
B1 <- B2 <- B3 <- B4 <- B5 <- B6 <- B7 <- B8 <- B9 <- BX <- numeric(80) # empty containers
C1 <- C2 <- C3 <- C4 <- C5 <- C6 <- C7 <- C8 <- C9 <- CX <- numeric(80) # empty containers
D1 <- D2 <- D3 <- D4 <- D5 <- D6 <- D7 <- D8 <- D9 <- DX <- numeric(80) # empty containers
E1 <- E2 <- E3 <- E4 <- E5 <- E6 <- E7 <- E8 <- E9 <- EX <- numeric(80) # empty containers
F1 <- F2 <- F3 <- F4 <- F5 <- F6 <- F7 <- F8 <- F9 <- FX <- numeric(80) # empty containers
G1 <- G2 <- G3 <- G4 <- G5 <- G6 <- G7 <- G8 <- G9 <- GX <- numeric(80) # empty containers
H1 <- H2 <- H3 <- H4 <- H5 <- H6 <- H7 <- H8 <- H9 <- HX <- numeric(80) # empty containers
I1 <- I2 <- I3 <- I4 <- I5 <- I6 <- I7 <- I8 <- I9 <- IX <- numeric(80) # empty containers
J1 <- J2 <- J3 <- J4 <- J5 <- J6 <- J7 <- J8 <- J9 <- JX <- numeric(80) # empty containers
K1 <- K2 <- K3 <- K4 <- K5 <- K6 <- K7 <- K8 <- K9 <- KX <- numeric(80) # empty containers
L1 <- L2 <- L3 <- L4 <- L5 <- L6 <- L7 <- L8 <- L9 <- LX <- numeric(80) # empty containers

# perform bootstrap resampling of means and medians
for (i in 1:80) {
  W <- paste0('BW',i)
  
  A1[i] <- wt.mn(P18$MV1, P18[,W]) # MV1 mean (total math)
  A2[i] <- wt.mn(P18$MV2, P18[,W]) # MV2
  A3[i] <- wt.mn(P18$MV3, P18[,W]) # MV3
  A4[i] <- wt.mn(P18$MV4, P18[,W]) # MV4
  A5[i] <- wt.mn(P18$MV5, P18[,W]) # MV5
  A6[i] <- wt.mn(P18$MV6, P18[,W]) # MV6
  A7[i] <- wt.mn(P18$MV7, P18[,W]) # MV7
  A8[i] <- wt.mn(P18$MV8, P18[,W]) # MV8
  A9[i] <- wt.mn(P18$MV9, P18[,W]) # MV9
  AX[i] <- wt.mn(P18$MVX, P18[,W]) # MVX
  B1[i] <- wt.mn(P18$RV1, P18[,W]) # RV1 mean (total reading)
  B2[i] <- wt.mn(P18$RV2, P18[,W]) # RV2
  B3[i] <- wt.mn(P18$RV3, P18[,W]) # RV3
  B4[i] <- wt.mn(P18$RV4, P18[,W]) # RV4
  B5[i] <- wt.mn(P18$RV5, P18[,W]) # RV5
  B6[i] <- wt.mn(P18$RV6, P18[,W]) # RV6
  B7[i] <- wt.mn(P18$RV7, P18[,W]) # RV7
  B8[i] <- wt.mn(P18$RV8, P18[,W]) # RV8
  B9[i] <- wt.mn(P18$RV9, P18[,W]) # RV9
  BX[i] <- wt.mn(P18$RVX, P18[,W]) # RVX
  
  C1[i] <- wt.qnt(P18$MV1, P18[,W], .5) # MV1 median (total math)
  C2[i] <- wt.qnt(P18$MV2, P18[,W], .5) # MV2
  C3[i] <- wt.qnt(P18$MV3, P18[,W], .5) # MV3
  C4[i] <- wt.qnt(P18$MV4, P18[,W], .5) # MV4
  C5[i] <- wt.qnt(P18$MV5, P18[,W], .5) # MV5
  C6[i] <- wt.qnt(P18$MV6, P18[,W], .5) # MV6
  C7[i] <- wt.qnt(P18$MV7, P18[,W], .5) # MV7
  C8[i] <- wt.qnt(P18$MV8, P18[,W], .5) # MV8
  C9[i] <- wt.qnt(P18$MV9, P18[,W], .5) # MV9
  CX[i] <- wt.qnt(P18$MVX, P18[,W], .5) # MVX
  D1[i] <- wt.qnt(P18$RV1, P18[,W], .5) # RV1 median (total reading)
  D2[i] <- wt.qnt(P18$RV2, P18[,W], .5) # RV2
  D3[i] <- wt.qnt(P18$RV3, P18[,W], .5) # RV3
  D4[i] <- wt.qnt(P18$RV4, P18[,W], .5) # RV4
  D5[i] <- wt.qnt(P18$RV5, P18[,W], .5) # RV5
  D6[i] <- wt.qnt(P18$RV6, P18[,W], .5) # RV6
  D7[i] <- wt.qnt(P18$RV7, P18[,W], .5) # RV7
  D8[i] <- wt.qnt(P18$RV8, P18[,W], .5) # RV8
  D9[i] <- wt.qnt(P18$RV9, P18[,W], .5) # RV9
  DX[i] <- wt.qnt(P18$RVX, P18[,W], .5) # RVX
  
  E1[i] <- wt.mn(P18_F$MV1, P18_F[,W]) # MV1 mean (female math)
  E2[i] <- wt.mn(P18_F$MV2, P18_F[,W]) # MV2
  E3[i] <- wt.mn(P18_F$MV3, P18_F[,W]) # MV3
  E4[i] <- wt.mn(P18_F$MV4, P18_F[,W]) # MV4
  E5[i] <- wt.mn(P18_F$MV5, P18_F[,W]) # MV5
  E6[i] <- wt.mn(P18_F$MV6, P18_F[,W]) # MV6
  E7[i] <- wt.mn(P18_F$MV7, P18_F[,W]) # MV7
  E8[i] <- wt.mn(P18_F$MV8, P18_F[,W]) # MV8
  E9[i] <- wt.mn(P18_F$MV9, P18_F[,W]) # MV9
  EX[i] <- wt.mn(P18_F$MVX, P18_F[,W]) # MVX
  F1[i] <- wt.mn(P18_F$RV1, P18_F[,W]) # RV1 mean (female reading)
  F2[i] <- wt.mn(P18_F$RV2, P18_F[,W]) # RV2
  F3[i] <- wt.mn(P18_F$RV3, P18_F[,W]) # RV3
  F4[i] <- wt.mn(P18_F$RV4, P18_F[,W]) # RV4
  F5[i] <- wt.mn(P18_F$RV5, P18_F[,W]) # RV5
  F6[i] <- wt.mn(P18_F$RV6, P18_F[,W]) # RV6
  F7[i] <- wt.mn(P18_F$RV7, P18_F[,W]) # RV7
  F8[i] <- wt.mn(P18_F$RV8, P18_F[,W]) # RV8
  F9[i] <- wt.mn(P18_F$RV9, P18_F[,W]) # RV9
  FX[i] <- wt.mn(P18_F$RVX, P18_F[,W]) # RVX
  
  G1[i] <- wt.qnt(P18_F$MV1, P18_F[,W], .5) # MV1 median (female math)
  G2[i] <- wt.qnt(P18_F$MV2, P18_F[,W], .5) # MV2
  G3[i] <- wt.qnt(P18_F$MV3, P18_F[,W], .5) # MV3
  G4[i] <- wt.qnt(P18_F$MV4, P18_F[,W], .5) # MV4
  G5[i] <- wt.qnt(P18_F$MV5, P18_F[,W], .5) # MV5
  G6[i] <- wt.qnt(P18_F$MV6, P18_F[,W], .5) # MV6
  G7[i] <- wt.qnt(P18_F$MV7, P18_F[,W], .5) # MV7
  G8[i] <- wt.qnt(P18_F$MV8, P18_F[,W], .5) # MV8
  G9[i] <- wt.qnt(P18_F$MV9, P18_F[,W], .5) # MV9
  GX[i] <- wt.qnt(P18_F$MVX, P18_F[,W], .5) # MVX
  H1[i] <- wt.qnt(P18_F$RV1, P18_F[,W], .5) # RV1 median (female reading)
  H2[i] <- wt.qnt(P18_F$RV2, P18_F[,W], .5) # RV2
  H3[i] <- wt.qnt(P18_F$RV3, P18_F[,W], .5) # RV3
  H4[i] <- wt.qnt(P18_F$RV4, P18_F[,W], .5) # RV4
  H5[i] <- wt.qnt(P18_F$RV5, P18_F[,W], .5) # RV5
  H6[i] <- wt.qnt(P18_F$RV6, P18_F[,W], .5) # RV6
  H7[i] <- wt.qnt(P18_F$RV7, P18_F[,W], .5) # RV7
  H8[i] <- wt.qnt(P18_F$RV8, P18_F[,W], .5) # RV8
  H9[i] <- wt.qnt(P18_F$RV9, P18_F[,W], .5) # RV9
  HX[i] <- wt.qnt(P18_F$RVX, P18_F[,W], .5) # RVX
  
  I1[i] <- wt.mn(P18_M$MV1, P18_M[,W]) # MV1 mean (male math)
  I2[i] <- wt.mn(P18_M$MV2, P18_M[,W]) # MV2
  I3[i] <- wt.mn(P18_M$MV3, P18_M[,W]) # MV3
  I4[i] <- wt.mn(P18_M$MV4, P18_M[,W]) # MV4
  I5[i] <- wt.mn(P18_M$MV5, P18_M[,W]) # MV5
  I6[i] <- wt.mn(P18_M$MV6, P18_M[,W]) # MV6
  I7[i] <- wt.mn(P18_M$MV7, P18_M[,W]) # MV7
  I8[i] <- wt.mn(P18_M$MV8, P18_M[,W]) # MV8
  I9[i] <- wt.mn(P18_M$MV9, P18_M[,W]) # MV9
  IX[i] <- wt.mn(P18_M$MVX, P18_M[,W]) # MVX
  J1[i] <- wt.mn(P18_M$RV1, P18_M[,W]) # RV1 mean (male reading)
  J2[i] <- wt.mn(P18_M$RV2, P18_M[,W]) # RV2
  J3[i] <- wt.mn(P18_M$RV3, P18_M[,W]) # RV3
  J4[i] <- wt.mn(P18_M$RV4, P18_M[,W]) # RV4
  J5[i] <- wt.mn(P18_M$RV5, P18_M[,W]) # RV5
  J6[i] <- wt.mn(P18_M$RV6, P18_M[,W]) # RV6
  J7[i] <- wt.mn(P18_M$RV7, P18_M[,W]) # RV7
  J8[i] <- wt.mn(P18_M$RV8, P18_M[,W]) # RV8
  J9[i] <- wt.mn(P18_M$RV9, P18_M[,W]) # RV9
  JX[i] <- wt.mn(P18_M$RVX, P18_M[,W]) # RVX
  
  K1[i] <- wt.qnt(P18_M$MV1, P18_M[,W], .5) # MV1 median (male math)
  K2[i] <- wt.qnt(P18_M$MV2, P18_M[,W], .5) # MV2
  K3[i] <- wt.qnt(P18_M$MV3, P18_M[,W], .5) # MV3
  K4[i] <- wt.qnt(P18_M$MV4, P18_M[,W], .5) # MV4
  K5[i] <- wt.qnt(P18_M$MV5, P18_M[,W], .5) # MV5
  K6[i] <- wt.qnt(P18_M$MV6, P18_M[,W], .5) # MV6
  K7[i] <- wt.qnt(P18_M$MV7, P18_M[,W], .5) # MV7
  K8[i] <- wt.qnt(P18_M$MV8, P18_M[,W], .5) # MV8
  K9[i] <- wt.qnt(P18_M$MV9, P18_M[,W], .5) # MV9
  KX[i] <- wt.qnt(P18_M$MVX, P18_M[,W], .5) # MVX
  L1[i] <- wt.qnt(P18_M$RV1, P18_M[,W], .5) # RV1 median (male reading)
  L2[i] <- wt.qnt(P18_M$RV2, P18_M[,W], .5) # RV2
  L3[i] <- wt.qnt(P18_M$RV3, P18_M[,W], .5) # RV3
  L4[i] <- wt.qnt(P18_M$RV4, P18_M[,W], .5) # RV4
  L5[i] <- wt.qnt(P18_M$RV5, P18_M[,W], .5) # RV5
  L6[i] <- wt.qnt(P18_M$RV6, P18_M[,W], .5) # RV6
  L7[i] <- wt.qnt(P18_M$RV7, P18_M[,W], .5) # RV7
  L8[i] <- wt.qnt(P18_M$RV8, P18_M[,W], .5) # RV8
  L9[i] <- wt.qnt(P18_M$RV9, P18_M[,W], .5) # RV9
  LX[i] <- wt.qnt(P18_M$RVX, P18_M[,W], .5) # RVX
}

# total variance = sampling variance + imputation variance
TV_MnM_T <- mean(c(sum((A1-MnMs[1])^2), # mean (total math)
                   sum((A2-MnMs[2])^2),
                   sum((A3-MnMs[3])^2),
                   sum((A4-MnMs[4])^2),
                   sum((A5-MnMs[5])^2),
                   sum((A6-MnMs[6])^2),
                   sum((A7-MnMs[7])^2),
                   sum((A8-MnMs[8])^2),
                   sum((A9-MnMs[9])^2),
                   sum((AX-MnMs[10])^2)))/20 + 11/90*sum((MnMs-MnM)^2)
TV_MnR_T <- mean(c(sum((B1-MnRs[1])^2), # mean (total reading)
                   sum((B2-MnRs[2])^2),
                   sum((B3-MnRs[3])^2),
                   sum((B4-MnRs[4])^2),
                   sum((B5-MnRs[5])^2),
                   sum((B6-MnRs[6])^2),
                   sum((B7-MnRs[7])^2),
                   sum((B8-MnRs[8])^2),
                   sum((B9-MnRs[9])^2),
                   sum((BX-MnRs[10])^2)))/20 + 11/90*sum((MnRs-MnR)^2)
TV_MdM_T <- mean(c(sum((C1-MdMs[1])^2), # median (total math)
                   sum((C2-MdMs[2])^2),
                   sum((C3-MdMs[3])^2),
                   sum((C4-MdMs[4])^2),
                   sum((C5-MdMs[5])^2),
                   sum((C6-MdMs[6])^2),
                   sum((C7-MdMs[7])^2),
                   sum((C8-MdMs[8])^2),
                   sum((C9-MdMs[9])^2),
                   sum((CX-MdMs[10])^2)))/20 + 11/90*sum((MdMs-MdM)^2)
TV_MdR_T <- mean(c(sum((D1-MdRs[1])^2), # median (total reading)
                   sum((D2-MdRs[2])^2),
                   sum((D3-MdRs[3])^2),
                   sum((D4-MdRs[4])^2),
                   sum((D5-MdRs[5])^2),
                   sum((D6-MdRs[6])^2),
                   sum((D7-MdRs[7])^2),
                   sum((D8-MdRs[8])^2),
                   sum((D9-MdRs[9])^2),
                   sum((DX-MdRs[10])^2)))/20 + 11/90*sum((MdRs-MdR)^2)
TV_MnM_F <- mean(c(sum((E1-MnMs_F[1])^2), # mean (female math)
                   sum((E2-MnMs_F[2])^2),
                   sum((E3-MnMs_F[3])^2),
                   sum((E4-MnMs_F[4])^2),
                   sum((E5-MnMs_F[5])^2),
                   sum((E6-MnMs_F[6])^2),
                   sum((E7-MnMs_F[7])^2),
                   sum((E8-MnMs_F[8])^2),
                   sum((E9-MnMs_F[9])^2),
                   sum((EX-MnMs_F[10])^2)))/20 + 11/90*sum((MnMs_F-MnM_F)^2)
TV_MnR_F <- mean(c(sum((F1-MnRs_F[1])^2), # mean (female reading)
                   sum((F2-MnRs_F[2])^2),
                   sum((F3-MnRs_F[3])^2),
                   sum((F4-MnRs_F[4])^2),
                   sum((F5-MnRs_F[5])^2),
                   sum((F6-MnRs_F[6])^2),
                   sum((F7-MnRs_F[7])^2),
                   sum((F8-MnRs_F[8])^2),
                   sum((F9-MnRs_F[9])^2),
                   sum((FX-MnRs_F[10])^2)))/20 + 11/90*sum((MnRs_F-MnR_F)^2)
TV_MdM_F <- mean(c(sum((G1-MdMs_F[1])^2), # median (female math)
                   sum((G2-MdMs_F[2])^2),
                   sum((G3-MdMs_F[3])^2),
                   sum((G4-MdMs_F[4])^2),
                   sum((G5-MdMs_F[5])^2),
                   sum((G6-MdMs_F[6])^2),
                   sum((G7-MdMs_F[7])^2),
                   sum((G8-MdMs_F[8])^2),
                   sum((G9-MdMs_F[9])^2),
                   sum((GX-MdMs_F[10])^2)))/20 + 11/90*sum((MdMs_F-MdM_F)^2)
TV_MdR_F <- mean(c(sum((H1-MdRs_F[1])^2), # median (female reading)
                   sum((H2-MdRs_F[2])^2),
                   sum((H3-MdRs_F[3])^2),
                   sum((H4-MdRs_F[4])^2),
                   sum((H5-MdRs_F[5])^2),
                   sum((H6-MdRs_F[6])^2),
                   sum((H7-MdRs_F[7])^2),
                   sum((H8-MdRs_F[8])^2),
                   sum((H9-MdRs_F[9])^2),
                   sum((HX-MdRs_F[10])^2)))/20 + 11/90*sum((MdRs_F-MdR_F)^2)
TV_MnM_M <- mean(c(sum((I1-MnMs_M[1])^2), # mean (male math)
                   sum((I2-MnMs_M[2])^2),
                   sum((I3-MnMs_M[3])^2),
                   sum((I4-MnMs_M[4])^2),
                   sum((I5-MnMs_M[5])^2),
                   sum((I6-MnMs_M[6])^2),
                   sum((I7-MnMs_M[7])^2),
                   sum((I8-MnMs_M[8])^2),
                   sum((I9-MnMs_M[9])^2),
                   sum((IX-MnMs_M[10])^2)))/20 + 11/90*sum((MnMs_M-MnM_M)^2)
TV_MnR_M <- mean(c(sum((J1-MnRs_M[1])^2), # mean (male reading)
                   sum((J2-MnRs_M[2])^2),
                   sum((J3-MnRs_M[3])^2),
                   sum((J4-MnRs_M[4])^2),
                   sum((J5-MnRs_M[5])^2),
                   sum((J6-MnRs_M[6])^2),
                   sum((J7-MnRs_M[7])^2),
                   sum((J8-MnRs_M[8])^2),
                   sum((J9-MnRs_M[9])^2),
                   sum((JX-MnRs_M[10])^2)))/20 + 11/90*sum((MnRs_M-MnR_M)^2)
TV_MdM_M <- mean(c(sum((K1-MdMs_M[1])^2), # median (male math)
                   sum((K2-MdMs_M[2])^2),
                   sum((K3-MdMs_M[3])^2),
                   sum((K4-MdMs_M[4])^2),
                   sum((K5-MdMs_M[5])^2),
                   sum((K6-MdMs_M[6])^2),
                   sum((K7-MdMs_M[7])^2),
                   sum((K8-MdMs_M[8])^2),
                   sum((K9-MdMs_M[9])^2),
                   sum((KX-MdMs_M[10])^2)))/20 + 11/90*sum((MdMs_M-MdM_M)^2)
TV_MdR_M <- mean(c(sum((L1-MdRs_M[1])^2), # median (male reading)
                   sum((L2-MdRs_M[2])^2),
                   sum((L3-MdRs_M[3])^2),
                   sum((L4-MdRs_M[4])^2),
                   sum((L5-MdRs_M[5])^2),
                   sum((L6-MdRs_M[6])^2),
                   sum((L7-MdRs_M[7])^2),
                   sum((L8-MdRs_M[8])^2),
                   sum((L9-MdRs_M[9])^2),
                   sum((LX-MdRs_M[10])^2)))/20 + 11/90*sum((MdRs_M-MdR_M)^2)
TV_MnDfM <- mean(c(sum((I1-E1-MnDfMs[1])^2), # mean difference (math)
                   sum((I2-E2-MnDfMs[2])^2),
                   sum((I3-E3-MnDfMs[3])^2),
                   sum((I4-E4-MnDfMs[4])^2),
                   sum((I5-E5-MnDfMs[5])^2),
                   sum((I6-E6-MnDfMs[6])^2),
                   sum((I7-E7-MnDfMs[7])^2),
                   sum((I8-E8-MnDfMs[8])^2),
                   sum((I9-E9-MnDfMs[9])^2),
                   sum((IX-EX-MnDfMs[10])^2)))/20 + 11/90*sum((MnDfMs-MnDfM)^2)
TV_MnDfR <- mean(c(sum((J1-F1-MnDfRs[1])^2), # mean difference (reading)
                   sum((J2-F2-MnDfRs[2])^2),
                   sum((J3-F3-MnDfRs[3])^2),
                   sum((J4-F4-MnDfRs[4])^2),
                   sum((J5-F5-MnDfRs[5])^2),
                   sum((J6-F6-MnDfRs[6])^2),
                   sum((J7-F7-MnDfRs[7])^2),
                   sum((J8-F8-MnDfRs[8])^2),
                   sum((J9-F9-MnDfRs[9])^2),
                   sum((JX-FX-MnDfRs[10])^2)))/20 + 11/90*sum((MnDfRs-MnDfR)^2)
TV_MdDfM <- mean(c(sum((K1-G1-MdDfMs[1])^2), # median difference (math)
                   sum((K2-G2-MdDfMs[2])^2),
                   sum((K3-G3-MdDfMs[3])^2),
                   sum((K4-G4-MdDfMs[4])^2),
                   sum((K5-G5-MdDfMs[5])^2),
                   sum((K6-G6-MdDfMs[6])^2),
                   sum((K7-G7-MdDfMs[7])^2),
                   sum((K8-G8-MdDfMs[8])^2),
                   sum((K9-G9-MdDfMs[9])^2),
                   sum((KX-GX-MdDfMs[10])^2)))/20 + 11/90*sum((MdDfMs-MdDfM)^2)
TV_MdDfR <- mean(c(sum((L1-H1-MdDfRs[1])^2), # median difference (reading)
                   sum((L2-H2-MdDfRs[2])^2),
                   sum((L3-H3-MdDfRs[3])^2),
                   sum((L4-H4-MdDfRs[4])^2),
                   sum((L5-H5-MdDfRs[5])^2),
                   sum((L6-H6-MdDfRs[6])^2),
                   sum((L7-H7-MdDfRs[7])^2),
                   sum((L8-H8-MdDfRs[8])^2),
                   sum((L9-H9-MdDfRs[9])^2),
                   sum((LX-HX-MdDfRs[10])^2)))/20 + 11/90*sum((MdDfRs-MdDfR)^2)

# standard errors
SE_MnM_T <- sqrt(TV_MnM_T) # mean (total math)
SE_MnR_T <- sqrt(TV_MnR_T) # mean (total reading)
SE_MdM_T <- sqrt(TV_MdM_T) # median (total math)
SE_MdR_T <- sqrt(TV_MdR_T) # median (total reading)
SE_MnM_F <- sqrt(TV_MnM_F) # mean (female math)
SE_MnR_F <- sqrt(TV_MnR_F) # mean (female reading)
SE_MdM_F <- sqrt(TV_MdM_F) # median (female math)
SE_MdR_F <- sqrt(TV_MdR_F) # median (female reading)
SE_MnM_M <- sqrt(TV_MnM_M) # mean (male math)
SE_MnR_M <- sqrt(TV_MnR_M) # mean (male reading)
SE_MdM_M <- sqrt(TV_MdM_M) # median (male math)
SE_MdR_M <- sqrt(TV_MdR_M) # median (male reading)
SE_MnDfM <- sqrt(TV_MnDfM) # mean difference (math)
SE_MnDfR <- sqrt(TV_MnDfR) # mean difference (reading)
SE_MdDfM <- sqrt(TV_MdDfM) # median difference (math)
SE_MdDfR <- sqrt(TV_MdDfR) # median difference (reading)



#### SEs: LTPRs and LTPR tail-center differences ####

A11 <- A12 <- A13 <- A14 <- A18 <- A16 <- A17 <- A18 <- A19 <- A20 <- numeric(80) # empty containers
A21 <- A22 <- A23 <- A24 <- A25 <- A26 <- A27 <- A28 <- A29 <- A30 <- numeric(80) # empty containers
B11 <- B12 <- B13 <- B14 <- B15 <- B16 <- B17 <- B18 <- B19 <- B20 <- numeric(80) # empty containers
B21 <- B22 <- B23 <- B24 <- B25 <- B26 <- B27 <- B28 <- B29 <- B30 <- numeric(80) # empty containers
C11 <- C12 <- C13 <- C14 <- C15 <- C16 <- C17 <- C18 <- C19 <- C20 <- numeric(80) # empty containers
C21 <- C22 <- C23 <- C24 <- C25 <- C26 <- C27 <- C28 <- C29 <- C30 <- numeric(80) # empty containers
D11 <- D12 <- D13 <- D14 <- D15 <- D16 <- D17 <- D18 <- D19 <- D20 <- numeric(80) # empty containers
D21 <- D22 <- D23 <- D24 <- D25 <- D26 <- D27 <- D28 <- D29 <- D30 <- numeric(80) # empty containers
E11 <- E12 <- E13 <- E14 <- E15 <- E16 <- E17 <- E18 <- E19 <- E20 <- numeric(80) # empty containers
E21 <- E22 <- E23 <- E24 <- E25 <- E26 <- E27 <- E28 <- E29 <- E30 <- numeric(80) # empty containers
F11 <- F12 <- F13 <- F14 <- F15 <- F16 <- F17 <- F18 <- F19 <- F20 <- numeric(80) # empty containers
F21 <- F22 <- F23 <- F24 <- F25 <- F26 <- F27 <- F28 <- F29 <- F30 <- numeric(80) # empty containers
G11 <- G12 <- G13 <- G14 <- G15 <- G16 <- G17 <- G18 <- G19 <- G20 <- numeric(80) # empty containers
G21 <- G22 <- G23 <- G24 <- G25 <- G26 <- G27 <- G28 <- G29 <- G30 <- numeric(80) # empty containers
H11 <- H12 <- H13 <- H14 <- H15 <- H16 <- H17 <- H18 <- H19 <- H20 <- numeric(80) # empty containers
H21 <- H22 <- H23 <- H24 <- H25 <- H26 <- H27 <- H28 <- H29 <- H30 <- numeric(80) # empty containers
I11 <- I12 <- I13 <- I14 <- I15 <- I16 <- I17 <- I18 <- I19 <- I20 <- numeric(80) # empty containers
I21 <- I22 <- I23 <- I24 <- I25 <- I26 <- I27 <- I28 <- I29 <- I30 <- numeric(80) # empty containers
J11 <- J12 <- J13 <- J14 <- J15 <- J16 <- J17 <- J18 <- J19 <- J20 <- numeric(80) # empty containers
J21 <- J22 <- J23 <- J24 <- J25 <- J26 <- J27 <- J28 <- J29 <- J30 <- numeric(80) # empty containers
K11 <- K12 <- K13 <- K14 <- K15 <- K16 <- K17 <- K18 <- K19 <- K20 <- numeric(80) # empty containers
K21 <- K22 <- K23 <- K24 <- K25 <- K26 <- K27 <- K28 <- K29 <- K30 <- numeric(80) # empty containers
L11 <- L12 <- L13 <- L14 <- L15 <- L16 <- L17 <- L18 <- L19 <- L20 <- numeric(80) # empty containers
L21 <- L22 <- L23 <- L24 <- L25 <- L26 <- L27 <- L28 <- L29 <- L30 <- numeric(80) # empty containers
M11 <- M12 <- M13 <- M14 <- M15 <- M16 <- M17 <- M18 <- M19 <- M20 <- numeric(80) # empty containers
M21 <- M22 <- M23 <- M24 <- M25 <- M26 <- M27 <- M28 <- M29 <- M30 <- numeric(80) # empty containers
N11 <- N12 <- N13 <- N14 <- N15 <- N16 <- N17 <- N18 <- N19 <- N20 <- numeric(80) # empty containers
N21 <- N22 <- N23 <- N24 <- N25 <- N26 <- N27 <- N28 <- N29 <- N30 <- numeric(80) # empty containers
O11 <- O12 <- O13 <- O14 <- O15 <- O16 <- O17 <- O18 <- O19 <- O20 <- numeric(80) # empty containers
O21 <- O22 <- O23 <- O24 <- O25 <- O26 <- O27 <- O28 <- O29 <- O30 <- numeric(80) # empty containers
Q11 <- Q12 <- Q13 <- Q14 <- Q15 <- Q16 <- Q17 <- Q18 <- Q19 <- Q20 <- numeric(80) # empty containers
Q21 <- Q22 <- Q23 <- Q24 <- Q25 <- Q26 <- Q27 <- Q28 <- Q29 <- Q30 <- numeric(80) # empty containers
R11 <- R12 <- R13 <- R14 <- R15 <- R16 <- R17 <- R18 <- R19 <- R20 <- numeric(80) # empty containers
R21 <- R22 <- R23 <- R24 <- R25 <- R26 <- R27 <- R28 <- R29 <- R30 <- numeric(80) # empty containers
S11 <- S12 <- S13 <- S14 <- S15 <- S16 <- S17 <- S18 <- S19 <- S20 <- numeric(80) # empty containers
S21 <- S22 <- S23 <- S24 <- S25 <- S26 <- S27 <- S28 <- S29 <- S30 <- numeric(80) # empty containers
T11 <- T12 <- T13 <- T14 <- T15 <- T16 <- T17 <- T18 <- T19 <- T20 <- numeric(80) # empty containers
T21 <- T22 <- T23 <- T24 <- T25 <- T26 <- T27 <- T28 <- T29 <- T30 <- numeric(80) # empty containers
U11 <- U12 <- U13 <- U14 <- U15 <- U16 <- U17 <- U18 <- U19 <- U20 <- numeric(80) # empty containers
U21 <- U22 <- U23 <- U24 <- U25 <- U26 <- U27 <- U28 <- U29 <- U30 <- numeric(80) # empty containers

# perform bootstrap resampling of LTPRs
for (i in 1:80) {
  W <- paste0('BW',i)
  
  A11[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV1, P18[,W]), W, 'MV1') # MV1 mean LTPR
  A12[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV2, P18[,W]), W, 'MV2') # MV2
  A13[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV3, P18[,W]), W, 'MV3') # MV3
  A14[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV4, P18[,W]), W, 'MV4') # MV4
  A18[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV5, P18[,W]), W, 'MV5') # MV5
  A16[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV6, P18[,W]), W, 'MV6') # MV6
  A17[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV7, P18[,W]), W, 'MV7') # MV7
  A18[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV8, P18[,W]), W, 'MV8') # MV8
  A19[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MV9, P18[,W]), W, 'MV9') # MV9
  A20[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$MVX, P18[,W]), W, 'MVX') # MVX
  A21[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV1, P18[,W]), W, 'RV1') # RV1 mean LTPR
  A22[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV2, P18[,W]), W, 'RV2') # RV2
  A23[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV3, P18[,W]), W, 'RV3') # RV3
  A24[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV4, P18[,W]), W, 'RV4') # RV4
  A25[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV5, P18[,W]), W, 'RV5') # RV5
  A26[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV6, P18[,W]), W, 'RV6') # RV6
  A27[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV7, P18[,W]), W, 'RV7') # RV7
  A28[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV8, P18[,W]), W, 'RV8') # RV8
  A29[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RV9, P18[,W]), W, 'RV9') # RV9
  A30[i] <- LTPRfn(P18_M, P18_F, wt.mn(P18$RVX, P18[,W]), W, 'RVX') # RVX
  
  B11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .05), W, 'MV1') # MV1 5th LTPR
  B12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .05), W, 'MV2') # MV2
  B13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .05), W, 'MV3') # MV3
  B14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .05), W, 'MV4') # MV4
  B15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .05), W, 'MV5') # MV5
  B16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .05), W, 'MV6') # MV6
  B17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .05), W, 'MV7') # MV7
  B18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .05), W, 'MV8') # MV8
  B19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .05), W, 'MV9') # MV9
  B20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .05), W, 'MVX') # MVX
  B21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .05), W, 'RV1') # RV1 5th LTPR
  B22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .05), W, 'RV2') # RV2
  B23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .05), W, 'RV3') # RV3
  B24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .05), W, 'RV4') # RV4
  B25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .05), W, 'RV5') # RV5
  B26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .05), W, 'RV6') # RV6
  B27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .05), W, 'RV7') # RV7
  B28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .05), W, 'RV8') # RV8
  B29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .05), W, 'RV9') # RV9
  B30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .05), W, 'RVX') # RVX
  
  C11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .10), W, 'MV1') # MV1 10th LTPR
  C12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .10), W, 'MV2') # MV2
  C13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .10), W, 'MV3') # MV3
  C14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .10), W, 'MV4') # MV4
  C15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .10), W, 'MV5') # MV5
  C16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .10), W, 'MV6') # MV6
  C17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .10), W, 'MV7') # MV7
  C18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .10), W, 'MV8') # MV8
  C19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .10), W, 'MV9') # MV9
  C20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .10), W, 'MVX') # MVX
  C21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .10), W, 'RV1') # RV1 10th LTPR
  C22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .10), W, 'RV2') # RV2
  C23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .10), W, 'RV3') # RV3
  C24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .10), W, 'RV4') # RV4
  C25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .10), W, 'RV5') # RV5
  C26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .10), W, 'RV6') # RV6
  C27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .10), W, 'RV7') # RV7
  C28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .10), W, 'RV8') # RV8
  C29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .10), W, 'RV9') # RV9
  C30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .10), W, 'RVX') # RVX
  
  D11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .15), W, 'MV1') # MV1 15th LTPR
  D12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .15), W, 'MV2') # MV2
  D13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .15), W, 'MV3') # MV3
  D14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .15), W, 'MV4') # MV4
  D15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .15), W, 'MV5') # MV5
  D16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .15), W, 'MV6') # MV6
  D17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .15), W, 'MV7') # MV7
  D18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .15), W, 'MV8') # MV8
  D19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .15), W, 'MV9') # MV9
  D20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .15), W, 'MVX') # MVX
  D21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .15), W, 'RV1') # RV1 15th LTPR
  D22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .15), W, 'RV2') # RV2
  D23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .15), W, 'RV3') # RV3
  D24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .15), W, 'RV4') # RV4
  D25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .15), W, 'RV5') # RV5
  D26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .15), W, 'RV6') # RV6
  D27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .15), W, 'RV7') # RV7
  D28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .15), W, 'RV8') # RV8
  D29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .15), W, 'RV9') # RV9
  D30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .15), W, 'RVX') # RVX
  
  E11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .20), W, 'MV1') # MV1 20th LTPR
  E12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .20), W, 'MV2') # MV2
  E13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .20), W, 'MV3') # MV3
  E14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .20), W, 'MV4') # MV4
  E15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .20), W, 'MV5') # MV5
  E16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .20), W, 'MV6') # MV6
  E17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .20), W, 'MV7') # MV7
  E18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .20), W, 'MV8') # MV8
  E19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .20), W, 'MV9') # MV9
  E20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .20), W, 'MVX') # MVX
  E21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .20), W, 'RV1') # RV1 20th LTPR
  E22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .20), W, 'RV2') # RV2
  E23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .20), W, 'RV3') # RV3
  E24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .20), W, 'RV4') # RV4
  E25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .20), W, 'RV5') # RV5
  E26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .20), W, 'RV6') # RV6
  E27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .20), W, 'RV7') # RV7
  E28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .20), W, 'RV8') # RV8
  E29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .20), W, 'RV9') # RV9
  E30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .20), W, 'RVX') # RVX
  
  F11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .25), W, 'MV1') # MV1 25th LTPR
  F12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .25), W, 'MV2') # MV2
  F13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .25), W, 'MV3') # MV3
  F14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .25), W, 'MV4') # MV4
  F15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .25), W, 'MV5') # MV5
  F16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .25), W, 'MV6') # MV6
  F17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .25), W, 'MV7') # MV7
  F18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .25), W, 'MV8') # MV8
  F19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .25), W, 'MV9') # MV9
  F20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .25), W, 'MVX') # MVX
  F21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .25), W, 'RV1') # RV1 25th LTPR
  F22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .25), W, 'RV2') # RV2
  F23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .25), W, 'RV3') # RV3
  F24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .25), W, 'RV4') # RV4
  F25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .25), W, 'RV5') # RV5
  F26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .25), W, 'RV6') # RV6
  F27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .25), W, 'RV7') # RV7
  F28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .25), W, 'RV8') # RV8
  F29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .25), W, 'RV9') # RV9
  F30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .25), W, 'RVX') # RVX
  
  G11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .30), W, 'MV1') # MV1 30th LTPR
  G12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .30), W, 'MV2') # MV2
  G13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .30), W, 'MV3') # MV3
  G14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .30), W, 'MV4') # MV4
  G15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .30), W, 'MV5') # MV5
  G16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .30), W, 'MV6') # MV6
  G17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .30), W, 'MV7') # MV7
  G18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .30), W, 'MV8') # MV8
  G19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .30), W, 'MV9') # MV9
  G20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .30), W, 'MVX') # MVX
  G21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .30), W, 'RV1') # RV1 30th LTPR
  G22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .30), W, 'RV2') # RV2
  G23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .30), W, 'RV3') # RV3
  G24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .30), W, 'RV4') # RV4
  G25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .30), W, 'RV5') # RV5
  G26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .30), W, 'RV6') # RV6
  G27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .30), W, 'RV7') # RV7
  G28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .30), W, 'RV8') # RV8
  G29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .30), W, 'RV9') # RV9
  G30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .30), W, 'RVX') # RVX
  
  H11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .35), W, 'MV1') # MV1 35th LTPR
  H12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .35), W, 'MV2') # MV2
  H13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .35), W, 'MV3') # MV3
  H14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .35), W, 'MV4') # MV4
  H15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .35), W, 'MV5') # MV5
  H16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .35), W, 'MV6') # MV6
  H17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .35), W, 'MV7') # MV7
  H18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .35), W, 'MV8') # MV8
  H19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .35), W, 'MV9') # MV9
  H20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .35), W, 'MVX') # MVX
  H21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .35), W, 'RV1') # RV1 35th LTPR
  H22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .35), W, 'RV2') # RV2
  H23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .35), W, 'RV3') # RV3
  H24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .35), W, 'RV4') # RV4
  H25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .35), W, 'RV5') # RV5
  H26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .35), W, 'RV6') # RV6
  H27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .35), W, 'RV7') # RV7
  H28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .35), W, 'RV8') # RV8
  H29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .35), W, 'RV9') # RV9
  H30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .35), W, 'RVX') # RVX
  
  I11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .40), W, 'MV1') # MV1 40th LTPR
  I12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .40), W, 'MV2') # MV2
  I13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .40), W, 'MV3') # MV3
  I14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .40), W, 'MV4') # MV4
  I15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .40), W, 'MV5') # MV5
  I16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .40), W, 'MV6') # MV6
  I17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .40), W, 'MV7') # MV7
  I18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .40), W, 'MV8') # MV8
  I19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .40), W, 'MV9') # MV9
  I20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .40), W, 'MVX') # MVX
  I21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .40), W, 'RV1') # RV1 40th LTPR
  I22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .40), W, 'RV2') # RV2
  I23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .40), W, 'RV3') # RV3
  I24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .40), W, 'RV4') # RV4
  I25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .40), W, 'RV5') # RV5
  I26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .40), W, 'RV6') # RV6
  I27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .40), W, 'RV7') # RV7
  I28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .40), W, 'RV8') # RV8
  I29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .40), W, 'RV9') # RV9
  I30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .40), W, 'RVX') # RVX
  
  J11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .45), W, 'MV1') # MV1 45th LTPR
  J12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .45), W, 'MV2') # MV2
  J13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .45), W, 'MV3') # MV3
  J14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .45), W, 'MV4') # MV4
  J15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .45), W, 'MV5') # MV5
  J16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .45), W, 'MV6') # MV6
  J17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .45), W, 'MV7') # MV7
  J18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .45), W, 'MV8') # MV8
  J19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .45), W, 'MV9') # MV9
  J20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .45), W, 'MVX') # MVX
  J21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .45), W, 'RV1') # RV1 45th LTPR
  J22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .45), W, 'RV2') # RV2
  J23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .45), W, 'RV3') # RV3
  J24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .45), W, 'RV4') # RV4
  J25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .45), W, 'RV5') # RV5
  J26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .45), W, 'RV6') # RV6
  J27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .45), W, 'RV7') # RV7
  J28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .45), W, 'RV8') # RV8
  J29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .45), W, 'RV9') # RV9
  J30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .45), W, 'RVX') # RVX
  
  K11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .50), W, 'MV1') # MV1 50th LTPR
  K12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .50), W, 'MV2') # MV2
  K13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .50), W, 'MV3') # MV3
  K14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .50), W, 'MV4') # MV4
  K15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .50), W, 'MV5') # MV5
  K16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .50), W, 'MV6') # MV6
  K17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .50), W, 'MV7') # MV7
  K18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .50), W, 'MV8') # MV8
  K19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .50), W, 'MV9') # MV9
  K20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .50), W, 'MVX') # MVX
  K21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .50), W, 'RV1') # RV1 50th LTPR
  K22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .50), W, 'RV2') # RV2
  K23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .50), W, 'RV3') # RV3
  K24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .50), W, 'RV4') # RV4
  K25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .50), W, 'RV5') # RV5
  K26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .50), W, 'RV6') # RV6
  K27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .50), W, 'RV7') # RV7
  K28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .50), W, 'RV8') # RV8
  K29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .50), W, 'RV9') # RV9
  K30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .50), W, 'RVX') # RVX
  
  L11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .55), W, 'MV1') # MV1 55th LTPR
  L12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .55), W, 'MV2') # MV2
  L13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .55), W, 'MV3') # MV3
  L14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .55), W, 'MV4') # MV4
  L15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .55), W, 'MV5') # MV5
  L16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .55), W, 'MV6') # MV6
  L17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .55), W, 'MV7') # MV7
  L18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .55), W, 'MV8') # MV8
  L19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .55), W, 'MV9') # MV9
  L20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .55), W, 'MVX') # MVX
  L21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .55), W, 'RV1') # RV1 55th LTPR
  L22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .55), W, 'RV2') # RV2
  L23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .55), W, 'RV3') # RV3
  L24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .55), W, 'RV4') # RV4
  L25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .55), W, 'RV5') # RV5
  L26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .55), W, 'RV6') # RV6
  L27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .55), W, 'RV7') # RV7
  L28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .55), W, 'RV8') # RV8
  L29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .55), W, 'RV9') # RV9
  L30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .55), W, 'RVX') # RVX
  
  M11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .60), W, 'MV1') # MV1 60th LTPR
  M12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .60), W, 'MV2') # MV2
  M13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .60), W, 'MV3') # MV3
  M14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .60), W, 'MV4') # MV4
  M15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .60), W, 'MV5') # MV5
  M16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .60), W, 'MV6') # MV6
  M17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .60), W, 'MV7') # MV7
  M18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .60), W, 'MV8') # MV8
  M19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .60), W, 'MV9') # MV9
  M20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .60), W, 'MVX') # MVX
  M21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .60), W, 'RV1') # RV1 60th LTPR
  M22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .60), W, 'RV2') # RV2
  M23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .60), W, 'RV3') # RV3
  M24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .60), W, 'RV4') # RV4
  M25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .60), W, 'RV5') # RV5
  M26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .60), W, 'RV6') # RV6
  M27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .60), W, 'RV7') # RV7
  M28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .60), W, 'RV8') # RV8
  M29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .60), W, 'RV9') # RV9
  M30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .60), W, 'RVX') # RVX
  
  N11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .65), W, 'MV1') # MV1 65th LTPR
  N12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .65), W, 'MV2') # MV2
  N13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .65), W, 'MV3') # MV3
  N14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .65), W, 'MV4') # MV4
  N15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .65), W, 'MV5') # MV5
  N16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .65), W, 'MV6') # MV6
  N17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .65), W, 'MV7') # MV7
  N18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .65), W, 'MV8') # MV8
  N19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .65), W, 'MV9') # MV9
  N20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .65), W, 'MVX') # MVX
  N21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .65), W, 'RV1') # RV1 65th LTPR
  N22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .65), W, 'RV2') # RV2
  N23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .65), W, 'RV3') # RV3
  N24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .65), W, 'RV4') # RV4
  N25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .65), W, 'RV5') # RV5
  N26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .65), W, 'RV6') # RV6
  N27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .65), W, 'RV7') # RV7
  N28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .65), W, 'RV8') # RV8
  N29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .65), W, 'RV9') # RV9
  N30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .65), W, 'RVX') # RVX
  
  O11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .70), W, 'MV1') # MV1 70th LTPR
  O12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .70), W, 'MV2') # MV2
  O13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .70), W, 'MV3') # MV3
  O14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .70), W, 'MV4') # MV4
  O15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .70), W, 'MV5') # MV5
  O16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .70), W, 'MV6') # MV6
  O17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .70), W, 'MV7') # MV7
  O18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .70), W, 'MV8') # MV8
  O19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .70), W, 'MV9') # MV9
  O20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .70), W, 'MVX') # MVX
  O21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .70), W, 'RV1') # RV1 70th LTPR
  O22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .70), W, 'RV2') # RV2
  O23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .70), W, 'RV3') # RV3
  O24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .70), W, 'RV4') # RV4
  O25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .70), W, 'RV5') # RV5
  O26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .70), W, 'RV6') # RV6
  O27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .70), W, 'RV7') # RV7
  O28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .70), W, 'RV8') # RV8
  O29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .70), W, 'RV9') # RV9
  O30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .70), W, 'RVX') # RVX
  
  Q11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .75), W, 'MV1') # MV1 75th LTPR
  Q12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .75), W, 'MV2') # MV2
  Q13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .75), W, 'MV3') # MV3
  Q14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .75), W, 'MV4') # MV4
  Q15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .75), W, 'MV5') # MV5
  Q16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .75), W, 'MV6') # MV6
  Q17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .75), W, 'MV7') # MV7
  Q18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .75), W, 'MV8') # MV8
  Q19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .75), W, 'MV9') # MV9
  Q20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .75), W, 'MVX') # MVX
  Q21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .75), W, 'RV1') # RV1 75th LTPR
  Q22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .75), W, 'RV2') # RV2
  Q23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .75), W, 'RV3') # RV3
  Q24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .75), W, 'RV4') # RV4
  Q25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .75), W, 'RV5') # RV5
  Q26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .75), W, 'RV6') # RV6
  Q27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .75), W, 'RV7') # RV7
  Q28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .75), W, 'RV8') # RV8
  Q29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .75), W, 'RV9') # RV9
  Q30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .75), W, 'RVX') # RVX
  
  R11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .80), W, 'MV1') # MV1 80th LTPR
  R12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .80), W, 'MV2') # MV2
  R13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .80), W, 'MV3') # MV3
  R14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .80), W, 'MV4') # MV4
  R15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .80), W, 'MV5') # MV5
  R16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .80), W, 'MV6') # MV6
  R17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .80), W, 'MV7') # MV7
  R18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .80), W, 'MV8') # MV8
  R19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .80), W, 'MV9') # MV9
  R20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .80), W, 'MVX') # MVX
  R21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .80), W, 'RV1') # RV1 80th LTPR
  R22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .80), W, 'RV2') # RV2
  R23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .80), W, 'RV3') # RV3
  R24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .80), W, 'RV4') # RV4
  R25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .80), W, 'RV5') # RV5
  R26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .80), W, 'RV6') # RV6
  R27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .80), W, 'RV7') # RV7
  R28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .80), W, 'RV8') # RV8
  R29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .80), W, 'RV9') # RV9
  R30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .80), W, 'RVX') # RVX
  
  S11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .85), W, 'MV1') # MV1 85th LTPR
  S12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .85), W, 'MV2') # MV2
  S13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .85), W, 'MV3') # MV3
  S14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .85), W, 'MV4') # MV4
  S15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .85), W, 'MV5') # MV5
  S16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .85), W, 'MV6') # MV6
  S17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .85), W, 'MV7') # MV7
  S18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .85), W, 'MV8') # MV8
  S19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .85), W, 'MV9') # MV9
  S20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .85), W, 'MVX') # MVX
  S21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .85), W, 'RV1') # RV1 85th LTPR
  S22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .85), W, 'RV2') # RV2
  S23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .85), W, 'RV3') # RV3
  S24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .85), W, 'RV4') # RV4
  S25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .85), W, 'RV5') # RV5
  S26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .85), W, 'RV6') # RV6
  S27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .85), W, 'RV7') # RV7
  S28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .85), W, 'RV8') # RV8
  S29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .85), W, 'RV9') # RV9
  S30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .85), W, 'RVX') # RVX
  
  T11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .90), W, 'MV1') # MV1 90th LTPR
  T12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .90), W, 'MV2') # MV2
  T13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .90), W, 'MV3') # MV3
  T14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .90), W, 'MV4') # MV4
  T15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .90), W, 'MV5') # MV5
  T16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .90), W, 'MV6') # MV6
  T17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .90), W, 'MV7') # MV7
  T18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .90), W, 'MV8') # MV8
  T19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .90), W, 'MV9') # MV9
  T20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .90), W, 'MVX') # MVX
  T21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .90), W, 'RV1') # RV1 90th LTPR
  T22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .90), W, 'RV2') # RV2
  T23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .90), W, 'RV3') # RV3
  T24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .90), W, 'RV4') # RV4
  T25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .90), W, 'RV5') # RV5
  T26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .90), W, 'RV6') # RV6
  T27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .90), W, 'RV7') # RV7
  T28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .90), W, 'RV8') # RV8
  T29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .90), W, 'RV9') # RV9
  T30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .90), W, 'RVX') # RVX
  
  U11[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV1, P18[,W], .95), W, 'MV1') # MV1 95th LTPR
  U12[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV2, P18[,W], .95), W, 'MV2') # MV2
  U13[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV3, P18[,W], .95), W, 'MV3') # MV3
  U14[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV4, P18[,W], .95), W, 'MV4') # MV4
  U15[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV5, P18[,W], .95), W, 'MV5') # MV5
  U16[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV6, P18[,W], .95), W, 'MV6') # MV6
  U17[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV7, P18[,W], .95), W, 'MV7') # MV7
  U18[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV8, P18[,W], .95), W, 'MV8') # MV8
  U19[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MV9, P18[,W], .95), W, 'MV9') # MV9
  U20[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$MVX, P18[,W], .95), W, 'MVX') # MVX
  U21[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV1, P18[,W], .95), W, 'RV1') # RV1 95th LTPR
  U22[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV2, P18[,W], .95), W, 'RV2') # RV2
  U23[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV3, P18[,W], .95), W, 'RV3') # RV3
  U24[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV4, P18[,W], .95), W, 'RV4') # RV4
  U25[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV5, P18[,W], .95), W, 'RV5') # RV5
  U26[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV6, P18[,W], .95), W, 'RV6') # RV6
  U27[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV7, P18[,W], .95), W, 'RV7') # RV7
  U28[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV8, P18[,W], .95), W, 'RV8') # RV8
  U29[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RV9, P18[,W], .95), W, 'RV9') # RV9
  U30[i] <- LTPRfn(P18_M, P18_F, wt.qnt(P18$RVX, P18[,W], .95), W, 'RVX') # RVX
  
  if (i %% 10 == 0) {print(paste0(i, '/', 80, ' at ', Sys.time()), quote = F)} # print updates
}

# total variance = sampling variance + imputation variance
TV_MnM <- mean(c(sum((A11-LTPRMnMs[1])^2), # mean LTPR (math)
                 sum((A12-LTPRMnMs[2])^2),
                 sum((A13-LTPRMnMs[3])^2),
                 sum((A14-LTPRMnMs[4])^2),
                 sum((A18-LTPRMnMs[5])^2),
                 sum((A16-LTPRMnMs[6])^2),
                 sum((A17-LTPRMnMs[7])^2),
                 sum((A18-LTPRMnMs[8])^2),
                 sum((A19-LTPRMnMs[9])^2),
                 sum((A20-LTPRMnMs[10])^2)))/20 + 11/90*sum((LTPRMnMs-LTPRMnM)^2)
TV_MnR <- mean(c(sum((A21-LTPRMnRs[1])^2), # mean LTPR (reading)
                 sum((A22-LTPRMnRs[2])^2),
                 sum((A23-LTPRMnRs[3])^2),
                 sum((A24-LTPRMnRs[4])^2),
                 sum((A25-LTPRMnRs[5])^2),
                 sum((A26-LTPRMnRs[6])^2),
                 sum((A27-LTPRMnRs[7])^2),
                 sum((A28-LTPRMnRs[8])^2),
                 sum((A29-LTPRMnRs[9])^2),
                 sum((A30-LTPRMnRs[10])^2)))/20 + 11/90*sum((LTPRMnRs-LTPRMnR)^2)
TV_05TM <- mean(c(sum((B11-LTPR05Ms[1])^2), # 5th LTPR (math)
                  sum((B12-LTPR05Ms[2])^2),
                  sum((B13-LTPR05Ms[3])^2),
                  sum((B14-LTPR05Ms[4])^2),
                  sum((B15-LTPR05Ms[5])^2),
                  sum((B16-LTPR05Ms[6])^2),
                  sum((B17-LTPR05Ms[7])^2),
                  sum((B18-LTPR05Ms[8])^2),
                  sum((B19-LTPR05Ms[9])^2),
                  sum((B20-LTPR05Ms[10])^2)))/20 + 11/90*sum((LTPR05Ms-LTPR05M)^2)
TV_05TR <- mean(c(sum((B21-LTPR05Rs[1])^2), # 5th LTPR (reading)
                  sum((B22-LTPR05Rs[2])^2),
                  sum((B23-LTPR05Rs[3])^2),
                  sum((B24-LTPR05Rs[4])^2),
                  sum((B25-LTPR05Rs[5])^2),
                  sum((B26-LTPR05Rs[6])^2),
                  sum((B27-LTPR05Rs[7])^2),
                  sum((B28-LTPR05Rs[8])^2),
                  sum((B29-LTPR05Rs[9])^2),
                  sum((B30-LTPR05Rs[10])^2)))/20 + 11/90*sum((LTPR05Rs-LTPR05R)^2)
TV_10TM <- mean(c(sum((C11-LTPR10Ms[1])^2), # 10th LTPR (math)
                  sum((C12-LTPR10Ms[2])^2),
                  sum((C13-LTPR10Ms[3])^2),
                  sum((C14-LTPR10Ms[4])^2),
                  sum((C15-LTPR10Ms[5])^2),
                  sum((C16-LTPR10Ms[6])^2),
                  sum((C17-LTPR10Ms[7])^2),
                  sum((C18-LTPR10Ms[8])^2),
                  sum((C19-LTPR10Ms[9])^2),
                  sum((C20-LTPR10Ms[10])^2)))/20 + 11/90*sum((LTPR10Ms-LTPR10M)^2)
TV_10TR <- mean(c(sum((C21-LTPR10Rs[1])^2), # 10th LTPR (reading)
                  sum((C22-LTPR10Rs[2])^2),
                  sum((C23-LTPR10Rs[3])^2),
                  sum((C24-LTPR10Rs[4])^2),
                  sum((C25-LTPR10Rs[5])^2),
                  sum((C26-LTPR10Rs[6])^2),
                  sum((C27-LTPR10Rs[7])^2),
                  sum((C28-LTPR10Rs[8])^2),
                  sum((C29-LTPR10Rs[9])^2),
                  sum((C30-LTPR10Rs[10])^2)))/20 + 11/90*sum((LTPR10Rs-LTPR10R)^2)
TV_15TM <- mean(c(sum((D11-LTPR15Ms[1])^2), # 15th LTPR (math)
                  sum((D12-LTPR15Ms[2])^2),
                  sum((D13-LTPR15Ms[3])^2),
                  sum((D14-LTPR15Ms[4])^2),
                  sum((D15-LTPR15Ms[5])^2),
                  sum((D16-LTPR15Ms[6])^2),
                  sum((D17-LTPR15Ms[7])^2),
                  sum((D18-LTPR15Ms[8])^2),
                  sum((D19-LTPR15Ms[9])^2),
                  sum((D20-LTPR15Ms[10])^2)))/20 + 11/90*sum((LTPR15Ms-LTPR15M)^2)
TV_15TR <- mean(c(sum((D21-LTPR15Rs[1])^2), # 15th LTPR (reading)
                  sum((D22-LTPR15Rs[2])^2),
                  sum((D23-LTPR15Rs[3])^2),
                  sum((D24-LTPR15Rs[4])^2),
                  sum((D25-LTPR15Rs[5])^2),
                  sum((D26-LTPR15Rs[6])^2),
                  sum((D27-LTPR15Rs[7])^2),
                  sum((D28-LTPR15Rs[8])^2),
                  sum((D29-LTPR15Rs[9])^2),
                  sum((D30-LTPR15Rs[10])^2)))/20 + 11/90*sum((LTPR15Rs-LTPR15R)^2)
TV_20TM <- mean(c(sum((E11-LTPR20Ms[1])^2), # 20th LTPR (math)
                  sum((E12-LTPR20Ms[2])^2),
                  sum((E13-LTPR20Ms[3])^2),
                  sum((E14-LTPR20Ms[4])^2),
                  sum((E15-LTPR20Ms[5])^2),
                  sum((E16-LTPR20Ms[6])^2),
                  sum((E17-LTPR20Ms[7])^2),
                  sum((E18-LTPR20Ms[8])^2),
                  sum((E19-LTPR20Ms[9])^2),
                  sum((E20-LTPR20Ms[10])^2)))/20 + 11/90*sum((LTPR20Ms-LTPR20M)^2)
TV_20TR <- mean(c(sum((E21-LTPR20Rs[1])^2), # 20th LTPR (reading)
                  sum((E22-LTPR20Rs[2])^2),
                  sum((E23-LTPR20Rs[3])^2),
                  sum((E24-LTPR20Rs[4])^2),
                  sum((E25-LTPR20Rs[5])^2),
                  sum((E26-LTPR20Rs[6])^2),
                  sum((E27-LTPR20Rs[7])^2),
                  sum((E28-LTPR20Rs[8])^2),
                  sum((E29-LTPR20Rs[9])^2),
                  sum((E30-LTPR20Rs[10])^2)))/20 + 11/90*sum((LTPR20Rs-LTPR20R)^2)
TV_25TM <- mean(c(sum((F11-LTPR25Ms[1])^2), # 25th LTPR (math)
                  sum((F12-LTPR25Ms[2])^2),
                  sum((F13-LTPR25Ms[3])^2),
                  sum((F14-LTPR25Ms[4])^2),
                  sum((F15-LTPR25Ms[5])^2),
                  sum((F16-LTPR25Ms[6])^2),
                  sum((F17-LTPR25Ms[7])^2),
                  sum((F18-LTPR25Ms[8])^2),
                  sum((F19-LTPR25Ms[9])^2),
                  sum((F20-LTPR25Ms[10])^2)))/20 + 11/90*sum((LTPR25Ms-LTPR25M)^2)
TV_25TR <- mean(c(sum((F21-LTPR25Rs[1])^2), # 25th LTPR (reading)
                  sum((F22-LTPR25Rs[2])^2),
                  sum((F23-LTPR25Rs[3])^2),
                  sum((F24-LTPR25Rs[4])^2),
                  sum((F25-LTPR25Rs[5])^2),
                  sum((F26-LTPR25Rs[6])^2),
                  sum((F27-LTPR25Rs[7])^2),
                  sum((F28-LTPR25Rs[8])^2),
                  sum((F29-LTPR25Rs[9])^2),
                  sum((F30-LTPR25Rs[10])^2)))/20 + 11/90*sum((LTPR25Rs-LTPR25R)^2)
TV_30TM <- mean(c(sum((G11-LTPR30Ms[1])^2), # 30th LTPR (math)
                  sum((G12-LTPR30Ms[2])^2),
                  sum((G13-LTPR30Ms[3])^2),
                  sum((G14-LTPR30Ms[4])^2),
                  sum((G15-LTPR30Ms[5])^2),
                  sum((G16-LTPR30Ms[6])^2),
                  sum((G17-LTPR30Ms[7])^2),
                  sum((G18-LTPR30Ms[8])^2),
                  sum((G19-LTPR30Ms[9])^2),
                  sum((G20-LTPR30Ms[10])^2)))/20 + 11/90*sum((LTPR30Ms-LTPR30M)^2)
TV_30TR <- mean(c(sum((G21-LTPR30Rs[1])^2), # 30th LTPR (reading)
                  sum((G22-LTPR30Rs[2])^2),
                  sum((G23-LTPR30Rs[3])^2),
                  sum((G24-LTPR30Rs[4])^2),
                  sum((G25-LTPR30Rs[5])^2),
                  sum((G26-LTPR30Rs[6])^2),
                  sum((G27-LTPR30Rs[7])^2),
                  sum((G28-LTPR30Rs[8])^2),
                  sum((G29-LTPR30Rs[9])^2),
                  sum((G30-LTPR30Rs[10])^2)))/20 + 11/90*sum((LTPR30Rs-LTPR30R)^2)
TV_35TM <- mean(c(sum((H11-LTPR35Ms[1])^2), # 35th LTPR (math)
                  sum((H12-LTPR35Ms[2])^2),
                  sum((H13-LTPR35Ms[3])^2),
                  sum((H14-LTPR35Ms[4])^2),
                  sum((H15-LTPR35Ms[5])^2),
                  sum((H16-LTPR35Ms[6])^2),
                  sum((H17-LTPR35Ms[7])^2),
                  sum((H18-LTPR35Ms[8])^2),
                  sum((H19-LTPR35Ms[9])^2),
                  sum((H20-LTPR35Ms[10])^2)))/20 + 11/90*sum((LTPR35Ms-LTPR35M)^2)
TV_35TR <- mean(c(sum((H21-LTPR35Rs[1])^2), # 35th LTPR (reading)
                  sum((H22-LTPR35Rs[2])^2),
                  sum((H23-LTPR35Rs[3])^2),
                  sum((H24-LTPR35Rs[4])^2),
                  sum((H25-LTPR35Rs[5])^2),
                  sum((H26-LTPR35Rs[6])^2),
                  sum((H27-LTPR35Rs[7])^2),
                  sum((H28-LTPR35Rs[8])^2),
                  sum((H29-LTPR35Rs[9])^2),
                  sum((H30-LTPR35Rs[10])^2)))/20 + 11/90*sum((LTPR35Rs-LTPR35R)^2)
TV_40TM <- mean(c(sum((I11-LTPR40Ms[1])^2), # 40th LTPR (math)
                  sum((I12-LTPR40Ms[2])^2),
                  sum((I13-LTPR40Ms[3])^2),
                  sum((I14-LTPR40Ms[4])^2),
                  sum((I15-LTPR40Ms[5])^2),
                  sum((I16-LTPR40Ms[6])^2),
                  sum((I17-LTPR40Ms[7])^2),
                  sum((I18-LTPR40Ms[8])^2),
                  sum((I19-LTPR40Ms[9])^2),
                  sum((I20-LTPR40Ms[10])^2)))/20 + 11/90*sum((LTPR40Ms-LTPR40M)^2)
TV_40TR <- mean(c(sum((I21-LTPR40Rs[1])^2), # 40th LTPR (reading)
                  sum((I22-LTPR40Rs[2])^2),
                  sum((I23-LTPR40Rs[3])^2),
                  sum((I24-LTPR40Rs[4])^2),
                  sum((I25-LTPR40Rs[5])^2),
                  sum((I26-LTPR40Rs[6])^2),
                  sum((I27-LTPR40Rs[7])^2),
                  sum((I28-LTPR40Rs[8])^2),
                  sum((I29-LTPR40Rs[9])^2),
                  sum((I30-LTPR40Rs[10])^2)))/20 + 11/90*sum((LTPR40Rs-LTPR40R)^2)
TV_45TM <- mean(c(sum((J11-LTPR45Ms[1])^2), # 45th LTPR (math)
                  sum((J12-LTPR45Ms[2])^2),
                  sum((J13-LTPR45Ms[3])^2),
                  sum((J14-LTPR45Ms[4])^2),
                  sum((J15-LTPR45Ms[5])^2),
                  sum((J16-LTPR45Ms[6])^2),
                  sum((J17-LTPR45Ms[7])^2),
                  sum((J18-LTPR45Ms[8])^2),
                  sum((J19-LTPR45Ms[9])^2),
                  sum((J20-LTPR45Ms[10])^2)))/20 + 11/90*sum((LTPR45Ms-LTPR45M)^2)
TV_45TR <- mean(c(sum((J21-LTPR45Rs[1])^2), # 45th LTPR (reading)
                  sum((J22-LTPR45Rs[2])^2),
                  sum((J23-LTPR45Rs[3])^2),
                  sum((J24-LTPR45Rs[4])^2),
                  sum((J25-LTPR45Rs[5])^2),
                  sum((J26-LTPR45Rs[6])^2),
                  sum((J27-LTPR45Rs[7])^2),
                  sum((J28-LTPR45Rs[8])^2),
                  sum((J29-LTPR45Rs[9])^2),
                  sum((J30-LTPR45Rs[10])^2)))/20 + 11/90*sum((LTPR45Rs-LTPR45R)^2)
TV_50TM <- mean(c(sum((K11-LTPR50Ms[1])^2), # 50th LTPR (math)
                  sum((K12-LTPR50Ms[2])^2),
                  sum((K13-LTPR50Ms[3])^2),
                  sum((K14-LTPR50Ms[4])^2),
                  sum((K15-LTPR50Ms[5])^2),
                  sum((K16-LTPR50Ms[6])^2),
                  sum((K17-LTPR50Ms[7])^2),
                  sum((K18-LTPR50Ms[8])^2),
                  sum((K19-LTPR50Ms[9])^2),
                  sum((K20-LTPR50Ms[10])^2)))/20 + 11/90*sum((LTPR50Ms-LTPR50M)^2)
TV_50TR <- mean(c(sum((K21-LTPR50Rs[1])^2), # 50th LTPR (reading)
                  sum((K22-LTPR50Rs[2])^2),
                  sum((K23-LTPR50Rs[3])^2),
                  sum((K24-LTPR50Rs[4])^2),
                  sum((K25-LTPR50Rs[5])^2),
                  sum((K26-LTPR50Rs[6])^2),
                  sum((K27-LTPR50Rs[7])^2),
                  sum((K28-LTPR50Rs[8])^2),
                  sum((K29-LTPR50Rs[9])^2),
                  sum((K30-LTPR50Rs[10])^2)))/20 + 11/90*sum((LTPR50Rs-LTPR50R)^2)
TV_55TM <- mean(c(sum((L11-LTPR55Ms[1])^2), # 55th LTPR (math)
                  sum((L12-LTPR55Ms[2])^2),
                  sum((L13-LTPR55Ms[3])^2),
                  sum((L14-LTPR55Ms[4])^2),
                  sum((L15-LTPR55Ms[5])^2),
                  sum((L16-LTPR55Ms[6])^2),
                  sum((L17-LTPR55Ms[7])^2),
                  sum((L18-LTPR55Ms[8])^2),
                  sum((L19-LTPR55Ms[9])^2),
                  sum((L20-LTPR55Ms[10])^2)))/20 + 11/90*sum((LTPR55Ms-LTPR55M)^2)
TV_55TR <- mean(c(sum((L21-LTPR55Rs[1])^2), # 55th LTPR (reading)
                  sum((L22-LTPR55Rs[2])^2),
                  sum((L23-LTPR55Rs[3])^2),
                  sum((L24-LTPR55Rs[4])^2),
                  sum((L25-LTPR55Rs[5])^2),
                  sum((L26-LTPR55Rs[6])^2),
                  sum((L27-LTPR55Rs[7])^2),
                  sum((L28-LTPR55Rs[8])^2),
                  sum((L29-LTPR55Rs[9])^2),
                  sum((L30-LTPR55Rs[10])^2)))/20 + 11/90*sum((LTPR55Rs-LTPR55R)^2)
TV_60TM <- mean(c(sum((M11-LTPR60Ms[1])^2), # 60th LTPR (math)
                  sum((M12-LTPR60Ms[2])^2),
                  sum((M13-LTPR60Ms[3])^2),
                  sum((M14-LTPR60Ms[4])^2),
                  sum((M15-LTPR60Ms[5])^2),
                  sum((M16-LTPR60Ms[6])^2),
                  sum((M17-LTPR60Ms[7])^2),
                  sum((M18-LTPR60Ms[8])^2),
                  sum((M19-LTPR60Ms[9])^2),
                  sum((M20-LTPR60Ms[10])^2)))/20 + 11/90*sum((LTPR60Ms-LTPR60M)^2)
TV_60TR <- mean(c(sum((M21-LTPR60Rs[1])^2), # 60th LTPR (reading)
                  sum((M22-LTPR60Rs[2])^2),
                  sum((M23-LTPR60Rs[3])^2),
                  sum((M24-LTPR60Rs[4])^2),
                  sum((M25-LTPR60Rs[5])^2),
                  sum((M26-LTPR60Rs[6])^2),
                  sum((M27-LTPR60Rs[7])^2),
                  sum((M28-LTPR60Rs[8])^2),
                  sum((M29-LTPR60Rs[9])^2),
                  sum((M30-LTPR60Rs[10])^2)))/20 + 11/90*sum((LTPR60Rs-LTPR60R)^2)
TV_65TM <- mean(c(sum((N11-LTPR65Ms[1])^2), # 65th LTPR (math)
                  sum((N12-LTPR65Ms[2])^2),
                  sum((N13-LTPR65Ms[3])^2),
                  sum((N14-LTPR65Ms[4])^2),
                  sum((N15-LTPR65Ms[5])^2),
                  sum((N16-LTPR65Ms[6])^2),
                  sum((N17-LTPR65Ms[7])^2),
                  sum((N18-LTPR65Ms[8])^2),
                  sum((N19-LTPR65Ms[9])^2),
                  sum((N20-LTPR65Ms[10])^2)))/20 + 11/90*sum((LTPR65Ms-LTPR65M)^2)
TV_65TR <- mean(c(sum((N21-LTPR65Rs[1])^2), # 65th LTPR (reading)
                  sum((N22-LTPR65Rs[2])^2),
                  sum((N23-LTPR65Rs[3])^2),
                  sum((N24-LTPR65Rs[4])^2),
                  sum((N25-LTPR65Rs[5])^2),
                  sum((N26-LTPR65Rs[6])^2),
                  sum((N27-LTPR65Rs[7])^2),
                  sum((N28-LTPR65Rs[8])^2),
                  sum((N29-LTPR65Rs[9])^2),
                  sum((N30-LTPR65Rs[10])^2)))/20 + 11/90*sum((LTPR65Rs-LTPR65R)^2)
TV_70TM <- mean(c(sum((O11-LTPR70Ms[1])^2), # 70th LTPR (math)
                  sum((O12-LTPR70Ms[2])^2),
                  sum((O13-LTPR70Ms[3])^2),
                  sum((O14-LTPR70Ms[4])^2),
                  sum((O15-LTPR70Ms[5])^2),
                  sum((O16-LTPR70Ms[6])^2),
                  sum((O17-LTPR70Ms[7])^2),
                  sum((O18-LTPR70Ms[8])^2),
                  sum((O19-LTPR70Ms[9])^2),
                  sum((O20-LTPR70Ms[10])^2)))/20 + 11/90*sum((LTPR70Ms-LTPR70M)^2)
TV_70TR <- mean(c(sum((O21-LTPR70Rs[1])^2), # 70th LTPR (reading)
                  sum((O22-LTPR70Rs[2])^2),
                  sum((O23-LTPR70Rs[3])^2),
                  sum((O24-LTPR70Rs[4])^2),
                  sum((O25-LTPR70Rs[5])^2),
                  sum((O26-LTPR70Rs[6])^2),
                  sum((O27-LTPR70Rs[7])^2),
                  sum((O28-LTPR70Rs[8])^2),
                  sum((O29-LTPR70Rs[9])^2),
                  sum((O30-LTPR70Rs[10])^2)))/20 + 11/90*sum((LTPR70Rs-LTPR70R)^2)
TV_75TM <- mean(c(sum((Q11-LTPR75Ms[1])^2), # 75th LTPR (math)
                  sum((Q12-LTPR75Ms[2])^2),
                  sum((Q13-LTPR75Ms[3])^2),
                  sum((Q14-LTPR75Ms[4])^2),
                  sum((Q15-LTPR75Ms[5])^2),
                  sum((Q16-LTPR75Ms[6])^2),
                  sum((Q17-LTPR75Ms[7])^2),
                  sum((Q18-LTPR75Ms[8])^2),
                  sum((Q19-LTPR75Ms[9])^2),
                  sum((Q20-LTPR75Ms[10])^2)))/20 + 11/90*sum((LTPR75Ms-LTPR75M)^2)
TV_75TR <- mean(c(sum((Q21-LTPR75Rs[1])^2), # 75th LTPR (reading)
                  sum((Q22-LTPR75Rs[2])^2),
                  sum((Q23-LTPR75Rs[3])^2),
                  sum((Q24-LTPR75Rs[4])^2),
                  sum((Q25-LTPR75Rs[5])^2),
                  sum((Q26-LTPR75Rs[6])^2),
                  sum((Q27-LTPR75Rs[7])^2),
                  sum((Q28-LTPR75Rs[8])^2),
                  sum((Q29-LTPR75Rs[9])^2),
                  sum((Q30-LTPR75Rs[10])^2)))/20 + 11/90*sum((LTPR75Rs-LTPR75R)^2)
TV_80TM <- mean(c(sum((R11-LTPR80Ms[1])^2), # 80th LTPR (math)
                  sum((R12-LTPR80Ms[2])^2),
                  sum((R13-LTPR80Ms[3])^2),
                  sum((R14-LTPR80Ms[4])^2),
                  sum((R15-LTPR80Ms[5])^2),
                  sum((R16-LTPR80Ms[6])^2),
                  sum((R17-LTPR80Ms[7])^2),
                  sum((R18-LTPR80Ms[8])^2),
                  sum((R19-LTPR80Ms[9])^2),
                  sum((R20-LTPR80Ms[10])^2)))/20 + 11/90*sum((LTPR80Ms-LTPR80M)^2)
TV_80TR <- mean(c(sum((R21-LTPR80Rs[1])^2), # 80th LTPR (reading)
                  sum((R22-LTPR80Rs[2])^2),
                  sum((R23-LTPR80Rs[3])^2),
                  sum((R24-LTPR80Rs[4])^2),
                  sum((R25-LTPR80Rs[5])^2),
                  sum((R26-LTPR80Rs[6])^2),
                  sum((R27-LTPR80Rs[7])^2),
                  sum((R28-LTPR80Rs[8])^2),
                  sum((R29-LTPR80Rs[9])^2),
                  sum((R30-LTPR80Rs[10])^2)))/20 + 11/90*sum((LTPR80Rs-LTPR80R)^2)
TV_85TM <- mean(c(sum((S11-LTPR85Ms[1])^2), # 85th LTPR (math)
                  sum((S12-LTPR85Ms[2])^2),
                  sum((S13-LTPR85Ms[3])^2),
                  sum((S14-LTPR85Ms[4])^2),
                  sum((S15-LTPR85Ms[5])^2),
                  sum((S16-LTPR85Ms[6])^2),
                  sum((S17-LTPR85Ms[7])^2),
                  sum((S18-LTPR85Ms[8])^2),
                  sum((S19-LTPR85Ms[9])^2),
                  sum((S20-LTPR85Ms[10])^2)))/20 + 11/90*sum((LTPR85Ms-LTPR85M)^2)
TV_85TR <- mean(c(sum((S21-LTPR85Rs[1])^2), # 85th LTPR (reading)
                  sum((S22-LTPR85Rs[2])^2),
                  sum((S23-LTPR85Rs[3])^2),
                  sum((S24-LTPR85Rs[4])^2),
                  sum((S25-LTPR85Rs[5])^2),
                  sum((S26-LTPR85Rs[6])^2),
                  sum((S27-LTPR85Rs[7])^2),
                  sum((S28-LTPR85Rs[8])^2),
                  sum((S29-LTPR85Rs[9])^2),
                  sum((S30-LTPR85Rs[10])^2)))/20 + 11/90*sum((LTPR85Rs-LTPR85R)^2)
TV_90TM <- mean(c(sum((T11-LTPR90Ms[1])^2), # 90th LTPR (math)
                  sum((T12-LTPR90Ms[2])^2),
                  sum((T13-LTPR90Ms[3])^2),
                  sum((T14-LTPR90Ms[4])^2),
                  sum((T15-LTPR90Ms[5])^2),
                  sum((T16-LTPR90Ms[6])^2),
                  sum((T17-LTPR90Ms[7])^2),
                  sum((T18-LTPR90Ms[8])^2),
                  sum((T19-LTPR90Ms[9])^2),
                  sum((T20-LTPR90Ms[10])^2)))/20 + 11/90*sum((LTPR90Ms-LTPR90M)^2)
TV_90TR <- mean(c(sum((T21-LTPR90Rs[1])^2), # 90th LTPR (reading)
                  sum((T22-LTPR90Rs[2])^2),
                  sum((T23-LTPR90Rs[3])^2),
                  sum((T24-LTPR90Rs[4])^2),
                  sum((T25-LTPR90Rs[5])^2),
                  sum((T26-LTPR90Rs[6])^2),
                  sum((T27-LTPR90Rs[7])^2),
                  sum((T28-LTPR90Rs[8])^2),
                  sum((T29-LTPR90Rs[9])^2),
                  sum((T30-LTPR90Rs[10])^2)))/20 + 11/90*sum((LTPR90Rs-LTPR90R)^2)
TV_95TM <- mean(c(sum((U11-LTPR95Ms[1])^2), # 95th LTPR (math)
                  sum((U12-LTPR95Ms[2])^2),
                  sum((U13-LTPR95Ms[3])^2),
                  sum((U14-LTPR95Ms[4])^2),
                  sum((U15-LTPR95Ms[5])^2),
                  sum((U16-LTPR95Ms[6])^2),
                  sum((U17-LTPR95Ms[7])^2),
                  sum((U18-LTPR95Ms[8])^2),
                  sum((U19-LTPR95Ms[9])^2),
                  sum((U20-LTPR95Ms[10])^2)))/20 + 11/90*sum((LTPR95Ms-LTPR95M)^2)
TV_95TR <- mean(c(sum((U21-LTPR95Rs[1])^2), # 95th LTPR (reading)
                  sum((U22-LTPR95Rs[2])^2),
                  sum((U23-LTPR95Rs[3])^2),
                  sum((U24-LTPR95Rs[4])^2),
                  sum((U25-LTPR95Rs[5])^2),
                  sum((U26-LTPR95Rs[6])^2),
                  sum((U27-LTPR95Rs[7])^2),
                  sum((U28-LTPR95Rs[8])^2),
                  sum((U29-LTPR95Rs[9])^2),
                  sum((U30-LTPR95Rs[10])^2)))/20 + 11/90*sum((LTPR95Rs-LTPR95R)^2)
TV_Md95TM <- mean(c(sum((U11-K11-Md95TMs[1])^2), # Md95TM
                    sum((U12-K12-Md95TMs[2])^2),
                    sum((U13-K13-Md95TMs[3])^2),
                    sum((U14-K14-Md95TMs[4])^2),
                    sum((U15-K15-Md95TMs[5])^2),
                    sum((U16-K16-Md95TMs[6])^2),
                    sum((U17-K17-Md95TMs[7])^2),
                    sum((U18-K18-Md95TMs[8])^2),
                    sum((U19-K19-Md95TMs[9])^2),
                    sum((U20-K20-Md95TMs[10])^2)))
TV_Md95TR <- mean(c(sum((U21-K21-Md95TRs[1])^2), # Md95TR
                    sum((U22-K22-Md95TRs[2])^2),
                    sum((U23-K23-Md95TRs[3])^2),
                    sum((U24-K24-Md95TRs[4])^2),
                    sum((U25-K25-Md95TRs[5])^2),
                    sum((U26-K26-Md95TRs[6])^2),
                    sum((U27-K27-Md95TRs[7])^2),
                    sum((U28-K28-Md95TRs[8])^2),
                    sum((U29-K29-Md95TRs[9])^2),
                    sum((U30-K30-Md95TRs[10])^2)))
TV_Md90TM <- mean(c(sum((T11-K11-Md90TMs[1])^2), # Md90TM
                    sum((T12-K12-Md90TMs[2])^2),
                    sum((T13-K13-Md90TMs[3])^2),
                    sum((T14-K14-Md90TMs[4])^2),
                    sum((T15-K15-Md90TMs[5])^2),
                    sum((T16-K16-Md90TMs[6])^2),
                    sum((T17-K17-Md90TMs[7])^2),
                    sum((T18-K18-Md90TMs[8])^2),
                    sum((T19-K19-Md90TMs[9])^2),
                    sum((T20-K20-Md90TMs[10])^2)))
TV_Md90TR <- mean(c(sum((T21-K21-Md90TRs[1])^2), # Md90TR
                    sum((T22-K22-Md90TRs[2])^2),
                    sum((T23-K23-Md90TRs[3])^2),
                    sum((T24-K24-Md90TRs[4])^2),
                    sum((T25-K25-Md90TRs[5])^2),
                    sum((T26-K26-Md90TRs[6])^2),
                    sum((T27-K27-Md90TRs[7])^2),
                    sum((T28-K28-Md90TRs[8])^2),
                    sum((T29-K29-Md90TRs[9])^2),
                    sum((T30-K30-Md90TRs[10])^2)))
TV_Md10TM <- mean(c(sum((K11-C11-Md10TMs[1])^2), # Md10TM
                    sum((K12-C12-Md10TMs[2])^2),
                    sum((K13-C13-Md10TMs[3])^2),
                    sum((K14-C14-Md10TMs[4])^2),
                    sum((K15-C15-Md10TMs[5])^2),
                    sum((K16-C16-Md10TMs[6])^2),
                    sum((K17-C17-Md10TMs[7])^2),
                    sum((K18-C18-Md10TMs[8])^2),
                    sum((K19-C19-Md10TMs[9])^2),
                    sum((K20-C20-Md10TMs[10])^2)))
TV_Md10TR <- mean(c(sum((K21-C21-Md10TRs[1])^2), # Md10TR
                    sum((K22-C22-Md10TRs[2])^2),
                    sum((K23-C23-Md10TRs[3])^2),
                    sum((K24-C24-Md10TRs[4])^2),
                    sum((K25-C25-Md10TRs[5])^2),
                    sum((K26-C26-Md10TRs[6])^2),
                    sum((K27-C27-Md10TRs[7])^2),
                    sum((K28-C28-Md10TRs[8])^2),
                    sum((K29-C29-Md10TRs[9])^2),
                    sum((K30-C30-Md10TRs[10])^2)))
TV_Md05TM <- mean(c(sum((K11-B11-Md05TMs[1])^2), # Md05TM
                    sum((K12-B12-Md05TMs[2])^2),
                    sum((K13-B13-Md05TMs[3])^2),
                    sum((K14-B14-Md05TMs[4])^2),
                    sum((K15-B15-Md05TMs[5])^2),
                    sum((K16-B16-Md05TMs[6])^2),
                    sum((K17-B17-Md05TMs[7])^2),
                    sum((K18-B18-Md05TMs[8])^2),
                    sum((K19-B19-Md05TMs[9])^2),
                    sum((K20-B20-Md05TMs[10])^2)))
TV_Md05TR <- mean(c(sum((K21-B21-Md05TRs[1])^2), # Md05TR
                    sum((K22-B22-Md05TRs[2])^2),
                    sum((K23-B23-Md05TRs[3])^2),
                    sum((K24-B24-Md05TRs[4])^2),
                    sum((K25-B25-Md05TRs[5])^2),
                    sum((K26-B26-Md05TRs[6])^2),
                    sum((K27-B27-Md05TRs[7])^2),
                    sum((K28-B28-Md05TRs[8])^2),
                    sum((K29-B29-Md05TRs[9])^2),
                    sum((K30-B30-Md05TRs[10])^2)))
TV_Mn95TM <- mean(c(sum((U11-A11-Mn95TMs[1])^2), # Mn95TM
                    sum((U12-A12-Mn95TMs[2])^2),
                    sum((U13-A13-Mn95TMs[3])^2),
                    sum((U14-A14-Mn95TMs[4])^2),
                    sum((U15-A18-Mn95TMs[5])^2),
                    sum((U16-A16-Mn95TMs[6])^2),
                    sum((U17-A17-Mn95TMs[7])^2),
                    sum((U18-A18-Mn95TMs[8])^2),
                    sum((U19-A19-Mn95TMs[9])^2),
                    sum((U20-A20-Mn95TMs[10])^2)))
TV_Mn95TR <- mean(c(sum((U21-A21-Mn95TRs[1])^2), # Mn95TR
                    sum((U22-A22-Mn95TRs[2])^2),
                    sum((U23-A23-Mn95TRs[3])^2),
                    sum((U24-A24-Mn95TRs[4])^2),
                    sum((U25-A25-Mn95TRs[5])^2),
                    sum((U26-A26-Mn95TRs[6])^2),
                    sum((U27-A27-Mn95TRs[7])^2),
                    sum((U28-A28-Mn95TRs[8])^2),
                    sum((U29-A29-Mn95TRs[9])^2),
                    sum((U30-A30-Mn95TRs[10])^2)))
TV_Mn90TM <- mean(c(sum((T11-A11-Mn90TMs[1])^2), # Mn90TM
                    sum((T12-A12-Mn90TMs[2])^2),
                    sum((T13-A13-Mn90TMs[3])^2),
                    sum((T14-A14-Mn90TMs[4])^2),
                    sum((T15-A18-Mn90TMs[5])^2),
                    sum((T16-A16-Mn90TMs[6])^2),
                    sum((T17-A17-Mn90TMs[7])^2),
                    sum((T18-A18-Mn90TMs[8])^2),
                    sum((T19-A19-Mn90TMs[9])^2),
                    sum((T20-A20-Mn90TMs[10])^2)))
TV_Mn90TR <- mean(c(sum((T21-A21-Mn90TRs[1])^2), # Mn90TR
                    sum((T22-A22-Mn90TRs[2])^2),
                    sum((T23-A23-Mn90TRs[3])^2),
                    sum((T24-A24-Mn90TRs[4])^2),
                    sum((T25-A25-Mn90TRs[5])^2),
                    sum((T26-A26-Mn90TRs[6])^2),
                    sum((T27-A27-Mn90TRs[7])^2),
                    sum((T28-A28-Mn90TRs[8])^2),
                    sum((T29-A29-Mn90TRs[9])^2),
                    sum((T30-A30-Mn90TRs[10])^2)))
TV_Mn10TM <- mean(c(sum((A11-C11-Mn10TMs[1])^2), # Mn10TM
                    sum((A12-C12-Mn10TMs[2])^2),
                    sum((A13-C13-Mn10TMs[3])^2),
                    sum((A14-C14-Mn10TMs[4])^2),
                    sum((A18-C15-Mn10TMs[5])^2),
                    sum((A16-C16-Mn10TMs[6])^2),
                    sum((A17-C17-Mn10TMs[7])^2),
                    sum((A18-C18-Mn10TMs[8])^2),
                    sum((A19-C19-Mn10TMs[9])^2),
                    sum((A20-C20-Mn10TMs[10])^2)))
TV_Mn10TR <- mean(c(sum((A21-C21-Mn10TRs[1])^2), # Mn10TR
                    sum((A22-C22-Mn10TRs[2])^2),
                    sum((A23-C23-Mn10TRs[3])^2),
                    sum((A24-C24-Mn10TRs[4])^2),
                    sum((A25-C25-Mn10TRs[5])^2),
                    sum((A26-C26-Mn10TRs[6])^2),
                    sum((A27-C27-Mn10TRs[7])^2),
                    sum((A28-C28-Mn10TRs[8])^2),
                    sum((A29-C29-Mn10TRs[9])^2),
                    sum((A30-C30-Mn10TRs[10])^2)))
TV_Mn05TM <- mean(c(sum((A11-B11-Mn05TMs[1])^2), # Mn05TM
                    sum((A12-B12-Mn05TMs[2])^2),
                    sum((A13-B13-Mn05TMs[3])^2),
                    sum((A14-B14-Mn05TMs[4])^2),
                    sum((A18-B15-Mn05TMs[5])^2),
                    sum((A16-B16-Mn05TMs[6])^2),
                    sum((A17-B17-Mn05TMs[7])^2),
                    sum((A18-B18-Mn05TMs[8])^2),
                    sum((A19-B19-Mn05TMs[9])^2),
                    sum((A20-B20-Mn05TMs[10])^2)))
TV_Mn05TR <- mean(c(sum((A21-B21-Mn05TRs[1])^2), # Mn05TR
                    sum((A22-B22-Mn05TRs[2])^2),
                    sum((A23-B23-Mn05TRs[3])^2),
                    sum((A24-B24-Mn05TRs[4])^2),
                    sum((A25-B25-Mn05TRs[5])^2),
                    sum((A26-B26-Mn05TRs[6])^2),
                    sum((A27-B27-Mn05TRs[7])^2),
                    sum((A28-B28-Mn05TRs[8])^2),
                    sum((A29-B29-Mn05TRs[9])^2),
                    sum((A30-B30-Mn05TRs[10])^2)))

# standard errors
SE_MnM <- sqrt(TV_MnM) # mean LTPR (math)
SE_MnR <- sqrt(TV_MnR) # mean LTPR (reading)
SE_05TM <- sqrt(TV_05TM) # 5th LTPR (math)
SE_05TR <- sqrt(TV_05TR) # 5th LTPR (reading)
SE_10TM <- sqrt(TV_10TM) # 10th LTPR (math)
SE_10TR <- sqrt(TV_10TR) # 10th LTPR (reading)
SE_15TM <- sqrt(TV_15TM) # 15th LTPR (math)
SE_15TR <- sqrt(TV_15TR) # 15th LTPR (reading)
SE_20TM <- sqrt(TV_20TM) # 20th LTPR (math)
SE_20TR <- sqrt(TV_20TR) # 20th LTPR (reading)
SE_25TM <- sqrt(TV_25TM) # 25th LTPR (math)
SE_25TR <- sqrt(TV_25TR) # 25th LTPR (reading)
SE_30TM <- sqrt(TV_30TM) # 30th LTPR (math)
SE_30TR <- sqrt(TV_30TR) # 30th LTPR (reading)
SE_35TM <- sqrt(TV_35TM) # 35th LTPR (math)
SE_35TR <- sqrt(TV_35TR) # 35th LTPR (reading)
SE_40TM <- sqrt(TV_40TM) # 40th LTPR (math)
SE_40TR <- sqrt(TV_40TR) # 40th LTPR (reading)
SE_45TM <- sqrt(TV_45TM) # 45th LTPR (math)
SE_45TR <- sqrt(TV_45TR) # 45th LTPR (reading)
SE_50TM <- sqrt(TV_50TM) # 50th LTPR (math)
SE_50TR <- sqrt(TV_50TR) # 50th LTPR (reading)
SE_55TM <- sqrt(TV_55TM) # 55th LTPR (math)
SE_55TR <- sqrt(TV_55TR) # 55th LTPR (reading)
SE_60TM <- sqrt(TV_60TM) # 60th LTPR (math)
SE_60TR <- sqrt(TV_60TR) # 60th LTPR (reading)
SE_65TM <- sqrt(TV_65TM) # 65th LTPR (math)
SE_65TR <- sqrt(TV_65TR) # 65th LTPR (reading)
SE_70TM <- sqrt(TV_70TM) # 70th LTPR (math)
SE_70TR <- sqrt(TV_70TR) # 70th LTPR (reading)
SE_75TM <- sqrt(TV_75TM) # 75th LTPR (math)
SE_75TR <- sqrt(TV_75TR) # 75th LTPR (reading)
SE_80TM <- sqrt(TV_80TM) # 80th LTPR (math)
SE_80TR <- sqrt(TV_80TR) # 80th LTPR (reading)
SE_85TM <- sqrt(TV_85TM) # 85th LTPR (math)
SE_85TR <- sqrt(TV_85TR) # 85th LTPR (reading)
SE_90TM <- sqrt(TV_90TM) # 90th LTPR (math)
SE_90TR <- sqrt(TV_90TR) # 90th LTPR (reading)
SE_95TM <- sqrt(TV_95TM) # 95th LTPR (math)
SE_95TR <- sqrt(TV_95TR) # 95th LTPR (reading)
SE_Md95TM <- sqrt(TV_Md95TM) # Md95T (math)
SE_Md95TR <- sqrt(TV_Md95TR) # Md95T (reading)
SE_Md90TM <- sqrt(TV_Md90TM) # Md90T (math)
SE_Md90TR <- sqrt(TV_Md90TR) # Md90T (reading)
SE_Md10TM <- sqrt(TV_Md10TM) # Md10T (math)
SE_Md10TR <- sqrt(TV_Md10TR) # Md10T (reading)
SE_Md05TM <- sqrt(TV_Md05TM) # Md05T (math)
SE_Md05TR <- sqrt(TV_Md05TR) # Md05T (reading)
SE_Mn95TM <- sqrt(TV_Mn95TM) # Mn95T (math)
SE_Mn95TR <- sqrt(TV_Mn95TR) # Mn95T (reading)
SE_Mn90TM <- sqrt(TV_Mn90TM) # Mn90T (math)
SE_Mn90TR <- sqrt(TV_Mn90TR) # Mn90T (reading)
SE_Mn10TM <- sqrt(TV_Mn10TM) # Mn10T (math)
SE_Mn10TR <- sqrt(TV_Mn10TR) # Mn10T (reading)
SE_Mn05TM <- sqrt(TV_Mn05TM) # Mn05T (math)
SE_Mn05TR <- sqrt(TV_Mn05TR) # Mn05T (reading)



#### SEs: LU3Rs and LU3R tail-center differences ####

B11 <- B12 <- B13 <- B14 <- B15 <- B16 <- B17 <- B18 <- B19 <- B20 <- numeric(80) # empty containers
B21 <- B22 <- B23 <- B24 <- B25 <- B26 <- B27 <- B28 <- B29 <- B30 <- numeric(80) # empty containers
C11 <- C12 <- C13 <- C14 <- C15 <- C16 <- C17 <- C18 <- C19 <- C20 <- numeric(80) # empty containers
C21 <- C22 <- C23 <- C24 <- C25 <- C26 <- C27 <- C28 <- C29 <- C30 <- numeric(80) # empty containers
D11 <- D12 <- D13 <- D14 <- D15 <- D16 <- D17 <- D18 <- D19 <- D20 <- numeric(80) # empty containers
D21 <- D22 <- D23 <- D24 <- D25 <- D26 <- D27 <- D28 <- D29 <- D30 <- numeric(80) # empty containers
E11 <- E12 <- E13 <- E14 <- E15 <- E16 <- E17 <- E18 <- E19 <- E20 <- numeric(80) # empty containers
E21 <- E22 <- E23 <- E24 <- E25 <- E26 <- E27 <- E28 <- E29 <- E30 <- numeric(80) # empty containers
F11 <- F12 <- F13 <- F14 <- F15 <- F16 <- F17 <- F18 <- F19 <- F20 <- numeric(80) # empty containers
F21 <- F22 <- F23 <- F24 <- F25 <- F26 <- F27 <- F28 <- F29 <- F30 <- numeric(80) # empty containers
G11 <- G12 <- G13 <- G14 <- G15 <- G16 <- G17 <- G18 <- G19 <- G20 <- numeric(80) # empty containers
G21 <- G22 <- G23 <- G24 <- G25 <- G26 <- G27 <- G28 <- G29 <- G30 <- numeric(80) # empty containers
H11 <- H12 <- H13 <- H14 <- H15 <- H16 <- H17 <- H18 <- H19 <- H20 <- numeric(80) # empty containers
H21 <- H22 <- H23 <- H24 <- H25 <- H26 <- H27 <- H28 <- H29 <- H30 <- numeric(80) # empty containers
I11 <- I12 <- I13 <- I14 <- I15 <- I16 <- I17 <- I18 <- I19 <- I20 <- numeric(80) # empty containers
I21 <- I22 <- I23 <- I24 <- I25 <- I26 <- I27 <- I28 <- I29 <- I30 <- numeric(80) # empty containers
J11 <- J12 <- J13 <- J14 <- J15 <- J16 <- J17 <- J18 <- J19 <- J20 <- numeric(80) # empty containers
J21 <- J22 <- J23 <- J24 <- J25 <- J26 <- J27 <- J28 <- J29 <- J30 <- numeric(80) # empty containers
K11 <- K12 <- K13 <- K14 <- K15 <- K16 <- K17 <- K18 <- K19 <- K20 <- numeric(80) # empty containers
K21 <- K22 <- K23 <- K24 <- K25 <- K26 <- K27 <- K28 <- K29 <- K30 <- numeric(80) # empty containers
L11 <- L12 <- L13 <- L14 <- L15 <- L16 <- L17 <- L18 <- L19 <- L20 <- numeric(80) # empty containers
L21 <- L22 <- L23 <- L24 <- L25 <- L26 <- L27 <- L28 <- L29 <- L30 <- numeric(80) # empty containers
M11 <- M12 <- M13 <- M14 <- M15 <- M16 <- M17 <- M18 <- M19 <- M20 <- numeric(80) # empty containers
M21 <- M22 <- M23 <- M24 <- M25 <- M26 <- M27 <- M28 <- M29 <- M30 <- numeric(80) # empty containers
N11 <- N12 <- N13 <- N14 <- N15 <- N16 <- N17 <- N18 <- N19 <- N20 <- numeric(80) # empty containers
N21 <- N22 <- N23 <- N24 <- N25 <- N26 <- N27 <- N28 <- N29 <- N30 <- numeric(80) # empty containers
O11 <- O12 <- O13 <- O14 <- O15 <- O16 <- O17 <- O18 <- O19 <- O20 <- numeric(80) # empty containers
O21 <- O22 <- O23 <- O24 <- O25 <- O26 <- O27 <- O28 <- O29 <- O30 <- numeric(80) # empty containers
Q11 <- Q12 <- Q13 <- Q14 <- Q15 <- Q16 <- Q17 <- Q18 <- Q19 <- Q20 <- numeric(80) # empty containers
Q21 <- Q22 <- Q23 <- Q24 <- Q25 <- Q26 <- Q27 <- Q28 <- Q29 <- Q30 <- numeric(80) # empty containers
R11 <- R12 <- R13 <- R14 <- R15 <- R16 <- R17 <- R18 <- R19 <- R20 <- numeric(80) # empty containers
R21 <- R22 <- R23 <- R24 <- R25 <- R26 <- R27 <- R28 <- R29 <- R30 <- numeric(80) # empty containers
S11 <- S12 <- S13 <- S14 <- S15 <- S16 <- S17 <- S18 <- S19 <- S20 <- numeric(80) # empty containers
S21 <- S22 <- S23 <- S24 <- S25 <- S26 <- S27 <- S28 <- S29 <- S30 <- numeric(80) # empty containers
T11 <- T12 <- T13 <- T14 <- T15 <- T16 <- T17 <- T18 <- T19 <- T20 <- numeric(80) # empty containers
T21 <- T22 <- T23 <- T24 <- T25 <- T26 <- T27 <- T28 <- T29 <- T30 <- numeric(80) # empty containers
U11 <- U12 <- U13 <- U14 <- U15 <- U16 <- U17 <- U18 <- U19 <- U20 <- numeric(80) # empty containers
U21 <- U22 <- U23 <- U24 <- U25 <- U26 <- U27 <- U28 <- U29 <- U30 <- numeric(80) # empty containers

# perform bootstrap resampling of LU3Rs
for (i in 1:80) {
  W <- paste0('BW',i)
  
  B11[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV1') # MV1 reweighted 5th LU3R
  B12[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV2') # MV2
  B13[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV3') # MV3
  B14[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV4') # MV4
  B15[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV5') # MV5
  B16[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV6') # MV6
  B17[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV7') # MV7
  B18[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV8') # MV8
  B19[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MV9') # MV9
  B20[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'MVX') # MVX
  B21[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV1') # RV1 reweighted 5th LU3R
  B22[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV2') # RV2
  B23[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV3') # RV3
  B24[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV4') # RV4
  B25[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV5') # RV5
  B26[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV6') # RV6
  B27[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV7') # RV7
  B28[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV8') # RV8
  B29[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RV9') # RV9
  B30[i] <- LU3Rfn(P18_M, P18_F, .05, W, 'RVX') # RVX
  
  C11[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV1') # MV1 reweighted 10th LU3R
  C12[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV2') # MV2
  C13[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV3') # MV3
  C14[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV4') # MV4
  C15[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV5') # MV5
  C16[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV6') # MV6
  C17[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV7') # MV7
  C18[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV8') # MV8
  C19[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MV9') # MV9
  C20[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'MVX') # MVX
  C21[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV1') # RV1 reweighted 10th LU3R
  C22[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV2') # RV2
  C23[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV3') # RV3
  C24[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV4') # RV4
  C25[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV5') # RV5
  C26[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV6') # RV6
  C27[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV7') # RV7
  C28[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV8') # RV8
  C29[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RV9') # RV9
  C30[i] <- LU3Rfn(P18_M, P18_F, .10, W, 'RVX') # RVX
  
  D11[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV1') # MV1 reweighted 15th LU3R
  D12[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV2') # MV2
  D13[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV3') # MV3
  D14[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV4') # MV4
  D15[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV5') # MV5
  D16[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV6') # MV6
  D17[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV7') # MV7
  D18[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV8') # MV8
  D19[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MV9') # MV9
  D20[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'MVX') # MVX
  D21[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV1') # RV1 reweighted 15th LU3R
  D22[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV2') # RV2
  D23[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV3') # RV3
  D24[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV4') # RV4
  D25[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV5') # RV5
  D26[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV6') # RV6
  D27[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV7') # RV7
  D28[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV8') # RV8
  D29[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RV9') # RV9
  D30[i] <- LU3Rfn(P18_M, P18_F, .15, W, 'RVX') # RVX
  
  E11[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV1') # MV1 reweighted 20th LU3R
  E12[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV2') # MV2
  E13[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV3') # MV3
  E14[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV4') # MV4
  E15[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV5') # MV5
  E16[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV6') # MV6
  E17[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV7') # MV7
  E18[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV8') # MV8
  E19[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MV9') # MV9
  E20[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'MVX') # MVX
  E21[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV1') # RV1 reweighted 20th LU3R
  E22[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV2') # RV2
  E23[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV3') # RV3
  E24[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV4') # RV4
  E25[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV5') # RV5
  E26[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV6') # RV6
  E27[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV7') # RV7
  E28[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV8') # RV8
  E29[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RV9') # RV9
  E30[i] <- LU3Rfn(P18_M, P18_F, .20, W, 'RVX') # RVX
  
  F11[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV1') # MV1 reweighted 25th LU3R
  F12[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV2') # MV2
  F13[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV3') # MV3
  F14[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV4') # MV4
  F15[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV5') # MV5
  F16[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV6') # MV6
  F17[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV7') # MV7
  F18[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV8') # MV8
  F19[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MV9') # MV9
  F20[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'MVX') # MVX
  F21[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV1') # RV1 reweighted 25th LU3R
  F22[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV2') # RV2
  F23[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV3') # RV3
  F24[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV4') # RV4
  F25[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV5') # RV5
  F26[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV6') # RV6
  F27[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV7') # RV7
  F28[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV8') # RV8
  F29[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RV9') # RV9
  F30[i] <- LU3Rfn(P18_M, P18_F, .25, W, 'RVX') # RVX
  
  G11[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV1') # MV1 reweighted 30th LU3R
  G12[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV2') # MV2
  G13[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV3') # MV3
  G14[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV4') # MV4
  G15[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV5') # MV5
  G16[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV6') # MV6
  G17[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV7') # MV7
  G18[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV8') # MV8
  G19[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MV9') # MV9
  G20[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'MVX') # MVX
  G21[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV1') # RV1 reweighted 30th LU3R
  G22[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV2') # RV2
  G23[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV3') # RV3
  G24[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV4') # RV4
  G25[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV5') # RV5
  G26[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV6') # RV6
  G27[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV7') # RV7
  G28[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV8') # RV8
  G29[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RV9') # RV9
  G30[i] <- LU3Rfn(P18_M, P18_F, .30, W, 'RVX') # RVX
  
  H11[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV1') # MV1 reweighted 35th LU3R
  H12[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV2') # MV2
  H13[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV3') # MV3
  H14[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV4') # MV4
  H15[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV5') # MV5
  H16[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV6') # MV6
  H17[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV7') # MV7
  H18[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV8') # MV8
  H19[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MV9') # MV9
  H20[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'MVX') # MVX
  H21[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV1') # RV1 reweighted 35th LU3R
  H22[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV2') # RV2
  H23[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV3') # RV3
  H24[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV4') # RV4
  H25[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV5') # RV5
  H26[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV6') # RV6
  H27[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV7') # RV7
  H28[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV8') # RV8
  H29[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RV9') # RV9
  H30[i] <- LU3Rfn(P18_M, P18_F, .35, W, 'RVX') # RVX
  
  I11[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV1') # MV1 reweighted 40th LU3R
  I12[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV2') # MV2
  I13[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV3') # MV3
  I14[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV4') # MV4
  I15[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV5') # MV5
  I16[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV6') # MV6
  I17[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV7') # MV7
  I18[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV8') # MV8
  I19[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MV9') # MV9
  I20[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'MVX') # MVX
  I21[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV1') # RV1 reweighted 40th LU3R
  I22[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV2') # RV2
  I23[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV3') # RV3
  I24[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV4') # RV4
  I25[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV5') # RV5
  I26[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV6') # RV6
  I27[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV7') # RV7
  I28[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV8') # RV8
  I29[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RV9') # RV9
  I30[i] <- LU3Rfn(P18_M, P18_F, .40, W, 'RVX') # RVX
  
  J11[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV1') # MV1 reweighted 45th LU3R
  J12[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV2') # MV2
  J13[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV3') # MV3
  J14[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV4') # MV4
  J15[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV5') # MV5
  J16[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV6') # MV6
  J17[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV7') # MV7
  J18[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV8') # MV8
  J19[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MV9') # MV9
  J20[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'MVX') # MVX
  J21[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV1') # RV1 reweighted 45th LU3R
  J22[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV2') # RV2
  J23[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV3') # RV3
  J24[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV4') # RV4
  J25[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV5') # RV5
  J26[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV6') # RV6
  J27[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV7') # RV7
  J28[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV8') # RV8
  J29[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RV9') # RV9
  J30[i] <- LU3Rfn(P18_M, P18_F, .45, W, 'RVX') # RVX
  
  K11[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV1') # MV1 reweighted 50th LU3R
  K12[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV2') # MV2
  K13[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV3') # MV3
  K14[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV4') # MV4
  K15[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV5') # MV5
  K16[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV6') # MV6
  K17[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV7') # MV7
  K18[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV8') # MV8
  K19[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MV9') # MV9
  K20[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'MVX') # MVX
  K21[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV1') # RV1 reweighted 50th LU3R
  K22[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV2') # RV2
  K23[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV3') # RV3
  K24[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV4') # RV4
  K25[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV5') # RV5
  K26[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV6') # RV6
  K27[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV7') # RV7
  K28[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV8') # RV8
  K29[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RV9') # RV9
  K30[i] <- LU3Rfn(P18_M, P18_F, .50, W, 'RVX') # RVX
  
  L11[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV1') # MV1 reweighted 55th LU3R
  L12[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV2') # MV2
  L13[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV3') # MV3
  L14[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV4') # MV4
  L15[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV5') # MV5
  L16[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV6') # MV6
  L17[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV7') # MV7
  L18[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV8') # MV8
  L19[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MV9') # MV9
  L20[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'MVX') # MVX
  L21[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV1') # RV1 reweighted 55th LU3R
  L22[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV2') # RV2
  L23[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV3') # RV3
  L24[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV4') # RV4
  L25[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV5') # RV5
  L26[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV6') # RV6
  L27[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV7') # RV7
  L28[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV8') # RV8
  L29[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RV9') # RV9
  L30[i] <- LU3Rfn(P18_M, P18_F, .55, W, 'RVX') # RVX
  
  M11[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV1') # MV1 reweighted 60th LU3R
  M12[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV2') # MV2
  M13[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV3') # MV3
  M14[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV4') # MV4
  M15[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV5') # MV5
  M16[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV6') # MV6
  M17[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV7') # MV7
  M18[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV8') # MV8
  M19[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MV9') # MV9
  M20[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'MVX') # MVX
  M21[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV1') # RV1 reweighted 60th LU3R
  M22[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV2') # RV2
  M23[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV3') # RV3
  M24[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV4') # RV4
  M25[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV5') # RV5
  M26[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV6') # RV6
  M27[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV7') # RV7
  M28[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV8') # RV8
  M29[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RV9') # RV9
  M30[i] <- LU3Rfn(P18_M, P18_F, .60, W, 'RVX') # RVX
  
  N11[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV1') # MV1 reweighted 65th LU3R
  N12[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV2') # MV2
  N13[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV3') # MV3
  N14[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV4') # MV4
  N15[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV5') # MV5
  N16[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV6') # MV6
  N17[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV7') # MV7
  N18[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV8') # MV8
  N19[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MV9') # MV9
  N20[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'MVX') # MVX
  N21[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV1') # RV1 reweighted 65th LU3R
  N22[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV2') # RV2
  N23[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV3') # RV3
  N24[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV4') # RV4
  N25[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV5') # RV5
  N26[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV6') # RV6
  N27[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV7') # RV7
  N28[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV8') # RV8
  N29[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RV9') # RV9
  N30[i] <- LU3Rfn(P18_M, P18_F, .65, W, 'RVX') # RVX
  
  O11[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV1') # MV1 reweighted 70th LU3R
  O12[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV2') # MV2
  O13[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV3') # MV3
  O14[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV4') # MV4
  O15[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV5') # MV5
  O16[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV6') # MV6
  O17[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV7') # MV7
  O18[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV8') # MV8
  O19[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MV9') # MV9
  O20[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'MVX') # MVX
  O21[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV1') # RV1 reweighted 70th LU3R
  O22[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV2') # RV2
  O23[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV3') # RV3
  O24[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV4') # RV4
  O25[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV5') # RV5
  O26[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV6') # RV6
  O27[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV7') # RV7
  O28[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV8') # RV8
  O29[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RV9') # RV9
  O30[i] <- LU3Rfn(P18_M, P18_F, .70, W, 'RVX') # RVX
  
  Q11[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV1') # MV1 reweighted 75th LU3R
  Q12[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV2') # MV2
  Q13[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV3') # MV3
  Q14[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV4') # MV4
  Q15[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV5') # MV5
  Q16[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV6') # MV6
  Q17[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV7') # MV7
  Q18[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV8') # MV8
  Q19[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MV9') # MV9
  Q20[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'MVX') # MVX
  Q21[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV1') # RV1 reweighted 75th LU3R
  Q22[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV2') # RV2
  Q23[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV3') # RV3
  Q24[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV4') # RV4
  Q25[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV5') # RV5
  Q26[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV6') # RV6
  Q27[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV7') # RV7
  Q28[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV8') # RV8
  Q29[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RV9') # RV9
  Q30[i] <- LU3Rfn(P18_M, P18_F, .75, W, 'RVX') # RVX
  
  R11[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV1') # MV1 reweighted 80th LU3R
  R12[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV2') # MV2
  R13[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV3') # MV3
  R14[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV4') # MV4
  R15[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV5') # MV5
  R16[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV6') # MV6
  R17[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV7') # MV7
  R18[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV8') # MV8
  R19[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MV9') # MV9
  R20[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'MVX') # MVX
  R21[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV1') # RV1 reweighted 80th LU3R
  R22[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV2') # RV2
  R23[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV3') # RV3
  R24[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV4') # RV4
  R25[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV5') # RV5
  R26[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV6') # RV6
  R27[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV7') # RV7
  R28[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV8') # RV8
  R29[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RV9') # RV9
  R30[i] <- LU3Rfn(P18_M, P18_F, .80, W, 'RVX') # RVX
  
  S11[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV1') # MV1 reweighted 85th LU3R
  S12[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV2') # MV2
  S13[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV3') # MV3
  S14[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV4') # MV4
  S15[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV5') # MV5
  S16[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV6') # MV6
  S17[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV7') # MV7
  S18[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV8') # MV8
  S19[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MV9') # MV9
  S20[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'MVX') # MVX
  S21[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV1') # RV1 reweighted 85th LU3R
  S22[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV2') # RV2
  S23[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV3') # RV3
  S24[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV4') # RV4
  S25[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV5') # RV5
  S26[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV6') # RV6
  S27[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV7') # RV7
  S28[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV8') # RV8
  S29[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RV9') # RV9
  S30[i] <- LU3Rfn(P18_M, P18_F, .85, W, 'RVX') # RVX
  
  T11[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV1') # MV1 reweighted 90th LU3R
  T12[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV2') # MV2
  T13[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV3') # MV3
  T14[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV4') # MV4
  T15[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV5') # MV5
  T16[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV6') # MV6
  T17[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV7') # MV7
  T18[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV8') # MV8
  T19[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MV9') # MV9
  T20[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'MVX') # MVX
  T21[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV1') # RV1 reweighted 90th LU3R
  T22[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV2') # RV2
  T23[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV3') # RV3
  T24[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV4') # RV4
  T25[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV5') # RV5
  T26[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV6') # RV6
  T27[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV7') # RV7
  T28[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV8') # RV8
  T29[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RV9') # RV9
  T30[i] <- LU3Rfn(P18_M, P18_F, .90, W, 'RVX') # RVX
  
  U11[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV1') # MV1 reweighted 95th LU3R
  U12[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV2') # MV2
  U13[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV3') # MV3
  U14[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV4') # MV4
  U15[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV5') # MV5
  U16[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV6') # MV6
  U17[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV7') # MV7
  U18[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV8') # MV8
  U19[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MV9') # MV9
  U20[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'MVX') # MVX
  U21[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV1') # RV1 reweighted 95th LU3R
  U22[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV2') # RV2
  U23[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV3') # RV3
  U24[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV4') # RV4
  U25[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV5') # RV5
  U26[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV6') # RV6
  U27[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV7') # RV7
  U28[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV8') # RV8
  U29[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RV9') # RV9
  U30[i] <- LU3Rfn(P18_M, P18_F, .95, W, 'RVX') # RVX
  
  if (i %% 10 == 0) {print(paste0(i, '/', 80, ' at ', Sys.time()), quote = F)} # print updates
}

# total variance = sampling variance + imputation variance
TV_05UM <- mean(c(sum((B11-LU3R05Ms[1])^2), # 5th LU3R (math)
                  sum((B12-LU3R05Ms[2])^2),
                  sum((B13-LU3R05Ms[3])^2),
                  sum((B14-LU3R05Ms[4])^2),
                  sum((B15-LU3R05Ms[5])^2),
                  sum((B16-LU3R05Ms[6])^2),
                  sum((B17-LU3R05Ms[7])^2),
                  sum((B18-LU3R05Ms[8])^2),
                  sum((B19-LU3R05Ms[9])^2),
                  sum((B20-LU3R05Ms[10])^2)))/20 + 11/90*sum((LU3R05Ms-LU3R05M)^2)
TV_05UR <- mean(c(sum((B21-LU3R05Rs[1])^2), # 5th LU3R (reading)
                  sum((B22-LU3R05Rs[2])^2),
                  sum((B23-LU3R05Rs[3])^2),
                  sum((B24-LU3R05Rs[4])^2),
                  sum((B25-LU3R05Rs[5])^2),
                  sum((B26-LU3R05Rs[6])^2),
                  sum((B27-LU3R05Rs[7])^2),
                  sum((B28-LU3R05Rs[8])^2),
                  sum((B29-LU3R05Rs[9])^2),
                  sum((B30-LU3R05Rs[10])^2)))/20 + 11/90*sum((LU3R05Rs-LU3R05R)^2)
TV_10UM <- mean(c(sum((C11-LU3R10Ms[1])^2), # 10th LU3R (math)
                  sum((C12-LU3R10Ms[2])^2),
                  sum((C13-LU3R10Ms[3])^2),
                  sum((C14-LU3R10Ms[4])^2),
                  sum((C15-LU3R10Ms[5])^2),
                  sum((C16-LU3R10Ms[6])^2),
                  sum((C17-LU3R10Ms[7])^2),
                  sum((C18-LU3R10Ms[8])^2),
                  sum((C19-LU3R10Ms[9])^2),
                  sum((C20-LU3R10Ms[10])^2)))/20 + 11/90*sum((LU3R10Ms-LU3R10M)^2)
TV_10UR <- mean(c(sum((C21-LU3R10Rs[1])^2), # 10th LU3R (reading)
                  sum((C22-LU3R10Rs[2])^2),
                  sum((C23-LU3R10Rs[3])^2),
                  sum((C24-LU3R10Rs[4])^2),
                  sum((C25-LU3R10Rs[5])^2),
                  sum((C26-LU3R10Rs[6])^2),
                  sum((C27-LU3R10Rs[7])^2),
                  sum((C28-LU3R10Rs[8])^2),
                  sum((C29-LU3R10Rs[9])^2),
                  sum((C30-LU3R10Rs[10])^2)))/20 + 11/90*sum((LU3R10Rs-LU3R10R)^2)
TV_15UM <- mean(c(sum((D11-LU3R15Ms[1])^2), # 15th LU3R (math)
                  sum((D12-LU3R15Ms[2])^2),
                  sum((D13-LU3R15Ms[3])^2),
                  sum((D14-LU3R15Ms[4])^2),
                  sum((D15-LU3R15Ms[5])^2),
                  sum((D16-LU3R15Ms[6])^2),
                  sum((D17-LU3R15Ms[7])^2),
                  sum((D18-LU3R15Ms[8])^2),
                  sum((D19-LU3R15Ms[9])^2),
                  sum((D20-LU3R15Ms[10])^2)))/20 + 11/90*sum((LU3R15Ms-LU3R15M)^2)
TV_15UR <- mean(c(sum((D21-LU3R15Rs[1])^2), # 15th LU3R (reading)
                  sum((D22-LU3R15Rs[2])^2),
                  sum((D23-LU3R15Rs[3])^2),
                  sum((D24-LU3R15Rs[4])^2),
                  sum((D25-LU3R15Rs[5])^2),
                  sum((D26-LU3R15Rs[6])^2),
                  sum((D27-LU3R15Rs[7])^2),
                  sum((D28-LU3R15Rs[8])^2),
                  sum((D29-LU3R15Rs[9])^2),
                  sum((D30-LU3R15Rs[10])^2)))/20 + 11/90*sum((LU3R15Rs-LU3R15R)^2)
TV_20UM <- mean(c(sum((E11-LU3R20Ms[1])^2), # 20th LU3R (math)
                  sum((E12-LU3R20Ms[2])^2),
                  sum((E13-LU3R20Ms[3])^2),
                  sum((E14-LU3R20Ms[4])^2),
                  sum((E15-LU3R20Ms[5])^2),
                  sum((E16-LU3R20Ms[6])^2),
                  sum((E17-LU3R20Ms[7])^2),
                  sum((E18-LU3R20Ms[8])^2),
                  sum((E19-LU3R20Ms[9])^2),
                  sum((E20-LU3R20Ms[10])^2)))/20 + 11/90*sum((LU3R20Ms-LU3R20M)^2)
TV_20UR <- mean(c(sum((E21-LU3R20Rs[1])^2), # 20th LU3R (reading)
                  sum((E22-LU3R20Rs[2])^2),
                  sum((E23-LU3R20Rs[3])^2),
                  sum((E24-LU3R20Rs[4])^2),
                  sum((E25-LU3R20Rs[5])^2),
                  sum((E26-LU3R20Rs[6])^2),
                  sum((E27-LU3R20Rs[7])^2),
                  sum((E28-LU3R20Rs[8])^2),
                  sum((E29-LU3R20Rs[9])^2),
                  sum((E30-LU3R20Rs[10])^2)))/20 + 11/90*sum((LU3R20Rs-LU3R20R)^2)
TV_25UM <- mean(c(sum((F11-LU3R25Ms[1])^2), # 25th LU3R (math)
                  sum((F12-LU3R25Ms[2])^2),
                  sum((F13-LU3R25Ms[3])^2),
                  sum((F14-LU3R25Ms[4])^2),
                  sum((F15-LU3R25Ms[5])^2),
                  sum((F16-LU3R25Ms[6])^2),
                  sum((F17-LU3R25Ms[7])^2),
                  sum((F18-LU3R25Ms[8])^2),
                  sum((F19-LU3R25Ms[9])^2),
                  sum((F20-LU3R25Ms[10])^2)))/20 + 11/90*sum((LU3R25Ms-LU3R25M)^2)
TV_25UR <- mean(c(sum((F21-LU3R25Rs[1])^2), # 25th LU3R (reading)
                  sum((F22-LU3R25Rs[2])^2),
                  sum((F23-LU3R25Rs[3])^2),
                  sum((F24-LU3R25Rs[4])^2),
                  sum((F25-LU3R25Rs[5])^2),
                  sum((F26-LU3R25Rs[6])^2),
                  sum((F27-LU3R25Rs[7])^2),
                  sum((F28-LU3R25Rs[8])^2),
                  sum((F29-LU3R25Rs[9])^2),
                  sum((F30-LU3R25Rs[10])^2)))/20 + 11/90*sum((LU3R25Rs-LU3R25R)^2)
TV_30UM <- mean(c(sum((G11-LU3R30Ms[1])^2), # 30th LU3R (math)
                  sum((G12-LU3R30Ms[2])^2),
                  sum((G13-LU3R30Ms[3])^2),
                  sum((G14-LU3R30Ms[4])^2),
                  sum((G15-LU3R30Ms[5])^2),
                  sum((G16-LU3R30Ms[6])^2),
                  sum((G17-LU3R30Ms[7])^2),
                  sum((G18-LU3R30Ms[8])^2),
                  sum((G19-LU3R30Ms[9])^2),
                  sum((G20-LU3R30Ms[10])^2)))/20 + 11/90*sum((LU3R30Ms-LU3R30M)^2)
TV_30UR <- mean(c(sum((G21-LU3R30Rs[1])^2), # 30th LU3R (reading)
                  sum((G22-LU3R30Rs[2])^2),
                  sum((G23-LU3R30Rs[3])^2),
                  sum((G24-LU3R30Rs[4])^2),
                  sum((G25-LU3R30Rs[5])^2),
                  sum((G26-LU3R30Rs[6])^2),
                  sum((G27-LU3R30Rs[7])^2),
                  sum((G28-LU3R30Rs[8])^2),
                  sum((G29-LU3R30Rs[9])^2),
                  sum((G30-LU3R30Rs[10])^2)))/20 + 11/90*sum((LU3R30Rs-LU3R30R)^2)
TV_35UM <- mean(c(sum((H11-LU3R35Ms[1])^2), # 35th LU3R (math)
                  sum((H12-LU3R35Ms[2])^2),
                  sum((H13-LU3R35Ms[3])^2),
                  sum((H14-LU3R35Ms[4])^2),
                  sum((H15-LU3R35Ms[5])^2),
                  sum((H16-LU3R35Ms[6])^2),
                  sum((H17-LU3R35Ms[7])^2),
                  sum((H18-LU3R35Ms[8])^2),
                  sum((H19-LU3R35Ms[9])^2),
                  sum((H20-LU3R35Ms[10])^2)))/20 + 11/90*sum((LU3R35Ms-LU3R35M)^2)
TV_35UR <- mean(c(sum((H21-LU3R35Rs[1])^2), # 35th LU3R (reading)
                  sum((H22-LU3R35Rs[2])^2),
                  sum((H23-LU3R35Rs[3])^2),
                  sum((H24-LU3R35Rs[4])^2),
                  sum((H25-LU3R35Rs[5])^2),
                  sum((H26-LU3R35Rs[6])^2),
                  sum((H27-LU3R35Rs[7])^2),
                  sum((H28-LU3R35Rs[8])^2),
                  sum((H29-LU3R35Rs[9])^2),
                  sum((H30-LU3R35Rs[10])^2)))/20 + 11/90*sum((LU3R35Rs-LU3R35R)^2)
TV_40UM <- mean(c(sum((I11-LU3R40Ms[1])^2), # 40th LU3R (math)
                  sum((I12-LU3R40Ms[2])^2),
                  sum((I13-LU3R40Ms[3])^2),
                  sum((I14-LU3R40Ms[4])^2),
                  sum((I15-LU3R40Ms[5])^2),
                  sum((I16-LU3R40Ms[6])^2),
                  sum((I17-LU3R40Ms[7])^2),
                  sum((I18-LU3R40Ms[8])^2),
                  sum((I19-LU3R40Ms[9])^2),
                  sum((I20-LU3R40Ms[10])^2)))/20 + 11/90*sum((LU3R40Ms-LU3R40M)^2)
TV_40UR <- mean(c(sum((I21-LU3R40Rs[1])^2), # 40th LU3R (reading)
                  sum((I22-LU3R40Rs[2])^2),
                  sum((I23-LU3R40Rs[3])^2),
                  sum((I24-LU3R40Rs[4])^2),
                  sum((I25-LU3R40Rs[5])^2),
                  sum((I26-LU3R40Rs[6])^2),
                  sum((I27-LU3R40Rs[7])^2),
                  sum((I28-LU3R40Rs[8])^2),
                  sum((I29-LU3R40Rs[9])^2),
                  sum((I30-LU3R40Rs[10])^2)))/20 + 11/90*sum((LU3R40Rs-LU3R40R)^2)
TV_45UM <- mean(c(sum((J11-LU3R45Ms[1])^2), # 45th LU3R (math)
                  sum((J12-LU3R45Ms[2])^2),
                  sum((J13-LU3R45Ms[3])^2),
                  sum((J14-LU3R45Ms[4])^2),
                  sum((J15-LU3R45Ms[5])^2),
                  sum((J16-LU3R45Ms[6])^2),
                  sum((J17-LU3R45Ms[7])^2),
                  sum((J18-LU3R45Ms[8])^2),
                  sum((J19-LU3R45Ms[9])^2),
                  sum((J20-LU3R45Ms[10])^2)))/20 + 11/90*sum((LU3R45Ms-LU3R45M)^2)
TV_45UR <- mean(c(sum((J21-LU3R45Rs[1])^2), # 45th LU3R (reading)
                  sum((J22-LU3R45Rs[2])^2),
                  sum((J23-LU3R45Rs[3])^2),
                  sum((J24-LU3R45Rs[4])^2),
                  sum((J25-LU3R45Rs[5])^2),
                  sum((J26-LU3R45Rs[6])^2),
                  sum((J27-LU3R45Rs[7])^2),
                  sum((J28-LU3R45Rs[8])^2),
                  sum((J29-LU3R45Rs[9])^2),
                  sum((J30-LU3R45Rs[10])^2)))/20 + 11/90*sum((LU3R45Rs-LU3R45R)^2)
TV_50UM <- mean(c(sum((K11-LU3R50Ms[1])^2), # 50th LU3R (math)
                  sum((K12-LU3R50Ms[2])^2),
                  sum((K13-LU3R50Ms[3])^2),
                  sum((K14-LU3R50Ms[4])^2),
                  sum((K15-LU3R50Ms[5])^2),
                  sum((K16-LU3R50Ms[6])^2),
                  sum((K17-LU3R50Ms[7])^2),
                  sum((K18-LU3R50Ms[8])^2),
                  sum((K19-LU3R50Ms[9])^2),
                  sum((K20-LU3R50Ms[10])^2)))/20 + 11/90*sum((LU3R50Ms-LU3R50M)^2)
TV_50UR <- mean(c(sum((K21-LU3R50Rs[1])^2), # 50th LU3R (reading)
                  sum((K22-LU3R50Rs[2])^2),
                  sum((K23-LU3R50Rs[3])^2),
                  sum((K24-LU3R50Rs[4])^2),
                  sum((K25-LU3R50Rs[5])^2),
                  sum((K26-LU3R50Rs[6])^2),
                  sum((K27-LU3R50Rs[7])^2),
                  sum((K28-LU3R50Rs[8])^2),
                  sum((K29-LU3R50Rs[9])^2),
                  sum((K30-LU3R50Rs[10])^2)))/20 + 11/90*sum((LU3R50Rs-LU3R50R)^2)
TV_55UM <- mean(c(sum((L11-LU3R55Ms[1])^2), # 55th LU3R (math)
                  sum((L12-LU3R55Ms[2])^2),
                  sum((L13-LU3R55Ms[3])^2),
                  sum((L14-LU3R55Ms[4])^2),
                  sum((L15-LU3R55Ms[5])^2),
                  sum((L16-LU3R55Ms[6])^2),
                  sum((L17-LU3R55Ms[7])^2),
                  sum((L18-LU3R55Ms[8])^2),
                  sum((L19-LU3R55Ms[9])^2),
                  sum((L20-LU3R55Ms[10])^2)))/20 + 11/90*sum((LU3R55Ms-LU3R55M)^2)
TV_55UR <- mean(c(sum((L21-LU3R55Rs[1])^2), # 55th LU3R (reading)
                  sum((L22-LU3R55Rs[2])^2),
                  sum((L23-LU3R55Rs[3])^2),
                  sum((L24-LU3R55Rs[4])^2),
                  sum((L25-LU3R55Rs[5])^2),
                  sum((L26-LU3R55Rs[6])^2),
                  sum((L27-LU3R55Rs[7])^2),
                  sum((L28-LU3R55Rs[8])^2),
                  sum((L29-LU3R55Rs[9])^2),
                  sum((L30-LU3R55Rs[10])^2)))/20 + 11/90*sum((LU3R55Rs-LU3R55R)^2)
TV_60UM <- mean(c(sum((M11-LU3R60Ms[1])^2), # 60th LU3R (math)
                  sum((M12-LU3R60Ms[2])^2),
                  sum((M13-LU3R60Ms[3])^2),
                  sum((M14-LU3R60Ms[4])^2),
                  sum((M15-LU3R60Ms[5])^2),
                  sum((M16-LU3R60Ms[6])^2),
                  sum((M17-LU3R60Ms[7])^2),
                  sum((M18-LU3R60Ms[8])^2),
                  sum((M19-LU3R60Ms[9])^2),
                  sum((M20-LU3R60Ms[10])^2)))/20 + 11/90*sum((LU3R60Ms-LU3R60M)^2)
TV_60UR <- mean(c(sum((M21-LU3R60Rs[1])^2), # 60th LU3R (reading)
                  sum((M22-LU3R60Rs[2])^2),
                  sum((M23-LU3R60Rs[3])^2),
                  sum((M24-LU3R60Rs[4])^2),
                  sum((M25-LU3R60Rs[5])^2),
                  sum((M26-LU3R60Rs[6])^2),
                  sum((M27-LU3R60Rs[7])^2),
                  sum((M28-LU3R60Rs[8])^2),
                  sum((M29-LU3R60Rs[9])^2),
                  sum((M30-LU3R60Rs[10])^2)))/20 + 11/90*sum((LU3R60Rs-LU3R60R)^2)
TV_65UM <- mean(c(sum((N11-LU3R65Ms[1])^2), # 65th LU3R (math)
                  sum((N12-LU3R65Ms[2])^2),
                  sum((N13-LU3R65Ms[3])^2),
                  sum((N14-LU3R65Ms[4])^2),
                  sum((N15-LU3R65Ms[5])^2),
                  sum((N16-LU3R65Ms[6])^2),
                  sum((N17-LU3R65Ms[7])^2),
                  sum((N18-LU3R65Ms[8])^2),
                  sum((N19-LU3R65Ms[9])^2),
                  sum((N20-LU3R65Ms[10])^2)))/20 + 11/90*sum((LU3R65Ms-LU3R65M)^2)
TV_65UR <- mean(c(sum((N21-LU3R65Rs[1])^2), # 65th LU3R (reading)
                  sum((N22-LU3R65Rs[2])^2),
                  sum((N23-LU3R65Rs[3])^2),
                  sum((N24-LU3R65Rs[4])^2),
                  sum((N25-LU3R65Rs[5])^2),
                  sum((N26-LU3R65Rs[6])^2),
                  sum((N27-LU3R65Rs[7])^2),
                  sum((N28-LU3R65Rs[8])^2),
                  sum((N29-LU3R65Rs[9])^2),
                  sum((N30-LU3R65Rs[10])^2)))/20 + 11/90*sum((LU3R65Rs-LU3R65R)^2)
TV_70UM <- mean(c(sum((O11-LU3R70Ms[1])^2), # 70th LU3R (math)
                  sum((O12-LU3R70Ms[2])^2),
                  sum((O13-LU3R70Ms[3])^2),
                  sum((O14-LU3R70Ms[4])^2),
                  sum((O15-LU3R70Ms[5])^2),
                  sum((O16-LU3R70Ms[6])^2),
                  sum((O17-LU3R70Ms[7])^2),
                  sum((O18-LU3R70Ms[8])^2),
                  sum((O19-LU3R70Ms[9])^2),
                  sum((O20-LU3R70Ms[10])^2)))/20 + 11/90*sum((LU3R70Ms-LU3R70M)^2)
TV_70UR <- mean(c(sum((O21-LU3R70Rs[1])^2), # 70th LU3R (reading)
                  sum((O22-LU3R70Rs[2])^2),
                  sum((O23-LU3R70Rs[3])^2),
                  sum((O24-LU3R70Rs[4])^2),
                  sum((O25-LU3R70Rs[5])^2),
                  sum((O26-LU3R70Rs[6])^2),
                  sum((O27-LU3R70Rs[7])^2),
                  sum((O28-LU3R70Rs[8])^2),
                  sum((O29-LU3R70Rs[9])^2),
                  sum((O30-LU3R70Rs[10])^2)))/20 + 11/90*sum((LU3R70Rs-LU3R70R)^2)
TV_75UM <- mean(c(sum((Q11-LU3R75Ms[1])^2), # 75th LU3R (math)
                  sum((Q12-LU3R75Ms[2])^2),
                  sum((Q13-LU3R75Ms[3])^2),
                  sum((Q14-LU3R75Ms[4])^2),
                  sum((Q15-LU3R75Ms[5])^2),
                  sum((Q16-LU3R75Ms[6])^2),
                  sum((Q17-LU3R75Ms[7])^2),
                  sum((Q18-LU3R75Ms[8])^2),
                  sum((Q19-LU3R75Ms[9])^2),
                  sum((Q20-LU3R75Ms[10])^2)))/20 + 11/90*sum((LU3R75Ms-LU3R75M)^2)
TV_75UR <- mean(c(sum((Q21-LU3R75Rs[1])^2), # 75th LU3R (reading)
                  sum((Q22-LU3R75Rs[2])^2),
                  sum((Q23-LU3R75Rs[3])^2),
                  sum((Q24-LU3R75Rs[4])^2),
                  sum((Q25-LU3R75Rs[5])^2),
                  sum((Q26-LU3R75Rs[6])^2),
                  sum((Q27-LU3R75Rs[7])^2),
                  sum((Q28-LU3R75Rs[8])^2),
                  sum((Q29-LU3R75Rs[9])^2),
                  sum((Q30-LU3R75Rs[10])^2)))/20 + 11/90*sum((LU3R75Rs-LU3R75R)^2)
TV_80UM <- mean(c(sum((R11-LU3R80Ms[1])^2), # 80th LU3R (math)
                  sum((R12-LU3R80Ms[2])^2),
                  sum((R13-LU3R80Ms[3])^2),
                  sum((R14-LU3R80Ms[4])^2),
                  sum((R15-LU3R80Ms[5])^2),
                  sum((R16-LU3R80Ms[6])^2),
                  sum((R17-LU3R80Ms[7])^2),
                  sum((R18-LU3R80Ms[8])^2),
                  sum((R19-LU3R80Ms[9])^2),
                  sum((R20-LU3R80Ms[10])^2)))/20 + 11/90*sum((LU3R80Ms-LU3R80M)^2)
TV_80UR <- mean(c(sum((R21-LU3R80Rs[1])^2), # 80th LU3R (reading)
                  sum((R22-LU3R80Rs[2])^2),
                  sum((R23-LU3R80Rs[3])^2),
                  sum((R24-LU3R80Rs[4])^2),
                  sum((R25-LU3R80Rs[5])^2),
                  sum((R26-LU3R80Rs[6])^2),
                  sum((R27-LU3R80Rs[7])^2),
                  sum((R28-LU3R80Rs[8])^2),
                  sum((R29-LU3R80Rs[9])^2),
                  sum((R30-LU3R80Rs[10])^2)))/20 + 11/90*sum((LU3R80Rs-LU3R80R)^2)
TV_85UM <- mean(c(sum((S11-LU3R85Ms[1])^2), # 85th LU3R (math)
                  sum((S12-LU3R85Ms[2])^2),
                  sum((S13-LU3R85Ms[3])^2),
                  sum((S14-LU3R85Ms[4])^2),
                  sum((S15-LU3R85Ms[5])^2),
                  sum((S16-LU3R85Ms[6])^2),
                  sum((S17-LU3R85Ms[7])^2),
                  sum((S18-LU3R85Ms[8])^2),
                  sum((S19-LU3R85Ms[9])^2),
                  sum((S20-LU3R85Ms[10])^2)))/20 + 11/90*sum((LU3R85Ms-LU3R85M)^2)
TV_85UR <- mean(c(sum((S21-LU3R85Rs[1])^2), # 85th LU3R (reading)
                  sum((S22-LU3R85Rs[2])^2),
                  sum((S23-LU3R85Rs[3])^2),
                  sum((S24-LU3R85Rs[4])^2),
                  sum((S25-LU3R85Rs[5])^2),
                  sum((S26-LU3R85Rs[6])^2),
                  sum((S27-LU3R85Rs[7])^2),
                  sum((S28-LU3R85Rs[8])^2),
                  sum((S29-LU3R85Rs[9])^2),
                  sum((S30-LU3R85Rs[10])^2)))/20 + 11/90*sum((LU3R85Rs-LU3R85R)^2)
TV_90UM <- mean(c(sum((T11-LU3R90Ms[1])^2), # 90th LU3R (math)
                  sum((T12-LU3R90Ms[2])^2),
                  sum((T13-LU3R90Ms[3])^2),
                  sum((T14-LU3R90Ms[4])^2),
                  sum((T15-LU3R90Ms[5])^2),
                  sum((T16-LU3R90Ms[6])^2),
                  sum((T17-LU3R90Ms[7])^2),
                  sum((T18-LU3R90Ms[8])^2),
                  sum((T19-LU3R90Ms[9])^2),
                  sum((T20-LU3R90Ms[10])^2)))/20 + 11/90*sum((LU3R90Ms-LU3R90M)^2)
TV_90UR <- mean(c(sum((T21-LU3R90Rs[1])^2), # 90th LU3R (reading)
                  sum((T22-LU3R90Rs[2])^2),
                  sum((T23-LU3R90Rs[3])^2),
                  sum((T24-LU3R90Rs[4])^2),
                  sum((T25-LU3R90Rs[5])^2),
                  sum((T26-LU3R90Rs[6])^2),
                  sum((T27-LU3R90Rs[7])^2),
                  sum((T28-LU3R90Rs[8])^2),
                  sum((T29-LU3R90Rs[9])^2),
                  sum((T30-LU3R90Rs[10])^2)))/20 + 11/90*sum((LU3R90Rs-LU3R90R)^2)
TV_95UM <- mean(c(sum((U11-LU3R95Ms[1])^2), # 95th LU3R (math)
                  sum((U12-LU3R95Ms[2])^2),
                  sum((U13-LU3R95Ms[3])^2),
                  sum((U14-LU3R95Ms[4])^2),
                  sum((U15-LU3R95Ms[5])^2),
                  sum((U16-LU3R95Ms[6])^2),
                  sum((U17-LU3R95Ms[7])^2),
                  sum((U18-LU3R95Ms[8])^2),
                  sum((U19-LU3R95Ms[9])^2),
                  sum((U20-LU3R95Ms[10])^2)))/20 + 11/90*sum((LU3R95Ms-LU3R95M)^2)
TV_95UR <- mean(c(sum((U21-LU3R95Rs[1])^2), # 95th LU3R (reading)
                  sum((U22-LU3R95Rs[2])^2),
                  sum((U23-LU3R95Rs[3])^2),
                  sum((U24-LU3R95Rs[4])^2),
                  sum((U25-LU3R95Rs[5])^2),
                  sum((U26-LU3R95Rs[6])^2),
                  sum((U27-LU3R95Rs[7])^2),
                  sum((U28-LU3R95Rs[8])^2),
                  sum((U29-LU3R95Rs[9])^2),
                  sum((U30-LU3R95Rs[10])^2)))/20 + 11/90*sum((LU3R95Rs-LU3R95R)^2)
TV_Md95UM <- mean(c(sum((U11-K11-Md95UMs[1])^2), # Md95UM
                    sum((U12-K12-Md95UMs[2])^2),
                    sum((U13-K13-Md95UMs[3])^2),
                    sum((U14-K14-Md95UMs[4])^2),
                    sum((U15-K15-Md95UMs[5])^2),
                    sum((U16-K16-Md95UMs[6])^2),
                    sum((U17-K17-Md95UMs[7])^2),
                    sum((U18-K18-Md95UMs[8])^2),
                    sum((U19-K19-Md95UMs[9])^2),
                    sum((U20-K20-Md95UMs[10])^2)))
TV_Md95UR <- mean(c(sum((U21-K21-Md95URs[1])^2), # Md95UR
                    sum((U22-K22-Md95URs[2])^2),
                    sum((U23-K23-Md95URs[3])^2),
                    sum((U24-K24-Md95URs[4])^2),
                    sum((U25-K25-Md95URs[5])^2),
                    sum((U26-K26-Md95URs[6])^2),
                    sum((U27-K27-Md95URs[7])^2),
                    sum((U28-K28-Md95URs[8])^2),
                    sum((U29-K29-Md95URs[9])^2),
                    sum((U30-K30-Md95URs[10])^2)))
TV_Md90UM <- mean(c(sum((T11-K11-Md90UMs[1])^2), # Md90UM
                    sum((T12-K12-Md90UMs[2])^2),
                    sum((T13-K13-Md90UMs[3])^2),
                    sum((T14-K14-Md90UMs[4])^2),
                    sum((T15-K15-Md90UMs[5])^2),
                    sum((T16-K16-Md90UMs[6])^2),
                    sum((T17-K17-Md90UMs[7])^2),
                    sum((T18-K18-Md90UMs[8])^2),
                    sum((T19-K19-Md90UMs[9])^2),
                    sum((T20-K20-Md90UMs[10])^2)))
TV_Md90UR <- mean(c(sum((T21-K21-Md90URs[1])^2), # Md90UR
                    sum((T22-K22-Md90URs[2])^2),
                    sum((T23-K23-Md90URs[3])^2),
                    sum((T24-K24-Md90URs[4])^2),
                    sum((T25-K25-Md90URs[5])^2),
                    sum((T26-K26-Md90URs[6])^2),
                    sum((T27-K27-Md90URs[7])^2),
                    sum((T28-K28-Md90URs[8])^2),
                    sum((T29-K29-Md90URs[9])^2),
                    sum((T30-K30-Md90URs[10])^2)))
TV_Md10UM <- mean(c(sum((K11-C11-Md10UMs[1])^2), # Md10UM
                    sum((K12-C12-Md10UMs[2])^2),
                    sum((K13-C13-Md10UMs[3])^2),
                    sum((K14-C14-Md10UMs[4])^2),
                    sum((K15-C15-Md10UMs[5])^2),
                    sum((K16-C16-Md10UMs[6])^2),
                    sum((K17-C17-Md10UMs[7])^2),
                    sum((K18-C18-Md10UMs[8])^2),
                    sum((K19-C19-Md10UMs[9])^2),
                    sum((K20-C20-Md10UMs[10])^2)))
TV_Md10UR <- mean(c(sum((K21-C21-Md10URs[1])^2), # Md10UR
                    sum((K22-C22-Md10URs[2])^2),
                    sum((K23-C23-Md10URs[3])^2),
                    sum((K24-C24-Md10URs[4])^2),
                    sum((K25-C25-Md10URs[5])^2),
                    sum((K26-C26-Md10URs[6])^2),
                    sum((K27-C27-Md10URs[7])^2),
                    sum((K28-C28-Md10URs[8])^2),
                    sum((K29-C29-Md10URs[9])^2),
                    sum((K30-C30-Md10URs[10])^2)))
TV_Md05UM <- mean(c(sum((K11-B11-Md05UMs[1])^2), # Md05UM
                    sum((K12-B12-Md05UMs[2])^2),
                    sum((K13-B13-Md05UMs[3])^2),
                    sum((K14-B14-Md05UMs[4])^2),
                    sum((K15-B15-Md05UMs[5])^2),
                    sum((K16-B16-Md05UMs[6])^2),
                    sum((K17-B17-Md05UMs[7])^2),
                    sum((K18-B18-Md05UMs[8])^2),
                    sum((K19-B19-Md05UMs[9])^2),
                    sum((K20-B20-Md05UMs[10])^2)))
TV_Md05UR <- mean(c(sum((K21-B21-Md05URs[1])^2), # Md05UR
                    sum((K22-B22-Md05URs[2])^2),
                    sum((K23-B23-Md05URs[3])^2),
                    sum((K24-B24-Md05URs[4])^2),
                    sum((K25-B25-Md05URs[5])^2),
                    sum((K26-B26-Md05URs[6])^2),
                    sum((K27-B27-Md05URs[7])^2),
                    sum((K28-B28-Md05URs[8])^2),
                    sum((K29-B29-Md05URs[9])^2),
                    sum((K30-B30-Md05URs[10])^2)))

# standard errors
SE_05UM <- sqrt(TV_05UM) # 5th LU3R (math)
SE_05UR <- sqrt(TV_05UR) # 5th LU3R (reading)
SE_10UM <- sqrt(TV_10UM) # 10th LU3R (math)
SE_10UR <- sqrt(TV_10UR) # 10th LU3R (reading)
SE_15UM <- sqrt(TV_15UM) # 15th LU3R (math)
SE_15UR <- sqrt(TV_15UR) # 15th LU3R (reading)
SE_20UM <- sqrt(TV_20UM) # 20th LU3R (math)
SE_20UR <- sqrt(TV_20UR) # 20th LU3R (reading)
SE_25UM <- sqrt(TV_25UM) # 25th LU3R (math)
SE_25UR <- sqrt(TV_25UR) # 25th LU3R (reading)
SE_30UM <- sqrt(TV_30UM) # 30th LU3R (math)
SE_30UR <- sqrt(TV_30UR) # 30th LU3R (reading)
SE_35UM <- sqrt(TV_35UM) # 35th LU3R (math)
SE_35UR <- sqrt(TV_35UR) # 35th LU3R (reading)
SE_40UM <- sqrt(TV_40UM) # 40th LU3R (math)
SE_40UR <- sqrt(TV_40UR) # 40th LU3R (reading)
SE_45UM <- sqrt(TV_45UM) # 45th LU3R (math)
SE_45UR <- sqrt(TV_45UR) # 45th LU3R (reading)
SE_50UM <- sqrt(TV_50UM) # 50th LU3R (math)
SE_50UR <- sqrt(TV_50UR) # 50th LU3R (reading)
SE_55UM <- sqrt(TV_55UM) # 55th LU3R (math)
SE_55UR <- sqrt(TV_55UR) # 55th LU3R (reading)
SE_60UM <- sqrt(TV_60UM) # 60th LU3R (math)
SE_60UR <- sqrt(TV_60UR) # 60th LU3R (reading)
SE_65UM <- sqrt(TV_65UM) # 65th LU3R (math)
SE_65UR <- sqrt(TV_65UR) # 65th LU3R (reading)
SE_70UM <- sqrt(TV_70UM) # 70th LU3R (math)
SE_70UR <- sqrt(TV_70UR) # 70th LU3R (reading)
SE_75UM <- sqrt(TV_75UM) # 75th LU3R (math)
SE_75UR <- sqrt(TV_75UR) # 75th LU3R (reading)
SE_80UM <- sqrt(TV_80UM) # 80th LU3R (math)
SE_80UR <- sqrt(TV_80UR) # 80th LU3R (reading)
SE_85UM <- sqrt(TV_85UM) # 85th LU3R (math)
SE_85UR <- sqrt(TV_85UR) # 85th LU3R (reading)
SE_90UM <- sqrt(TV_90UM) # 90th LU3R (math)
SE_90UR <- sqrt(TV_90UR) # 90th LU3R (reading)
SE_95UM <- sqrt(TV_95UM) # 95th LU3R (math)
SE_95UR <- sqrt(TV_95UR) # 95th LU3R (reading)
SE_Md95UM <- sqrt(TV_Md95UM) # Md95T (math)
SE_Md95UR <- sqrt(TV_Md95UR) # Md95T (reading)
SE_Md90UM <- sqrt(TV_Md90UM) # Md90T (math)
SE_Md90UR <- sqrt(TV_Md90UR) # Md90T (reading)
SE_Md10UM <- sqrt(TV_Md10UM) # Md10T (math)
SE_Md10UR <- sqrt(TV_Md10UR) # Md10T (reading)
SE_Md05UM <- sqrt(TV_Md05UM) # Md05T (math)
SE_Md05UR <- sqrt(TV_Md05UR) # Md05T (reading)



#### SEs: d, U3, LVR, LVR_L, LVR_R, LMADR, LMADR_L, LMADR_R, LGMDR ####

A1 <- A2 <- A3 <- A4 <- A5 <- A6 <- A7 <- A8 <- A9 <- AX <- numeric(80) # empty containers
B1 <- B2 <- B3 <- B4 <- B5 <- B6 <- B7 <- B8 <- B9 <- BX <- numeric(80) # empty containers
C1 <- C2 <- C3 <- C4 <- C5 <- C6 <- C7 <- C8 <- C9 <- CX <- numeric(80) # empty containers
D1 <- D2 <- D3 <- D4 <- D5 <- D6 <- D7 <- D8 <- D9 <- DX <- numeric(80) # empty containers
E1 <- E2 <- E3 <- E4 <- E5 <- E6 <- E7 <- E8 <- E9 <- EX <- numeric(80) # empty containers
F1 <- F2 <- F3 <- F4 <- F5 <- F6 <- F7 <- F8 <- F9 <- FX <- numeric(80) # empty containers
G1 <- G2 <- G3 <- G4 <- G5 <- G6 <- G7 <- G8 <- G9 <- GX <- numeric(80) # empty containers
H1 <- H2 <- H3 <- H4 <- H5 <- H6 <- H7 <- H8 <- H9 <- HX <- numeric(80) # empty containers
I1 <- I2 <- I3 <- I4 <- I5 <- I6 <- I7 <- I8 <- I9 <- IX <- numeric(80) # empty containers
J1 <- J2 <- J3 <- J4 <- J5 <- J6 <- J7 <- J8 <- J9 <- JX <- numeric(80) # empty containers
K1 <- K2 <- K3 <- K4 <- K5 <- K6 <- K7 <- K8 <- K9 <- KX <- numeric(80) # empty containers
L1 <- L2 <- L3 <- L4 <- L5 <- L6 <- L7 <- L8 <- L9 <- LX <- numeric(80) # empty containers
M1 <- M2 <- M3 <- M4 <- M5 <- M6 <- M7 <- M8 <- M9 <- MX <- numeric(80) # empty containers
N1 <- N2 <- N3 <- N4 <- N5 <- N6 <- N7 <- N8 <- N9 <- NX <- numeric(80) # empty containers
O1 <- O2 <- O3 <- O4 <- O5 <- O6 <- O7 <- O8 <- O9 <- OX <- numeric(80) # empty containers
P1 <- P2 <- P3 <- P4 <- P5 <- P6 <- P7 <- P8 <- P9 <- PX <- numeric(80) # empty containers
Q1 <- Q2 <- Q3 <- Q4 <- Q5 <- Q6 <- Q7 <- Q8 <- Q9 <- QX <- numeric(80) # empty containers
R1 <- R2 <- R3 <- R4 <- R5 <- R6 <- R7 <- R8 <- R9 <- RX <- numeric(80) # empty containers

# perform bootstrap resampling of d, U3, LVR, LVR_L, LVR_R, LMADR, LMADR_L, LMADR_R, LGMDR
for (i in 1:80) {
  W <- paste0('BW',i)
  
  A1[i] <- dfn(P18_M, P18_F, W, 'MV1') # MV1 d (math)
  A2[i] <- dfn(P18_M, P18_F, W, 'MV2') # MV2
  A3[i] <- dfn(P18_M, P18_F, W, 'MV3') # MV3
  A4[i] <- dfn(P18_M, P18_F, W, 'MV4') # MV4
  A5[i] <- dfn(P18_M, P18_F, W, 'MV5') # MV5
  A6[i] <- dfn(P18_M, P18_F, W, 'MV6') # MV6
  A7[i] <- dfn(P18_M, P18_F, W, 'MV7') # MV7
  A8[i] <- dfn(P18_M, P18_F, W, 'MV8') # MV8
  A9[i] <- dfn(P18_M, P18_F, W, 'MV9') # MV9
  AX[i] <- dfn(P18_M, P18_F, W, 'MVX') # MVX
  B1[i] <- dfn(P18_M, P18_F, W, 'RV1') # RV1 d (reading)
  B2[i] <- dfn(P18_M, P18_F, W, 'RV2') # RV2
  B3[i] <- dfn(P18_M, P18_F, W, 'RV3') # RV3
  B4[i] <- dfn(P18_M, P18_F, W, 'RV4') # RV4
  B5[i] <- dfn(P18_M, P18_F, W, 'RV5') # RV5
  B6[i] <- dfn(P18_M, P18_F, W, 'RV6') # RV6
  B7[i] <- dfn(P18_M, P18_F, W, 'RV7') # RV7
  B8[i] <- dfn(P18_M, P18_F, W, 'RV8') # RV8
  B9[i] <- dfn(P18_M, P18_F, W, 'RV9') # RV9
  BX[i] <- dfn(P18_M, P18_F, W, 'RVX') # RVX
  
  C1[i] <- U3fn(P18_M, P18_F, W, 'MV1') # MV1 U3 (math)
  C2[i] <- U3fn(P18_M, P18_F, W, 'MV2') # MV2
  C3[i] <- U3fn(P18_M, P18_F, W, 'MV3') # MV3
  C4[i] <- U3fn(P18_M, P18_F, W, 'MV4') # MV4
  C5[i] <- U3fn(P18_M, P18_F, W, 'MV5') # MV5
  C6[i] <- U3fn(P18_M, P18_F, W, 'MV6') # MV6
  C7[i] <- U3fn(P18_M, P18_F, W, 'MV7') # MV7
  C8[i] <- U3fn(P18_M, P18_F, W, 'MV8') # MV8
  C9[i] <- U3fn(P18_M, P18_F, W, 'MV9') # MV9
  CX[i] <- U3fn(P18_M, P18_F, W, 'MVX') # MVX
  D1[i] <- U3fn(P18_M, P18_F, W, 'RV1') # RV1 U3 (reading)
  D2[i] <- U3fn(P18_M, P18_F, W, 'RV2') # RV2
  D3[i] <- U3fn(P18_M, P18_F, W, 'RV3') # RV3
  D4[i] <- U3fn(P18_M, P18_F, W, 'RV4') # RV4
  D5[i] <- U3fn(P18_M, P18_F, W, 'RV5') # RV5
  D6[i] <- U3fn(P18_M, P18_F, W, 'RV6') # RV6
  D7[i] <- U3fn(P18_M, P18_F, W, 'RV7') # RV7
  D8[i] <- U3fn(P18_M, P18_F, W, 'RV8') # RV8
  D9[i] <- U3fn(P18_M, P18_F, W, 'RV9') # RV9
  DX[i] <- U3fn(P18_M, P18_F, W, 'RVX') # RVX
  
  E1[i] <- LVRfn(P18_M, P18_F, W, 'MV1') # MV1 LVR (math)
  E2[i] <- LVRfn(P18_M, P18_F, W, 'MV2') # MV2
  E3[i] <- LVRfn(P18_M, P18_F, W, 'MV3') # MV3
  E4[i] <- LVRfn(P18_M, P18_F, W, 'MV4') # MV4
  E5[i] <- LVRfn(P18_M, P18_F, W, 'MV5') # MV5
  E6[i] <- LVRfn(P18_M, P18_F, W, 'MV6') # MV6
  E7[i] <- LVRfn(P18_M, P18_F, W, 'MV7') # MV7
  E8[i] <- LVRfn(P18_M, P18_F, W, 'MV8') # MV8
  E9[i] <- LVRfn(P18_M, P18_F, W, 'MV9') # MV9
  EX[i] <- LVRfn(P18_M, P18_F, W, 'MVX') # MVX
  F1[i] <- LVRfn(P18_M, P18_F, W, 'RV1') # RV1 LVR (reading)
  F2[i] <- LVRfn(P18_M, P18_F, W, 'RV2') # RV2
  F3[i] <- LVRfn(P18_M, P18_F, W, 'RV3') # RV3
  F4[i] <- LVRfn(P18_M, P18_F, W, 'RV4') # RV4
  F5[i] <- LVRfn(P18_M, P18_F, W, 'RV5') # RV5
  F6[i] <- LVRfn(P18_M, P18_F, W, 'RV6') # RV6
  F7[i] <- LVRfn(P18_M, P18_F, W, 'RV7') # RV7
  F8[i] <- LVRfn(P18_M, P18_F, W, 'RV8') # RV8
  F9[i] <- LVRfn(P18_M, P18_F, W, 'RV9') # RV9
  FX[i] <- LVRfn(P18_M, P18_F, W, 'RVX') # RVX
  
  G1[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV1', 'L') # MV1 LVR_L (math)
  G2[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV2', 'L') # MV2
  G3[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV3', 'L') # MV3
  G4[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV4', 'L') # MV4
  G5[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV5', 'L') # MV5
  G6[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV6', 'L') # MV6
  G7[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV7', 'L') # MV7
  G8[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV8', 'L') # MV8
  G9[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV9', 'L') # MV9
  GX[i] <- LVR_Tfn(P18_M, P18_F, W, 'MVX', 'L') # MVX
  H1[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV1', 'L') # RV1 LVR_L (reading)
  H2[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV2', 'L') # RV2
  H3[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV3', 'L') # RV3
  H4[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV4', 'L') # RV4
  H5[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV5', 'L') # RV5
  H6[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV6', 'L') # RV6
  H7[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV7', 'L') # RV7
  H8[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV8', 'L') # RV8
  H9[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV9', 'L') # RV9
  HX[i] <- LVR_Tfn(P18_M, P18_F, W, 'RVX', 'L') # RVX
  
  I1[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV1', 'R') # MV1 LVR_R (math)
  I2[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV2', 'R') # MV2
  I3[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV3', 'R') # MV3
  I4[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV4', 'R') # MV4
  I5[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV5', 'R') # MV5
  I6[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV6', 'R') # MV6
  I7[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV7', 'R') # MV7
  I8[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV8', 'R') # MV8
  I9[i] <- LVR_Tfn(P18_M, P18_F, W, 'MV9', 'R') # MV9
  IX[i] <- LVR_Tfn(P18_M, P18_F, W, 'MVX', 'R') # MVX
  J1[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV1', 'R') # RV1 LVR_R (reading)
  J2[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV2', 'R') # RV2
  J3[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV3', 'R') # RV3
  J4[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV4', 'R') # RV4
  J5[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV5', 'R') # RV5
  J6[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV6', 'R') # RV6
  J7[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV7', 'R') # RV7
  J8[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV8', 'R') # RV8
  J9[i] <- LVR_Tfn(P18_M, P18_F, W, 'RV9', 'R') # RV9
  JX[i] <- LVR_Tfn(P18_M, P18_F, W, 'RVX', 'R') # RVX
  
  K1[i] <- LMADRfn(P18_M, P18_F, W, 'MV1') # MV1 LMADR (math)
  K2[i] <- LMADRfn(P18_M, P18_F, W, 'MV2') # MV2
  K3[i] <- LMADRfn(P18_M, P18_F, W, 'MV3') # MV3
  K4[i] <- LMADRfn(P18_M, P18_F, W, 'MV4') # MV4
  K5[i] <- LMADRfn(P18_M, P18_F, W, 'MV5') # MV5
  K6[i] <- LMADRfn(P18_M, P18_F, W, 'MV6') # MV6
  K7[i] <- LMADRfn(P18_M, P18_F, W, 'MV7') # MV7
  K8[i] <- LMADRfn(P18_M, P18_F, W, 'MV8') # MV8
  K9[i] <- LMADRfn(P18_M, P18_F, W, 'MV9') # MV9
  KX[i] <- LMADRfn(P18_M, P18_F, W, 'MVX') # MVX
  L1[i] <- LMADRfn(P18_M, P18_F, W, 'RV1') # RV1 LMADR (reading)
  L2[i] <- LMADRfn(P18_M, P18_F, W, 'RV2') # RV2
  L3[i] <- LMADRfn(P18_M, P18_F, W, 'RV3') # RV3
  L4[i] <- LMADRfn(P18_M, P18_F, W, 'RV4') # RV4
  L5[i] <- LMADRfn(P18_M, P18_F, W, 'RV5') # RV5
  L6[i] <- LMADRfn(P18_M, P18_F, W, 'RV6') # RV6
  L7[i] <- LMADRfn(P18_M, P18_F, W, 'RV7') # RV7
  L8[i] <- LMADRfn(P18_M, P18_F, W, 'RV8') # RV8
  L9[i] <- LMADRfn(P18_M, P18_F, W, 'RV9') # RV9
  LX[i] <- LMADRfn(P18_M, P18_F, W, 'RVX') # RVX
  
  M1[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV1', 'L') # MV1 LMADR_L (math)
  M2[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV2', 'L') # MV2
  M3[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV3', 'L') # MV3
  M4[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV4', 'L') # MV4
  M5[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV5', 'L') # MV5
  M6[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV6', 'L') # MV6
  M7[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV7', 'L') # MV7
  M8[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV8', 'L') # MV8
  M9[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV9', 'L') # MV9
  MX[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MVX', 'L') # MVX
  N1[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV1', 'L') # RV1 LMADR_L (reading)
  N2[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV2', 'L') # RV2
  N3[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV3', 'L') # RV3
  N4[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV4', 'L') # RV4
  N5[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV5', 'L') # RV5
  N6[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV6', 'L') # RV6
  N7[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV7', 'L') # RV7
  N8[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV8', 'L') # RV8
  N9[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV9', 'L') # RV9
  NX[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RVX', 'L') # RVX
  
  O1[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV1', 'R') # MV1 LMADR_R (math)
  O2[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV2', 'R') # MV2
  O3[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV3', 'R') # MV3
  O4[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV4', 'R') # MV4
  O5[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV5', 'R') # MV5
  O6[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV6', 'R') # MV6
  O7[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV7', 'R') # MV7
  O8[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV8', 'R') # MV8
  O9[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MV9', 'R') # MV9
  OX[i] <- LMADR_Tfn(P18_M, P18_F, W, 'MVX', 'R') # MVX
  P1[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV1', 'R') # RV1 LMADR_R (reading)
  P2[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV2', 'R') # RV2
  P3[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV3', 'R') # RV3
  P4[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV4', 'R') # RV4
  P5[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV5', 'R') # RV5
  P6[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV6', 'R') # RV6
  P7[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV7', 'R') # RV7
  P8[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV8', 'R') # RV8
  P9[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RV9', 'R') # RV9
  PX[i] <- LMADR_Tfn(P18_M, P18_F, W, 'RVX', 'R') # RVX
  
  Q1[i] <- LGMDRfn(P18_M, P18_F, W, 'MV1') # MV1 LGMDR (math)
  Q2[i] <- LGMDRfn(P18_M, P18_F, W, 'MV2') # MV2
  Q3[i] <- LGMDRfn(P18_M, P18_F, W, 'MV3') # MV3
  Q4[i] <- LGMDRfn(P18_M, P18_F, W, 'MV4') # MV4
  Q5[i] <- LGMDRfn(P18_M, P18_F, W, 'MV5') # MV5
  Q6[i] <- LGMDRfn(P18_M, P18_F, W, 'MV6') # MV6
  Q7[i] <- LGMDRfn(P18_M, P18_F, W, 'MV7') # MV7
  Q8[i] <- LGMDRfn(P18_M, P18_F, W, 'MV8') # MV8
  Q9[i] <- LGMDRfn(P18_M, P18_F, W, 'MV9') # MV9
  QX[i] <- LGMDRfn(P18_M, P18_F, W, 'MVX') # MVX
  R1[i] <- LGMDRfn(P18_M, P18_F, W, 'RV1') # RV1 LGMDR (reading)
  R2[i] <- LGMDRfn(P18_M, P18_F, W, 'RV2') # RV2
  R3[i] <- LGMDRfn(P18_M, P18_F, W, 'RV3') # RV3
  R4[i] <- LGMDRfn(P18_M, P18_F, W, 'RV4') # RV4
  R5[i] <- LGMDRfn(P18_M, P18_F, W, 'RV5') # RV5
  R6[i] <- LGMDRfn(P18_M, P18_F, W, 'RV6') # RV6
  R7[i] <- LGMDRfn(P18_M, P18_F, W, 'RV7') # RV7
  R8[i] <- LGMDRfn(P18_M, P18_F, W, 'RV8') # RV8
  R9[i] <- LGMDRfn(P18_M, P18_F, W, 'RV9') # RV9
  RX[i] <- LGMDRfn(P18_M, P18_F, W, 'RVX') # RVX
  
  if (i %% 10 == 0) {print(paste0(i, '/', 80, ' at ', Sys.time()), quote = F)} # print updates
}

# total variance = sampling variance + imputation variance
TV_dM <- mean(c(sum((A1-dMs[1])^2), # d (math)
                sum((A2-dMs[2])^2),
                sum((A3-dMs[3])^2),
                sum((A4-dMs[4])^2),
                sum((A5-dMs[5])^2),
                sum((A6-dMs[6])^2),
                sum((A7-dMs[7])^2),
                sum((A8-dMs[8])^2),
                sum((A9-dMs[9])^2),
                sum((AX-dMs[10])^2)))/20 + 11/90*sum((dMs-dM)^2)
TV_dR <- mean(c(sum((B1-dRs[1])^2), # d (reading)
                sum((B2-dRs[2])^2),
                sum((B3-dRs[3])^2),
                sum((B4-dRs[4])^2),
                sum((B5-dRs[5])^2),
                sum((B6-dRs[6])^2),
                sum((B7-dRs[7])^2),
                sum((B8-dRs[8])^2),
                sum((B9-dRs[9])^2),
                sum((BX-dRs[10])^2)))/20 + 11/90*sum((dRs-dR)^2)
TV_U3M <- mean(c(sum((C1-U3Ms[1])^2), # U3 (math)
                 sum((C2-U3Ms[2])^2),
                 sum((C3-U3Ms[3])^2),
                 sum((C4-U3Ms[4])^2),
                 sum((C5-U3Ms[5])^2),
                 sum((C6-U3Ms[6])^2),
                 sum((C7-U3Ms[7])^2),
                 sum((C8-U3Ms[8])^2),
                 sum((C9-U3Ms[9])^2),
                 sum((CX-U3Ms[10])^2)))/20 + 11/90*sum((U3Ms-U3M)^2)
TV_U3R <- mean(c(sum((D1-U3Rs[1])^2), # U3 (reading)
                 sum((D2-U3Rs[2])^2),
                 sum((D3-U3Rs[3])^2),
                 sum((D4-U3Rs[4])^2),
                 sum((D5-U3Rs[5])^2),
                 sum((D6-U3Rs[6])^2),
                 sum((D7-U3Rs[7])^2),
                 sum((D8-U3Rs[8])^2),
                 sum((D9-U3Rs[9])^2),
                 sum((DX-U3Rs[10])^2)))/20 + 11/90*sum((U3Rs-U3R)^2)
TV_LVRM <- mean(c(sum((E1-LVRMs[1])^2), # LVR (math)
                  sum((E2-LVRMs[2])^2),
                  sum((E3-LVRMs[3])^2),
                  sum((E4-LVRMs[4])^2),
                  sum((E5-LVRMs[5])^2),
                  sum((E6-LVRMs[6])^2),
                  sum((E7-LVRMs[7])^2),
                  sum((E8-LVRMs[8])^2),
                  sum((E9-LVRMs[9])^2),
                  sum((EX-LVRMs[10])^2)))/20 + 11/90*sum((LVRMs-LVRM)^2)
TV_LVRR <- mean(c(sum((F1-LVRRs[1])^2), # LVR (reading)
                  sum((F2-LVRRs[2])^2),
                  sum((F3-LVRRs[3])^2),
                  sum((F4-LVRRs[4])^2),
                  sum((F5-LVRRs[5])^2),
                  sum((F6-LVRRs[6])^2),
                  sum((F7-LVRRs[7])^2),
                  sum((F8-LVRRs[8])^2),
                  sum((F9-LVRRs[9])^2),
                  sum((FX-LVRRs[10])^2)))/20 + 11/90*sum((LVRRs-LVRR)^2)
TV_LVRM_L <- mean(c(sum((G1-LVRM_Ls[1])^2), # LVR_L (math)
                    sum((G2-LVRM_Ls[2])^2),
                    sum((G3-LVRM_Ls[3])^2),
                    sum((G4-LVRM_Ls[4])^2),
                    sum((G5-LVRM_Ls[5])^2),
                    sum((G6-LVRM_Ls[6])^2),
                    sum((G7-LVRM_Ls[7])^2),
                    sum((G8-LVRM_Ls[8])^2),
                    sum((G9-LVRM_Ls[9])^2),
                    sum((GX-LVRM_Ls[10])^2)))/20 + 11/90*sum((LVRM_Ls-LVRM_L)^2)
TV_LVRR_L <- mean(c(sum((H1-LVRR_Ls[1])^2), # LVR_L (reading)
                    sum((H2-LVRR_Ls[2])^2),
                    sum((H3-LVRR_Ls[3])^2),
                    sum((H4-LVRR_Ls[4])^2),
                    sum((H5-LVRR_Ls[5])^2),
                    sum((H6-LVRR_Ls[6])^2),
                    sum((H7-LVRR_Ls[7])^2),
                    sum((H8-LVRR_Ls[8])^2),
                    sum((H9-LVRR_Ls[9])^2),
                    sum((HX-LVRR_Ls[10])^2)))/20 + 11/90*sum((LVRR_Ls-LVRR_L)^2)
TV_LVRM_R <- mean(c(sum((I1-LVRM_Rs[1])^2), # LVR_R (math)
                    sum((I2-LVRM_Rs[2])^2),
                    sum((I3-LVRM_Rs[3])^2),
                    sum((I4-LVRM_Rs[4])^2),
                    sum((I5-LVRM_Rs[5])^2),
                    sum((I6-LVRM_Rs[6])^2),
                    sum((I7-LVRM_Rs[7])^2),
                    sum((I8-LVRM_Rs[8])^2),
                    sum((I9-LVRM_Rs[9])^2),
                    sum((IX-LVRM_Rs[10])^2)))/20 + 11/90*sum((LVRM_Rs-LVRM_R)^2)
TV_LVRR_R <- mean(c(sum((J1-LVRR_Rs[1])^2), # LVR_R (reading)
                    sum((J2-LVRR_Rs[2])^2),
                    sum((J3-LVRR_Rs[3])^2),
                    sum((J4-LVRR_Rs[4])^2),
                    sum((J5-LVRR_Rs[5])^2),
                    sum((J6-LVRR_Rs[6])^2),
                    sum((J7-LVRR_Rs[7])^2),
                    sum((J8-LVRR_Rs[8])^2),
                    sum((J9-LVRR_Rs[9])^2),
                    sum((JX-LVRR_Rs[10])^2)))/20 + 11/90*sum((LVRR_Rs-LVRR_R)^2)
TV_LMADRM <- mean(c(sum((K1-LMADRMs[1])^2), # LMADR (math)
                    sum((K2-LMADRMs[2])^2),
                    sum((K3-LMADRMs[3])^2),
                    sum((K4-LMADRMs[4])^2),
                    sum((K5-LMADRMs[5])^2),
                    sum((K6-LMADRMs[6])^2),
                    sum((K7-LMADRMs[7])^2),
                    sum((K8-LMADRMs[8])^2),
                    sum((K9-LMADRMs[9])^2),
                    sum((KX-LMADRMs[10])^2)))/20 + 11/90*sum((LMADRMs-LMADRM)^2)
TV_LMADRR <- mean(c(sum((L1-LMADRRs[1])^2), # LMADR (reading)
                    sum((L2-LMADRRs[2])^2),
                    sum((L3-LMADRRs[3])^2),
                    sum((L4-LMADRRs[4])^2),
                    sum((L5-LMADRRs[5])^2),
                    sum((L6-LMADRRs[6])^2),
                    sum((L7-LMADRRs[7])^2),
                    sum((L8-LMADRRs[8])^2),
                    sum((L9-LMADRRs[9])^2),
                    sum((LX-LMADRRs[10])^2)))/20 + 11/90*sum((LMADRRs-LMADRR)^2)
TV_LMADRM_L <- mean(c(sum((M1-LMADRM_Ls[1])^2), # LMADR_L (math)
                      sum((M2-LMADRM_Ls[2])^2),
                      sum((M3-LMADRM_Ls[3])^2),
                      sum((M4-LMADRM_Ls[4])^2),
                      sum((M5-LMADRM_Ls[5])^2),
                      sum((M6-LMADRM_Ls[6])^2),
                      sum((M7-LMADRM_Ls[7])^2),
                      sum((M8-LMADRM_Ls[8])^2),
                      sum((M9-LMADRM_Ls[9])^2),
                      sum((MX-LMADRM_Ls[10])^2)))/20 + 11/90*sum((LMADRM_Ls-LMADRM_L)^2)
TV_LMADRR_L <- mean(c(sum((N1-LMADRR_Ls[1])^2), # LMADR_L (reading)
                      sum((N2-LMADRR_Ls[2])^2),
                      sum((N3-LMADRR_Ls[3])^2),
                      sum((N4-LMADRR_Ls[4])^2),
                      sum((N5-LMADRR_Ls[5])^2),
                      sum((N6-LMADRR_Ls[6])^2),
                      sum((N7-LMADRR_Ls[7])^2),
                      sum((N8-LMADRR_Ls[8])^2),
                      sum((N9-LMADRR_Ls[9])^2),
                      sum((NX-LMADRR_Ls[10])^2)))/20 + 11/90*sum((LMADRR_Ls-LMADRR_L)^2)
TV_LMADRM_R <- mean(c(sum((O1-LMADRM_Rs[1])^2), # LMADR_R (math)
                      sum((O2-LMADRM_Rs[2])^2),
                      sum((O3-LMADRM_Rs[3])^2),
                      sum((O4-LMADRM_Rs[4])^2),
                      sum((O5-LMADRM_Rs[5])^2),
                      sum((O6-LMADRM_Rs[6])^2),
                      sum((O7-LMADRM_Rs[7])^2),
                      sum((O8-LMADRM_Rs[8])^2),
                      sum((O9-LMADRM_Rs[9])^2),
                      sum((OX-LMADRM_Rs[10])^2)))/20 + 11/90*sum((LMADRM_Rs-LMADRM_R)^2)
TV_LMADRR_R <- mean(c(sum((P1-LMADRR_Rs[1])^2), # LMADR_R (reading)
                      sum((P2-LMADRR_Rs[2])^2),
                      sum((P3-LMADRR_Rs[3])^2),
                      sum((P4-LMADRR_Rs[4])^2),
                      sum((P5-LMADRR_Rs[5])^2),
                      sum((P6-LMADRR_Rs[6])^2),
                      sum((P7-LMADRR_Rs[7])^2),
                      sum((P8-LMADRR_Rs[8])^2),
                      sum((P9-LMADRR_Rs[9])^2),
                      sum((PX-LMADRR_Rs[10])^2)))/20 + 11/90*sum((LMADRR_Rs-LMADRR_R)^2)
TV_LGMDRM <- mean(c(sum((Q1-LGMDRMs[1])^2), # LGMDR (math)
                    sum((Q2-LGMDRMs[2])^2),
                    sum((Q3-LGMDRMs[3])^2),
                    sum((Q4-LGMDRMs[4])^2),
                    sum((Q5-LGMDRMs[5])^2),
                    sum((Q6-LGMDRMs[6])^2),
                    sum((Q7-LGMDRMs[7])^2),
                    sum((Q8-LGMDRMs[8])^2),
                    sum((Q9-LGMDRMs[9])^2),
                    sum((QX-LGMDRMs[10])^2)))/20 + 11/90*sum((LGMDRMs-LGMDRM)^2)
TV_LGMDRR <- mean(c(sum((R1-LGMDRRs[1])^2), # LGMDR (reading)
                    sum((R2-LGMDRRs[2])^2),
                    sum((R3-LGMDRRs[3])^2),
                    sum((R4-LGMDRRs[4])^2),
                    sum((R5-LGMDRRs[5])^2),
                    sum((R6-LGMDRRs[6])^2),
                    sum((R7-LGMDRRs[7])^2),
                    sum((R8-LGMDRRs[8])^2),
                    sum((R9-LGMDRRs[9])^2),
                    sum((RX-LGMDRRs[10])^2)))/20 + 11/90*sum((LGMDRRs-LGMDRR)^2)

# standard errors
SE_dM <- sqrt(TV_dM) # d (math)
SE_dR <- sqrt(TV_dR) # d (reading)
SE_U3M <- sqrt(TV_U3M) # U3 (math)
SE_U3R <- sqrt(TV_U3R) # U3 (reading)
SE_LVRM <- sqrt(TV_LVRM) # LVR (math)
SE_LVRR <- sqrt(TV_LVRR) # LVR (reading)
SE_LVRM_L <- sqrt(TV_LVRM_L) # LVR_L (math)
SE_LVRR_L <- sqrt(TV_LVRR_L) # LVR_L (reading)
SE_LVRM_R <- sqrt(TV_LVRM_R) # LVR_R (math)
SE_LVRR_R <- sqrt(TV_LVRR_R) # LVR_R (reading)
SE_LMADRM <- sqrt(TV_LMADRM) # LMADR (math)
SE_LMADRR <- sqrt(TV_LMADRR) # LMADR (reading)
SE_LMADRM_L <- sqrt(TV_LMADRM_L) # LMADR_L (math)
SE_LMADRR_L <- sqrt(TV_LMADRR_L) # LMADR_L (reading)
SE_LMADRM_R <- sqrt(TV_LMADRM_R) # LMADR_R (math)
SE_LMADRR_R <- sqrt(TV_LMADRR_R) # LMADR_R (reading)
SE_LGMDRM <- sqrt(TV_LGMDRM) # LGMDR (math)
SE_LGMDRR <- sqrt(TV_LGMDRR) # LGMDR (reading)



#### SEs: Probability of superiority ####

A1 <- A2 <- A3 <- A4 <- A5 <- A6 <- A7 <- A8 <- A9 <- AX <- numeric(80) # empty containers
B1 <- B2 <- B3 <- B4 <- B5 <- B6 <- B7 <- B8 <- B9 <- BX <- numeric(80) # empty containers

# perform bootstrap resampling of PSs
for (i in 1:80) {
  W <- paste0('BW',i)
  
  A1[i] <- PSfn(P18_M, P18_F, W, 'MV1') # MV1 PS (math)
  A2[i] <- PSfn(P18_M, P18_F, W, 'MV2') # MV2
  A3[i] <- PSfn(P18_M, P18_F, W, 'MV3') # MV3
  A4[i] <- PSfn(P18_M, P18_F, W, 'MV4') # MV4
  A5[i] <- PSfn(P18_M, P18_F, W, 'MV5') # MV5
  A6[i] <- PSfn(P18_M, P18_F, W, 'MV6') # MV6
  A7[i] <- PSfn(P18_M, P18_F, W, 'MV7') # MV7
  A8[i] <- PSfn(P18_M, P18_F, W, 'MV8') # MV8
  A9[i] <- PSfn(P18_M, P18_F, W, 'MV9') # MV9
  AX[i] <- PSfn(P18_M, P18_F, W, 'MVX') # MVX
  B1[i] <- PSfn(P18_M, P18_F, W, 'RV1') # RV1 PS (reading)
  B2[i] <- PSfn(P18_M, P18_F, W, 'RV2') # RV2
  B3[i] <- PSfn(P18_M, P18_F, W, 'RV3') # RV3
  B4[i] <- PSfn(P18_M, P18_F, W, 'RV4') # RV4
  B5[i] <- PSfn(P18_M, P18_F, W, 'RV5') # RV5
  B6[i] <- PSfn(P18_M, P18_F, W, 'RV6') # RV6
  B7[i] <- PSfn(P18_M, P18_F, W, 'RV7') # RV7
  B8[i] <- PSfn(P18_M, P18_F, W, 'RV8') # RV8
  B9[i] <- PSfn(P18_M, P18_F, W, 'RV9') # RV9
  BX[i] <- PSfn(P18_M, P18_F, W, 'RVX') # RVX
  
  print(paste0(i, '/', 80, ' at ', Sys.time()), quote = F)
}

# total variance = sampling variance + imputation variance
TV_PSM <- mean(c(sum((A1-PSMs[1])^2), # math
                 sum((A2-PSMs[2])^2),
                 sum((A3-PSMs[3])^2),
                 sum((A4-PSMs[4])^2),
                 sum((A5-PSMs[5])^2),
                 sum((A6-PSMs[6])^2),
                 sum((A7-PSMs[7])^2),
                 sum((A8-PSMs[8])^2),
                 sum((A9-PSMs[9])^2),
                 sum((AX-PSMs[10])^2)))/20 + 11/90*sum((PSMs-PSM)^2)
TV_PSR <- mean(c(sum((B1-PSRs[1])^2), # reading
                 sum((B2-PSRs[2])^2),
                 sum((B3-PSRs[3])^2),
                 sum((B4-PSRs[4])^2),
                 sum((B5-PSRs[5])^2),
                 sum((B6-PSRs[6])^2),
                 sum((B7-PSRs[7])^2),
                 sum((B8-PSRs[8])^2),
                 sum((B9-PSRs[9])^2),
                 sum((BX-PSRs[10])^2)))/20 + 11/90*sum((PSRs-PSR)^2)

# standard errors
SE_PSM <- sqrt(TV_PSM) # math
SE_PSR <- sqrt(TV_PSR) # reading




####################################
##### 95% Confidence Intervals #####
####################################

# 95% CIs computed under the assumption of normality
CI95 <- qnorm(.975)


### Means and Medians
MnM_T_lo <- MnM-SE_MnM_T*CI95
MnM_T_up <- MnM+SE_MnM_T*CI95
MnR_T_lo <- MnR-SE_MnR_T*CI95
MnR_T_up <- MnR+SE_MnR_T*CI95

MdM_T_lo <- MdM-SE_MdM_T*CI95
MdM_T_up <- MdM+SE_MdM_T*CI95
MdR_T_lo <- MdR-SE_MdR_T*CI95
MdR_T_up <- MdR+SE_MdR_T*CI95

MnM_F_lo <- MnM_F-SE_MnM_F*CI95
MnM_F_up <- MnM_F+SE_MnM_F*CI95
MnR_F_lo <- MnR_F-SE_MnR_F*CI95
MnR_F_up <- MnR_F+SE_MnR_F*CI95

MdM_F_lo <- MdM_F-SE_MdM_F*CI95
MdM_F_up <- MdM_F+SE_MdM_F*CI95
MdR_F_lo <- MdR_F-SE_MdR_F*CI95
MdR_F_up <- MdR_F+SE_MdR_F*CI95

MnM_M_lo <- MnM_M-SE_MnM_M*CI95
MnM_M_up <- MnM_M+SE_MnM_M*CI95
MnR_M_lo <- MnR_M-SE_MnR_M*CI95
MnR_M_up <- MnR_M+SE_MnR_M*CI95

MdM_M_lo <- MdM_M-SE_MdM_M*CI95
MdM_M_up <- MdM_M+SE_MdM_M*CI95
MdR_M_lo <- MdR_M-SE_MdR_M*CI95
MdR_M_up <- MdR_M+SE_MdR_M*CI95

MnDfM_lo <- MnDfM-SE_MnDfM*CI95
MnDfM_up <- MnDfM+SE_MnDfM*CI95
MnDfR_lo <- MnDfR-SE_MnDfR*CI95
MnDfR_up <- MnDfR+SE_MnDfR*CI95

MdDfM_lo <- MdDfM-SE_MdDfM*CI95
MdDfM_up <- MdDfM+SE_MdDfM*CI95
MdDfR_lo <- MdDfR-SE_MdDfR*CI95
MdDfR_up <- MdDfR+SE_MdDfR*CI95


### TPRs
TPRMnM_lo <- exp(LTPRMnM-SE_MnM*CI95)
TPRMnM_up <- exp(LTPRMnM+SE_MnM*CI95)
TPRMnR_lo <- exp(LTPRMnR-SE_MnR*CI95)
TPRMnR_up <- exp(LTPRMnR+SE_MnR*CI95)

TPR05M_lo <- exp(LTPR05M-SE_05TM*CI95)
TPR05M_up <- exp(LTPR05M+SE_05TM*CI95)
TPR05R_lo <- exp(LTPR05R-SE_05TR*CI95)
TPR05R_up <- exp(LTPR05R+SE_05TR*CI95)

TPR10M_lo <- exp(LTPR10M-SE_10TM*CI95)
TPR10M_up <- exp(LTPR10M+SE_10TM*CI95)
TPR10R_lo <- exp(LTPR10R-SE_10TR*CI95)
TPR10R_up <- exp(LTPR10R+SE_10TR*CI95)

TPR15M_lo <- exp(LTPR15M-SE_15TM*CI95)
TPR15M_up <- exp(LTPR15M+SE_15TM*CI95)
TPR15R_lo <- exp(LTPR15R-SE_15TR*CI95)
TPR15R_up <- exp(LTPR15R+SE_15TR*CI95)

TPR20M_lo <- exp(LTPR20M-SE_20TM*CI95)
TPR20M_up <- exp(LTPR20M+SE_20TM*CI95)
TPR20R_lo <- exp(LTPR20R-SE_20TR*CI95)
TPR20R_up <- exp(LTPR20R+SE_20TR*CI95)

TPR25M_lo <- exp(LTPR25M-SE_25TM*CI95)
TPR25M_up <- exp(LTPR25M+SE_25TM*CI95)
TPR25R_lo <- exp(LTPR25R-SE_25TR*CI95)
TPR25R_up <- exp(LTPR25R+SE_25TR*CI95)

TPR30M_lo <- exp(LTPR30M-SE_30TM*CI95)
TPR30M_up <- exp(LTPR30M+SE_30TM*CI95)
TPR30R_lo <- exp(LTPR30R-SE_30TR*CI95)
TPR30R_up <- exp(LTPR30R+SE_30TR*CI95)

TPR35M_lo <- exp(LTPR35M-SE_35TM*CI95)
TPR35M_up <- exp(LTPR35M+SE_35TM*CI95)
TPR35R_lo <- exp(LTPR35R-SE_35TR*CI95)
TPR35R_up <- exp(LTPR35R+SE_35TR*CI95)

TPR40M_lo <- exp(LTPR40M-SE_40TM*CI95)
TPR40M_up <- exp(LTPR40M+SE_40TM*CI95)
TPR40R_lo <- exp(LTPR40R-SE_40TR*CI95)
TPR40R_up <- exp(LTPR40R+SE_40TR*CI95)

TPR45M_lo <- exp(LTPR45M-SE_45TM*CI95)
TPR45M_up <- exp(LTPR45M+SE_45TM*CI95)
TPR45R_lo <- exp(LTPR45R-SE_45TR*CI95)
TPR45R_up <- exp(LTPR45R+SE_45TR*CI95)

TPR50M_lo <- exp(LTPR50M-SE_50TM*CI95)
TPR50M_up <- exp(LTPR50M+SE_50TM*CI95)
TPR50R_lo <- exp(LTPR50R-SE_50TR*CI95)
TPR50R_up <- exp(LTPR50R+SE_50TR*CI95)

TPR55M_lo <- exp(LTPR55M-SE_55TM*CI95)
TPR55M_up <- exp(LTPR55M+SE_55TM*CI95)
TPR55R_lo <- exp(LTPR55R-SE_55TR*CI95)
TPR55R_up <- exp(LTPR55R+SE_55TR*CI95)

TPR60M_lo <- exp(LTPR60M-SE_60TM*CI95)
TPR60M_up <- exp(LTPR60M+SE_60TM*CI95)
TPR60R_lo <- exp(LTPR60R-SE_60TR*CI95)
TPR60R_up <- exp(LTPR60R+SE_60TR*CI95)

TPR65M_lo <- exp(LTPR65M-SE_65TM*CI95)
TPR65M_up <- exp(LTPR65M+SE_65TM*CI95)
TPR65R_lo <- exp(LTPR65R-SE_65TR*CI95)
TPR65R_up <- exp(LTPR65R+SE_65TR*CI95)

TPR70M_lo <- exp(LTPR70M-SE_70TM*CI95)
TPR70M_up <- exp(LTPR70M+SE_70TM*CI95)
TPR70R_lo <- exp(LTPR70R-SE_70TR*CI95)
TPR70R_up <- exp(LTPR70R+SE_70TR*CI95)

TPR75M_lo <- exp(LTPR75M-SE_75TM*CI95)
TPR75M_up <- exp(LTPR75M+SE_75TM*CI95)
TPR75R_lo <- exp(LTPR75R-SE_75TR*CI95)
TPR75R_up <- exp(LTPR75R+SE_75TR*CI95)

TPR80M_lo <- exp(LTPR80M-SE_80TM*CI95)
TPR80M_up <- exp(LTPR80M+SE_80TM*CI95)
TPR80R_lo <- exp(LTPR80R-SE_80TR*CI95)
TPR80R_up <- exp(LTPR80R+SE_80TR*CI95)

TPR85M_lo <- exp(LTPR85M-SE_85TM*CI95)
TPR85M_up <- exp(LTPR85M+SE_85TM*CI95)
TPR85R_lo <- exp(LTPR85R-SE_85TR*CI95)
TPR85R_up <- exp(LTPR85R+SE_85TR*CI95)

TPR90M_lo <- exp(LTPR90M-SE_90TM*CI95)
TPR90M_up <- exp(LTPR90M+SE_90TM*CI95)
TPR90R_lo <- exp(LTPR90R-SE_90TR*CI95)
TPR90R_up <- exp(LTPR90R+SE_90TR*CI95)

TPR95M_lo <- exp(LTPR95M-SE_95TM*CI95)
TPR95M_up <- exp(LTPR95M+SE_95TM*CI95)
TPR95R_lo <- exp(LTPR95R-SE_95TR*CI95)
TPR95R_up <- exp(LTPR95R+SE_95TR*CI95)

Md95TM_lo <- Md95TM-SE_Md95TM*CI95
Md95TM_up <- Md95TM+SE_Md95TM*CI95
Md95TR_lo <- Md95TR-SE_Md95TR*CI95
Md95TR_up <- Md95TR+SE_Md95TR*CI95
Md90TM_lo <- Md90TM-SE_Md90TM*CI95
Md90TM_up <- Md90TM+SE_Md90TM*CI95
Md90TR_lo <- Md90TR-SE_Md90TR*CI95
Md90TR_up <- Md90TR+SE_Md90TR*CI95
Md10TM_lo <- Md10TM-SE_Md10TM*CI95
Md10TM_up <- Md10TM+SE_Md10TM*CI95
Md10TR_lo <- Md10TR-SE_Md10TR*CI95
Md10TR_up <- Md10TR+SE_Md10TR*CI95
Md05TM_lo <- Md05TM-SE_Md05TM*CI95
Md05TM_up <- Md05TM+SE_Md05TM*CI95
Md05TR_lo <- Md05TR-SE_Md05TR*CI95
Md05TR_up <- Md05TR+SE_Md05TR*CI95

Mn95TM_lo <- Mn95TM-SE_Mn95TM*CI95
Mn95TM_up <- Mn95TM+SE_Mn95TM*CI95
Mn95TR_lo <- Mn95TR-SE_Mn95TR*CI95
Mn95TR_up <- Mn95TR+SE_Mn95TR*CI95
Mn90TM_lo <- Mn90TM-SE_Mn90TM*CI95
Mn90TM_up <- Mn90TM+SE_Mn90TM*CI95
Mn90TR_lo <- Mn90TR-SE_Mn90TR*CI95
Mn90TR_up <- Mn90TR+SE_Mn90TR*CI95
Mn10TM_lo <- Mn10TM-SE_Mn10TM*CI95
Mn10TM_up <- Mn10TM+SE_Mn10TM*CI95
Mn10TR_lo <- Mn10TR-SE_Mn10TR*CI95
Mn10TR_up <- Mn10TR+SE_Mn10TR*CI95
Mn05TM_lo <- Mn05TM-SE_Mn05TM*CI95
Mn05TM_up <- Mn05TM+SE_Mn05TM*CI95
Mn05TR_lo <- Mn05TR-SE_Mn05TR*CI95
Mn05TR_up <- Mn05TR+SE_Mn05TR*CI95


### U3Rs
U3R05M_lo <- exp(LU3R05M-SE_05TM*CI95)
U3R05M_up <- exp(LU3R05M+SE_05TM*CI95)
U3R05R_lo <- exp(LU3R05R-SE_05TR*CI95)
U3R05R_up <- exp(LU3R05R+SE_05TR*CI95)

U3R10M_lo <- exp(LU3R10M-SE_10TM*CI95)
U3R10M_up <- exp(LU3R10M+SE_10TM*CI95)
U3R10R_lo <- exp(LU3R10R-SE_10TR*CI95)
U3R10R_up <- exp(LU3R10R+SE_10TR*CI95)

U3R15M_lo <- exp(LU3R15M-SE_15TM*CI95)
U3R15M_up <- exp(LU3R15M+SE_15TM*CI95)
U3R15R_lo <- exp(LU3R15R-SE_15TR*CI95)
U3R15R_up <- exp(LU3R15R+SE_15TR*CI95)

U3R20M_lo <- exp(LU3R20M-SE_20TM*CI95)
U3R20M_up <- exp(LU3R20M+SE_20TM*CI95)
U3R20R_lo <- exp(LU3R20R-SE_20TR*CI95)
U3R20R_up <- exp(LU3R20R+SE_20TR*CI95)

U3R25M_lo <- exp(LU3R25M-SE_25TM*CI95)
U3R25M_up <- exp(LU3R25M+SE_25TM*CI95)
U3R25R_lo <- exp(LU3R25R-SE_25TR*CI95)
U3R25R_up <- exp(LU3R25R+SE_25TR*CI95)

U3R30M_lo <- exp(LU3R30M-SE_30TM*CI95)
U3R30M_up <- exp(LU3R30M+SE_30TM*CI95)
U3R30R_lo <- exp(LU3R30R-SE_30TR*CI95)
U3R30R_up <- exp(LU3R30R+SE_30TR*CI95)

U3R35M_lo <- exp(LU3R35M-SE_35TM*CI95)
U3R35M_up <- exp(LU3R35M+SE_35TM*CI95)
U3R35R_lo <- exp(LU3R35R-SE_35TR*CI95)
U3R35R_up <- exp(LU3R35R+SE_35TR*CI95)

U3R40M_lo <- exp(LU3R40M-SE_40TM*CI95)
U3R40M_up <- exp(LU3R40M+SE_40TM*CI95)
U3R40R_lo <- exp(LU3R40R-SE_40TR*CI95)
U3R40R_up <- exp(LU3R40R+SE_40TR*CI95)

U3R45M_lo <- exp(LU3R45M-SE_45TM*CI95)
U3R45M_up <- exp(LU3R45M+SE_45TM*CI95)
U3R45R_lo <- exp(LU3R45R-SE_45TR*CI95)
U3R45R_up <- exp(LU3R45R+SE_45TR*CI95)

U3R50M_lo <- exp(LU3R50M-SE_50TM*CI95)
U3R50M_up <- exp(LU3R50M+SE_50TM*CI95)
U3R50R_lo <- exp(LU3R50R-SE_50TR*CI95)
U3R50R_up <- exp(LU3R50R+SE_50TR*CI95)

U3R55M_lo <- exp(LU3R55M-SE_55TM*CI95)
U3R55M_up <- exp(LU3R55M+SE_55TM*CI95)
U3R55R_lo <- exp(LU3R55R-SE_55TR*CI95)
U3R55R_up <- exp(LU3R55R+SE_55TR*CI95)

U3R60M_lo <- exp(LU3R60M-SE_60TM*CI95)
U3R60M_up <- exp(LU3R60M+SE_60TM*CI95)
U3R60R_lo <- exp(LU3R60R-SE_60TR*CI95)
U3R60R_up <- exp(LU3R60R+SE_60TR*CI95)

U3R65M_lo <- exp(LU3R65M-SE_65TM*CI95)
U3R65M_up <- exp(LU3R65M+SE_65TM*CI95)
U3R65R_lo <- exp(LU3R65R-SE_65TR*CI95)
U3R65R_up <- exp(LU3R65R+SE_65TR*CI95)

U3R70M_lo <- exp(LU3R70M-SE_70TM*CI95)
U3R70M_up <- exp(LU3R70M+SE_70TM*CI95)
U3R70R_lo <- exp(LU3R70R-SE_70TR*CI95)
U3R70R_up <- exp(LU3R70R+SE_70TR*CI95)

U3R75M_lo <- exp(LU3R75M-SE_75TM*CI95)
U3R75M_up <- exp(LU3R75M+SE_75TM*CI95)
U3R75R_lo <- exp(LU3R75R-SE_75TR*CI95)
U3R75R_up <- exp(LU3R75R+SE_75TR*CI95)

U3R80M_lo <- exp(LU3R80M-SE_80TM*CI95)
U3R80M_up <- exp(LU3R80M+SE_80TM*CI95)
U3R80R_lo <- exp(LU3R80R-SE_80TR*CI95)
U3R80R_up <- exp(LU3R80R+SE_80TR*CI95)

U3R85M_lo <- exp(LU3R85M-SE_85TM*CI95)
U3R85M_up <- exp(LU3R85M+SE_85TM*CI95)
U3R85R_lo <- exp(LU3R85R-SE_85TR*CI95)
U3R85R_up <- exp(LU3R85R+SE_85TR*CI95)

U3R90M_lo <- exp(LU3R90M-SE_90TM*CI95)
U3R90M_up <- exp(LU3R90M+SE_90TM*CI95)
U3R90R_lo <- exp(LU3R90R-SE_90TR*CI95)
U3R90R_up <- exp(LU3R90R+SE_90TR*CI95)

U3R95M_lo <- exp(LU3R95M-SE_95TM*CI95)
U3R95M_up <- exp(LU3R95M+SE_95TM*CI95)
U3R95R_lo <- exp(LU3R95R-SE_95TR*CI95)
U3R95R_up <- exp(LU3R95R+SE_95TR*CI95)

Md95UM_lo <- Md95UM-SE_Md95UM*CI95
Md95UM_up <- Md95UM+SE_Md95UM*CI95
Md95UR_lo <- Md95UR-SE_Md95UR*CI95
Md95UR_up <- Md95UR+SE_Md95UR*CI95
Md90UM_lo <- Md90UM-SE_Md90UM*CI95
Md90UM_up <- Md90UM+SE_Md90UM*CI95
Md90UR_lo <- Md90UR-SE_Md90UR*CI95
Md90UR_up <- Md90UR+SE_Md90UR*CI95
Md10UM_lo <- Md10UM-SE_Md10UM*CI95
Md10UM_up <- Md10UM+SE_Md10UM*CI95
Md10UR_lo <- Md10UR-SE_Md10UR*CI95
Md10UR_up <- Md10UR+SE_Md10UR*CI95
Md05UM_lo <- Md05UM-SE_Md05UM*CI95
Md05UM_up <- Md05UM+SE_Md05UM*CI95
Md05UR_lo <- Md05UR-SE_Md05UR*CI95
Md05UR_up <- Md05UR+SE_Md05UR*CI95



### Other effect sizes

# Cohen's d
dM_lo <- dM-SE_dM*CI95
dM_up <- dM+SE_dM*CI95
dR_lo <- dR-SE_dR*CI95
dR_up <- dR+SE_dR*CI95

# U3
U3M_lo <- U3M-SE_U3M*CI95
U3M_up <- U3M+SE_U3M*CI95
U3R_lo <- U3R-SE_U3R*CI95
U3R_up <- U3R+SE_U3R*CI95

# Probability of superiority
PSM_lo <- PSM-SE_PSM*CI95
PSM_up <- PSM+SE_PSM*CI95
PSR_lo <- PSR-SE_PSR*CI95
PSR_up <- PSR+SE_PSR*CI95

# Variance ratio
VRM_lo <- exp(LVRM-SE_LVRM*CI95)
VRM_up <- exp(LVRM+SE_LVRM*CI95)
VRR_lo <- exp(LVRR-SE_LVRR*CI95)
VRR_up <- exp(LVRR+SE_LVRR*CI95)

# Variance ratio (left tail)
VRM_L_lo <- exp(LVRM_L-SE_LVRM_L*CI95)
VRM_L_up <- exp(LVRM_L+SE_LVRM_L*CI95)
VRR_L_lo <- exp(LVRR_L-SE_LVRR_L*CI95)
VRR_L_up <- exp(LVRR_L+SE_LVRR_L*CI95)

# Variance ratio (right tail)
VRM_R_lo <- exp(LVRM_R-SE_LVRM_R*CI95)
VRM_R_up <- exp(LVRM_R+SE_LVRM_R*CI95)
VRR_R_lo <- exp(LVRR_R-SE_LVRR_R*CI95)
VRR_R_up <- exp(LVRR_R+SE_LVRR_R*CI95)

# Mean absolute deviation ratio
MADRM_lo <- exp(LMADRM-SE_LMADRM*CI95)
MADRM_up <- exp(LMADRM+SE_LMADRM*CI95)
MADRR_lo <- exp(LMADRR-SE_LMADRR*CI95)
MADRR_up <- exp(LMADRR+SE_LMADRR*CI95)

# Mean absolute deviation ratio (left tail)
MADRM_L_lo <- exp(LMADRM_L-SE_LMADRM_L*CI95)
MADRM_L_up <- exp(LMADRM_L+SE_LMADRM_L*CI95)
MADRR_L_lo <- exp(LMADRR_L-SE_LMADRR_L*CI95)
MADRR_L_up <- exp(LMADRR_L+SE_LMADRR_L*CI95)

# Mean absolute deviation ratio (right tail)
MADRM_R_lo <- exp(LMADRM_R-SE_LMADRM_R*CI95)
MADRM_R_up <- exp(LMADRM_R+SE_LMADRM_R*CI95)
MADRR_R_lo <- exp(LMADRR_R-SE_LMADRR_R*CI95)
MADRR_R_up <- exp(LMADRR_R+SE_LMADRR_R*CI95)

# Gini's mean difference ratio
GMDRM_lo <- exp(LGMDRM-SE_LGMDRM*CI95)
GMDRM_up <- exp(LGMDRM+SE_LGMDRM*CI95)
GMDRR_lo <- exp(LGMDRR-SE_LGMDRR*CI95)
GMDRR_up <- exp(LGMDRR+SE_LGMDRR*CI95)








##################
##### Output #####
##################

# further variables

WtRatio <- sum(P18_M$HWt)/sum(P18_F$HWt)

# Standard deviation ratio (SDR), as VR is not comparable to MADR and GMDR
SDRM <- sqrt(VRM)
SDRM_lo <- sqrt(VRM_lo)
SDRM_up <- sqrt(VRM_up)
LSDRM <- log(sqrt(VRM))
TV_LSDRM <- (log(SDRM_up/SDRM)/CI95)^2 # SDR (math)

SDRR <- sqrt(VRR)
SDRR_lo <- sqrt(VRR_lo)
SDRR_up <- sqrt(VRR_up)
LSDRR <- log(sqrt(VRR))
TV_LSDRR <- (log(SDRR_up/SDRR)/CI95)^2 # SDR (reading)

SDRM_L <- sqrt(VRM_L)
SDRM_L_lo <- sqrt(VRM_L_lo)
SDRM_L_up <- sqrt(VRM_L_up)
LSDRM_L <- log(sqrt(VRM_L))
TV_LSDRM_L <- (log(SDRM_L_up/SDRM_L)/CI95)^2 # SDR_L (math)

SDRR_L <- sqrt(VRR_L)
SDRR_L_lo <- sqrt(VRR_L_lo)
SDRR_L_up <- sqrt(VRR_L_up)
LSDRR_L <- log(sqrt(VRR_L))
TV_LSDRR_L <- (log(SDRR_L_up/SDRR_L)/CI95)^2 # SDR_L (reading)

SDRM_R <- sqrt(VRM_R)
SDRM_R_lo <- sqrt(VRM_R_lo)
SDRM_R_up <- sqrt(VRM_R_up)
LSDRM_R <- log(sqrt(VRM_R))
TV_LSDRM_R <- (log(SDRM_R_up/SDRM_R)/CI95)^2 # SDR_R (math)

SDRR_R <- sqrt(VRR_R)
SDRR_R_lo <- sqrt(VRR_R_lo)
SDRR_R_up <- sqrt(VRR_R_up)
LSDRR_R <- log(sqrt(VRR_R))
TV_LSDRR_R <- (log(SDRR_R_up/SDRR_R)/CI95)^2 # SDR_R (reading)


# summary table
Labels <- c('Country', 'CNT', 'Size', 'FSize', 'MSize', 'M/F Wt Ratio',
            'Mean', 'Mean Low', 'Mean Upp', 'Median', 'Median Low', 'Median Upp',
            'F Mean', 'F Mean Low', 'F Mean Upp', 'F Median', 'F Median Low', 'F Median Upp',
            'M Mean', 'M Mean Low', 'M Mean Upp', 'M Median', 'M Median Low', 'M Median Upp',
            'Mean Diff', 'Mean Diff Low', 'Mean Diff Upp',
            'Med Diff', 'Med Diff Low', 'Med Diff Upp',
            'TPRMn', 'TPRMn Low', 'TPRMn Upp', 'TPR05', 'TPR05 Low', 'TPR05 Upp',
            'TPR10', 'TPR10 Low', 'TPR10 Upp', 'TPR15', 'TPR15 Low', 'TPR15 Upp',
            'TPR20', 'TPR20 Low', 'TPR20 Upp', 'TPR25', 'TPR25 Low', 'TPR25 Upp',
            'TPR30', 'TPR30 Low', 'TPR30 Upp', 'TPR35', 'TPR35 Low', 'TPR35 Upp',
            'TPR40', 'TPR40 Low', 'TPR40 Upp', 'TPR45', 'TPR45 Low', 'TPR45 Upp',
            'TPR50', 'TPR50 Low', 'TPR50 Upp', 'TPR55', 'TPR55 Low', 'TPR55 Upp',
            'TPR60', 'TPR60 Low', 'TPR60 Upp', 'TPR65', 'TPR65 Low', 'TPR65 Upp',
            'TPR70', 'TPR70 Low', 'TPR70 Upp', 'TPR75', 'TPR75 Low', 'TPR75 Upp',
            'TPR80', 'TPR80 Low', 'TPR80 Upp', 'TPR85', 'TPR85 Low', 'TPR85 Upp',
            'TPR90', 'TPR90 Low', 'TPR90 Upp', 'TPR95', 'TPR95 Low', 'TPR95 Upp',
            'LTPRMn', 'LTPRMn TV', 'LTPR05', 'LTPR05 TV',
            'LTPR10', 'LTPR10 TV', 'LTPR15', 'LTPR15 TV',
            'LTPR20', 'LTPR20 TV', 'LTPR25', 'LTPR25 TV',
            'LTPR30', 'LTPR30 TV', 'LTPR35', 'LTPR35 TV',
            'LTPR40', 'LTPR40 TV', 'LTPR45', 'LTPR45 TV',
            'LTPR50', 'LTPR50 TV', 'LTPR55', 'LTPR55 TV',
            'LTPR60', 'LTPR60 TV', 'LTPR65', 'LTPR65 TV',
            'LTPR70', 'LTPR70 TV', 'LTPR75', 'LTPR75 TV',
            'LTPR80', 'LTPR80 TV', 'LTPR85', 'LTPR85 TV',
            'LTPR90', 'LTPR90 TV', 'LTPR95', 'LTPR95 TV',
            'Med95T', 'Med95T Low', 'Med95T Upp', 'Med95T TV',
            'Med90T', 'Med90T Low', 'Med90T Upp', 'Med90T TV',
            'Med10T', 'Med10T Low', 'Med10T Upp', 'Med10T TV',
            'Med05T', 'Med05T Low', 'Med05T Upp', 'Med05T TV',
            'Mn95T', 'Mn95T Low', 'Mn95T Upp', 'Mn95T TV',
            'Mn90T', 'Mn90T Low', 'Mn90T Upp', 'Mn90T TV',
            'Mn10T', 'Mn10T Low', 'Mn10T Upp', 'Mn10T TV',
            'Mn05T', 'Mn05T Low', 'Mn05T Upp', 'Mn05T TV',
            'U3R05', 'U3R05 Low', 'U3R05 Upp',
            'U3R10', 'U3R10 Low', 'U3R10 Upp', 'U3R15', 'U3R15 Low', 'U3R15 Upp',
            'U3R20', 'U3R20 Low', 'U3R20 Upp', 'U3R25', 'U3R25 Low', 'U3R25 Upp',
            'U3R30', 'U3R30 Low', 'U3R30 Upp', 'U3R35', 'U3R35 Low', 'U3R35 Upp',
            'U3R40', 'U3R40 Low', 'U3R40 Upp', 'U3R45', 'U3R45 Low', 'U3R45 Upp',
            'U3R50', 'U3R50 Low', 'U3R50 Upp', 'U3R55', 'U3R55 Low', 'U3R55 Upp',
            'U3R60', 'U3R60 Low', 'U3R60 Upp', 'U3R65', 'U3R65 Low', 'U3R65 Upp',
            'U3R70', 'U3R70 Low', 'U3R70 Upp', 'U3R75', 'U3R75 Low', 'U3R75 Upp',
            'U3R80', 'U3R80 Low', 'U3R80 Upp', 'U3R85', 'U3R85 Low', 'U3R85 Upp',
            'U3R90', 'U3R90 Low', 'U3R90 Upp', 'U3R95', 'U3R95 Low', 'U3R95 Upp',
            'LU3R05', 'LU3R05 TV',
            'LU3R10', 'LU3R10 TV', 'LU3R15', 'LU3R15 TV',
            'LU3R20', 'LU3R20 TV', 'LU3R25', 'LU3R25 TV',
            'LU3R30', 'LU3R30 TV', 'LU3R35', 'LU3R35 TV',
            'LU3R40', 'LU3R40 TV', 'LU3R45', 'LU3R45 TV',
            'LU3R50', 'LU3R50 TV', 'LU3R55', 'LU3R55 TV',
            'LU3R60', 'LU3R60 TV', 'LU3R65', 'LU3R65 TV',
            'LU3R70', 'LU3R70 TV', 'LU3R75', 'LU3R75 TV',
            'LU3R80', 'LU3R80 TV', 'LU3R85', 'LU3R85 TV',
            'LU3R90', 'LU3R90 TV', 'LU3R95', 'LU3R95 TV',
            'Med95U', 'Med95U Low', 'Med95U Upp', 'Med95U TV',
            'Med90U', 'Med90U Low', 'Med90U Upp', 'Med90U TV',
            'Med10U', 'Med10U Low', 'Med10U Upp', 'Med10U TV',
            'Med05U', 'Med05U Low', 'Med05U Upp', 'Med05U TV',
            'd', 'd Low', 'd Upp', 'd TV',
            'U3', 'U3 Low', 'U3 Upp', 'U3 TV',
            'PS', 'PS Low', 'PS Upp', 'PS TV',
            'VR', 'VR Low', 'VR Upp', 'LVR', 'LVR TV',
            'VR_L', 'VR_L Low', 'VR_L Upp', 'LVR_L', 'LVR_L TV',
            'VR_R', 'VR_R Low', 'VR_R Upp', 'LVR_R', 'LVR_R TV',
            'SDR', 'SDR Low', 'SDR Upp', 'LSDR', 'LSDR TV',
            'SDR_L', 'SDR_L Low', 'SDR_L Upp', 'LSDR_L', 'LSDR_L TV',
            'SDR_R', 'SDR_R Low', 'SDR_R Upp', 'LSDR_R', 'LSDR_R TV',
            'MADR', 'MADR Low', 'MADR Upp', 'LMADR', 'LMADR TV',
            'MADR_L', 'MADR_L Low', 'MADR_L Upp', 'LMADR_L', 'LMADR_L TV',
            'MADR_R', 'MADR_R Low', 'MADR_R Upp', 'LMADR_R', 'LMADR_R TV',
            'GMDR', 'GMDR Low', 'GMDR Upp', 'LGMDR', 'LGMDR TV',
            'Age U3', 'Age MADR', 'Age LMADR', 'd AgeCor', 'U3 AgeCor',
            'PS AgeCor', 'VR AgeCor', 'LVR AgeCor', 'VR_L AgeCor',
            'LVR_L AgeCor', 'VR_R AgeCor', 'LVR_R AgeCor',
            'MADR AgeCor', 'LMADR AgeCor', 'MADR_L AgeCor', 'LMADR_L AgeCor',
            'MADR_R AgeCor', 'LMADR_R AgeCor', 'GMDR AgeCor', 'LGMDR AgeCor')

MVars <- c(Country, CT, Size, FSize, MSize, WtRatio,
           MnM, MnM_T_lo, MnM_T_lo, MdM, MdM_T_lo, MdM_T_lo,
           MnM_F, MnM_F_lo, MnM_F_up, MdM_F, MdM_F_lo, MdM_F_up,
           MnM_M, MnM_M_lo, MnM_M_up, MdM_M, MdM_M_lo, MdM_M_up,
           MnDfM, MnDfM_lo, MnDfM_up,
           MdDfM, MdDfM_lo, MdDfM_up,
           TPRMnM, TPRMnM_lo, TPRMnM_up, TPR05M, TPR05M_lo, TPR05M_up,
           TPR10M, TPR10M_lo, TPR10M_up, TPR15M, TPR15M_lo, TPR15M_up,
           TPR20M, TPR20M_lo, TPR20M_up, TPR25M, TPR25M_lo, TPR25M_up,
           TPR30M, TPR30M_lo, TPR30M_up, TPR35M, TPR35M_lo, TPR35M_up,
           TPR40M, TPR40M_lo, TPR40M_up, TPR45M, TPR45M_lo, TPR45M_up,
           TPR50M, TPR50M_lo, TPR50M_up, TPR55M, TPR55M_lo, TPR55M_up,
           TPR60M, TPR60M_lo, TPR60M_up, TPR65M, TPR65M_lo, TPR65M_up,
           TPR70M, TPR70M_lo, TPR70M_up, TPR75M, TPR75M_lo, TPR75M_up,
           TPR80M, TPR80M_lo, TPR80M_up, TPR85M, TPR85M_lo, TPR85M_up,
           TPR90M, TPR90M_lo, TPR90M_up, TPR95M, TPR95M_lo, TPR95M_up,
           LTPRMnM, TV_MnM, LTPR05M, TV_05TM,
           LTPR10M, TV_10TM, LTPR15M, TV_15TM,
           LTPR20M, TV_20TM, LTPR25M, TV_25TM,
           LTPR30M, TV_30TM, LTPR35M, TV_35TM,
           LTPR40M, TV_40TM, LTPR45M, TV_45TM,
           LTPR50M, TV_50TM, LTPR55M, TV_55TM,
           LTPR60M, TV_60TM, LTPR65M, TV_65TM,
           LTPR70M, TV_70TM, LTPR75M, TV_75TM,
           LTPR80M, TV_80TM, LTPR85M, TV_85TM,
           LTPR90M, TV_90TM, LTPR95M, TV_95TM,
           Md95TM, Md95TM_lo, Md95TM_up, TV_Md95TM,
           Md90TM, Md90TM_lo, Md90TM_up, TV_Md90TM,
           Md10TM, Md10TM_lo, Md10TM_up, TV_Md10TM,
           Md05TM, Md05TM_lo, Md05TM_up, TV_Md05TM,
           Mn95TM, Mn95TM_lo, Mn95TM_up, TV_Mn95TM,
           Mn90TM, Mn90TM_lo, Mn90TM_up, TV_Mn90TM,
           Mn10TM, Mn10TM_lo, Mn10TM_up, TV_Mn10TM,
           Mn05TM, Mn05TM_lo, Mn05TM_up, TV_Mn05TM,
           U3R05M, U3R05M_lo, U3R05M_up,
           U3R10M, U3R10M_lo, U3R10M_up, U3R15M, U3R15M_lo, U3R15M_up,
           U3R20M, U3R20M_lo, U3R20M_up, U3R25M, U3R25M_lo, U3R25M_up,
           U3R30M, U3R30M_lo, U3R30M_up, U3R35M, U3R35M_lo, U3R35M_up,
           U3R40M, U3R40M_lo, U3R40M_up, U3R45M, U3R45M_lo, U3R45M_up,
           U3R50M, U3R50M_lo, U3R50M_up, U3R55M, U3R55M_lo, U3R55M_up,
           U3R60M, U3R60M_lo, U3R60M_up, U3R65M, U3R65M_lo, U3R65M_up,
           U3R70M, U3R70M_lo, U3R70M_up, U3R75M, U3R75M_lo, U3R75M_up,
           U3R80M, U3R80M_lo, U3R80M_up, U3R85M, U3R85M_lo, U3R85M_up,
           U3R90M, U3R90M_lo, U3R90M_up, U3R95M, U3R95M_lo, U3R95M_up,
           LU3R05M, TV_05UM,
           LU3R10M, TV_10UM, LU3R15M, TV_15UM,
           LU3R20M, TV_20UM, LU3R25M, TV_25UM,
           LU3R30M, TV_30UM, LU3R35M, TV_35UM,
           LU3R40M, TV_40UM, LU3R45M, TV_45UM,
           LU3R50M, TV_50UM, LU3R55M, TV_55UM,
           LU3R60M, TV_60UM, LU3R65M, TV_65UM,
           LU3R70M, TV_70UM, LU3R75M, TV_75UM,
           LU3R80M, TV_80UM, LU3R85M, TV_85UM,
           LU3R90M, TV_90UM, LU3R95M, TV_95UM,
           Md95UM, Md95UM_lo, Md95UM_up, TV_Md95UM,
           Md90UM, Md90UM_lo, Md90UM_up, TV_Md90UM,
           Md10UM, Md10UM_lo, Md10UM_up, TV_Md10UM,
           Md05UM, Md05UM_lo, Md05UM_up, TV_Md05UM,
           dM, dM_lo, dM_up, TV_dM,
           U3M, U3M_lo, U3M_up, TV_U3M,
           PSM, PSM_lo, PSM_up, TV_PSM,
           VRM, VRM_lo, VRM_up, LVRM, TV_LVRM,
           VRM_L, VRM_L_lo, VRM_L_up, LVRM_L, TV_LVRM_L,
           VRM_R, VRM_R_lo, VRM_R_up, LVRM_R, TV_LVRM_R,
           SDRM, SDRM_lo, SDRM_up, LSDRM, TV_LSDRM,
           SDRM_L, SDRM_L_lo, SDRM_L_up, LSDRM_L, TV_LSDRM_L,
           SDRM_R, SDRM_R_lo, SDRM_R_up, LSDRM_R, TV_LSDRM_R,
           MADRM, MADRM_lo, MADRM_up, LMADRM, TV_LMADRM,
           MADRM_L, MADRM_L_lo, MADRM_L_up, LMADRM_L, TV_LMADRM_L,
           MADRM_R, MADRM_R_lo, MADRM_R_up, LMADRM_R, TV_LMADRM_R,
           GMDRM, GMDRM_lo, GMDRM_up, LGMDRM, TV_LGMDRM, 
           AgeU3, AgeMADR, AgeLMADR, dM_A, U3M_A,
           PSM_A, VRM_A, LVRM_A, VRM_LA,
           LVRM_LA, VRM_RA, LVRM_RA,
           MADRM_A, LMADRM_A, MADRM_LA, LMADRM_LA,
           MADRM_RA, LMADRM_RA, GMDRM_A, LGMDRM_A)

RVars <- c(Country, CT, Size, FSize, MSize, WtRatio,
           MnR, MnR_T_lo, MnR_T_lo, MdR, MdR_T_lo, MdR_T_lo,
           MnR_F, MnR_F_lo, MnR_F_up, MdR_F, MdR_F_lo, MdR_F_up,
           MnR_M, MnR_M_lo, MnR_M_up, MdR_M, MdR_M_lo, MdR_M_up,
           MnDfR, MnDfR_lo, MnDfR_up,
           MdDfR, MdDfR_lo, MdDfR_up,
           TPRMnR, TPRMnR_lo, TPRMnR_up, TPR05R, TPR05R_lo, TPR05R_up,
           TPR10R, TPR10R_lo, TPR10R_up, TPR15R, TPR15R_lo, TPR15R_up,
           TPR20R, TPR20R_lo, TPR20R_up, TPR25R, TPR25R_lo, TPR25R_up,
           TPR30R, TPR30R_lo, TPR30R_up, TPR35R, TPR35R_lo, TPR35R_up,
           TPR40R, TPR40R_lo, TPR40R_up, TPR45R, TPR45R_lo, TPR45R_up,
           TPR50R, TPR50R_lo, TPR50R_up, TPR55R, TPR55R_lo, TPR55R_up,
           TPR60R, TPR60R_lo, TPR60R_up, TPR65R, TPR65R_lo, TPR65R_up,
           TPR70R, TPR70R_lo, TPR70R_up, TPR75R, TPR75R_lo, TPR75R_up,
           TPR80R, TPR80R_lo, TPR80R_up, TPR85R, TPR85R_lo, TPR85R_up,
           TPR90R, TPR90R_lo, TPR90R_up, TPR95R, TPR95R_lo, TPR95R_up,
           LTPRMnR, TV_MnR, LTPR05R, TV_05TR,
           LTPR10R, TV_10TR, LTPR15R, TV_15TR,
           LTPR20R, TV_20TR, LTPR25R, TV_25TR,
           LTPR30R, TV_30TR, LTPR35R, TV_35TR,
           LTPR40R, TV_40TR, LTPR45R, TV_45TR,
           LTPR50R, TV_50TR, LTPR55R, TV_55TR,
           LTPR60R, TV_60TR, LTPR65R, TV_65TR,
           LTPR70R, TV_70TR, LTPR75R, TV_75TR,
           LTPR80R, TV_80TR, LTPR85R, TV_85TR,
           LTPR90R, TV_90TR, LTPR95R, TV_95TR,
           Md95TR, Md95TR_lo, Md95TR_up, TV_Md95TR,
           Md90TR, Md90TR_lo, Md90TR_up, TV_Md90TR,
           Md10TR, Md10TR_lo, Md10TR_up, TV_Md10TR,
           Md05TR, Md05TR_lo, Md05TR_up, TV_Md05TR,
           Mn95TR, Mn95TR_lo, Mn95TR_up, TV_Mn95TR,
           Mn90TR, Mn90TR_lo, Mn90TR_up, TV_Mn90TR,
           Mn10TR, Mn10TR_lo, Mn10TR_up, TV_Mn10TR,
           Mn05TR, Mn05TR_lo, Mn05TR_up, TV_Mn05TR,
           U3R05R, U3R05R_lo, U3R05R_up,
           U3R10R, U3R10R_lo, U3R10R_up, U3R15R, U3R15R_lo, U3R15R_up,
           U3R20R, U3R20R_lo, U3R20R_up, U3R25R, U3R25R_lo, U3R25R_up,
           U3R30R, U3R30R_lo, U3R30R_up, U3R35R, U3R35R_lo, U3R35R_up,
           U3R40R, U3R40R_lo, U3R40R_up, U3R45R, U3R45R_lo, U3R45R_up,
           U3R50R, U3R50R_lo, U3R50R_up, U3R55R, U3R55R_lo, U3R55R_up,
           U3R60R, U3R60R_lo, U3R60R_up, U3R65R, U3R65R_lo, U3R65R_up,
           U3R70R, U3R70R_lo, U3R70R_up, U3R75R, U3R75R_lo, U3R75R_up,
           U3R80R, U3R80R_lo, U3R80R_up, U3R85R, U3R85R_lo, U3R85R_up,
           U3R90R, U3R90R_lo, U3R90R_up, U3R95R, U3R95R_lo, U3R95R_up,
           LU3R05R, TV_05UR,
           LU3R10R, TV_10UR, LU3R15R, TV_15UR,
           LU3R20R, TV_20UR, LU3R25R, TV_25UR,
           LU3R30R, TV_30UR, LU3R35R, TV_35UR,
           LU3R40R, TV_40UR, LU3R45R, TV_45UR,
           LU3R50R, TV_50UR, LU3R55R, TV_55UR,
           LU3R60R, TV_60UR, LU3R65R, TV_65UR,
           LU3R70R, TV_70UR, LU3R75R, TV_75UR,
           LU3R80R, TV_80UR, LU3R85R, TV_85UR,
           LU3R90R, TV_90UR, LU3R95R, TV_95UR,
           Md95UR, Md95UR_lo, Md95UR_up, TV_Md95UR,
           Md90UR, Md90UR_lo, Md90UR_up, TV_Md90UR,
           Md10UR, Md10UR_lo, Md10UR_up, TV_Md10UR,
           Md05UR, Md05UR_lo, Md05UR_up, TV_Md05UR,
           dR, dR_lo, dR_up, TV_dR,
           U3R, U3R_lo, U3R_up, TV_U3R,
           PSR, PSR_lo, PSR_up, TV_PSR,
           VRR, VRR_lo, VRR_up, LVRR, TV_LVRR,
           VRR_L, VRR_L_lo, VRR_L_up, LVRR_L, TV_LVRR_L,
           VRR_R, VRR_R_lo, VRR_R_up, LVRR_R, TV_LVRR_R,
           SDRR, SDRR_lo, SDRR_up, LSDRR, TV_LSDRR,
           SDRR_L, SDRR_L_lo, SDRR_L_up, LSDRR_L, TV_LSDRR_L,
           SDRR_R, SDRR_R_lo, SDRR_R_up, LSDRR_R, TV_LSDRR_R,
           MADRR, MADRR_lo, MADRR_up, LMADRR, TV_LMADRR,
           MADRR_L, MADRR_L_lo, MADRR_L_up, LMADRR_L, TV_LMADRR_L,
           MADRR_R, MADRR_R_lo, MADRR_R_up, LMADRR_R, TV_LMADRR_R,
           GMDRR, GMDRR_lo, GMDRR_up, LGMDRR, TV_LGMDRR, 
           AgeU3, AgeMADR, AgeLMADR, dR_A, U3R_A,
           PSR_A, VRR_A, LVRR_A, VRR_LA,
           LVRR_LA, VRR_RA, LVRR_RA,
           MADRR_A, LMADRR_A, MADRR_LA, LMADRR_LA,
           MADRR_RA, LMADRR_RA, GMDRR_A, LGMDRR_A)

OutputM <- format(data.frame(Labels, MVars), scientific = F) # put math in this
OutputR <- format(data.frame(Labels, RVars), scientific = F) # put reading in this

# select file names manually, store as a csv
write.csv(x = OutputM, file = 'PISA output/Math/2018/Countries/AUS.csv')
write.csv(x = OutputR, file = 'PISA output/Reading/2018/Countries/AUS.csv')



















