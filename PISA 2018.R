######################################################################################
######################################################################################
############### SEX DIFFERENCES IN THE PISA 2018 MATH AND READING DATA ###############
######################################################################################
######################################################################################

#### Outline ####

# [1] Read and Format Data
# select country
# objects needed throughout the script
# [2] Weighted Functions
# functions needed throughout the script, with descriptions
# [3] Effect Sizes
# various effect sizes for sex differences throughout the distribution
# [4] Some Effect Sizes Adjusted for Age
# dataframe with scores linearly controlled for age
# age-adjusted effect sizes
# [5] Standard Errors and Confidence Intervals
# Fay's method of balanced repeated replication
# total variance and CI bounds for effect sizes
# [6] Output
# write dataframe with all needed variables

# ratios are log-transformed to place them on a linear scale
# log-transformed ratios are signified by an 'L' preceding the acronym




##############################
#### Read and Format Data ####
##############################

#### Select Country ####

library(haven) # read SPSS
PISA18 <- read_spss('PISA/CY07_MSU_STU_QQQ.sav') # read data
write.csv(x = PISA18, file = 'PISA input/2018.csv') # store it as a csv

library(data.table) # efficient reading
PISA18 <- fread('PISA input/2018.csv')[,-1] # load all PISA 2018 with data.table


# manually subset a polity (* indicates a region or group of regions)
CT <- 'ALB' # Albania
CT <- 'ARG' # Argentina
CT <- 'AUS' # Australia
CT <- 'AUT' # Austria
CT <- 'QAZ' # Baku, Azerbaijan*
CT <- 'QCI' # B-S-J-Z, China (Beijing, Shanghai, Jiangsu, Zhejiang)*
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
CT <- 'HKG' # Hong Kong, China*
CT <- 'HUN' # Hungary
CT <- 'ISL' # Iceland
CT <- 'IDN' # Indonesia
CT <- 'IRL' # Ireland
CT <- 'ISR' # Israel
CT <- 'ITA' # Italy
CT <- 'JPN' # Japan
CT <- 'JOR' # Jordan
CT <- 'KAZ' # Kazakhstan
CT <- 'KSV' # Kosovo
CT <- 'LVA' # Latvia
CT <- 'LBN' # Lebanon
CT <- 'LTU' # Lithuania
CT <- 'LUX' # Luxembourg
CT <- 'MAC' # Macao, China*
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
CT <- 'QMR' # Moscow region, Russia*
CT <- 'QRT' # Tatarstan, Russia*
CT <- 'ROU' # Romania
CT <- 'RUS' # Russia
CT <- 'SAU' # Saudi Arabia
CT <- 'SRB' # Serbia
CT <- 'SGP' # Singapore
CT <- 'SVK' # Slovakia
CT <- 'SVN' # Slovenia
CT <- 'KOR' # South Korea
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

# lookup table
CNT <- data.frame('Albania', 'Argentina', 'Australia', 'Austria', 'Baku',
                  'B-S-J-Z', 'Belarus', 'Belgium', 'Bosnia and Herzegovina', 'Brazil',
                  'Brunei Darussalam', 'Bulgaria', 'Canada', 'Chile', 'Colombia',
                  'Costa Rica', 'Croatia', 'Czech Republic', 'Denmark', 'Dominican Republic',
                  'Estonia', 'Finland', 'France', 'Georgia', 'Germany',
                  'Greece', 'Hong Kong', 'Hungary', 'Iceland', 'Indonesia',
                  'Ireland', 'Israel', 'Italy', 'Japan', 'Jordan',
                  'Kazakhstan', 'Kosovo', 'Latvia', 'Lebanon', 'Lithuania',
                  'Luxembourg', 'Macao', 'Macedonia', 'Malaysia', 'Malta',
                  'Mexico', 'Moldova', 'Montenegro', 'Morocco', 'Netherlands',
                  'New Zealand', 'Norway', 'Panama', 'Peru', 'Phillipines',
                  'Poland', 'Portugal', 'Qatar', 'Moscow region', 'Tatarstan',
                  'Romania', 'Russia', 'Saudi Arabia', 'Serbia', 'Singapore',
                  'Slovakia', 'Slovenia', 'South Korea', 'Spain', 'Sweden',
                  'Switzerland', 'Taiwan', 'Thailand', 'Turkey', 'Ukraine',
                  'United Arab Emirates', 'United Kingdom', 'United States', 'Uruguay', 'Vietnam')
names(CNT) <- c('ALB', 'ARG', 'AUS', 'AUT', 'QAZ', 'QCI', 'BLR', 'BEL', 'BIH', 'BRA',
                'BRN', 'BGR', 'CAN', 'CHL', 'COL', 'CRI', 'HRV', 'CZE', 'DNK', 'DOM',
                'EST', 'FIN', 'FRA', 'GEO', 'DEU', 'GRC', 'HKG', 'HUN', 'ISL', 'IDN',
                'IRL', 'ISR', 'ITA', 'JPN', 'JOR', 'KAZ', 'KSV', 'LVA', 'LBN', 'LTU',
                'LUX', 'MAC', 'MKD', 'MYS', 'MLT', 'MEX', 'MDA', 'MNE', 'MAR', 'NLD',
                'NZL', 'NOR', 'PAN', 'PER', 'PHL', 'POL', 'PRT', 'QAT', 'QMR', 'QRT',
                'ROU', 'RUS', 'SAU', 'SRB', 'SGP', 'SVK', 'SVN', 'KOR', 'ESP', 'SWE',
                'CHE', 'TAP', 'THA', 'TUR', 'UKR', 'ARE', 'GBR', 'USA', 'URY', 'VNM')

Country <- as.character(CNT[,CT])

P18 <- data.frame(PISA18[CNT == CT])[,c(4, 18, 944, 835, 1027:1046, 945:1024)] # subset

colnms <- numeric(80)
for (i in 1:80) {colnms[i] <- paste0('FW',i)} # Fay's replicate weight columns: FW1-FW80

names(P18) <- c('Student', 'Sex', 'HWt', 'Age',
                'MV1', 'MV2', 'MV3', 'MV4', 'MV5', 'MV6', 'MV7', 'MV8', 'MV9', 'MV10',
                'RV1', 'RV2', 'RV3', 'RV4', 'RV5', 'RV6', 'RV7', 'RV8', 'RV9', 'RV10', colnms)

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
  i <- wt <- 0
  if (q > 0 & q <= .5) {
    ascend <- order(x)
    a <- x[ascend]
    b <- w[ascend]
    if (k <= b[1]) {return(min(x))}
    while (wt < k) {
      i <- i + 1
      wt <- wt + b[i]
    }
    if (i == length(a)) {
      if (a[i] == a[i-1]) {i <- -1} else {return(a[i]-(wt-k)/b[i]*(a[i]-a[i-1]))}
    } else if (a[i] == a[i-1] | a[i] == a[i+1]) {
      i <- -1
    } else {return(a[i]-(wt-k)/b[i]*(a[i]-a[i-1]))}
  } else if (q > .5 & q < 1) { 
    descend <- order(x, decreasing = T)
    a <- x[descend]
    b <- w[descend]
    if (k <= b[length(b)]) {return(min(x))}
    wt <- sum(w)
    while (wt > k) {
      i <- i + 1
      wt <- wt - b[i]
    }
    if (i+1 == length(a)) {
      if (a[i+1] == a[i]) {i <- -1} else {return(a[i+1]+(k-wt)/b[i]*(a[i]-a[i+1]))}
    } else if (a[i+1] == a[i] | a[i+1] == a[i+2]) {
      i <- -1
    } else {return(a[i+1]+(k-wt)/b[i]*(a[i]-a[i+1]))}
  } else if (q == 0) {return(min(x))}
  
  if (i < 0) {
    d <- data.frame(x, w)
    W <- data.frame(table(d$x))
    X <- W[W$Freq > 1,]
    Y <- as.numeric(as.character(X$Var1))
    Z <- numeric(length(Y))
    for (i in 1:length(Y)) {Z[i] <- sum(d[d$x == Y[i],'w'])}
    d <- d[! d$x %in% Y,]
    d <- rbind(d, data.frame(x = Y, w = Z))
    
    i <- wt <- 0
    ascend <- order(d$x)
    a <- d$x[ascend]
    b <- d$w[ascend]
    if (k <= b[1]) {return(min(x))}
    while (wt < k) {
      i <- i + 1
      wt <- wt + b[i]
    }
    return(a[i]-(wt-k)/b[i]*(a[i]-a[i-1]))
  }
}
# sort the scores and weights in ascending order of scores (a and b)
# add weights until the fraction of total weight exceeds q
# perform linear interpolation between the closest scores
# for efficiency, reverse the process if q exceeds .5
# if needed, divert to an alternate algorithm by assigning -1 to i (impossible otherwise)
# this sums the weights of students with identical scores before proceeding
# it is a less efficient algorithm that remedies the slight distortion which occurs in
# rare cases when a[i] (if q <= .5) or a[i+1] (if q > .5) is one of multiple equal scores
# other weighted quantile functions using linear interpolation would produce a different
# result if, e.g., one simply duplicated all values and weights, which should not happen
# edge cases are tested for and addressed; one of the tests is necessary only
# because of the adjustments made to check whether the alternate algorithm is needed


# empirical cumulative distribution function
wt.ecdf <- function(x, w, p) {
  ascend <- order(x)
  a <- x[ascend]
  b <- w[ascend]
  n <- length(x)
  z <- length(a[a <= p])
  
  if (z == 0) {
    return(0)
  } else if (z+1 == n) {
    s <- (p-a[z]) / (a[z+1]-a[z]) * b[z+1]
    return((s + sum(b[1:z])) / sum(b))
  } else if (z == n) {return(1)}
  
  if (a[z+1] == a[z+2]) {
    d <- data.frame(a, b)
    wt <- sum(d[d$a == a[z+1],'b'])
  } else {wt <- b[z+1]}
  
  s <- (p-a[z]) / (a[z+1]-a[z]) * wt
  return((s + sum(b[1:z])) / sum(b))
}
# sort the scores and weights in ascending order of scores (a and b)
# find the number of scores less than or equal to p (z); their weight is sum(b[1:z])
# perform linear interpolation between the closest scores to find the additional weight (s)
# the fraction of total weight gives the weighted quantile of p
# the rare case in which two or more students with identical scores have
# the score immediately above p is addressed by summing their weights


# Cohen's d
dfn <- function(d1, d2, w, v) {
  (wt.mn(d1[,v], d1[,w]) - wt.mn(d2[,v], d2[,w]))/sqrt((wt.var(d1[,v], d1[,w]) + wt.var(d2[,v], d2[,w]))/2)
}
# raw mean difference divided by quadratic mean of standard deviations


# U3
U3fn <- function(d1, d2, w, v) {1 - wt.ecdf(d1[,v], d1[,w], wt.qnt(d2[,v], d2[,w], .5))}
# the precise share of male weight above the female median


# probability of superiority (PS)
PSfn <- function(d1, d2, w, v) {
  d1 <- d1[order(d1[,v]),]
  SuperM <- numeric(FSize)
  for (i in 1:FSize) {SuperM[i] <- sum(d1[which(d1[,v] > d2[i,v]),w])}
  
  equal <- intersect(d1[,v], d2[,v])
  EWtsM <- EWtsF <- numeric(length(equal))
  for (i in 1:length(equal)) {
    EWtsM[i] <- sum(d1[which(d1[,v] %in% equal[i]),w])
    EWtsF[i] <- sum(d2[which(d2[,v] %in% equal[i]),w])
  }
  
  return(sum(c(SuperM*d2[,w], .5*EWtsM*EWtsF)) / (sum(d1[,w])*sum(d2[,w])))
}
# for efficiency, sort male scores and weights in order of scores (d1)
# compute the male weight above each female score (SuperM)
# store the small subset of scores that are equal to a score in the other subgroup (equal)
# find the associated weights in males and females (EWtsM, EWtsF)
# sum weights of superior males multiplied by associated female weights
# and half of the sum of equal male and female weights multiplied
# then divide by the total weight of male-female pairs
# this gives the probability that a random male has a higher score than a random female


# log-transformed standard deviation ratio (LSDR)
LSDRfn <- function(d1, d2, w, v) {log(sqrt(wt.var(d1[,v], d1[,w]) / wt.var(d2[,v], d2[,w])))}
# M/F ratio of standard deviation
# log-transform the ratio for linear scale


# log-transformed tail SDR (LSDR_T)
LSDR_Tfn <- function(d1, d2, w, v, t) {
  q1 <- wt.mn(d1[,v], d1[,w])
  q2 <- wt.mn(d2[,v], d2[,w])
  if (t == 'L') {
    Mtail <- d1[which(d1[,v] < q1),]
    Ftail <- d2[which(d2[,v] < q2),]
  } else if (t == 'R') {
    Mtail <- d1[which(d1[,v] > q1),]
    Ftail <- d2[which(d2[,v] > q2),]
  }
  m <- sum(Mtail[,w]*(Mtail[,v]-q1)^2) / sum(Mtail[,w])
  f <- sum(Ftail[,w]*(Ftail[,v]-q2)^2) / sum(Ftail[,w])
  return(log(sqrt(m/f)))
}
# subset the males and females below (t = 'L') or above (t = 'R') the subgroup mean
# compute sqrt of the M/F ratio of mean squared deviation from the mean in the tail
# log-transform the ratio for linear scale
# Bessel's correction not applicable because in this case the sample mean's
# deviation from the population mean causes random not systematic error


# log-transformed median absolute deviation ratio (LMADR)
LMADRfn <- function(d1, d2, w, v) {
  log(wt.qnt(abs(d1[,v]-wt.qnt(d1[,v], d1[,w], .5)), d1[,w], .5)/
        wt.qnt(abs(d2[,v]-wt.qnt(d2[,v], d2[,w], .5)), d2[,w], .5))
}
# M/F ratio of median absolute deviation from the median
# log-transform the ratio for linear scale


# log-transformed tail MADR (LMADR_T)
LMADR_Tfn <- function(d1, d2, w, v, t) {
  q1 <- wt.qnt(d1[,v], d1[,w], .5)
  q2 <- wt.qnt(d2[,v], d2[,w], .5)
  if (t == 'L') {
    Mtail <- d1[which(d1[,v] < q1),]
    Ftail <- d2[which(d2[,v] < q2),]
    return(log(wt.qnt(q1-Mtail[,v], Mtail[,w], .5)/wt.qnt(q2-Ftail[,v], Ftail[,w], .5)))
  } else if (t == 'R') {
    Mtail <- d1[which(d1[,v] > q1),]
    Ftail <- d2[which(d2[,v] > q2),]
    return(log(wt.qnt(Mtail[,v]-q1, Mtail[,w], .5)/wt.qnt(Ftail[,v]-q2, Ftail[,w], .5)))
  }
}
# subset the males and females below (t = 'L') or above (t = 'R') the subgroup median
# compute the M/F ratio of mean absolute deviation from the median in the left or right tail
# log-transform the ratio for linear scale


# log-transformed Gini's mean difference ratio (LGMDR)
LGMDRfn <- function(d1, d2, w, v) {
  a1 <- sort(d1[,v])
  a2 <- sort(d2[,v])
  b1 <- d1[,w][order(d1[,v])]/sum(d1[,w])
  b2 <- d2[,w][order(d2[,v])]/sum(d2[,w])
  s1 <- cumsum(b1)
  s2 <- cumsum(b2)
  y1 <- cumsum(a1*b1)
  y2 <- cumsum(a2*b2)
  z1 <- y1/y1[MSize]
  z2 <- y2/y2[FSize]
  Gini1 <- as.numeric(t(z1[-1]) %*% s1[-MSize] - t(z1[-MSize]) %*% s1[-1])
  Gini2 <- as.numeric(t(z2[-1]) %*% s2[-FSize] - t(z2[-FSize]) %*% s2[-1])
  return(log(Gini1*wt.mn(d1[,v], d1[,w])/(Gini2*wt.mn(d2[,v], d2[,w]))))
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
# log-transform the ratio for linear scale


# log-transformed U3 ratio (LU3R)
LU3Rfn <- function(d1, d2, q, w, v) {
  m <- wt.ecdf(d1[,v], d1[,w], wt.qnt(d2[,v], d2[,w], q))
  if (q < .5) {return(log(m/q))} else {return(log((1-m)/(1-q)))}
}
# share of male weight below a female subgroup quantile (m)
# if left tail, find the ratio of weight share below the female quantile; if right tail, above it
# log-transform the ratio for linear scale


# log-transformed tail proportion ratio (LTPR)
LTPRfn <- function(d1, d2, q, w, v) {
  d <- rbind(d1, d2)
  k <- wt.qnt(d[,v], d[,w], q)
  m <- wt.ecdf(d1[,v], d1[,w], k)
  f <- wt.ecdf(d2[,v], d2[,w], k)
  if (q < .5) {return(log(m/f))} else {return(log((1-m)/(1-f)))}
}
# compute proportions of male and female weight below a threshold k (m and f)
# if left tail, find the TPR below k; if right tail, above k 
# log-transform the ratio for linear scale


# log-transformed median-aligned U3 ratio (LMU3R)
LMU3Rfn <- function(d1, d2, q, w, v) {
  d1[,v] <- wt.qnt(d2[,v], d2[,w], .5) - wt.qnt(d1[,v], d1[,w], .5) + d1[,v]
  m <- wt.ecdf(d1[,v], d1[,w], wt.qnt(d2[,v], d2[,w], q))
  if (q < .5) {return(log(m/q))} else {return(log((1-m)/(1-q)))}
}
# move the male median to the female median (arbitrary method of alignment)
# compute the LMU3R (see LU3R description)


# quantile difference (QD)
QDfn <- function(d1, d2, q, w, v) {wt.qnt(d1[,v], d1[,w], q) - wt.qnt(d2[,v], d2[,w], q)}
# raw quantile M-F difference


# mean MAD (MMAD)
MMADfn <- function(d1, d2, w, v) {
  (wt.qnt(abs(d1[,v]-wt.qnt(d1[,v], d1[,w], .5)), d1[,w], .5) +
     wt.qnt(abs(d2[,v]-wt.qnt(d2[,v], d2[,w], .5)), d2[,w], .5)) / 2
}
# mean of male and female MADs, computed once outside of loop for efficiency


# standardized quantile difference (SQD)
SQDfn <- function(d1, d2, q, w, v) {
  QD <- wt.qnt(d1[,v], d1[,w], q) - wt.qnt(d2[,v], d2[,w], q)
  m <- wt.qnt(abs(d1[,v]-wt.qnt(d1[,v], d1[,w], .5)), d1[,w], .5)
  f <- wt.qnt(abs(d2[,v]-wt.qnt(d2[,v], d2[,w], .5)), d2[,w], .5)
  return(QD / ((m+f)/2) * 100)
}
# raw quantile M-F difference as a percentage of mean MAD: QDfn / MMADfn * 100




########################
##### Effect Sizes #####
########################

### means and medians (MM/MR), some effect sizes (EM/ER)

MMs <- MRs <- numeric(60) # empty containers
MM <- MR <- numeric(8) # empty containers
EMs <- ERs <- numeric(100) # empty containers
EM <- ER <- numeric(10) # empty containers

for (i in 1:10) {
  MV <- paste0('MV',i) # math PV
  RV <- paste0('RV',i) # reading PV
  MMs[i] <- wt.mn(P18[,MV], P18$HWt) # math means (total)
  MRs[i] <- wt.mn(P18[,RV], P18$HWt) # reading means (total)
  MMs[i+10] <- wt.qnt(P18[,MV], P18$HWt, .5) # math medians (total)
  MRs[i+10] <- wt.qnt(P18[,RV], P18$HWt, .5) # reading medians (total)
  MMs[i+20] <- wt.mn(P18_F[,MV], P18_F$HWt) # math means (female)
  MRs[i+20] <- wt.mn(P18_F[,RV], P18_F$HWt) # reading means (female)
  MMs[i+30] <- wt.qnt(P18_F[,MV], P18_F$HWt, .5) # math medians (female)
  MRs[i+30] <- wt.qnt(P18_F[,RV], P18_F$HWt, .5) # reading medians (female)
  MMs[i+40] <- wt.mn(P18_M[,MV], P18_M$HWt) # math means (male)
  MRs[i+40] <- wt.mn(P18_M[,RV], P18_M$HWt) # reading means (male)
  MMs[i+50] <- wt.qnt(P18_M[,MV], P18_M$HWt, .5) # math medians (male)
  MRs[i+50] <- wt.qnt(P18_M[,RV], P18_M$HWt, .5) # reading medians (male)
  EMs[i] <- dfn(P18_M, P18_F, 'HWt', MV) # Cohen's ds (math)
  ERs[i] <- dfn(P18_M, P18_F, 'HWt', RV) # Cohen's ds (reading)
  EMs[i+10] <- U3fn(P18_M, P18_F, 'HWt', MV) # U3s (math)
  ERs[i+10] <- U3fn(P18_M, P18_F, 'HWt', RV) # U3s (reading)
  EMs[i+20] <- PSfn(P18_M, P18_F, 'HWt', MV) # PSs (math)
  ERs[i+20] <- PSfn(P18_M, P18_F, 'HWt', RV) # PSs (reading)
  EMs[i+30] <- LSDRfn(P18_M, P18_F, 'HWt', MV) # LSDRs (math)
  ERs[i+30] <- LSDRfn(P18_M, P18_F, 'HWt', RV) # LSDRs (reading)
  EMs[i+40] <- LSDR_Tfn(P18_M, P18_F, 'HWt', MV, 'L') # LSDR_Ls (math)
  ERs[i+40] <- LSDR_Tfn(P18_M, P18_F, 'HWt', RV, 'L') # LSDR_Ls (reading)
  EMs[i+50] <- LSDR_Tfn(P18_M, P18_F, 'HWt', MV, 'R') # LSDR_Rs (math)
  ERs[i+50] <- LSDR_Tfn(P18_M, P18_F, 'HWt', RV, 'R') # LSDR_Rs (reading)
  EMs[i+60] <- LMADRfn(P18_M, P18_F, 'HWt', MV) # LMADRs (math)
  ERs[i+60] <- LMADRfn(P18_M, P18_F, 'HWt', RV) # LMADRs (reading)
  EMs[i+70] <- LMADR_Tfn(P18_M, P18_F, 'HWt', MV, 'L') # LMADR_Ls (math)
  ERs[i+70] <- LMADR_Tfn(P18_M, P18_F, 'HWt', RV, 'L') # LMADR_Ls (reading)
  EMs[i+80] <- LMADR_Tfn(P18_M, P18_F, 'HWt', MV, 'R') # LMADR_Rs (math)
  ERs[i+80] <- LMADR_Tfn(P18_M, P18_F, 'HWt', RV, 'R') # LMADR_Rs (reading)
  EMs[i+90] <- LGMDRfn(P18_M, P18_F, 'HWt', MV) # LGMDRs (math)
  ERs[i+90] <- LGMDRfn(P18_M, P18_F, 'HWt', RV) # LGMDRs (reading)
}

MMs <- c(MMs, MMs[41:50]-MMs[21:30], MMs[51:60]-MMs[31:40]) # mean and median differences (math)
MRs <- c(MRs, MRs[41:50]-MRs[21:30], MRs[51:60]-MRs[31:40]) # mean and median differences (reading)

for (i in 1:8) {
  MM[i] <- mean(MMs[(10*i-9):(10*i)]) # means and medians (math)
  MR[i] <- mean(MRs[(10*i-9):(10*i)]) # means and medians (reading)
}
for (i in 1:10) {
  EM[i] <- mean(EMs[(10*i-9):(10*i)]) # effect sizes (math)
  ER[i] <- mean(ERs[(10*i-9):(10*i)]) # effect sizes (reading)
}



### U3 Ratios (U3Rs) and Tail Proportion Ratios (TPRs)

RMs <- RRs <- numeric(80) # empty containers
RM <- RR <- numeric(8) # empty containers
P <- c(.05, .1, .9, .95) # quantiles

for (i in 1:4) { # for percentiles 5, 10, 90, 95
  for (s in 1:10) { # for each PV
    RMs[s+(i-1)*10] <- LU3Rfn(P18_M, P18_F, P[i], 'HWt', paste0('MV',s)) # LU3Rs (math)
    RRs[s+(i-1)*10] <- LU3Rfn(P18_M, P18_F, P[i], 'HWt', paste0('RV',s)) # LU3Rs (reading)
    RMs[s+(i+3)*10] <- LTPRfn(P18_M, P18_F, P[i], 'HWt', paste0('MV',s)) # LTPRs (math)
    RRs[s+(i+3)*10] <- LTPRfn(P18_M, P18_F, P[i], 'HWt', paste0('RV',s)) # LTPRs (reading)
  }
  RM[i] <- mean(RMs[(10*i-9):(10*i)]) # LU3R for each percentile (math)
  RR[i] <- mean(RRs[(10*i-9):(10*i)]) # LU3R for each percentile (reading)
  RM[i+4] <- mean(RMs[(10*i+31):(10*i+40)]) # LTPR for each percentile (math)
  RR[i+4] <- mean(RRs[(10*i+31):(10*i+40)]) # LTPR for each percentile (reading)
}



### Median-aligned U3 Ratios (MU3Rs) and Standardized Quantile Differences (SQDs)

LMU3RMs <- LMU3RRs <- SQDMs <- SQDRs <- numeric(990) # empty containers
LMU3RM <- LMU3RR <- SQDM <- SQDR <- numeric(99) # empty containers
MMADMs <- MMADRs <- numeric(10) # empty containers

for (i in 1:10) { # mean MADs
  MMADMs[i] <- MMADfn(P18_M, P18_F, 'HWt', paste0('MV',i))
  MMADRs[i] <- MMADfn(P18_M, P18_F, 'HWt', paste0('RV',i))
}

for (i in 1:99) { # for each percentile
  for (s in 1:10) { # for each PV
    LMU3RMs[s+(i-1)*10] <- LMU3Rfn(P18_M, P18_F, i/100, 'HWt', paste0('MV',s)) # LMU3Rs (math)
    LMU3RRs[s+(i-1)*10] <- LMU3Rfn(P18_M, P18_F, i/100, 'HWt', paste0('RV',s)) # LMU3Rs (reading)
    SQDMs[s+(i-1)*10] <- QDfn(P18_M, P18_F, i/100, 'HWt', paste0('MV',s)) / MMADMs[s] * 100 # SQDs (math)
    SQDRs[s+(i-1)*10] <- QDfn(P18_M, P18_F, i/100, 'HWt', paste0('RV',s)) / MMADRs[s] * 100 # SQDs (reading)
  }
  LMU3RM[i] <- mean(LMU3RMs[(10*i-9):(10*i)]) # LMU3R for each percentile (math)
  LMU3RR[i] <- mean(LMU3RRs[(10*i-9):(10*i)]) # LMU3R for each percentile (reading)
  SQDM[i] <- mean(SQDMs[(10*i-9):(10*i)]) # SQD for each percentile (math)
  SQDR[i] <- mean(SQDRs[(10*i-9):(10*i)]) # SD for each percentile (reading)
}

# SQD tail-center shifts (SQDTCs): Med-5, Med-10, 90-Med, 95-Med
QMs <- c(SQDMs[491:500]-SQDMs[41:50], SQDMs[491:500]-SQDMs[91:100],
         SQDMs[891:900]-SQDMs[491:500], SQDMs[941:950]-SQDMs[491:500])
QRs <- c(SQDRs[491:500]-SQDRs[41:50], SQDRs[491:500]-SQDRs[91:100],
         SQDRs[891:900]-SQDRs[491:500], SQDRs[941:950]-SQDRs[491:500])
QM <- c(SQDM[50]-SQDM[5], SQDM[50]-SQDM[10], SQDM[90]-SQDM[50], SQDM[95]-SQDM[50]) # SQDTCs (math)
QR <- c(SQDR[50]-SQDR[5], SQDR[50]-SQDR[10], SQDR[90]-SQDR[50], SQDR[95]-SQDR[50]) # SQDTCs (reading)

# ratios for standard errors: all LU3Rs and LTPRs, LMU3Rs at 5, 10, 90, 95
RMs <- c(RMs, LMU3RMs[c(41:50, 91:100, 891:900, 941:950)])
RRs <- c(RRs, LMU3RRs[c(41:50, 91:100, 891:900, 941:950)])
RM <- c(RM, LMU3RM[c(5, 10, 90, 95)])
RR <- c(RR, LMU3RR[c(5, 10, 90, 95)])




##############################################
##### Some Effect Sizes Adjusted for Age #####
##############################################

A18 <- P18[!is.na(P18$Age),] # P18 with Age NAs (if any) excluded
A18_F <- A18[which(A18$Sex == 1),] # female subset
A18_M <- A18[which(A18$Sex == 2),] # male subset

AgeU3 <- U3fn(P18_M, P18_F, 'HWt', 'Age') # sex differences in age
AgeMADR <- exp(LMADRfn(P18_M, P18_F, 'HWt', 'Age'))

SlopeM <- SlopeR <- numeric(10) # control for the correlation between age and score
SM <- c(MV1~Age, MV2~Age, MV3~Age, MV4~Age, MV5~Age, MV6~Age, MV7~Age, MV8~Age, MV9~Age, MV10~Age)
SR <- c(RV1~Age, RV2~Age, RV3~Age, RV4~Age, RV5~Age, RV6~Age, RV7~Age, RV8~Age, RV9~Age, RV10~Age)
for (i in 1:10) {
  SlopeM[i] <- lm(formula = SM[[i]], data = A18, weights = HWt)$coefficients[2]
  SlopeR[i] <- lm(formula = SR[[i]], data = A18, weights = HWt)$coefficients[2]
}

AgeMn <- wt.mn(A18$Age, A18$HWt) # mean age

# if there are Age NAs, replace with mean age so their nominally age-corrected scores do not change
if (length(P18[is.na(P18$Age),'Age']) != 0) {P18[is.na(P18$Age),'Age'] <- AgeMn}

A18 <- data.frame(P18, # P18 with age-corrected scores
                  MV1A = P18$MV1+(AgeMn-P18$Age)*SlopeM[1],
                  MV2A = P18$MV2+(AgeMn-P18$Age)*SlopeM[2],
                  MV3A = P18$MV3+(AgeMn-P18$Age)*SlopeM[3],
                  MV4A = P18$MV4+(AgeMn-P18$Age)*SlopeM[4],
                  MV5A = P18$MV5+(AgeMn-P18$Age)*SlopeM[5],
                  MV6A = P18$MV6+(AgeMn-P18$Age)*SlopeM[6],
                  MV7A = P18$MV7+(AgeMn-P18$Age)*SlopeM[7],
                  MV8A = P18$MV8+(AgeMn-P18$Age)*SlopeM[8],
                  MV9A = P18$MV9+(AgeMn-P18$Age)*SlopeM[9],
                  MV10A = P18$MV10+(AgeMn-P18$Age)*SlopeM[10],
                  RV1A = P18$RV1+(AgeMn-P18$Age)*SlopeR[1],
                  RV2A = P18$RV2+(AgeMn-P18$Age)*SlopeR[2],
                  RV3A = P18$RV3+(AgeMn-P18$Age)*SlopeR[3],
                  RV4A = P18$RV4+(AgeMn-P18$Age)*SlopeR[4],
                  RV5A = P18$RV5+(AgeMn-P18$Age)*SlopeR[5],
                  RV6A = P18$RV6+(AgeMn-P18$Age)*SlopeR[6],
                  RV7A = P18$RV7+(AgeMn-P18$Age)*SlopeR[7],
                  RV8A = P18$RV8+(AgeMn-P18$Age)*SlopeR[8],
                  RV9A = P18$RV9+(AgeMn-P18$Age)*SlopeR[9],
                  RV10A = P18$RV10+(AgeMn-P18$Age)*SlopeR[10])
A18_F <- A18[which(A18$Sex == 1),] # female subset
A18_M <- A18[which(A18$Sex == 2),] # male subset

EMAs <- ERAs <- numeric(100) # empty containers
EMA <- ERA <- numeric(10) # empty containers

for (i in 1:10) {
  MV <- paste0('MV',i,'A') # age-adjusted math PV
  RV <- paste0('RV',i,'A') # age-adjusted reading PV
  EMAs[i] <- dfn(A18_M, A18_F, 'HWt', MV) # Cohen's ds (math)
  ERAs[i] <- dfn(A18_M, A18_F, 'HWt', RV) # Cohen's ds (reading)
  EMAs[i+10] <- U3fn(A18_M, A18_F, 'HWt', MV) # U3s (math)
  ERAs[i+10] <- U3fn(A18_M, A18_F, 'HWt', RV) # U3s (reading)
  EMAs[i+20] <- PSfn(A18_M, A18_F, 'HWt', MV) # PSs (math)
  ERAs[i+20] <- PSfn(A18_M, A18_F, 'HWt', RV) # PSs (reading)
  EMAs[i+30] <- LSDRfn(A18_M, A18_F, 'HWt', MV) # LSDRs (math)
  ERAs[i+30] <- LSDRfn(A18_M, A18_F, 'HWt', RV) # LSDRs (reading)
  EMAs[i+40] <- LSDR_Tfn(A18_M, A18_F, 'HWt', MV, 'L') # LSDR_Ls (math)
  ERAs[i+40] <- LSDR_Tfn(A18_M, A18_F, 'HWt', RV, 'L') # LSDR_Ls (reading)
  EMAs[i+50] <- LSDR_Tfn(A18_M, A18_F, 'HWt', MV, 'R') # LSDR_Rs (math)
  ERAs[i+50] <- LSDR_Tfn(A18_M, A18_F, 'HWt', RV, 'R') # LSDR_Rs (reading)
  EMAs[i+60] <- LMADRfn(A18_M, A18_F, 'HWt', MV) # LMADRs (math)
  ERAs[i+60] <- LMADRfn(A18_M, A18_F, 'HWt', RV) # LMADRs (reading)
  EMAs[i+70] <- LMADR_Tfn(A18_M, A18_F, 'HWt', MV, 'L') # LMADR_Ls (math)
  ERAs[i+70] <- LMADR_Tfn(A18_M, A18_F, 'HWt', RV, 'L') # LMADR_Ls (reading)
  EMAs[i+80] <- LMADR_Tfn(A18_M, A18_F, 'HWt', MV, 'R') # LMADR_Rs (math)
  ERAs[i+80] <- LMADR_Tfn(A18_M, A18_F, 'HWt', RV, 'R') # LMADR_Rs (reading)
  EMAs[i+90] <- LGMDRfn(A18_M, A18_F, 'HWt', MV) # LGMDRs (math)
  ERAs[i+90] <- LGMDRfn(A18_M, A18_F, 'HWt', RV) # LGMDRs (reading)
}

for (i in 1:10) {
  EMA[i] <- mean(EMAs[(10*i-9):(10*i)]) # age-adjusted effect sizes (math)
  ERA[i] <- mean(ERAs[(10*i-9):(10*i)]) # age-adjusted effect sizes (reading)
}




####################################################
##### Standard Errors and Confidence Intervals #####
####################################################

Y <- c(.05, .1, .5, .9, .95) # quantiles for SQDTCs

# empty containers
MMB <- MRB <- numeric(4800)
EMB <- ERB <- numeric(8000)
QMB <- QRB <- numeric(4000)
RMB <- RRB <- numeric(9600)
MMsum <- MRsum <- numeric(80)
EMsum <- ERsum <- numeric(100)
QMsum <- QRsum <- numeric(40)
RMsum <- RRsum <- numeric(120)
TVMM <- TVMR <- numeric(8)
TVEM <- TVER <- numeric(10)
TVQM <- TVQR <- numeric(4)
TVRM <- TVRR <- numeric(12)
MMCI <- MRCI <- numeric(16)
EMCI <- ERCI <- numeric(20)
QMCI <- QRCI <- numeric(8)
RMCI <- RRCI <- numeric(24)

# perform Fay's method of balanced repeated replication
for (i in 1:80) { # for each set of replicate weights
  W <- paste0('FW',i)
  for (s in 1:10) { # for each PV
    MV <- paste0('MV',s) # math PV
    RV <- paste0('RV',s) # reading PV
    
    MMB[i+(s-1)*80] <- wt.mn(P18[,MV], P18[,W]) # reweighted math means (total)
    MRB[i+(s-1)*80] <- wt.mn(P18[,RV], P18[,W]) # reweighted reading means (total)
    MMB[i+(s+9)*80] <- wt.qnt(P18[,MV], P18[,W], .5) # reweighted math medians (total)
    MRB[i+(s+9)*80] <- wt.qnt(P18[,RV], P18[,W], .5) # reweighted reading medians (total)
    MMB[i+(s+19)*80] <- wt.mn(P18_F[,MV], P18_F[,W]) # reweighted math means (female)
    MRB[i+(s+19)*80] <- wt.mn(P18_F[,RV], P18_F[,W]) # reweighted reading means (female)
    MMB[i+(s+29)*80] <- wt.qnt(P18_F[,MV], P18_F[,W], .5) # reweighted math medians (female)
    MRB[i+(s+29)*80] <- wt.qnt(P18_F[,RV], P18_F[,W], .5) # reweighted reading medians (female)
    MMB[i+(s+39)*80] <- wt.mn(P18_M[,MV], P18_M[,W]) # reweighted math means (male)
    MRB[i+(s+39)*80] <- wt.mn(P18_M[,RV], P18_M[,W]) # reweighted reading means (male)
    MMB[i+(s+49)*80] <- wt.qnt(P18_M[,MV], P18_M[,W], .5) # reweighted math medians (male)
    MRB[i+(s+49)*80] <- wt.qnt(P18_M[,RV], P18_M[,W], .5) # reweighted reading medians (male)
    
    EMB[i+(s-1)*80] <- dfn(P18_M, P18_F, W, MV) # reweighted Cohen's ds (math)
    ERB[i+(s-1)*80] <- dfn(P18_M, P18_F, W, RV) # reweighted Cohen's ds (reading)
    EMB[i+(s+9)*80] <- U3fn(P18_M, P18_F, W, MV) # reweighted U3s (math)
    ERB[i+(s+9)*80] <- U3fn(P18_M, P18_F, W, RV) # reweighted U3s (reading)
    EMB[i+(s+19)*80] <- PSfn(P18_M, P18_F, W, MV) # reweighted PSs (math)
    ERB[i+(s+19)*80] <- PSfn(P18_M, P18_F, W, RV) # reweighted PSs (reading)
    EMB[i+(s+29)*80] <- LSDRfn(P18_M, P18_F, W, MV) # reweighted LSDRs (math)
    ERB[i+(s+29)*80] <- LSDRfn(P18_M, P18_F, W, RV) # reweighted LSDRs (reading)
    EMB[i+(s+39)*80] <- LSDR_Tfn(P18_M, P18_F, W, MV, 'L') # reweighted LSDR_Ls (math)
    ERB[i+(s+39)*80] <- LSDR_Tfn(P18_M, P18_F, W, RV, 'L') # reweighted LSDR_Ls (reading)
    EMB[i+(s+49)*80] <- LSDR_Tfn(P18_M, P18_F, W, MV, 'R') # reweighted LSDR_Rs (math)
    ERB[i+(s+49)*80] <- LSDR_Tfn(P18_M, P18_F, W, RV, 'R') # reweighted LSDR_Rs (reading)
    EMB[i+(s+59)*80] <- LMADRfn(P18_M, P18_F, W, MV) # reweighted LMADRs (math)
    ERB[i+(s+59)*80] <- LMADRfn(P18_M, P18_F, W, RV) # reweighted LMADRs (reading)
    EMB[i+(s+69)*80] <- LMADR_Tfn(P18_M, P18_F, W, MV, 'L') # reweighted LMADR_Ls (math)
    ERB[i+(s+69)*80] <- LMADR_Tfn(P18_M, P18_F, W, RV, 'L') # reweighted LMADR_Ls (reading)
    EMB[i+(s+79)*80] <- LMADR_Tfn(P18_M, P18_F, W, MV, 'R') # reweighted LMADR_Rs (math)
    ERB[i+(s+79)*80] <- LMADR_Tfn(P18_M, P18_F, W, RV, 'R') # reweighted LMADR_Rs (reading)
    EMB[i+(s+89)*80] <- LGMDRfn(P18_M, P18_F, W, MV) # reweighted LGMDRs (math)
    ERB[i+(s+89)*80] <- LGMDRfn(P18_M, P18_F, W, RV) # reweighted LGMDRs (reading)
    
    for (c in 1:5) {
      QMB[i+(s+10*c-11)*80] <- SQDfn(P18_M, P18_F, Y[c], W, MV) # reweighted SQDs (math)
      QRB[i+(s+10*c-11)*80] <- SQDfn(P18_M, P18_F, Y[c], W, RV) # reweighted SQDs (reading)
    }
    
    for (c in 1:4) {
      RMB[i+(s+10*c-11)*80] <- LU3Rfn(P18_M, P18_F, P[c], W, MV) # reweighted LU3Rs (math)
      RRB[i+(s+10*c-11)*80] <- LU3Rfn(P18_M, P18_F, P[c], W, RV) # reweighted LU3Rs (reading)
      RMB[i+(s+10*c+29)*80] <- LTPRfn(P18_M, P18_F, P[c], W, MV) # reweighted LTPRs (math)
      RRB[i+(s+10*c+29)*80] <- LTPRfn(P18_M, P18_F, P[c], W, RV) # reweighted LTPRs (reading)
      RMB[i+(s+10*c+69)*80] <- LMU3Rfn(P18_M, P18_F, P[c], W, MV) # reweighted LMU3Rs (math)
      RRB[i+(s+10*c+69)*80] <- LMU3Rfn(P18_M, P18_F, P[c], W, RV) # reweighted LMU3Rs (reading)
    }
  }
  print(paste0(i, '/', 80, ' at ', Sys.time()), quote = F) # print updates
}

# append reweighted mean and median differences to MMB/MRB
MMB <- c(MMB, MMB[3201:4000]-MMB[1601:2400], MMB[4001:4800]-MMB[2401:3200])
MRB <- c(MRB, MRB[3201:4000]-MRB[1601:2400], MRB[4001:4800]-MRB[2401:3200])

# replace reweighted SQDs in QMB/QRB with reweighted SQDTCs
QMB <- c(QMB[1601:2400]-QMB[1:800], QMB[1601:2400]-QMB[801:1600],
         QMB[2401:3200]-QMB[1601:2400], QMB[3201:4000]-QMB[1601:2400])
QRB <- c(QRB[1601:2400]-QRB[1:800], QRB[1601:2400]-QRB[801:1600],
         QRB[2401:3200]-QRB[1601:2400], QRB[3201:4000]-QRB[1601:2400])

CI95 <- qnorm(.975) # ratio of 95% CIs to SEs

# total variance = sampling variance + imputation variance
for (i in 1:12) { # for each effect size, up to the appropriate i
  for (s in 1:10) { # for each PV
    if (i < 9) {
      MMsum[s+(i-1)*10] <- sum((MMB[((s+10*i-11)*80+1):((s+(i-1)*10)*80)]-MMs[s+(i-1)*10])^2)
      MRsum[s+(i-1)*10] <- sum((MRB[((s+10*i-11)*80+1):((s+(i-1)*10)*80)]-MRs[s+(i-1)*10])^2)
    }
    if (i < 11) {
      EMsum[s+(i-1)*10] <- sum((EMB[((s+10*i-11)*80+1):((s+(i-1)*10)*80)]-EMs[s+(i-1)*10])^2)
      ERsum[s+(i-1)*10] <- sum((ERB[((s+10*i-11)*80+1):((s+(i-1)*10)*80)]-ERs[s+(i-1)*10])^2)
    }
    if (i < 5) {
      QMsum[s+(i-1)*10] <- sum((QMB[((s+10*i-11)*80+1):((s+(i-1)*10)*80)]-QMs[s+(i-1)*10])^2)
      QRsum[s+(i-1)*10] <- sum((QRB[((s+10*i-11)*80+1):((s+(i-1)*10)*80)]-QRs[s+(i-1)*10])^2)
    }
    RMsum[s+(i-1)*10] <- sum((RMB[((s+10*i-11)*80+1):((s+(i-1)*10)*80)]-RMs[s+(i-1)*10])^2)
    RRsum[s+(i-1)*10] <- sum((RRB[((s+10*i-11)*80+1):((s+(i-1)*10)*80)]-RRs[s+(i-1)*10])^2)
  }
  if (i < 9) { # total variance and 95% CI bounds of means and medians
    TVMM[i] <- mean(MMsum[(10*i-9):(10*i)])/20 + 11/90*sum((MMs[(10*i-9):(10*i)]-MM[i])^2)
    TVMR[i] <- mean(MRsum[(10*i-9):(10*i)])/20 + 11/90*sum((MRs[(10*i-9):(10*i)]-MR[i])^2)
    MMCI[2*i-1] <- MM[i]-sqrt(TVMM[i])*CI95
    MRCI[2*i-1] <- MR[i]-sqrt(TVMR[i])*CI95
    MMCI[2*i] <- MM[i]+sqrt(TVMM[i])*CI95
    MRCI[2*i] <- MR[i]+sqrt(TVMR[i])*CI95
  }
  if (i < 11) { # total variance and 95% CI bounds of some effect sizes
    TVEM[i] <- mean(EMsum[(10*i-9):(10*i)])/20 + 11/90*sum((EMs[(10*i-9):(10*i)]-EM[i])^2)
    TVER[i] <- mean(ERsum[(10*i-9):(10*i)])/20 + 11/90*sum((ERs[(10*i-9):(10*i)]-ER[i])^2)
    EMCI[2*i-1] <- EM[i]-sqrt(TVEM[i])*CI95
    ERCI[2*i-1] <- ER[i]-sqrt(TVER[i])*CI95
    EMCI[2*i] <- EM[i]+sqrt(TVEM[i])*CI95
    ERCI[2*i] <- ER[i]+sqrt(TVER[i])*CI95
  }
  if (i < 5) { # total variance and 95% CI bounds of SQDTCs
    TVQM[i] <- mean(QMsum[(10*i-9):(10*i)])/20 + 11/90*sum((QMs[(10*i-9):(10*i)]-QM[i])^2)
    TVQR[i] <- mean(QRsum[(10*i-9):(10*i)])/20 + 11/90*sum((QRs[(10*i-9):(10*i)]-QR[i])^2)
    QMCI[2*i-1] <- QM[i]-sqrt(TVQM[i])*CI95
    QRCI[2*i-1] <- QR[i]-sqrt(TVQR[i])*CI95
    QMCI[2*i] <- QM[i]+sqrt(TVQM[i])*CI95
    QRCI[2*i] <- QR[i]+sqrt(TVQR[i])*CI95
  }            # total variance and 95% CI bounds of U3Rs, TPRs, MU3Rs
  TVRM[i] <- mean(RMsum[(10*i-9):(10*i)])/20 + 11/90*sum((RMs[(10*i-9):(10*i)]-RM[i])^2)
  TVRR[i] <- mean(RRsum[(10*i-9):(10*i)])/20 + 11/90*sum((RRs[(10*i-9):(10*i)]-RR[i])^2)
  RMCI[2*i-1] <- RM[i]-sqrt(TVRM[i])*CI95
  RRCI[2*i-1] <- RR[i]-sqrt(TVRR[i])*CI95
  RMCI[2*i] <- RM[i]+sqrt(TVRM[i])*CI95
  RRCI[2*i] <- RR[i]+sqrt(TVRR[i])*CI95
}




##################
##### Output #####
##################

WtRatio <- sum(P18_M$HWt)/sum(P18_F$HWt) # M/F weight ratio

MMADM <- mean(MMADMs) # mean MAD (math)
MMADR <- mean(MMADRs) # mean MAD (reading)

LMU3Rnm <- SQDnm <- numeric(99)
for (i in 1:99) { # compressed names for LMU3Rs and SQDs
  LMU3Rnm[i] <- paste0('LMU3R',i)
  SQDnm[i] <- paste0('SQD',i)
}

# summary tables
Names <- c('Country', 'CNT', 'Size', 'FSize', 'MSize', 'M/F Wt Ratio',
           'Mean', 'Median', 'F Mean', 'F Median', 'M Mean', 'M Median', 'Mean Diff', 'Median Diff', 
           'd', 'U3', 'PS', 'SDR', 'SDR_L', 'SDR_R', 'MADR', 'MADR_L', 'MADR_R', 'GMDR',
           'U3R05', 'U3R10', 'U3R90', 'U3R95', 'TPR05', 'TPR10', 'TPR90', 'TPR95', 'MU3R05', 'MU3R10',
           'MU3R90', 'MU3R95', 'MMAD', 'SQDTC05', 'SQDTC10', 'SQDTC90', 'SQDTC95', LMU3Rnm, SQDnm,
           'Mean Low', 'Mean Upp', 'Median Low', 'Median Upp', 'F Mean Low', 'F Mean Upp', 'F Median Low',
           'F Median Upp', 'M Mean Low', 'M Mean Upp', 'M Median Low', 'M Median Upp', 'Mean Diff Low',
           'Mean Diff Upp', 'Med Diff Low', 'Med Diff Upp', 'd Low', 'd Upp', 'U3 Low', 'U3 Upp',
           'PS Low', 'PS Upp', 'SDR Low', 'SDR Upp', 'SDR_L Low', 'SDR_L Upp', 'SDR_R Low', 'SDR_R Upp',
           'MADR Low', 'MADR Upp', 'MADR_L Low', 'MADR_L Upp', 'MADR_R Low', 'MADR_R Upp', 'GMDR Low',
           'GMDR Upp', 'SQDTC05 Low', 'SQDTC05 Upp', 'SQDTC10 Low', 'SQDTC10 Upp', 'SQDTC90 Low',
           'SQDTC90 Upp', 'SQDTC95 Low', 'SQDTC95 Upp', 'U3R05 Low', 'U3R05 Upp', 'U3R10 Low', 'U3R10 Upp',
           'U3R90 Low', 'U3R90 Upp', 'U3R95 Low', 'U3R95 Upp', 'TPR05 Low', 'TPR05 Upp', 'TPR10 Low',
           'TPR10 Upp', 'TPR90 Low', 'TPR90 Upp', 'TPR95 Low', 'TPR95 Upp', 'MU3R05 Low', 'MU3R05 Upp',
           'MU3R10 Low', 'MU3R10 Upp', 'MU3R90 Low', 'MU3R90 Upp', 'MU3R95 Low', 'MU3R95 Upp', 'LSDR',
           'LSDR_L', 'LSDR_R', 'LMADR', 'LMADR_L', 'LMADR_R', 'LGMDR', 'LU3R05', 'LU3R10', 'LU3R90',
           'LU3R95', 'LTPR05', 'LTPR10', 'LTPR90', 'LTPR95', 'd TV', 'U3 TV', 'PS TV', 'LSDR TV',
           'LSDR_L TV', 'LSDR_R TV', 'LMADR TV', 'LMADR_L TV', 'LMADR_R TV', 'LGMDR TV', 'SQDTC05 TV',
           'SQDTC10 TV', 'SQDTC90 TV', 'SQDTC95 TV', 'LU3R05 TV', 'LU3R10 TV', 'LU3R90 TV', 'LU3R95 TV',
           'LTPR05 TV', 'LTPR10 TV', 'LTPR90 TV', 'LTPR95 TV', 'LMU3R05 TV', 'LMU3R10 TV', 'LMU3R90 TV',
           'LMU3R95 TV', 'Age U3', 'Age MADR', 'd A', 'U3 A', 'PS A', 'SDR A', 'SDR_L A', 'SDR_R A',
           'MADR A', 'MADR_L A', 'MADR_R A', 'GMDR A')

MVars <- c(Country, CT, Size, FSize, MSize, WtRatio, MM, EM[1:3], exp(c(EM[4:10], RM)), MMADM, QM, LMU3RM,
           SQDM, MMCI, EMCI[1:6], exp(EMCI[7:20]), QMCI, exp(RMCI), EM[4:10], RM[1:8], TVEM, TVQM, TVRM,
           AgeU3, AgeMADR, EMA[1:3], exp(EMA[4:10]))

RVars <- c(Country, CT, Size, FSize, MSize, WtRatio, MR, ER[1:3], exp(c(ER[4:10], RR)), MMADR, QR, LMU3RR,
           SQDR, MRCI, ERCI[1:6], exp(ERCI[7:20]), QRCI, exp(RRCI), ER[4:10], RR[1:8], TVER, TVQR, TVRR,
           AgeU3, AgeMADR, ERA[1:3], exp(ERA[4:10]))

OutputM <- format(data.frame(Names, MVars), scientific = F) # put math in this
OutputR <- format(data.frame(Names, RVars), scientific = F) # put reading in this

# select file names manually, store as a csv
write.csv(x = OutputM, file = paste0('PISA output/Math/2018/Countries/', CT, '.csv'))
write.csv(x = OutputR, file = paste0('PISA output/Reading/2018/Countries/', CT, '.csv'))




















