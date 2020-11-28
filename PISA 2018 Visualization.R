#########################################################
################ PISA 2018 Visualization ################
#########################################################

devtools::install_github('zeehio/facetscales') # install if needed
library(facetscales) # individually adjusted facet axes
library(ggplot2) # plotting beautifully
library(scales) # transparent colors

# download data from Github
P18M <- read.csv(url('https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Full%20output%20(math).csv'))[,-1]
P18R <- read.csv(url('https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Full%20output%20(reading).csv'))[,-1]

p <- numeric(9)
for (i in 1:9) {p[i] <- ceiling(74*i/10)} # ranks above or equal to each decile


### Central tendency differences ###

# math
cap <- paste0('Three measures of sex differences in central tendency are ',
              'plotted for nine countries: U3, probability of superiority ',
              '(PS), and Cohen\'s d. For \ncomparison between different scales, ',
              'faint gray marks are plotted indicating where the other ',
              'measures would convert given assumptions \nof normality and ',
              'homoscedasticity. Countries were drawn from a total of 74 to ',
              'represent each decile of the average of the three measures.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- mean(c(P18M[14,i+1],
                                  pnorm(0, qnorm(P18M[15,i+1])*sqrt(2), lower.tail = F),
                                  pnorm(0, P18M[13,i+1], lower.tail = F)))}
names(mns) <- names(P18M)[-1]

scores <- numeric(81)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[9*i-8] <- P18M[14,nm] # U3
  scores[9*i-4] <- P18M[15,nm] # PS
  scores[9*i] <- P18M[13,nm] # d
  
  scores[9*i-7] <- pnorm(0, qnorm(scores[9*i-4])*sqrt(2), lower.tail = F) # PS shadow of U3
  scores[9*i-6] <- pnorm(0, scores[9*i], lower.tail = F) # d shadow of U3
  
  scores[9*i-5] <- pnorm(qnorm(scores[9*i-8])/sqrt(2)) # U3 shadow of PS
  scores[9*i-3] <- pnorm(scores[9*i]/sqrt(2)) # d shadow of PS
  
  scores[9*i-2] <- qnorm(scores[9*i-8]) # U3 shadow of d
  scores[9*i-1] <- qnorm(scores[9*i-4])*sqrt(2) # PS shadow of d
}

# change U3 and PS to percentages, for easier reading
scores[-c(7:9, 16:18, 25:27, 34:36, 43:45, 52:54, 61:63, 70:72, 79:81)] <-
  100*scores[-c(7:9, 16:18, 25:27, 34:36, 43:45, 52:54, 61:63, 70:72, 79:81)]

CNT <- c('UAE', 'Norway', 'Sweden', 'Netherlands', 'Romania',
         'France', 'Latvia', 'New Zealand', 'Austria')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 9),
                                    levels = CNT),
                   ratio = factor(rep(c('U3','PS','d'), each = 3, 9),
                                  levels = c('U3','PS','d')))

yscl <- list(
  U3 = scale_y_continuous(limits = c(46.1318, 55.7), breaks = seq(46, 54, 4),
                          minor_breaks = 46:56, labels = c('46%', '50%', '54%')),
  PS = scale_y_continuous(limits = c(47.2626, 54.0374), breaks = seq(48, 54, 2),
                          labels = c('48%', '50%', '52%', '54%')),
  d = scale_y_continuous(limits = c(-.097114, .143367), breaks = seq(-.1, .1, .1))
)

gray3 <- rep(alpha('gray', .4), 3) # color for shadows

tiff(filename = 'Central tendency (math).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

# the facetscales package enables control over all axes individually
# with ggplot alone, the line would be: facet_grid(ratio ~ country, scales = 'free_y')
ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', gray3, '#FF6C00', gray3, '#8000FF'), 9),
             shape = 1, size = 5) +
  facet_grid_sc(rows = vars(ratio), cols = vars(country), scales = list(y = yscl)) +
  ggtitle('Sex Differences in Central Tendency in PISA 2018 Math — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        strip.text.y.right = element_text(angle = 0),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = NULL, caption = cap)

dev.off() # write image to working directory


# reading
cap <- paste0('Three measures of sex differences in central tendency are ',
              'plotted for nine countries: U3, probability of superiority ',
              '(PS), and Cohen\'s d. For \ncomparison between different scales, ',
              'faint gray marks are plotted indicating where the other ',
              'measures would convert given assumptions \nof normality and ',
              'homoscedasticity. Countries were drawn from a total of 74 to ',
              'represent each decile of the average of the three measures.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- mean(c(P18R[14,i+1],
                                  pnorm(0, qnorm(P18R[15,i+1])*sqrt(2), lower.tail = F),
                                  pnorm(0, P18R[13,i+1], lower.tail = F)))}
names(mns) <- names(P18R)[-1]

scores <- numeric(81)
for (i in 1:9) {
  nm <- names(sort(mns, decreasing = T)[p[i]])
  scores[9*i-8] <- P18R[14,nm] # U3
  scores[9*i-4] <- P18R[15,nm] # PS
  scores[9*i] <- P18R[13,nm] # d
  
  scores[9*i-7] <- pnorm(0, qnorm(scores[9*i-4])*sqrt(2), lower.tail = F) # PS shadow of U3
  scores[9*i-6] <- pnorm(0, scores[9*i], lower.tail = F) # d shadow of U3
  
  scores[9*i-5] <- pnorm(qnorm(scores[9*i-8])/sqrt(2)) # U3 shadow of PS
  scores[9*i-3] <- pnorm(scores[9*i]/sqrt(2)) # d shadow of PS
  
  scores[9*i-2] <- qnorm(scores[9*i-8]) # U3 shadow of d
  scores[9*i-1] <- qnorm(scores[9*i-4])*sqrt(2) # PS shadow of d
}

# change U3 and PS to percentages, for easier reading
scores[-c(7:9, 16:18, 25:27, 34:36, 43:45, 52:54, 61:63, 70:72, 79:81)] <-
  100*scores[-c(7:9, 16:18, 25:27, 34:36, 43:45, 52:54, 61:63, 70:72, 79:81)]

CNT <- c('Japan', 'Germany', 'Vietnam', 'Spain', 'Brunei',
         'Romania', 'Serbia', 'Lithuania', 'Albania')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 9),
                                    levels = CNT),
                   ratio = factor(rep(c('U3','PS','d'), each = 3, 9),
                                  levels = c('U3','PS','d')))

yscl <- list(
  U3 = scale_y_continuous(limits = c(30.8, 43.28206), breaks = seq(31, 43, 4),
                          minor_breaks = 31:43, labels = c('31%', '35%', '39%', '43%')),
  PS = scale_y_continuous(limits = c(36.1432, 45.2384), breaks = seq(36, 44, 4),
                          minor_breaks = 36:45, labels = c('36%', '40%', '44%')),
  d = scale_y_continuous(limits = c(-.501527, -.169198), breaks = seq(-.5, -.2, .1))
)

gray3 <- rep(alpha('gray', .4), 3) # color for shadows

tiff(filename = 'Central tendency (reading).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

# the facetscales package enables control over all axes individually
# with ggplot alone, the line would be: facet_grid(ratio ~ country, scales = 'free_y')
ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', gray3, '#FF6C00', gray3, '#8000FF'), 9),
             shape = 1, size = 5) +
  facet_grid_sc(rows = vars(ratio), cols = vars(country), scales = list(y = yscl)) +
  ggtitle('Sex Differences in Central Tendency in PISA 2018 Reading — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        strip.text.y.right = element_text(angle = 0),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = NULL, caption = cap)

dev.off() # write image to working directory



### Tail differences ###

# left tail math
cap <- paste0('Two measures of sex ratios below the 5th percentile of scores ',
              'are plotted for nine countries: U3 ratios (U3Rs) and tail proportion',
              '\nratios (TPRs). U3Rs compute the male-female proportion ratio below ',
              'the female subgroup\'s 5th percentile, whereas TPRs do so for \nthe ',
              'total group\'s 5th percentile. Countries were drawn from a total ',
              'of 74 to represent each decile of the average of U3R and TPR values.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18M[23,i+1]), log(P18M[27,i+1]))))}
names(mns) <- names(P18M)[-1]

scores <- numeric(18)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[2*i-1] <- P18M[23,nm] # U3R05
  scores[2*i] <- P18M[27,nm] # TPR05
}

CNT <- c('Slovakia', 'Luxembourg', 'Estonia', 'Montenegro',
         'South Korea', 'Australia', 'Albania', 'Moldova', 'Thailand')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 2), levels = CNT),
                   ratio = factor(rep(c('U3R','TPR'), 9), levels = c('U3R','TPR')))

tiff(filename = 'Left tail (math).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('U3Rs and TPRs below the 5th percentile in PISA 2018 Math — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Tail Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = .9, to = 1.8, by = .1),
                     limits = c(.86, 1.78), trans = 'log')

dev.off() # write image to working directory


# left tail reading
cap <- paste0('Two measures of sex ratios below the 5th percentile of scores ',
              'are plotted for nine countries: U3 ratios (U3Rs) and tail proportion',
              '\nratios (TPRs). U3Rs compute the male-female proportion ratio below ',
              'the female subgroup\'s 5th percentile, whereas TPRs do so for \nthe ',
              'total group\'s 5th percentile. Countries were drawn from a total ',
              'of 74 to represent each decile of the average of U3R and TPR values.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18R[23,i+1]), log(P18R[27,i+1]))))}
names(mns) <- names(P18R)[-1]

scores <- numeric(18)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[2*i-1] <- P18R[23,nm] # U3R05
  scores[2*i] <- P18R[27,nm] # TPR05
}

CNT <- c('Colombia', 'Slovakia', 'Romania', 'Bosnia', 'Domin. Rep.', 
         'Switzerland', 'Serbia', 'Greece', 'Norway')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 2), levels = CNT),
                   ratio = factor(rep(c('U3R','TPR'), 9), levels = c('U3R','TPR')))

tiff(filename = 'Left tail (reading).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('U3Rs and TPRs below the 5th percentile in PISA 2018 Reading — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Tail Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = 1, to = 4.5, by = .5),
                     limits = c(1.04, 4.3), trans = 'log')

dev.off() # write image to working directory


# right tail math
cap <- paste0('Two measures of sex ratios above the 95th percentile of scores ',
              'are plotted for nine countries: U3 ratios (U3Rs) and tail proportion',
              '\nratios (TPRs). U3Rs compute the male-female proportion ratio above ',
              'the female subgroup\'s 95th percentile, whereas TPRs do so for \nthe ',
              'total group\'s 95th percentile. Countries were drawn from a total ',
              'of 74 to represent each decile of the average of U3R and TPR values.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18M[26,i+1]), log(P18M[30,i+1]))))}
names(mns) <- names(P18M)[-1]

scores <- numeric(18)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[2*i-1] <- P18M[26,nm] # U3R95
  scores[2*i] <- P18M[30,nm] # TPR95
}

CNT <- c('Albania', 'Georgia', 'Switzerland', 'Lithuania',
         'South Korea', 'Romania', 'Uruguay', 'Brazil', 'Kosovo')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 2), levels = CNT),
                   ratio = factor(rep(c('U3R','TPR'), 9), levels = c('U3R','TPR')))

tiff(filename = 'Right tail (math).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('U3Rs and TPRs above the 95th percentile in PISA 2018 Math — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Tail Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = 1, to = 1.8, by = .1),
                     limits = c(1.005, 1.79), trans = 'log')

dev.off() # write image to working directory


# right tail reading
cap <- paste0('Two measures of sex ratios above the 95th percentile of scores ',
              'are plotted for nine countries: U3 ratios (U3Rs) and tail proportion',
              '\nratios (TPRs). U3Rs compute the male-female proportion ratio above ',
              'the female subgroup\'s 95th percentile, whereas TPRs do so for \nthe ',
              'total group\'s 95th percentile. Countries were drawn from a total ',
              'of 74 to represent each decile of the average of U3R and TPR values.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18R[26,i+1]), log(P18R[30,i+1]))))}
names(mns) <- names(P18R)[-1]

scores <- numeric(18)
for (i in 1:9) {
  nm <- names(sort(mns, decreasing = T)[p[i]])
  scores[2*i-1] <- P18R[26,nm] # U3R95
  scores[2*i] <- P18R[30,nm] # TPR95
}

CNT <- c('France', 'Japan', 'Kosovo', 'Lebanon', 'Sweden',
         'Montenegro', 'Spain', 'Poland', 'Moldova')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 2), levels = CNT),
                   ratio = factor(rep(c('U3R','TPR'), 9), levels = c('U3R','TPR')))

tiff(filename = 'Right tail (reading).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('U3Rs and TPRs above the 95th percentile in PISA 2018 Reading — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Tail Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = .4, to = 1, by = .1),
                     limits = c(.412, .97), trans = 'log')

dev.off() # write image to working directory



### Variability differences ###

# total variability math
cap <- paste0('Three measures of male-female ratios in variability are plotted for nine ',
              'countries: median absolute deviation ratio (MADR), Gini\'s mean ',
              'difference \nratio (GMDR), and standard deviation ratio (SDR). ',
              'Countries were drawn from a total of 74 to represent each decile of ',
              'the average of the three ratios.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18M[16,i+1]), log(P18M[19,i+1]), log(P18M[22,i+1]))))}
names(mns) <- names(P18M)[-1]

scores <- numeric(27)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[3*i-2] <- P18M[19,nm] # MADR
  scores[3*i-1] <- P18M[22,nm] # GMDR
  scores[3*i] <- P18M[16,nm] # SDR
}

CNT <- c('Turkey', 'Thailand', 'Russia', 'Latvia', 'France',
         'New Zealand', 'Estonia', 'Italy', 'Kosovo')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 3), levels = CNT),
                   ratio = factor(rep(c('MADR','GMDR','SDR'), 9),
                                  levels = c('MADR','GMDR','SDR')))

tiff(filename = 'Variability (math).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#FF6C00', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('Sex Differences in Variability in PISA 2018 Math — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Variability Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = 1, to = 1.14, by = .02),
                     limits = c(1.002, 1.138), trans = 'log')

dev.off() # write image to working directory


# total variability reading
cap <- paste0('Three measures of male-female ratios in variability are plotted for nine ',
              'countries: median absolute deviation ratio (MADR), Gini\'s mean ',
              'difference \nratio (GMDR), and standard deviation ratio (SDR). ',
              'Countries were drawn from a total of 74 to represent each decile of ',
              'the average of the three ratios.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18R[16,i+1]), log(P18R[19,i+1]), log(P18R[22,i+1]))))}
names(mns) <- names(P18R)[-1]

scores <- numeric(27)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[3*i-2] <- P18R[19,nm] # MADR
  scores[3*i-1] <- P18R[22,nm] # GMDR
  scores[3*i] <- P18R[16,nm] # SDR
}

CNT <- c('Hungary', 'Brazil', 'Romania', 'Kazakhstan',
         'Italy', 'Sweden', 'Vietnam', 'Croatia', 'Iceland')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 3), levels = CNT),
                   ratio = factor(rep(c('MADR','GMDR','SDR'), 9),
                                  levels = c('MADR','GMDR','SDR')))

tiff(filename = 'Variability (reading).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#FF6C00', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('Sex Differences in Variability in PISA 2018 Reading — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Variability Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = 1, to = 1.16, by = .02),
                     limits = c(1.0015, 1.158), trans = 'log')

dev.off() # write image to working directory


# left tail variability math
cap <- paste0('Two measures of male-female ratios in left tail variability are ',
              'plotted for nine countries: median absolute deviation ratio (MADR) ',
              'and standard deviation \nratio (SDR), computed in the left half of the ',
              'distribution. Countries were drawn from a total of 74 to represent ',
              'each decile of the average of the two ratios.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18M[17,i+1]), log(P18M[20,i+1]))))}
names(mns) <- names(P18M)[-1]

scores <- numeric(18)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[2*i-1] <- P18M[17,nm] # SDR_L
  scores[2*i] <- P18M[20,nm] # MADR_L
}

CNT <- c('Thailand', 'Sweden', 'Bulgaria', 'Spain', 'Germany', 
         'Belarus', 'Greece', 'Colombia', 'Japan')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 2), levels = CNT),
                   ratio = factor(rep(c('MADR','SDR'), 9), levels = c('MADR','SDR')))

tiff(filename = 'Variability (math, left tail).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('Sex Differences in Left Tail Variability in PISA 2018 Math — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Left Tail Variability Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = 1, to = 1.14, by = .02),
                     limits = c(1.002, 1.1382), trans = 'log')

dev.off() # write image to working directory


# left tail variability reading
cap <- paste0('Two measures of male-female ratios in left tail variability are ',
              'plotted for nine countries: median absolute deviation ratio (MADR) ',
              'and standard deviation \nratio (SDR), computed in the left half of the ',
              'distribution. Countries were drawn from a total of 74 to represent ',
              'each decile of the average of the two ratios.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18R[17,i+1]), log(P18R[20,i+1]))))}
names(mns) <- names(P18R)[-1]

scores <- numeric(18)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[2*i-1] <- P18R[17,nm] # SDR_L
  scores[2*i] <- P18R[20,nm] # MADR_L
}

CNT <- c('Morocco', 'Hungary', 'Kazakhstan', 'Germany',
         'Montenegro', 'France', 'Croatia', 'Taiwan', 'South Korea')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 2), levels = CNT),
                   ratio = factor(rep(c('MADR','SDR'), 9), levels = c('MADR','SDR')))

tiff(filename = 'Variability (reading, left tail).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('Sex Differences in Left Tail Variability in PISA 2018 Reading — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Left Tail Variability Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = 1, to = 1.16, by = .04),
                     limits = c(.996, 1.167), trans = 'log')

dev.off() # write image to working directory


# right tail variability math
cap <- paste0('Two measures of male-female ratios in right tail variability are ',
              'plotted for nine countries: median absolute deviation ratio (MADR) ',
              'and standard deviation \nratio (SDR), computed in the right half of the ',
              'distribution. Countries were drawn from a total of 74 to represent ',
              'each decile of the average of the two ratios.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18M[18,i+1]), log(P18M[21,i+1]))))}
names(mns) <- names(P18M)[-1]

scores <- numeric(18)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[2*i-1] <- P18M[18,nm] # SDR_R
  scores[2*i] <- P18M[21,nm] # MADR_R
}

CNT <- c('United States', 'Slovenia', 'Singapore', 'Germany',
         'Argentina', 'Bosnia', 'Moldova', 'Philippines', 'Finland')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 2), levels = CNT),
                   ratio = factor(rep(c('MADR','SDR'), 9), levels = c('MADR','SDR')))

tiff(filename = 'Variability (math, right tail).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('Sex Differences in Right Tail Variability in PISA 2018 Math — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Right Tail Variability Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = 1, to = 1.16, by = .04),
                     minor_breaks = seq(1, 1.17, .01),
                     limits = c(1.003, 1.168), trans = 'log')

dev.off() # write image to working directory


# right tail variability reading
cap <- paste0('Two measures of male-female ratios in right tail variability are ',
              'plotted for nine countries: median absolute deviation ratio (MADR) ',
              'and standard deviation \nratio (SDR), computed in the right half of the ',
              'distribution. Countries were drawn from a total of 74 to represent ',
              'each decile of the average of the two ratios.')

mns <- numeric(74)
for (i in 1:74) {mns[i] <- exp(mean(c(log(P18R[18,i+1]), log(P18R[21,i+1]))))}
names(mns) <- names(P18R)[-1]

scores <- numeric(18)
for (i in 1:9) {
  nm <- names(sort(mns)[p[i]])
  scores[2*i-1] <- P18R[18,nm] # SDR_R
  scores[2*i] <- P18R[21,nm] # MADR_R
}

CNT <- c('Singapore', 'Switzerland', 'Bosnia', 'Ireland',
         'Canada', 'Peru', 'Sweden', 'Finland', 'Latvia')

data <- data.frame(scores = scores,
                   country = factor(rep(as.character(CNT), each = 2), levels = CNT),
                   ratio = factor(rep(c('MADR','SDR'), 9), levels = c('MADR','SDR')))

tiff(filename = 'Variability (reading, right tail).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = ratio, y = scores, colour = ratio)) +
  geom_point(colour = rep(c('#E82728', '#8000FF'), 9), shape = 1, size = 5) +
  facet_grid(.~country) +
  ggtitle('Sex Differences in Right Tail Variability in PISA 2018 Reading — deciles') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = NULL, y = 'Right Tail Variability Ratio', caption = cap) +
  scale_y_continuous(breaks = seq(from = 1, to = 1.2, by = .04),
                     minor_breaks = seq(1, 1.19, .01),
                     limits = c(1.0015, 1.198), trans = 'log')

dev.off() # write image to working directory


# MU3Rs and SQDs
a <- b <- q <- k <- numeric(99)
for (i in 1:99) {
  a[i] <- exp(mean(as.numeric(P18M[i+39,-1]))) # MU3Rs math
  b[i] <- exp(mean(as.numeric(P18R[i+39,-1]))) # MU3Rs reading
  q[i] <- mean(as.numeric(P18M[i+138,-1])) # SQDs math
  k[i] <- mean(as.numeric(P18R[i+138,-1])) # SQDs reading
}

cap <- paste0('The median-aligned U3 ratio (MU3R) was computed at each percentile ',
              'and averaged \nacross all countries. The U shapes indicate ',
              'greater male variability throughout the distribution.')

data <- data.frame(Percentile = rep(c(5:95), 2),
                   SQD = c(a[5:95], b[5:95]),
                   Subject = rep(c('Math', 'Reading'), each = 91))

tiff(filename = 'MU3Rs.png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data = data, aes(x = Percentile, y = SQD, group = Subject)) +
  geom_line(aes(color = Subject), size = .8) +
  ggtitle('Median-aligned U3 Ratios in PISA 2018') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        legend.position = 'bottom',
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 10))) +
  labs(x = 'Percentile', y = 'MU3R', caption = cap) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10),
                     limits = c(3, 97)) +
  scale_y_continuous(breaks = seq(from = 1, to = 1.4, by = .04),
                     limits = c(1, 1.4), trans = 'log') +
  scale_color_manual(values = c('#E82728', '#87D180'))

dev.off() # write image to working directory


cap <- paste0('The standardized quantile difference (SQD) was computed at each percentile ',
              'and averaged across all countries. Positive SQDs correspond to male \nadvantage; ',
              'negative SQDs to female advantage. The increasing trends from left to right ',
              'indicate greater male variability throughout the distribution.')

data <- data.frame(Percentile = rep(c(5:95), 2),
                   SQD = c(q[5:95], k[5:95]),
                   Subject = rep(c('Math', 'Reading'), each = 91))

tiff(filename = 'SQDs.png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = Percentile, y = SQD, group = Subject)) +
  geom_line(aes(color = Subject), size = .8) +
  ggtitle('Standardized Quantile Differences in PISA 2018') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        legend.position = 'bottom',
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 10))) +
  labs(x = 'Percentile', y = 'SQD', caption = cap) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10),
                     limits = c(3, 97)) +
  scale_y_continuous(breaks = seq(from = -60, to = 30, by = 10),
                     limits = c(-65, 25)) +
  scale_color_manual(values = c('#E82728', '#8000FF'))

dev.off() # write image to working directory


# quarters with regard to total shift
SQD595M <- SQD595R <- numeric(74)
for (i in 2:75) {
  SQD595M[i-1] <- as.numeric(P18M[233,i]) - as.numeric(P18M[143,i])
  SQD595R[i-1] <- as.numeric(P18R[233,i]) - as.numeric(P18R[143,i])
}

SQD595Mn <- names(P18M)[-1][order(SQD595M)]
SQD595Rn <- names(P18R)[-1][order(SQD595R)]

Q1Mn <- SQD595Mn[1:19]
Q2Mn <- SQD595Mn[20:37]
Q3Mn <- SQD595Mn[38:56]
Q4Mn <- SQD595Mn[57:74]
Q1Rn <- SQD595Rn[1:19]
Q2Rn <- SQD595Rn[20:37]
Q3Rn <- SQD595Rn[38:56]
Q4Rn <- SQD595Rn[57:74]

Q1M <- Q2M <- Q3M <- Q4M <- Q1R <- Q2R <- Q3R <- Q4R <- numeric(99)
for (i in 1:99) {
  Q1M[i] <- mean(as.numeric(P18M[i+138,Q1Mn]))
  Q2M[i] <- mean(as.numeric(P18M[i+138,Q2Mn]))
  Q3M[i] <- mean(as.numeric(P18M[i+138,Q3Mn]))
  Q4M[i] <- mean(as.numeric(P18M[i+138,Q4Mn]))
  Q1R[i] <- mean(as.numeric(P18R[i+138,Q1Rn]))
  Q2R[i] <- mean(as.numeric(P18R[i+138,Q2Rn]))
  Q3R[i] <- mean(as.numeric(P18R[i+138,Q3Rn]))
  Q4R[i] <- mean(as.numeric(P18R[i+138,Q4Rn]))
}

cap <- paste0('The standardized quantile difference (SQD) was computed at each percentile ',
              'for all 74 countries, which were split \ninto quarters based on the total shift ',
              'between the 5th and 95th percentiles (i.e., SQD at 95th minus SQD at 5th). ',
              'Q1 \ncorresponds to the quarter of countries with the most negative shifts; Q4 ',
              'corresponds to those with the most positive shifts.')

data <- data.frame(Percentile = rep(c(5:95), 8),
                   SQD = c(Q1M[5:95], Q2M[5:95], Q3M[5:95], Q4M[5:95],
                           Q1R[5:95], Q2R[5:95], Q3R[5:95], Q4R[5:95]),
                   Quarter = rep(c('Q1', 'Q2', 'Q3', 'Q4'), each = 91, 2),
                   Subject = rep(c('Math', 'Reading'), each = 364))

tiff(filename = 'SQDs quartered.png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data, aes(x = Percentile, y = SQD, group = interaction(Subject, Quarter))) +
  geom_line(aes(color = Quarter, linetype = Subject), size = .8) +
  ggtitle('Standardized Quantile Differences in PISA 2018, Quartered by Total Shifts') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        legend.position = 'bottom',
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 10))) +
  labs(x = 'Percentile', y = 'SQD', caption = cap) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10),
                     limits = c(3, 97)) +
  scale_y_continuous(breaks = seq(from = -80, to = 30, by = 10),
                     limits = c(-84.3, 29)) +
  scale_color_manual(values = c('#8000FF', '#87D180', '#E82728', '#FF6C00')) +
  scale_linetype_manual(values = c(61, 'solid'))

dev.off() # write image to working directory



### Global Gender Gap Index (GGGI) and U3

# math
cap <- paste0('Sex differences in math scores, expressed as U3, are plotted ',
              'as a function of scores on the \nGlobal Gender Gap Index (GGGI). ',
              'Countries with higher GGGI scores are closer to parity.')

tiff(filename = 'GGGI U3 (math).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data = data.frame(GI = GGGI$GGGI, U3M = as.numeric(P18M[14,-1])),
       aes(x = GI, y = U3M)) +
  geom_point(colour = '#FF6C00', shape = 1, size = 5) +
  ggtitle('Gender Inequality and U3 in PISA 2018 Math') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = 'Global Gender Gap Index', y = 'U3', caption = cap) +
  scale_x_continuous(breaks = seq(.6, .9, .1), minor_breaks = seq(.6, .9, .05),
                     limits = c(.598, .902)) +
  scale_y_continuous(breaks = seq(.4, .6, .05),
                     labels = c('40%', '45%', '50%', '55%', '60%'),
                     limits = c(.39, .61))

dev.off() # write image to working directory


# reading
cap <- paste0('Sex differences in reading scores, expressed as U3, are plotted ',
              'as a function of scores on the \nGlobal Gender Gap Index (GGGI). ',
              'Countries with higher GGGI scores are closer to parity.')

tiff(filename = 'GGGI U3 (reading).png', width = 10.5, height = 9,
     units = 'in', pointsize = 14, bg = 'white', res = 300) # image file

ggplot(data = data.frame(GI = GGGI$GGGI, U3R = as.numeric(P18R[14,-1])),
       aes(x = GI, y = U3R)) +
  geom_point(colour = '#FF6C00', shape = 1, size = 5) +
  ggtitle('Gender Inequality and U3 in PISA 2018 Reading') +
  theme(text = element_text(family = 'Optima', size = 14),
        panel.background = element_rect(fill = alpha('#45BCFF', .15)),
        plot.title = element_text(hjust = .5),
        plot.caption = element_text(size = 9.5, hjust = .5, margin = margin(t = 15))) +
  labs(x = 'Global Gender Gap Index', y = 'U3', caption = cap) +
  scale_x_continuous(breaks = seq(.6, .9, .1), minor_breaks = seq(.6, .9, .05),
                     limits = c(.598, .902)) +
  scale_y_continuous(breaks = seq(.25, .45, .05), minor_breaks = seq(.25, .475, .025),
                     labels = c('25%', '30%', '35%', '40%', '45%'),
                     limits = c(.25, .475))

dev.off() # write image to working directory




















