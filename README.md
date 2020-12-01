# Sex Differences in PISA 2018 Math and Reading Data
This [script](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R) analyzes sex differences in the most recently released data on exams given to 15-year-olds from the Programme for International Student Assessment (PISA). It is administered every three years by the Organisation for Economic Co-operation and Development (OECD). http://www.oecd.org/pisa

The first major section describes the results; the second section describes the code and statistics. See [here](https://github.com/rjwthree/TIMSS_PIRLS) for similar assessments of younger children by another organization.

## Results

The full output from all 74 countries is available for [math](https://github.com/rjwthree/PISA_2018/blob/master/Full%20output%20(math).csv) and [reading](https://github.com/rjwthree/PISA_2018/blob/master/Full%20output%20(reading).csv) in csv files, but is difficult to read because there are so many analyses and countries. I've visualized some of the results and hyperlinked them in the discussion below. See further down for definitions of all my effect sizes.

The code to download the output and create all of these beautiful plots is [here](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018%20Visualization.R).

### Summary of results

In accord with common understanding, females excel in [reading](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Central%20tendency%20(reading).png) and males in [mathematics](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Central%20tendency%20(math).png). Yet notably, the female advantage is far larger and more consistent. Females are more likely to be [outstanding readers](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Right%20tail%20(reading).png) than males, and much less likely to be among the [poorest readers](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Left%20tail%20(reading).png). Males outnumber females among [top performers in math](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Right%20tail%20(math).png), but also among the [worst](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Left%20tail%20(math).png) in most countries. How could this be, given that their average performance is better? In both [reading](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(reading).png) and [math](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(math).png), male scores are more widely dispersed. As shown here, males exhibit higher variability using three different measures:

<p align="center">
<img src="https://github.com/rjwthree/PISA_2018/blob/master/Variability%20(math).png" width="700" height="600"/>
</p>

[This](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(math%2C%20left%20tail).png) [conclusion](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(reading%2C%20left%20tail).png) [holds](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(math%2C%20right%20tail).png) [across](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(reading%2C%20right%20tail).png) [multiple](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/SQDs.png) [approaches](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/MU3Rs.png). The plot of [SQDs](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/SQDs.png) emphasizes the size of the female advantage in reading; at its lowest ebb in the right tail it is still slightly larger than the expanded male advantage in the right tail of math scores. Further details follow in the next three mini-sections, plus an analysis of how these patterns interact with gender inequality.

#### Central tendency differences

The differences for reading are straightforward: by all three measures, females had an [advantage](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Central%20tendency%20(reading).png) in all 74 countries. In the median country, only 38% of male scores exceeded the median female score (indicating that the female median was roughly equal to the 62nd percentile in males). Males had an [advantage in mathematics](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Central%20tendency%20(math).png) in about two-thirds of countries. In the median country with regard to all three measures, 51% of males scored above the median female.

#### Tail differences

Males predominate among the lowest scores in reading. [Below the 5th percentile](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Left%20tail%20(reading).png), the median country has more than twice as many males. Below the 5th percentile of math scores, males also predominate [most but not all of the time](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Left%20tail%20(math).png), and in the median country they are only 10% more prevalent.

Females predominate among those with [reading scores above the 95th percentile](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Right%20tail%20(reading).png), and in the median country females are about 50% more likely to score in that stratum. Males are disproportionately represented among the [top 5% of scores in math](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Right%20tail%20(math).png) at all nine deciles, with a median difference of about 40%.

The large ratios of males to females below the 5th percentile of reading scores are shown here:

<p align="center">
<img src="https://github.com/rjwthree/PISA_2018/blob/master/Left%20tail%20(reading).png" width="700" height="600"/>
</p>

#### Variability differences

Three measures of sex differences in variability are visualized for [math](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(math).png) and [reading](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(reading).png), and all nine deciles for both math and reading show larger variability in males than females. This effect holds in both tails: for [math](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(math%2C%20left%20tail).png) and [reading](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(reading%2C%20left%20tail).png) in the left tail, and [math](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(math%2C%20right%20tail).png) and [reading](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(reading%2C%20right%20tail).png) in the right tail. The only exceptions are in the reading scores of a small minority of countries: their left tails show greater spread in females rather than males (this is true with 95% confidence across both measures only in the Philippines). Right tails display more spread in males with very high consistency.

Median-aligned U3 ratios (MU3Rs) illustrate differences in variability throughout the distribution. A U shape — decreasing to the left of the median, increasing to the right — would indicate greater male variability, and this is clearly observed in the [plot of MU3Rs](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/MU3Rs.png) for both math and reading scores.

Another measure of variability that can be observed at every point throughout the distribution is the standardized quantile difference (SQD). If the male distribution is more widely dispersed, we would expect comparisons to be least favorable to males in the left tail and most favorable in the right tail, and so male-female differences would become more positive from left to right. Confirming this prediction in the aggregate of all 74 countries, the smoothly increasing trend [shown here](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/SQDs.png) for both math and reading indicates greater male variability in both tails. If we want to disaggregate the countries to observe heterogeneity, we can [sort them into quarters](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/SQDs%20quartered.png) according to their total shifts between the 5th and 95th percentiles. The four trends visualized for each subject are largely equivalent to the aggregate trend, but somewhat messier and with one unexpected result in the reading data (see the left tail of the lowest quarter).

<p align="center">
<img src="https://github.com/rjwthree/PISA_2018/blob/master/SQDs.png" width="700" height="600"/>
</p>

A [few](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(reading%2C%20left%20tail).png) [analyses](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/MU3Rs.png) [found](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/SQDs.png) [that](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/SQDs%20quartered.png) sex differences in the left tail for reading were unexpected in several countries. Preliminarily, I would speculate that this is at least partly due to a floor effect. A combination of low overall scores and the superior female mean compresses male scores in particular because they amass near the floor. This appears to be severe in the Philippines, the Dominican Republic, Lebanon, and Kosovo. They have the lowest reading scores, and all four exhibit larger female spread in the left tail by some or all measures, which is probably not a coincidence.

#### Gender inequality

Although flawed, the World Economic Forum's Global Gender Gap Index (GGGI) is arguably the best quantification of women's disadvantages around the world. The [report](https://www.weforum.org/reports/global-gender-gap-report-2020) published in Dec 2019, which corresponds best with the assessment timeline of PISA 2018, contains [data](https://github.com/rjwthree/PISA_2018/blob/master/GGGI.csv) on 72 of the [74 countries tested by PISA](https://github.com/rjwthree/PISA_2018/blob/master/Country%20codes.csv). (Kosovo and Taiwan were the exceptions, given their difficulties with universal, official recognition.)

The index is an average of four subindices which can range from 0 to 1. For each subindex, a score of 1 would indicate equality or female advantage. The highest overall index is .877 in Iceland, and the lowest among PISA countries is .599 in both Lebanon and Saudi Arabia. Spearman's correlation coefficient (_rs_) will be used here because non-parametric statistics are most suitable. <sub>hey</sub>

The correlations with mean and median scores in both math and reading are about .4, whether they be with the total, female, or male scores. This is consistent with an association between greater gender equality and human development in general. The correlation with sex differences at the center — U3 and probability of superiority — is .25-.28 in [math](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/GGGI%20U3%20(math).png) and about .21 in [reading](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/GGGI%20U3%20(reading).png). That is, there is a mild tendency for males to perform better relative to females in more gender-equal countries.

<p align="center">
<img src="https://github.com/rjwthree/PISA_2018/blob/master/GGGI%20U3%20(math).png" width="708" height="600"/>
</p>

Countries with lower gender equality tend to have males overrepresented to a greater degree in the bottom 5% and 10% of both math and reading scores (correlations roughly -.2). There is no relation in the right tail of reading. With regard to the top 5% and 10% of math scores, males are more strongly overrepresented in more gender-equal countries (correlations of .21 and .25, respectively).

Correlations between GGGI and sex differences in variability are all close to zero, suggesting that higher variability in males is not a product of gender inequality. These analyses of gender inequality are superficial and preliminary. There is some sign of a curvilinear relationship, in which the most gender-equal countries share some outcomes with the most unequal.

## Code

The [script](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R) proceeds as follows:

(1) Read and Format Data

(2) Weighted User-defined Functions

(3) Effect Sizes and effect sizes adjusted for age

(4) Standard Errors and Confidence Intervals

(5) Summary and Output

Effect sizes were selected (or created) to be robust, intuitive and useful. Common effect sizes which are not robust or intuitive, like Cohen's _d_, are also used for the sake of external comparison. An attitude of [multiverse analysis](https://journals.sagepub.com/doi/full/10.1177/1745691616658637) is applied: when multiple reasonable analyses exist, they are all conducted so that the sensitivity of substantive conclusions to specific analytic choices can be observed.

The data are complex for two reasons: (1) each student responds to only a subset of all questions, so each student is given ten imputed scores, also known as [plausible values](https://www.oecd.org/pisa/data/pisa2018technicalreport/Ch.09-Scaling-PISA-Data.pdf#page=31) (PVs); and (2) the sampling procedures employ clustering and stratification, so [Fay's method](https://www.oecd.org/pisa/data/pisa2018technicalreport/PISA2018%20TecReport-Ch-08-Survey-Weights.pdf#page=8) of balanced repeated replication is used to compute standard errors. Due to the first point, standard errors incorporate not only sampling variance but also imputation variance. The sampling design also means that each student is assigned their own weight. The data files are designed for SPSS and SAS and are analyzed almost exclusively as such.

Ratios are log-transformed to place them on a linear scale.

### [Read and Format Data](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R#L32)
The data are read from SPSS format using the 'haven' package and written in csv format so that subsequent reading can occur efficiently with the 'data.table' package. A country is subsetted and its data are converted to a dataframe. Some objects are created that will be needed throughout the script.

### [Weighted Functions](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R#L193)
All functions used for analysis are user-defined, for various reasons related to transparency, efficiency, and necessity - some effect sizes had to be created because I wasn't aware of any that were suitable (see next section).

### [Effect Sizes](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R#L492)
*Effect sizes with an asterisk are novel; they were adapted from other statistics.

Means and Medians — Means and medians of total group, females, and males, as well as male-female mean and median differences.

Cohen's _d_ — This is a very common effect size and it is implemented as usual: the raw mean difference divided by the quadratic mean of standard deviations.

U3* — The proportion of males above the female median. Adapted from the canonical Cohen's U3, which is incorrectly based on the mean rather than the median.

Probability of Superiority (PS) — The probability that a random male will have a higher score than a random female.

Median Absolute Deviation Ratio (MADR) — The male-female ratio of median absolute deviation (MAD) from the median.

Left MADR and Right MADR* — The MADR divided into the left and right tails. That is, the male-female ratio of the median absolute deviation from the median among the subset of scores below or above the median, respectively.

Standard Deviation Ratio (SDR) — The ratio of male SD to female SD.

Left SDR and Right SDR* — The SDR divided into the left and right tails. That is, the male-female ratio of the square root of the mean squared deviation from the mean among the subset of scores below or above the mean, respectively.

Gini's Mean Difference Ratio (GMDR) — Imagine randomly selecting two students and finding the distance between their scores. GMD gives the expected value of this interval: the mean absolute difference. The GMDR is the male-female ratio.

Tail Proportion Ratios (TPRs) — The proportion of males below (left tail) or above (right tail) a threshold, divided by the proportion of females below or above the threshold. The thresholds are defined by the total group. The left tail TPRs are below the 5th and 10th percentiles, and the right tail TPRs are above the 90th and 95th percentiles.

U3 Ratios (U3Rs)* — The proportion of males below (left tail) or above (right tail) a threshold defined by the female subgroup, divided by the natural proportion of females below or above that threshold (e.g., 10% above the 90th percentile). The left tail U3Rs are below the 5th and 10th percentiles, and the right tail U3Rs are above the 90th and 95th percentiles. Adapted from U3 (see above).

Median-aligned U3 Ratios (MU3Rs)* — U3Rs calculated after aligning the male and female medians, for each percentile. The MU3R below/above the median is 1 by definition. A U shape (values decrease to the left of the median and increase to the right) indicates higher male variability, and an upside-down U shape (values increase to the left of the median and decrease to the right) indicates higher female variability.

Standardized Quantile Differences (SQDs)* — Raw male-female differences at each percentile, as a percentage of the mean of male and female MADs. An increasing trend from left to right indicates higher male variability; a decreasing trend indicates higher female variability. The purpose of dividing by MAD is to standardize the quantile differences with a robust measure of scale.

The next [section](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R#L616) adjusts the scores linearly for age and recalculates the effect sizes from Cohen's _d_ to GMDR.

### [Standard Errors and Confidence Intervals](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R#L700)
Computing standard errors is the most computationally intensive procedure. The use of 80 sets of replicate weights, in combination with the ten sets of plausible values, means that each effect size must be recalculated 800 times. The sampling variance and imputation variance are then calculated according to procedures described by the '[PISA 2018 Technical Report](https://www.oecd.org/pisa/data/pisa2018technicalreport/)'. The sum of sampling and imputation variance is the total variance, and its square root is the standard error. The standard errors are then converted to 95% confidence intervals through typical methods, and the lower and upper bounds of these confidence intervals are recorded.

The results are then [summarized](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R#L847) in two dataframes, one each for math and reading, and written to the appropriate directory.
