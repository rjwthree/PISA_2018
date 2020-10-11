# Sex Differences in PISA 2018 Math and Reading Data
This script analyzes the most recently released data on exams given to 15-year-olds from the [Programme for International Student Assessment (PISA)](https://github.com/rjwthree/PISA_2018/blob/master/PISA%202018.R). It is administered every three years by the Organisation for Economic Co-operation and Development (OECD). http://www.oecd.org/pisa

## Results

The full output from all 74 countries and 6 regions is available for [math](https://github.com/rjwthree/PISA_2018/blob/master/Full%20output%20(math).csv) and [reading](https://github.com/rjwthree/PISA_2018/blob/master/Full%20output%20(reading).csv) in csv files, but is difficult to read because there are so many analyses and countries. I've started to visualize the results - see below for definitions of all my effect sizes.

Three measures of sex differences in variability are visualized for [math](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(math).png) and [reading](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/Variability%20(reading).png), and one country for each decile is selected to represent the distribution of results. Notably, all nine deciles for both math and reading show larger variability in males than females.

Another measure of variability that can be observed at every point throughout the distribution is the standardized quantile difference (SQD). If the male distribution is more dispersed, we would expect comparisons to be least favorable to males in the left tail most favorable in the right tail, and so male-female differences would become more positive from left to right. Confirming this prediction, the smoothly increasing trend [shown here](https://raw.githubusercontent.com/rjwthree/PISA_2018/master/SQDs.png) for both math and reading indicates greater male variability in both tails.

## Code

The script proceeds as follows:

(1) Read and Format Data

(2) Weighted User-defined Functions

(3) Effect Sizes and effect sizes adjusted for age

(4) Standard Errors and Confidence Intervals

(5) Summary and Output

Effect sizes were selected (or created) to be robust, intuitive and useful. They examine sex differences throughout the distribution, not just at the center. Common effect sizes (e.g., Cohen's d) are also used. An attitude of [multiverse analysis](https://journals.sagepub.com/doi/full/10.1177/1745691616658637) is applied: when multiple reasonable analyses exist, they are all conducted so that the sensitivity of substantive conclusions to specific analytic choices can be observed.

The data are complex for two reasons: (1) each student responds to only a subset of all questions, so each student is given ten imputed scores, also known as [plausible values](https://www.oecd.org/pisa/data/pisa2018technicalreport/Ch.09-Scaling-PISA-Data.pdf#page=31) (PVs); and (2) the sampling procedures employ clustering and stratification, so [Fay's method](https://www.oecd.org/pisa/data/pisa2018technicalreport/PISA2018%20TecReport-Ch-08-Survey-Weights.pdf#page=8) of balanced repeated replication is used to compute standard errors. Due to the first point, standard errors incorporate not only sampling variance but also imputation variance. The sampling design also means that each student is assigned their own weight. The data files are designed for SPSS and SAS and are analyzed almost exclusively as such.

Ratios are log-transformed to place them on a linear scale.

## Read and Format Data
The data are read from SPSS format using the 'haven' package and written in csv format so that subsequent reading can occur efficiently with the 'data.table' package. A country is subsetted and its data are converted to a dataframe. Some objects are created that will be needed throughout the script.

## Weighted Functions
All functions used for analysis are user-defined, for a few reasons:

(1) Some functions were not available, especially for weighted statistics and effect sizes I created because I wasn't aware of any that were suitable (see next section).

(2) Some existing functions were inefficient and runtime was far too long because of the large sample sizes and extensive repetition during Fay's balanced repeated replication. All efforts were made to maximize the functions' efficiency.

(3) User-defined functions are entirely transparent and explicit, which is particularly good when different packages produce slightly different results due to differing methods.

## Effect Sizes
*Effect sizes with an asterisk are novel; they were adapted from other statistics.

Means and Medians - Means and medians of total group, females, and males, as well as male-female mean and median differences.

Cohen's d - This is a very common effect size and it is implemented as usual: the raw mean difference divided by the quadratic mean of standard deviations.

U3* - The proportion of males above the female median. Adapted from the canonical Cohen's U3, which is incorrectly based on the mean rather than the median.

Probability of Superiority (PS) - The probability that a random male will have a higher score than a random female.

Standard Deviation Ratio (SDR) - The ratio of male SD to female SD.

Left SDR and Right SDR* - The SDR divided into the left and right tails. That is, the square root of the mean squared deviation from the mean among the subset of scores below or above the mean, respectively.

Median Absolute Deviation Ratio (MADR) - The male-female ratio of median absolute deviation (MAD) from the median.

Left MADR and Right MADR* - The MADR divided into the left and right tails. That is, the median absolute deviation from the median among the subset of scores below or above the median, respectively.

Gini's Mean Difference Ratio (GMDR) - Imagine randomly selecting two students and finding the distance between their scores. GMD gives the expected value of this interval: the mean absolute difference. The GMDR is the male-female ratio.

Tail Proportion Ratios (TPRs) - The proportion of males below (left tail) or above (right tail) a threshold, divided by the proportion of females below or above the threshold. The thresholds are defined by the total group. The left tail TPRs are below the 5th and 10th percentiles, and the right tail TPRs are above the 90th and 95th percentiles.

U3 Ratios (U3Rs)* - The proportion of males below (left tail) or above (right tail) a threshold defined by the female subgroup, divided by the natural proportion of females below or above that threshold (e.g., 10% above the 90th percentile). The left tail U3Rs are below the 5th and 10th percentiles, and the right tail U3Rs are above the 90th and 95th percentiles. Adapted from U3 (see above).

Median-aligned U3 Ratios (MU3Rs)* - U3Rs calculated after aligning the male and female medians, for each percentile. The MU3R below/above the median is 1 by definition. A U shape (values decrease to the left of the median and increase to the right) indicates higher male variability, and an upside-down U shape (values increase to the left of the median and decrease to the right) indicates higher female variability.

Standardized Quantile Differences (SQDs)* - Raw male-female differences at each percentile, as a percentage of the mean of male and female MADs. An increasing trend from left to right indicates higher male variability; a decreasing trend indicates higher female variability. The purpose of dividing by MAD is to standardize the quantile differences with a robust measure of scale.

The next section adjusts the scores linearly for age and recalculates the effect sizes from Cohen's d to GMDR.

## Standard Errors and Confidence Intervals
Computing standard errors is the most computationally intensive procedure. The use of 80 sets of replicate weights, in combination with the ten sets of plausible values, means that each effect size must be recalculated 800 times. The sampling variance and imputation variance are then calculated according to procedures described by the '[PISA 2018 Technical Report](https://www.oecd.org/pisa/data/pisa2018technicalreport/)'. The sum of sampling and imputation variance is the total variance, and its square root is the standard error. The standard errors are then converted to 95% confidence intervals through typical methods, and the lower and upper bounds of these confidence intervals are recorded.

The results are then summarized in two dataframes, one each for math and reading, and written to the appropriate directory.
