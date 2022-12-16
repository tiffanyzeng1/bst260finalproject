bst260finalproject
================

# BST 260 Final Project

## Factors Associated with Healthcare Utilization Among Elderly in Shanghai, China

### Introduction

China’s growing aging population can be seen as a threat to the
country’s healthcare system by pushing the government to shift their
resources to address the needs of the older population, therefore
putting more strain on the healthcare system. Although life expectancy
has increased by 17 years over the past 2 decades, the older population
experiences more years lived with disability due to chronic conditions
(Kennedy et al., 2014). The main challenge with a large elderly
population is that because almost 80% of deaths in China are attributed
to chronic NCDs among those aged 60 years and older, therefore it is
important to understand the factors that drive healthcare utilization
among China’s aging population.

According to the IHME, out of the top 10 causes of death for those ages
70 and older, 9 are NCDs, with cardiovascular diseases, neoplasms, and
chronic respiratory diseases to be the top 3 causes of death (Vos et
al., 2020). Additionally, with the growing older population, China will
be losing 70 million of its workforce over the next few years, which
will be a huge shock to the global supply chain as much of the world
depends on China for its manufacturing factories.By ensuring a healthier
elderly population, more people would remain in the workforce and
therefore continue to contribute to China’s growing economy. There is a
saying that China will “get old before it gets rich,” which accurately
describes what the country is currently experiencing. A growing elderly
population coupled with a shrinking pool of working adults will add
pressure to the country’s health and social welfare system.

The objective of this study is to focus on the health status of
Shanghai’s aging population, as Shanghai has shown a more dramatic
growth in its aging population in recent years. According to the
Shanghai Bureau of Statistics, the city had a population of 14.5 million
people living in households by the end of 2016, of which over 31% were
aged 60 and over (Shanghai Bureau of Statistics, 2011).

The methodology of identifying this dataset was through
datasetsearch.research.google.com and entering keywords “healthcare
utilization among elderly population in China.” The dataset used in this
study is publicly available and sourced from a cross-sectional study
that developed a questionnaire used for face-to-face interviews
conducted in June and August 2011 among Shanghai’s 18 districts.

### Results

To begin the analysis, we are going to load the dataset:

### Load Dataset

``` r
library(haven)
library(readxl)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(mlogit)
```

    ## Loading required package: dfidx
    ## 
    ## Attaching package: 'dfidx'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
data <- read_excel("elderly_china_data.xlsx")
head(data)
```

    ## # A tibble: 6 × 37
    ##   gender   age marry  race education children livingsta…¹ insur…² expen…³ income
    ##    <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>       <dbl>   <dbl>   <dbl>  <dbl>
    ## 1     NA     2     3    NA         1        3           3       3       1      2
    ## 2      1     4     3     1         2        3           2       1       3      1
    ## 3      2     5     3     1         2        4           1       2       1      1
    ## 4      2     5     3     1         2        3           2       2       3      1
    ## 5      2     5     3     1         1        3           2       5       2      1
    ## 6      1     5     3     1         2        3           1       1       3      1
    ## # … with 27 more variables: location <dbl>, `self－rated health` <dbl>,
    ## #   `sensory limitation` <dbl>, lonely <dbl>, nervious <dbl>,
    ## #   `life feeling` <dbl>, ADL <dbl>, changing <dbl>, smoking <dbl>,
    ## #   drink <dbl>, hypertension <dbl>, Diabetes <dbl>, Heartdiseases <dbl>,
    ## #   Cerebrovascular <dbl>, Cataract <dbl>, Bronchitis <dbl>,
    ## #   Gastroenteritis <dbl>, Regionaleconomiclevel <dbl>, activity <dbl>,
    ## #   `Relatives gathering` <dbl>, community <dbl>, `Children meeting` <dbl>, …

We see that some of the variables are either spelled or labeled
incorrectly, therefore we need to recode these variables before
beginning our analysis.

### Rename Variables

``` r
colnames(data)[8] = "insurance"
colnames(data)[9] = "pension_income"
colnames(data)[10] = "income_source"
colnames(data)[15] = "nervous"
colnames(data)[16] = "life_satisfaction"
colnames(data)[18] = "physical_health_change"
colnames(data)[23] = "Heart Disease" 
colnames(data)[28] = "regional_economic_level"
colnames(data)[29] = "outdoor_activity"
colnames(data)[31] = "community_activities"
colnames(data)[32] = "seeing_children"
colnames(data)[36] = "hospitalization"
colnames(data)[37] = "total_chronic"
```

We also want to double-check if there are any classification
discrepancies in the dataset as well as generate variables of interest
by converting them to binary variables if necessary. We are creating a
binary “chronic” variable to label those in our study population that
are diagnosed with 1 or more chronic conditions as “1” and those who do
not have chronic conditions as “0”. Additionally, we are creating a
“married” and “never_married” variable to further disaggregate the
“marry” variable. Lastly, we are generating a second gender and age
variable to easily identify their categories.

### Convert Variable Types/Generate Variables

``` r
class(data$gender)
```

    ## [1] "numeric"

``` r
data$gender2 <- factor(ifelse(data$gender == 1, "Male", "Female"))
data <- data |>
  mutate(age2 = factor (
    x = age,
    levels = 1:5,
    labels = c("60_64", "65_69", "70_74", "75_79", ">=80"
  )))

#generate binary chronic variable 
data$chronic <- ifelse(data$total_chronic>=1, 1, 0)

#generate indicator variables for married and never married
data$married <- ifelse(data$marry == 3, 1, 0)
data$never_married <- ifelse(data$marry == 1 & 2, 1, 0)
```

### Summarize Variables

``` r
data |>
  group_by(gender) |>
  summarize(n())
```

    ## # A tibble: 3 × 2
    ##   gender `n()`
    ##    <dbl> <int>
    ## 1      1   843
    ## 2      2  1154
    ## 3     NA     3

``` r
data |>
  group_by(age) |>
  summarize(n())
```

    ## # A tibble: 6 × 2
    ##     age `n()`
    ##   <dbl> <int>
    ## 1     1   643
    ## 2     2   339
    ## 3     3   314
    ## 4     4   378
    ## 5     5   325
    ## 6    NA     1

``` r
data |>
  group_by(race) |>
  summarize(n())
```

    ## # A tibble: 3 × 2
    ##    race `n()`
    ##   <dbl> <int>
    ## 1     1  1975
    ## 2     2    21
    ## 3    NA     4

``` r
data |>
  group_by(education) |>
  summarize(n())
```

    ## # A tibble: 5 × 2
    ##   education `n()`
    ##       <dbl> <int>
    ## 1         1   572
    ## 2         2   636
    ## 3         3   672
    ## 4         4   114
    ## 5        NA     6

``` r
data |>
  group_by(children) |>
  summarize(n())
```

    ## # A tibble: 5 × 2
    ##   children `n()`
    ##      <dbl> <int>
    ## 1        1    26
    ## 2        2  1057
    ## 3        3   758
    ## 4        4   148
    ## 5       NA    11

Lastly, summarizing variables of interest gives us a better
understanding of the percentage breakdown within each category.
Extensive data wrangling was necessary for this dataset since we wanted
to manipulate variables that are specific to this analysis. The original
dataset had many errors that would not have allowed for appropriate
analyses.

### Linear Regression

``` r
m1 <- lm(chronic ~ gender + age + married + never_married + race + education + children + livingstatus + insurance + pension_income + location + lonely + smoking + drink, 
         data=data)

summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = chronic ~ gender + age + married + never_married + 
    ##     race + education + children + livingstatus + insurance + 
    ##     pension_income + location + lonely + smoking + drink, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.01185  0.06804  0.20046  0.25588  0.40154 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     0.496439   0.150736   3.293  0.00101 ** 
    ## gender          0.013107   0.024853   0.527  0.59799    
    ## age             0.018514   0.008686   2.132  0.03318 *  
    ## married         0.066425   0.060941   1.090  0.27586    
    ## never_married   0.041467   0.064320   0.645  0.51920    
    ## race            0.078767   0.090766   0.868  0.38561    
    ## education      -0.005181   0.013440  -0.386  0.69990    
    ## children        0.019607   0.018503   1.060  0.28944    
    ## livingstatus    0.017559   0.014157   1.240  0.21503    
    ## insurance      -0.021157   0.009197  -2.300  0.02153 *  
    ## pension_income -0.008260   0.015810  -0.522  0.60142    
    ## location       -0.029241   0.016897  -1.731  0.08370 .  
    ## lonely          0.089905   0.020301   4.429    1e-05 ***
    ## smoking        -0.012913   0.013576  -0.951  0.34165    
    ## drink           0.011095   0.012049   0.921  0.35726    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.412 on 1896 degrees of freedom
    ##   (89 observations deleted due to missingness)
    ## Multiple R-squared:  0.02773,    Adjusted R-squared:  0.02055 
    ## F-statistic: 3.862 on 14 and 1896 DF,  p-value: 1.573e-06

The first model we are interested in building is a simple linear
regression. We see that the “location” variable is significant at the
0.1 level, the “age” and “insurance” variables are significant at the
0.05 level, and the “lonely” variable is significant at the 0.0001
level.

### Logistic Regression

``` r
m2 <- glm(chronic ~ gender + age + married + never_married + race + education + children + livingstatus + insurance + pension_income + location + lonely + smoking + drink, 
          family = binomial(link = "logit"),
               data = data)

summary(m2)
```

    ## 
    ## Call:
    ## glm(formula = chronic ~ gender + age + married + never_married + 
    ##     race + education + children + livingstatus + insurance + 
    ##     pension_income + location + lonely + smoking + drink, family = binomial(link = "logit"), 
    ##     data = data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4730   0.4079   0.6579   0.7651   1.0905  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -0.64828    0.94051  -0.689   0.4906    
    ## gender          0.07363    0.14536   0.507   0.6125    
    ## age             0.11073    0.05203   2.128   0.0333 *  
    ## married         0.39468    0.34016   1.160   0.2459    
    ## never_married   0.24415    0.36460   0.670   0.5031    
    ## race            0.51243    0.63168   0.811   0.4172    
    ## education      -0.03171    0.08036  -0.395   0.6932    
    ## children        0.12752    0.11311   1.127   0.2596    
    ## livingstatus    0.10114    0.08372   1.208   0.2271    
    ## insurance      -0.12381    0.05270  -2.349   0.0188 *  
    ## pension_income -0.04056    0.09284  -0.437   0.6622    
    ## location       -0.16845    0.09759  -1.726   0.0843 .  
    ## lonely          0.70438    0.15854   4.443 8.87e-06 ***
    ## smoking        -0.07641    0.07915  -0.965   0.3343    
    ## drink           0.06400    0.07055   0.907   0.3643    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2027.9  on 1910  degrees of freedom
    ## Residual deviance: 1969.5  on 1896  degrees of freedom
    ##   (89 observations deleted due to missingness)
    ## AIC: 1999.5
    ## 
    ## Number of Fisher Scoring iterations: 4

Because a simple linear regression is oftentimes not a feasible method
to measure the effects of different factors on chronic health outcomes,
a better method would be a logistic regression. In this analysis, we
also see that the “location” variable is significant at the 0.1 level,
the “age” and “insurance” variables are significant at the 0.05 level,
and the “lonely” variable is significant at the 0.0001 level.

### Poisson Test

``` r
m3 <- glm(chronic ~ gender + age + married + never_married + race + education + children + livingstatus + insurance + pension_income + location + lonely + smoking + drink, 
          family = "poisson",
               data = data)

summary(m3)
```

    ## 
    ## Call:
    ## glm(formula = chronic ~ gender + age + married + never_married + 
    ##     race + education + children + livingstatus + insurance + 
    ##     pension_income + location + lonely + smoking + drink, family = "poisson", 
    ##     data = data)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.43653   0.06638   0.21825   0.28265   0.44858  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)    -0.601653   0.411906  -1.461   0.1441  
    ## gender          0.017230   0.068772   0.251   0.8022  
    ## age             0.023786   0.023845   0.998   0.3185  
    ## married         0.086344   0.172914   0.499   0.6175  
    ## never_married   0.054415   0.181461   0.300   0.7643  
    ## race            0.098419   0.238166   0.413   0.6794  
    ## education      -0.006587   0.036868  -0.179   0.8582  
    ## children        0.024427   0.050398   0.485   0.6279  
    ## livingstatus    0.022784   0.039014   0.584   0.5592  
    ## insurance      -0.027452   0.025751  -1.066   0.2864  
    ## pension_income -0.011362   0.043697  -0.260   0.7948  
    ## location       -0.038075   0.047054  -0.809   0.4184  
    ## lonely          0.107386   0.052609   2.041   0.0412 *
    ## smoking        -0.016670   0.037672  -0.443   0.6581  
    ## drink           0.014429   0.033335   0.433   0.6651  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 749.07  on 1910  degrees of freedom
    ## Residual deviance: 737.53  on 1896  degrees of freedom
    ##   (89 observations deleted due to missingness)
    ## AIC: 3737.5
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
library(mfx)
```

    ## Loading required package: sandwich

    ## Loading required package: lmtest

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dfidx':
    ## 
    ##     select

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## Loading required package: betareg

``` r
library(sandwich)
library(lmtest)
library(zoo)

poissonmfx(formula = chronic ~ gender + age + married + never_married + race + education + children + livingstatus + insurance + pension_income + location + lonely + smoking + drink, 
           data=data)
```

    ## Call:
    ## poissonmfx(formula = chronic ~ gender + age + married + never_married + 
    ##     race + education + children + livingstatus + insurance + 
    ##     pension_income + location + lonely + smoking + drink, data = data)
    ## 
    ## Marginal Effects:
    ##                     dF/dx  Std. Err.       z   P>|z|  
    ## gender          0.0133212  0.0531017  0.2509 0.80192  
    ## age             0.0184138  0.0184530  0.9979 0.31834  
    ## married         0.0653462  0.1279337  0.5108 0.60950  
    ## never_married   0.0428012  0.1450204  0.2951 0.76789  
    ## race            0.0799781  0.2030002  0.3940 0.69360  
    ## education      -0.0050989  0.0285405 -0.1787 0.85821  
    ## children        0.0189099  0.0390118  0.4847 0.62787  
    ## livingstatus    0.0176376  0.0301981  0.5841 0.55918  
    ## insurance      -0.0212518  0.0199268 -1.0665 0.28620  
    ## pension_income -0.0087960  0.0338264 -0.2600 0.79484  
    ## location       -0.0294749  0.0364181 -0.8093 0.41831  
    ## lonely          0.0831310  0.0406777  2.0436 0.04099 *
    ## smoking        -0.0129048  0.0291611 -0.4425 0.65810  
    ## drink           0.0111696  0.0258039  0.4329 0.66511  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## dF/dx is for discrete change for the following variables:
    ## 
    ## [1] "gender"        "married"       "never_married" "race"

Although a Poisson model would not be too informative for this analysis,
I wanted to explore whether the “lonely” variable was still significant
in this context, which it is.

### Chi-squared Test

``` r
chisq <- chisq.test(data$chronic, data$lonely)
chisq
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  data$chronic and data$lonely
    ## X-squared = 24.348, df = 2, p-value = 5.164e-06

Because the “lonely” variable has consistently shown to be significant,
I wanted to do a chi-squared test between “chronic” and “lonely” to
validate that an elderly person experiencing loneliness is a significant
indicator to whether they are diagnosed by chronic conditions. Because p
\< 0.05, it is in fact significant.

## Conclusion

The research question of this study is to explore the effect of
different factors on developing chronic conditions among the elderly
population in China. The results showed the impact of economic status,
health status, demographic and social characteristics on developing
chronic conditions. The most surprising finding was that loneliness was
a significant indicator of whether an elderly person develops chronic
conditions, pointing towards the shift in Chinese culture where
traditionally, children would eventually become the main caretakers of
their parents. However, the Only-Child Policy in China has impacted this
tradition as parents increasingly encourage their children to pursue
their careers and not worry about their health, causing many children to
not return to their families as often as they would if each family was
still able to raise more children. This policy has proven to be
devastating and burdening on China’s health system as doctors are pushed
to see more patients.

## References

Kennedy, B. K., Berger, S. L., Brunet, A., Campisi, J., Cuervo, A. M.,
Epel, E. S., … & Sierra, F. (2014). Aging: a common driver of chronic
diseases and a target for novel interventions. Cell, 159(4), 709.

Vos, T., Lim, S. S., Abbafati, C., Abbas, K. M., Abbasi, M., Abbasifard,
M., … & Bhutta, Z. A. (2020). Global burden of 369 diseases and injuries
in 204 countries and territories, 1990–2019: a systematic analysis for
the Global Burden of Disease Study 2019. The Lancet, 396(10258),
1204-1222.

Shanghai Bureau of Statistics (2011). Shanghai statistical yearbook.
SSB, Shanghai <http://wwwstats-shgovcn/tjnj/nj11htm>.

## Appendix: All code for this report

``` r
knitr::opts_chunk$set(echo = TRUE)
library(haven)
library(readxl)
library(tidyverse)
library(dplyr)
library(mlogit)

data <- read_excel("elderly_china_data.xlsx")
head(data)
colnames(data)[8] = "insurance"
colnames(data)[9] = "pension_income"
colnames(data)[10] = "income_source"
colnames(data)[15] = "nervous"
colnames(data)[16] = "life_satisfaction"
colnames(data)[18] = "physical_health_change"
colnames(data)[23] = "Heart Disease" 
colnames(data)[28] = "regional_economic_level"
colnames(data)[29] = "outdoor_activity"
colnames(data)[31] = "community_activities"
colnames(data)[32] = "seeing_children"
colnames(data)[36] = "hospitalization"
colnames(data)[37] = "total_chronic"
class(data$gender)

data$gender2 <- factor(ifelse(data$gender == 1, "Male", "Female"))
data <- data |>
  mutate(age2 = factor (
    x = age,
    levels = 1:5,
    labels = c("60_64", "65_69", "70_74", "75_79", ">=80"
  )))

#generate binary chronic variable 
data$chronic <- ifelse(data$total_chronic>=1, 1, 0)

#generate indicator variables for married and never married
data$married <- ifelse(data$marry == 3, 1, 0)
data$never_married <- ifelse(data$marry == 1 & 2, 1, 0)
data |>
  group_by(gender) |>
  summarize(n())

data |>
  group_by(age) |>
  summarize(n())

data |>
  group_by(race) |>
  summarize(n())

data |>
  group_by(education) |>
  summarize(n())

data |>
  group_by(children) |>
  summarize(n())
m1 <- lm(chronic ~ gender + age + married + never_married + race + education + children + livingstatus + insurance + pension_income + location + lonely + smoking + drink, 
         data=data)

summary(m1)
m2 <- glm(chronic ~ gender + age + married + never_married + race + education + children + livingstatus + insurance + pension_income + location + lonely + smoking + drink, 
          family = binomial(link = "logit"),
               data = data)

summary(m2)
m3 <- glm(chronic ~ gender + age + married + never_married + race + education + children + livingstatus + insurance + pension_income + location + lonely + smoking + drink, 
          family = "poisson",
               data = data)

summary(m3)

library(mfx)
library(sandwich)
library(lmtest)
library(zoo)

poissonmfx(formula = chronic ~ gender + age + married + never_married + race + education + children + livingstatus + insurance + pension_income + location + lonely + smoking + drink, 
           data=data)
chisq <- chisq.test(data$chronic, data$lonely)
chisq
```
