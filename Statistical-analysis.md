R Notebook
================

``` r
pacman::p_load(tidyverse, 
               FactoMineR,factoextra,
               gridExtra, plotly)

data <- read.csv("real_estate_db.csv")
colnames(data)
```

    ##  [1] "UID"                         "BLOCKID"                    
    ##  [3] "SUMLEVEL"                    "COUNTYID"                   
    ##  [5] "STATEID"                     "state"                      
    ##  [7] "state_ab"                    "city"                       
    ##  [9] "place"                       "type"                       
    ## [11] "primary"                     "zip_code"                   
    ## [13] "area_code"                   "lat"                        
    ## [15] "lng"                         "ALand"                      
    ## [17] "AWater"                      "pop"                        
    ## [19] "male_pop"                    "female_pop"                 
    ## [21] "rent_mean"                   "rent_median"                
    ## [23] "rent_stdev"                  "rent_sample_weight"         
    ## [25] "rent_samples"                "rent_gt_10"                 
    ## [27] "rent_gt_15"                  "rent_gt_20"                 
    ## [29] "rent_gt_25"                  "rent_gt_30"                 
    ## [31] "rent_gt_35"                  "rent_gt_40"                 
    ## [33] "rent_gt_50"                  "universe_samples"           
    ## [35] "used_samples"                "hi_mean"                    
    ## [37] "hi_median"                   "hi_stdev"                   
    ## [39] "hi_sample_weight"            "hi_samples"                 
    ## [41] "family_mean"                 "family_median"              
    ## [43] "family_stdev"                "family_sample_weight"       
    ## [45] "family_samples"              "hc_mortgage_mean"           
    ## [47] "hc_mortgage_median"          "hc_mortgage_stdev"          
    ## [49] "hc_mortgage_sample_weight"   "hc_mortgage_samples"        
    ## [51] "hc_mean"                     "hc_median"                  
    ## [53] "hc_stdev"                    "hc_samples"                 
    ## [55] "hc_sample_weight"            "home_equity_second_mortgage"
    ## [57] "second_mortgage"             "home_equity"                
    ## [59] "debt"                        "second_mortgage_cdf"        
    ## [61] "home_equity_cdf"             "debt_cdf"                   
    ## [63] "hs_degree"                   "hs_degree_male"             
    ## [65] "hs_degree_female"            "male_age_mean"              
    ## [67] "male_age_median"             "male_age_stdev"             
    ## [69] "male_age_sample_weight"      "male_age_samples"           
    ## [71] "female_age_mean"             "female_age_median"          
    ## [73] "female_age_stdev"            "female_age_sample_weight"   
    ## [75] "female_age_samples"          "pct_own"                    
    ## [77] "married"                     "married_snp"                
    ## [79] "separated"                   "divorced"

Given the high dimensionality of the data, then it would be useful to
apply factor analysis to see which variables capture the highest level
of variance in the dataset.

Skip on using summary() on the data since the output would be massively
bulky and unkind to the naked eye.

Find those variables that are numeric first and observe some of their
distributions.

``` r
num <- select_if(data, is.numeric)
colnames(num)
```

    ##  [1] "UID"                         "SUMLEVEL"                   
    ##  [3] "COUNTYID"                    "STATEID"                    
    ##  [5] "zip_code"                    "area_code"                  
    ##  [7] "lat"                         "lng"                        
    ##  [9] "ALand"                       "AWater"                     
    ## [11] "pop"                         "male_pop"                   
    ## [13] "female_pop"                  "rent_mean"                  
    ## [15] "rent_median"                 "rent_stdev"                 
    ## [17] "rent_sample_weight"          "rent_samples"               
    ## [19] "rent_gt_10"                  "rent_gt_15"                 
    ## [21] "rent_gt_20"                  "rent_gt_25"                 
    ## [23] "rent_gt_30"                  "rent_gt_35"                 
    ## [25] "rent_gt_40"                  "rent_gt_50"                 
    ## [27] "universe_samples"            "used_samples"               
    ## [29] "hi_mean"                     "hi_median"                  
    ## [31] "hi_stdev"                    "hi_sample_weight"           
    ## [33] "hi_samples"                  "family_mean"                
    ## [35] "family_median"               "family_stdev"               
    ## [37] "family_sample_weight"        "family_samples"             
    ## [39] "hc_mortgage_mean"            "hc_mortgage_median"         
    ## [41] "hc_mortgage_stdev"           "hc_mortgage_sample_weight"  
    ## [43] "hc_mortgage_samples"         "hc_mean"                    
    ## [45] "hc_median"                   "hc_stdev"                   
    ## [47] "hc_samples"                  "hc_sample_weight"           
    ## [49] "home_equity_second_mortgage" "second_mortgage"            
    ## [51] "home_equity"                 "debt"                       
    ## [53] "second_mortgage_cdf"         "home_equity_cdf"            
    ## [55] "debt_cdf"                    "hs_degree"                  
    ## [57] "hs_degree_male"              "hs_degree_female"           
    ## [59] "male_age_mean"               "male_age_median"            
    ## [61] "male_age_stdev"              "male_age_sample_weight"     
    ## [63] "male_age_samples"            "female_age_mean"            
    ## [65] "female_age_median"           "female_age_stdev"           
    ## [67] "female_age_sample_weight"    "female_age_samples"         
    ## [69] "pct_own"                     "married"                    
    ## [71] "married_snp"                 "separated"                  
    ## [73] "divorced"

``` r
# Function to calculate % of missing data 
pMiss <- function(x) {sum(is.na(x))/length(x)*100}

# Identify columns with NA 
apply(num, 2, pMiss)
```

    ##                         UID                    SUMLEVEL 
    ##                   0.0000000                   0.0000000 
    ##                    COUNTYID                     STATEID 
    ##                   0.0000000                   0.0000000 
    ##                    zip_code                   area_code 
    ##                   0.0000000                   0.0000000 
    ##                         lat                         lng 
    ##                   0.0000000                   0.0000000 
    ##                       ALand                      AWater 
    ##                   0.0000000                   0.0000000 
    ##                         pop                    male_pop 
    ##                   0.0000000                   0.0000000 
    ##                  female_pop                   rent_mean 
    ##                   0.0000000                   1.1837048 
    ##                 rent_median                  rent_stdev 
    ##                   1.1837048                   1.1837048 
    ##          rent_sample_weight                rent_samples 
    ##                   1.1837048                   1.1837048 
    ##                  rent_gt_10                  rent_gt_15 
    ##                   1.1862670                   1.1862670 
    ##                  rent_gt_20                  rent_gt_25 
    ##                   1.1862670                   1.1862670 
    ##                  rent_gt_30                  rent_gt_35 
    ##                   1.1862670                   1.1862670 
    ##                  rent_gt_40                  rent_gt_50 
    ##                   1.1862670                   1.1862670 
    ##            universe_samples                used_samples 
    ##                   0.0000000                   0.0000000 
    ##                     hi_mean                   hi_median 
    ##                   0.9992314                   0.9992314 
    ##                    hi_stdev            hi_sample_weight 
    ##                   0.9992314                   0.9992314 
    ##                  hi_samples                 family_mean 
    ##                   0.9992314                   1.1119652 
    ##               family_median                family_stdev 
    ##                   1.1119652                   1.1119652 
    ##        family_sample_weight              family_samples 
    ##                   1.1119652                   1.1119652 
    ##            hc_mortgage_mean          hc_mortgage_median 
    ##                   2.1547528                   2.1547528 
    ##           hc_mortgage_stdev   hc_mortgage_sample_weight 
    ##                   2.1547528                   2.1547528 
    ##         hc_mortgage_samples                     hc_mean 
    ##                   2.1547528                   2.2802972 
    ##                   hc_median                    hc_stdev 
    ##                   2.2802972                   2.2802972 
    ##                  hc_samples            hc_sample_weight 
    ##                   2.2802972                   2.2802972 
    ## home_equity_second_mortgage             second_mortgage 
    ##                   1.7345632                   1.7345632 
    ##                 home_equity                        debt 
    ##                   1.7345632                   1.7345632 
    ##         second_mortgage_cdf             home_equity_cdf 
    ##                   1.7345632                   1.7345632 
    ##                    debt_cdf                   hs_degree 
    ##                   1.7345632                   0.7045862 
    ##              hs_degree_male            hs_degree_female 
    ##                   0.7404561                   0.8403792 
    ##               male_age_mean             male_age_median 
    ##                   0.6994620                   0.6994620 
    ##              male_age_stdev      male_age_sample_weight 
    ##                   0.6994620                   0.6994620 
    ##            male_age_samples             female_age_mean 
    ##                   0.6994620                   0.7737638 
    ##           female_age_median            female_age_stdev 
    ##                   0.7737638                   0.7737638 
    ##    female_age_sample_weight          female_age_samples 
    ##                   0.7737638                   0.7737638 
    ##                     pct_own                     married 
    ##                   0.9992314                   0.7045862 
    ##                 married_snp                   separated 
    ##                   0.7045862                   0.7045862 
    ##                    divorced 
    ##                   0.7045862

## filter out the missing observations on variables of interest

``` r
f1 <- num %>%
  filter(!is.na(rent_mean)) %>%
  filter(!is.na(debt)) %>%
  filter(!is.na(male_age_mean)) %>%
  filter(!is.na(female_age_mean))
```

``` r
x <- f1[, c("rent_mean", "debt",
            "male_age_mean", "female_age_mean")]

summary(x)
```

    ##    rent_mean           debt        male_age_mean   female_age_mean
    ##  Min.   : 159.1   Min.   :0.0000   Min.   :16.14   Min.   :18.68  
    ##  1st Qu.: 742.7   1st Qu.:0.5393   1st Qu.:35.06   1st Qu.:36.93  
    ##  Median : 952.5   Median :0.6487   Median :38.32   Median :40.36  
    ##  Mean   :1054.2   Mean   :0.6299   Mean   :38.35   Mean   :40.33  
    ##  3rd Qu.:1259.0   3rd Qu.:0.7383   3rd Qu.:41.35   3rd Qu.:43.56  
    ##  Max.   :3962.3   Max.   :1.0000   Max.   :80.11   Max.   :79.84

## plot the complete data

``` r
x <- ggplot(f1, aes(x = rent_mean)) + 
  geom_histogram(aes(y = ..density..),
                 bins = 50, fill = "turquoise") + 
  stat_function(fun = dnorm, 
                color = "black",
                args = list(mean = mean(f1$rent_mean),
                            sd = sd(f1$rent_mean))) +
  theme_light() +
  labs(x = "rent", y = "freq")

y <- ggplot(f1, aes(x = debt)) +
  geom_histogram(aes(y = ..density..),
                 bins = 50, fill = "tomato3") + 
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = mean(f1$debt),
                            sd = sd(f1$debt))) + 
  theme_light() + 
  labs(x = "debt", y = "freq")

grid.arrange(x, y)
```

![](Statistical-analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
w <- ggplot(f1, aes(x = female_age_mean))+ 
  geom_histogram(aes(y = ..density..),
                 bins = 50,
                 fill = "orchid") + 
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = mean(f1$female_age_mean),
                            sd = sd(f1$female_age_mean))) + 
  labs(x = "female age",
       y = "freq")

z <- ggplot(f1, aes(x = male_age_mean))+ 
  geom_histogram(aes(y = ..density..),
                 bins = 50,
                 fill = "gold2") + 
  stat_function(fun = dnorm,
                color = "black",
                args = list(mean = mean(f1$male_age_mean),
                            sd = sd(f1$male_age_mean))) + 
  labs(x = "male age",
       y = "freq")

grid.arrange(w, z)
```

![](Statistical-analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
sums <- num %>% select(rent_mean, debt, 
                        female_age_mean,
                        male_age_mean) %>%
  filter(!is.na(rent_mean), !is.na(debt),
         !is.na(female_age_mean),
         !is.na(male_age_mean))

#do.call(cbind, lapply(sums, summary))
lapply(sums, summary)
```

    ## $rent_mean
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   159.1   742.7   952.5  1054.2  1259.0  3962.3 
    ## 
    ## $debt
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.5393  0.6487  0.6299  0.7383  1.0000 
    ## 
    ## $female_age_mean
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   18.68   36.93   40.36   40.33   43.56   79.84 
    ## 
    ## $male_age_mean
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16.14   35.06   38.32   38.35   41.35   80.11

## means and medians

``` r
par(mfrow = c(2, 1))
# debt

hist(sums$debt,
           col = "tomato2", 
           xlab = "debt",
           main = "Debt distribution") %>%
abline(v = mean(sums$debt),
       col = "slateblue") %>%
abline(v = median(sums$debt),
       col = "black")

# rent
hist(sums$rent_mean,
           col = "cyan", xlab = "rent",
           main = "rent distribution") %>%
abline(v = mean(sums$rent_mean),
       col = "red") %>%
abline(v = median(sums$rent_mean),
       col = "black") 
```

![](Statistical-analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
rent_type <- data[, c("rent_mean", "type")] %>%
  filter(!is.na("rent_mean"), 
         !is.na("type"))


ggplot(rent_type, 
       aes(x = type, 
       y = rent_mean)) +
  geom_boxplot(fill = "gray",
               color = "black",
               outlier.color = "tomato", 
               outlier.shape = 2) + 
  theme_light()
```

    ## Warning: Removed 462 rows containing non-finite values (stat_boxplot).

![](Statistical-analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Confidence interval plots 
fm_type <- data[, c("female_age_mean",
                    "male_age_mean",
                    "type")] %>%
  filter(!is.na("female_age_mean"), !is.na("male_age_mean"), !is.na("type"))

x2 <- ggplot(fm_type, aes(x = type, 
                          y = female_age_mean)) +
  stat_summary(fun.data = mean_cl_normal,
               color = "tomato") + 
  theme_light() + 
  labs(title = "Average female age by zone type (Confidence interval)",
       x = "type",
       y = "age")
  
y2 <- ggplot(fm_type, aes(x = type, 
                          y = male_age_mean)) +
  stat_summary(fun.data = mean_cl_normal,
               color = "dodgerblue") + 
  theme_light() + 
  labs(title = "Average male age by zone type (Confidence interval)",
       x = "type",
       y = "age")
  
grid.arrange(x2, y2)
```

    ## Warning: Removed 302 rows containing non-finite values (stat_summary).

    ## Warning: Removed 273 rows containing non-finite values (stat_summary).

![](Statistical-analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Inferential statistics

  - Confidence intervals = how certain a value exist between two points
  - P-value = probability of obtaining extreme results, provided the
    null hypothesis is true. High p-value = strong evidence against null
    hypothesis.
  - significance level = P(rejecting null hypothesis).

Calculate 95% confidence interval for population mean

``` r
# sample size 
n <- sums %>% select(male_age_mean) %>%
  nrow()

# sd
male_std <- sums %>% summarize(std = sd(male_age_mean))

# 
x_bar <- sums %>% summarize(avg = mean(male_age_mean))

standard_error <- -qnorm(0.025)*male_std/sqrt(n)

# lower and upper
low <- x_bar - standard_error
high <- x_bar + standard_error

# female sample size

nf <- sums %>% select(female_age_mean) %>%
  nrow()

female_std <- sums %>%  filter(!is.na(female_age_mean)) %>% summarize(std = sd(female_age_mean))

x_bar <- sums %>%  filter(!is.na(female_age_mean)) %>%
  summarize(avg = mean(female_age_mean))

standard_error <- -qnorm(0.025)*female_std/sqrt(nf)

# lower and upper
low <- x_bar - standard_error
high <- x_bar + standard_error
```

## 95% of random samples from 38, 283 (male and female) will have confidence intervals that have the true population age mean of male and females between ‘high’ and ‘low’

## H0: the true population age mean of females is within the high and low range

## H1: true population age mean of females is NOT within the high and low

range

``` r
set.seed(42)
f_age <- data %>% select(female_age_mean) %>%
  filter(!is.na(female_age_mean))

mu <- mean(f_age$female_age_mean)
n <- 100

x_bar <- mean(sample(f_age$female_age_mean,
                     n))

std <- sd(sample(f_age$female_age_mean,
                 n))

# 95% CI 
st_error <- qnorm(0.025)*std/sqrt(n)

# Z-score
z_score <- (x_bar - mu)/st_error
print(paste("Z-score:", z_score))
```

    ## [1] "Z-score: -0.029448200784584"

``` r
# P-value (one & two tailed)
two_tail_pval <- 2*pnorm(-abs(z_score))
one_tail_pval <- two_tail_pval / 2

print(paste0("Two tailed p-value:", two_tail_pval))
```

    ## [1] "Two tailed p-value:0.97650713078873"

``` r
print(paste0("One tailed p-value:", one_tail_pval))
```

    ## [1] "One tailed p-value:0.488253565394365"

## Insufficient evidence to reject H0

## Contigency tables are used here to summarize the association between various variables

  - find if two variables are independent or dependent i.e. their
    relationship.
  - independence is a condition for the central limit theorem.
  - Central limit theorem = increasing sample size results in the sample
    mean approximating a normal distribution.
  - Increasing sample size causes the measured sample means to be more
    closely distributed around the population mean.

<!-- end list -->

``` r
# Chi-squared test of independence on categorical variables 
cats <- select_if(data, is.factor)

colnames(cats)
```

    ## [1] "state"    "state_ab" "city"     "place"    "type"     "primary"

``` r
tab <- table(cats$type, cats$state)
#summary(tab)
chisq.test(tab)
```

    ## Warning in chisq.test(tab): Chi-squared approximation may be incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tab
    ## X-squared = 195150, df = 255, p-value < 2.2e-16

## p-val is less than 0.05, indicating a strong association

``` r
# Chisq test by unique observation in each variable  
contingency_tab <- cats %>%
  select(state, type) %>%
  table() %>% 
  prop.table() %>%
  round(4)

contingency_tab
```

    ##                       type
    ## state                  Borough    CDP   City   Town  Urban Village
    ##   Alabama               0.0000 0.0000 0.0000 0.0157 0.0000  0.0000
    ##   Alaska                0.0000 0.0000 0.0027 0.0000 0.0000  0.0000
    ##   Arizona               0.0000 0.0204 0.0000 0.0000 0.0000  0.0000
    ##   Arkansas              0.0000 0.0000 0.0093 0.0000 0.0000  0.0000
    ##   California            0.0000 0.0000 0.1074 0.0000 0.0000  0.0000
    ##   Colorado              0.0000 0.0000 0.0171 0.0000 0.0000  0.0000
    ##   Connecticut           0.0000 0.0114 0.0000 0.0000 0.0000  0.0000
    ##   Delaware              0.0000 0.0000 0.0000 0.0028 0.0000  0.0000
    ##   District of Columbia  0.0000 0.0000 0.0025 0.0000 0.0000  0.0000
    ##   Florida               0.0000 0.0000 0.0586 0.0000 0.0000  0.0000
    ##   Georgia               0.0000 0.0000 0.0276 0.0000 0.0000  0.0000
    ##   Hawaii                0.0000 0.0045 0.0000 0.0000 0.0000  0.0000
    ##   Idaho                 0.0000 0.0000 0.0038 0.0000 0.0000  0.0000
    ##   Illinois              0.0000 0.0000 0.0000 0.0000 0.0000  0.0408
    ##   Indiana               0.0000 0.0000 0.0205 0.0000 0.0000  0.0000
    ##   Iowa                  0.0000 0.0000 0.0106 0.0000 0.0000  0.0000
    ##   Kansas                0.0000 0.0000 0.0113 0.0000 0.0000  0.0000
    ##   Kentucky              0.0000 0.0000 0.0148 0.0000 0.0000  0.0000
    ##   Louisiana             0.0000 0.0000 0.0156 0.0000 0.0000  0.0000
    ##   Maine                 0.0000 0.0000 0.0049 0.0000 0.0000  0.0000
    ##   Maryland              0.0000 0.0196 0.0000 0.0000 0.0000  0.0000
    ##   Massachusetts         0.0000 0.0000 0.0199 0.0000 0.0000  0.0000
    ##   Michigan              0.0000 0.0375 0.0000 0.0000 0.0000  0.0000
    ##   Minnesota             0.0000 0.0000 0.0179 0.0000 0.0000  0.0000
    ##   Mississippi           0.0000 0.0090 0.0000 0.0000 0.0000  0.0000
    ##   Missouri              0.0000 0.0000 0.0188 0.0000 0.0000  0.0000
    ##   Montana               0.0000 0.0041 0.0000 0.0000 0.0000  0.0000
    ##   Nebraska              0.0000 0.0000 0.0000 0.0000 0.0000  0.0070
    ##   Nevada                0.0000 0.0000 0.0091 0.0000 0.0000  0.0000
    ##   New Hampshire         0.0000 0.0040 0.0000 0.0000 0.0000  0.0000
    ##   New Jersey            0.0000 0.0000 0.0259 0.0000 0.0000  0.0000
    ##   New Mexico            0.0000 0.0000 0.0074 0.0000 0.0000  0.0000
    ##   New York              0.0000 0.0000 0.0657 0.0000 0.0000  0.0000
    ##   North Carolina        0.0000 0.0000 0.0000 0.0000 0.0000  0.0299
    ##   North Dakota          0.0000 0.0000 0.0028 0.0000 0.0000  0.0000
    ##   Ohio                  0.0000 0.0000 0.0000 0.0000 0.0000  0.0394
    ##   Oklahoma              0.0000 0.0141 0.0000 0.0000 0.0000  0.0000
    ##   Oregon                0.0000 0.0000 0.0116 0.0000 0.0000  0.0000
    ##   Pennsylvania          0.0445 0.0000 0.0000 0.0000 0.0000  0.0000
    ##   Puerto Rico           0.0000 0.0000 0.0000 0.0000 0.0125  0.0000
    ##   Rhode Island          0.0000 0.0033 0.0000 0.0000 0.0000  0.0000
    ##   South Carolina        0.0000 0.0000 0.0144 0.0000 0.0000  0.0000
    ##   South Dakota          0.0000 0.0033 0.0000 0.0000 0.0000  0.0000
    ##   Tennessee             0.0000 0.0000 0.0203 0.0000 0.0000  0.0000
    ##   Texas                 0.0000 0.0000 0.0000 0.0709 0.0000  0.0000
    ##   Utah                  0.0000 0.0000 0.0085 0.0000 0.0000  0.0000
    ##   Vermont               0.0000 0.0024 0.0000 0.0000 0.0000  0.0000
    ##   Virginia              0.0000 0.0000 0.0000 0.0258 0.0000  0.0000
    ##   Washington            0.0000 0.0000 0.0000 0.0207 0.0000  0.0000
    ##   West Virginia         0.0000 0.0000 0.0068 0.0000 0.0000  0.0000
    ##   Wisconsin             0.0000 0.0000 0.0188 0.0000 0.0000  0.0000
    ##   Wyoming               0.0000 0.0000 0.0019 0.0000 0.0000  0.0000

## T-distribution

  - Used for experimenting with small sample sizes
  - n \< 30

<!-- end list -->

``` r
states <- data %>% select(state, rent_mean) %>%
  filter(state == "Texas" | state == "Georgia") %>%
  filter(!is.na(rent_mean))

set.seed(42)
n <- 21
sample <- sample_n(states, n)

Tex <- sample %>%
  filter(state == "Texas")

# Texas test 
Tex_n <- Tex %>%
  nrow()

Tex_x_bar <- Tex %>% summarize(avg = mean(rent_mean))

t_score <- abs(qt(0.025, df = 11))

Tex_sd <- Tex %>%
  summarize(sd = sd(rent_mean))

upper <- Tex_x_bar + t_score * (Tex_sd / sqrt(Tex_n))
lower <- Tex_x_bar - t_score * (Tex_sd / sqrt(Tex_n))
upper
```

    ##        avg
    ## 1 1262.897

``` r
lower
```

    ##        avg
    ## 1 902.1304

## 95% sure the average rent for Texas is between the lower and upper bounds

## Assume the average rent for Georgia is 888

  - Find the p-value i.e. amount of evidence to reject null hypothesis

<!-- end list -->

``` r
Geo <- states %>% filter(state == "Georgia")

Geo_n <- Geo %>%
  nrow()

Geo_x_bar <- Geo %>% summarize(avg = mean(rent_mean))

Geo_sd <- Geo %>% summarize(sd = sd(rent_mean))

standard_error <- Geo_sd / sqrt(Geo_n)

mu = 888
t_score <- (Geo_x_bar - mu) / standard_error
print(paste0(t_score))
```

    ## [1] "6.37136760547347"

``` r
2*pt(6.371, df = 11, lower.tail = FALSE)
```

    ## [1] 5.288613e-05

## p-value \< 0.05, sufficient evidence to reject null hypothesis

## Difference between two means of independent groups

  - Independent groups = 2 separate subject groups
  - Assumes 2 groups (populations) have homogeneity of variance
  - The groups are normally distributed
  - Each value is sampled independently

<!-- end list -->

``` r
states <- data %>%
  filter(state == "New York" | state == "Texas") %>%
  filter(!is.na(rent_mean))

ggplot(states, aes(x=state, y = rent_mean)) + 
  geom_boxplot()+
  stat_summary(fun.y = mean,
               color = "red",
               geom = "point",
               size = 2) + 
  theme_light() +
  labs(x = "State", y = "rent")
```

    ## Warning: `fun.y` is deprecated. Use `fun` instead.

![](Statistical-analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

  - H0 = no difference between average rent in NY and Texas
  - H1 = difference exists

<!-- end list -->

``` r
# p-value

NY_n <- states %>%
  filter(state == "New York") %>%
  nrow()

NY_x_bar <- states %>% filter(state == "New York") %>%
  summarize(avg = mean(rent_mean))

NY_sd <- states %>% filter(state == "New York") %>%
  summarize(sd = sd(rent_mean))

# standard error
Tex_se <- Tex_sd/sqrt(Tex_n)
NY_se <- NY_sd/sqrt(NY_n)

standard_error <- sqrt(Tex_se)^2 + sqrt(NY_se)^2

# Point estimate
p_est <- (Tex_x_bar - NY_x_bar)

t_score <- abs((p_est - 0)/standard_error)
t_score <- as.numeric(t_score)

p_val <- 2*pt(t_score, df = 21, lower.tail = FALSE)
p_val
```

    ## [1] 0.07127319

## No significant evidence available to reject null hypothesis (no difference between average rent in Texas and New York)

## ANOVA tests

  - H0 = mean of rent is the same across the sampled group of states
  - H1 = at least 2 states are different from each other

<!-- end list -->

``` r
state4 <- data %>%
  select(state, rent_mean) %>%
  group_by(state) %>%
  filter(!is.na(rent_mean)) %>%
  filter(state == "New York"| state == "Texas" | state == "Washington" | state == "California") %>%
  summarize(avg = mean(rent_mean), sd = sd(rent_mean), count = n())

state4
```

    ## # A tibble: 4 x 4
    ##   state        avg    sd count
    ##   <fct>      <dbl> <dbl> <int>
    ## 1 California 1468.  485.  4151
    ## 2 New York   1256.  477.  2511
    ## 3 Texas       988.  368.  2741
    ## 4 Washington 1132.  331.   805

``` r
state4 <- data %>%
  select(state, rent_mean) %>%
  filter(!is.na(rent_mean)) %>%
  filter(state == "New York"| state == "Texas" | state == "Washington" | state == "California") %>%
  group_by(state)

ggplot(state4, aes(x = state, y = rent_mean,
                   fill = state)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               color = "red",
               geom = "point",
               size = 2) + 
  labs(x = "States", y = "Rent") + 
  theme_light()
```

    ## Warning: `fun.y` is deprecated. Use `fun` instead.

![](Statistical-analysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
ungroup4 <- state4 %>%
  ungroup()

anova_state4 <- aov(rent_mean ~ state,
                    data = ungroup4)

summary(anova_state4)
```

    ##                Df    Sum Sq   Mean Sq F value Pr(>F)    
    ## state           3 3.963e+08 132113437   672.2 <2e-16 ***
    ## Residuals   10204 2.005e+09    196539                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Significant evidence to believe that the mean rent is different for at least 2 states.
