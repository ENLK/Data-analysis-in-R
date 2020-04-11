# https://www.kdnuggets.com/2017/09/missing-data-imputation-using-r.html
# https://stats.stackexchange.com/questions/51006/full-information-maximum-likelihood-for-missing-data-in-r
# https://www.theanalysisfactor.com/missing-data-mechanism/
# https://stats.stackexchange.com/questions/45598/type-of-data-missingness-in-r

### https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html
require(dummies)
require(visdat)
require(tidyverse)

# https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html
# https://naniar.njtierney.com/
require(naniar)
require(finalfit)
require(stats)
require(dplyr)
require(mice)
require(forcats)

require(caret)
require(caTools)

store <- read_csv("store.csv")
train <- read_csv("store.csv")

copy <- store

copy$group1 <- as.numeric(copy$PromoInterval == 'Jan,Apr,Jul,Oct')
copy$group2 <- as.numeric(copy$PromoInterval == 'Feb,May,Aug,Nov')
copy$group3 <- as.numeric(copy$PromoInterval == 'Mar,Jun,Sept,Dec')

copy <- subset(copy, select = -c(X11, X12))

# Function to calculate % of missing data 
pMiss <- function(x) {sum(is.na(x)) / length(x)*100}

# Identify columns with NA 
apply(copy, 2, pMiss)

# Patterns

ff_glimpse(copy)

copy %>%
  missing_plot()

as_shadow(copy)

# !NA, NA
NA_copy <- bind_shadow(copy)

# Promo
copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = Promo2SinceYear_NA)) + 
  facet_wrap(~StoreType, ncol = 5) + 
  geom_density(alpha = 0.5) + 
  theme_light()

copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = Promo2SinceWeek_NA)) + 
  facet_wrap(~StoreType, ncol = 5) + 
  geom_density(alpha = 0.5) + 
  theme_light()

copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = PromoInterval_NA)) + 
  facet_wrap(~StoreType, ncol = 5) + 
  geom_density(alpha = 0.5) + 
  theme_light()

# Competition
copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = CompetitionOpenSinceMonth_NA)) +
  geom_density(alpha = 0.5)

copy %>%
  bind_shadow() %>%
  ggplot(aes(x = Store,
             fill = CompetitionOpenSinceYear_NA)) +
  geom_density(alpha = 0.5)

gg_miss_var(copy)

# Check missingness differences by column
missing_pairs(copy, position = 'fill')

copy1 %>%
  summary_factorlist(dependent, explanatory,
                     na_include = TRUE, p = TRUE)

# Data seems to be MAR 
# missingness does differ by variable 

# MCAR = missingness does not differ by variable 

# Confirm
copy$StoreType <- factor(copy$StoreType)
copy$Assortment <- factor(copy$Assortment)

copy$Promofactor<- factor(copy$PromoInterval)
copy$Assortfactor <- factor(copy$Assortment)

copy$PromoYear <- factor(copy$Promo2SinceYear)

copy$StoreType <- factor(copy$StoreType)

explanatory = c('Promofactor')
# explanatory = c('PromoInterval', 'Promo2SinceYear', 'Promo2SinceWeek')
dependent = 'StoreType'

str(copy)

str(copy$Assortfactor)

copy %>%
  missing_pattern(dependent, explanatory)

explanatory = c('CompetitionDistance', 
                'CompetitionOpenSinceMonth', 
                'CompetitionOpenSinceYear',
                'Promo2SinceYear',
                'Promo2SinceWeek')

dependent = 'StoreType'

copy %>%
  select(explanatory) %>%
  MissMech::TestMCARNormality()

copy1$StoreType <- as.factor(copy1$StoreType)

dependent = 'StoreType'

explanatory = c('CompetitionDistance', 
                'CompetitionOpenSinceMonth',
                'CompetitionOpenSinceYear',
                'Assortment')

#https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html

