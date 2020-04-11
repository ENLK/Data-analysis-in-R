require(tidyverse)
require(lubridate)
require(zoo)
require(forecast)
require(data.table)

train = fread('train.csv')
test = fread('test.csv')
store = fread('store.csv')

store_zeros = sort(tapply(train$Sales,
                          list(train$Store),
                          function(x) sum(x == 0)))

tail(store_zeros, 10) # Highest numbers of 0s 
head(store_zeros, 10)

train <- train[order(Date)]
test <- test[order(Date)]

# No 0s 
plot(train[Store == 85, Sales],
     ylab = "Sales", xlab = "Days", 
     main = "Store 85")

plot(train[Store == 262, Sales],
     ylab = "Sales", xlab = "Days", 
     main = "Store 262")

# Most 0s
plot(train[Store == 105, Sales],
     ylab = "Sales", xlab = "Days", 
     main = "Store 105")

plot(train[Store == 339, Sales], 
     ylab = "Sales", xlab = "Days", 
     main = "Store 339")

plot(train[Store == 837, Sales], 
     ylab = "Sales", xlab = "Days", 
     main = "Store 837")

# Some stores are never closed (opened on sundays + holidays)

# Sundays 
ggplot(train[Store == 85], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), 
           shape = factor(DayOfWeek == 7))) + 
  geom_point(size = 3) + 
  ggtitle("Store 85 Sales")

ggplot(train[Store == 262], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), 
           shape = factor(DayOfWeek == 7))) + 
  geom_point(size = 3) + 
  ggtitle("Store 262 Sales")

ggplot(train[Store == 335], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), 
           shape = factor(DayOfWeek == 7))) + 
  geom_point(size = 3) + 
  ggtitle("Store 335 Sales")

# Non Sundays 
ggplot(train[Store == 339], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), 
           shape = factor(DayOfWeek == 7))) + 
  geom_point(size = 3) + 
  ggtitle("Store 339 Sales")

ggplot(train[Store == 837], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), 
           shape = factor(DayOfWeek == 7))) + 
  geom_point(size = 3) + 
  ggtitle("Store 837 Sales")

# Combine 
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, 
                                               sep = "-"))

train_store <- merge(train, store, by = "Store")

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
  geom_smooth(size = 2) + 
  theme_light()

ggplot(train_store[Customers != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(StoreType))) + 
  geom_smooth(size = 2) + 
  theme_light()

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + 
  geom_smooth(size = 2) + 
  theme_light()

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(Assortment))) + 
  geom_smooth(size = 2) + 
  theme_light()

ggplot(train_store[Sales != 0],
       aes(x = factor(!is.na(CompetitionOpenSinceYear)), y = Sales)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "orchid3", outlier.colour = NA, fill = NA) +
  ggtitle("Competition presence") + 
  theme_light()

# CompetitionDistance 

Dist <- aggregate(train_store[Sales != 0 & !is.na(CompetitionDistance)]$Sales, 
                         by = list(train_store[Sales != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), 
                  mean)

colnames(Dist) <- c("CompetitionDistance", "Mean")

ggplot(Dist, 
       aes(x = CompetitionDistance, y = Mean)) + 
  geom_point() + 
  geom_smooth() + 
  theme_light()

train_store$DateYearmon <- as.yearmon(train_store$Date)
train_store <- train_store[order(Date)]

timespan <- 150 # Days before and after competition opens
timespan1 <- 100

# Before and after competition  
beforeAndAfterComp <- function(s) {
  x <- train_store[Store == s]
  daysWithComp <- x$CompetitionOpenSince >= x$DateYearmon
  if (any(!daysWithComp)) {
    compOpening <- head(which(!daysWithComp), 1) - 1
    if (compOpening > timespan & compOpening < (nrow(x) - timespan)) {
      x <- x[(compOpening - timespan):(compOpening + timespan), ] 
      x$Day <- 1:nrow(x)
      return(x)
    }
  }
}

beforeAndAfterComp1 <- function(s) {
  x <- train_store[Store == s]
  daysWithComp <- x$CompetitionOpenSince >= x$DateYearmon
  if (any(!daysWithComp)) {
    compOpening <- head(which(!daysWithComp), 1) - 1
    if (compOpening > timespan1 & compOpening < (nrow(x) - timespan1)) {
      x <- x[(compOpening - timespan1):(compOpening + timespan1), ] 
      x$Day <- 1:nrow(x)
      return(x)
    }
  }
}

temp <- lapply(unique(train_store[!is.na(CompetitionOpenSince)]$Store), 
               beforeAndAfterComp)
temp <- do.call(rbind, temp)

temp1 <- lapply(unique(train_store[!is.na(CompetitionOpenSince)]$Store), 
               beforeAndAfterComp1)

temp1 <- do.call(rbind, temp1)

# number of stores with competitors entering during timespan
# stores had no competitors for at least -timespan- days before final date 
length(unique(temp$Store))

ggplot(temp[Sales != 0], aes(x = Day, y = Sales)) + 
  geom_smooth() + 
  ggtitle(paste('Effect of competition opening (150 days)')) +
  theme_light()

length(unique(temp1$Store))

ggplot(temp1[Sales != 0], aes(x = Day, y = Sales)) + 
  geom_smooth() + 
  ggtitle(paste('Effect of competition opening (100 days)')) +
  theme_light()

# Add avg sales and customers 
store <- left_join(store, 
                   summarize(group_by(train[train$Open == 1,], 
                                      Store),
                             Avg_Sales = mean(Sales, na.rm = TRUE),
                             Avg_CM = mean(Customers, na.rm = TRUE)),
                   'Store')

store$CompDistBin <- ntile(store$CompetitionDistance, 5)
store$CompDistBin <- paste(store$CompDistBin, "quintile")
store$CompDistBin[store$CompDistBin == "NA quintile"] <- NA

store$CompOpenDate <- ymd(paste(store$CompetitionOpenSinceYear
                             , store$CompetitionOpenSinceMonth
                             , "01"))

store$CompOpenDate <- as.Date(store$CompOpenDate)

store <- select(store
             , Store
             , StoreType
             , Assortment
             , CompDist = CompetitionDistance
             , CompDistBin
             , CompOpenDate
             , Avg_Sales
             , Avg_CM)

store$Avg_Sales_CM <- store$Avg_Sales / store$Avg_CM

store$CompDays <- as.numeric(as.Date("2015-09-17") - store$CompOpenDate)

ggplot(store ,aes(CompOpenDate, fill = StoreType)) + 
  stat_bin(binwidth = 365) +
  scale_x_date("Date of Competition Opening") +
  scale_y_continuous("# of Stores") + 
  theme_light()

lbrk <- c(100, 1000, 10000)
ggplot(store, aes(CompDist, fill = StoreType)) + 
  geom_density(adjust = .7, alpha = .5) +
  scale_x_log10("Competition Distance", 
                breaks = lbrk) +
  scale_y_continuous("# of Stores") +
  theme_light()

int_TilePlot <- function(data, response, x, y
                         , r_lab = response, x_lab = x, y_lab = y
                         , r_scale = comma, x_scale = comma, y_scale = comma
                         , bins = 10, exp = 8){
  library(ggplot2)
  library(scales)
  x_brk <- seq(min(data[[x]]), max(data[[x]]), length.out = bins)
  y_brk <- seq(min(data[[y]]), max(data[[y]]), length.out = bins)
  df <- data.frame(x_val = rep(x_brk, bins)
                   , y_val = rep(y_brk, each = bins)
                   , r_val = NA)
  df$x_z <- (df$x_val - mean(data[[x]])) / sd(data[[x]])
  df$y_z <- (df$y_val - mean(data[[y]])) / sd(data[[y]])
  data$x_z <- (data[[x]] - mean(data[[x]])) / sd(data[[x]])
  data$y_z <- (data[[y]] - mean(data[[y]])) / sd(data[[y]])
  for (i in 1:nrow(df)){
    dist <- numeric()
    for (j in 1:nrow(data)){
      dist <- append(dist, sqrt(abs(df$x_z[i] - data$x_z[j])^2
                                + abs(df$y_z[i] - data$y_z[j])^2))
    }
    adjDist <- abs(dist - max(dist))^exp
    wtdResp <- sum(adjDist * data[[response]]) / sum(adjDist)
    df$r_val[i] <- wtdResp
  }
  ggplot(df, aes(x = x_val, y = y_val, z = r_val)) +
    geom_tile(aes(fill = r_val)) +
    stat_contour(bins = 15) +
    scale_x_continuous(x_lab, labels = x_scale) +
    scale_y_continuous(y_lab, labels = y_scale) +
    scale_fill_continuous(r_lab, labels = r_scale) +
    theme_light()
}

int_TilePlot(store[!is.na(store$CompDist),]
             , "CompDist", "Avg_Sales_CM", "Avg_CM"
             , "Competition Distance"
             , "Average Sales per Customer"
             , "Average # of Customers"
             , comma, comma)

train <- left_join(train, 
                   select(store, 
                          -Avg_Sales_CM, -Avg_Sales, -Avg_CM),
                          'Store')

cdf <-  train[!is.na(train$CompOpenDate)
           & train$CompOpenDate >= as.Date("2013-01-01")
           & train$CompOpenDate <= as.Date("2015-07-31")
           & !is.na(train$Sales)
           & train$Sales != 0, ]

cdf$Avg_Sales_Customers <- cdf$Sales / cdf$Customers

cdf$IsCompOpen <- ifelse(cdf$Date >= as.Date(cdf$CompOpenDate), 1, 0)
cdf$IsCompOpen[!is.na(cdf$CompDist) & is.na(cdf$CompOpenDate)] <- 1
cdf$IsCompOpen[is.na(cdf$CompDist)] <- NA

cdf$DaysSinceNewComp <- as.numeric(cdf$Date - cdf$CompOpenDate)
cdf <- cdf[abs(cdf$DaysSinceNewComp) < 365,]

ggplot(cdf, aes(CompOpenDate)) +
  stat_bin(binwidth = 7) +
  scale_x_date('Competition Open Date') +
  scale_y_continuous('Count of New Competition Observations') + 
  theme_light()

ggplot(cdf, aes(DaysSinceNewComp, Customers
               , col = as.factor(IsCompOpen))) +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE, span = .5, size = 1.5, col = "gray50") +
  scale_x_continuous("Days before and after new competition") +
  scale_y_continuous("Daily customers") +
  scale_color_discrete("Is competition open?") + 
  theme_light()

# LM

str(cdf)

m1 <- lm(Avg_Sales_Customers ~ Promo +
           as.factor(SchoolHoliday) +
           as.factor(StoreType) + 
           as.factor(Assortment) + 
           as.factor(DayOfWeek), 
         cdf)

m2 <- lm(Avg_Sales_Customers ~ Promo +
           as.factor(SchoolHoliday) +
           as.factor(StoreType) + 
           as.factor(Assortment) + 
           as.factor(DayOfWeek) + 
           CompDist + 
           CompOpenDate + 
           CompDays +
           DaysSinceNewComp, 
         cdf)

cdf$m1_prediction <- predict(m1)
cdf$m1_res <- cdf$Avg_Sales_Customers - predict(m1)

cdf$m2_prediction <- predict(m1)
cdf$m2_res <- cdf$Avg_Sales_Customers - predict(m1)

cdf_res <- summarize(group_by(cdf, DaysSinceNewComp)
                 , NumObs = length(Date)
                 , IsCompOpen = max(IsCompOpen)
                 , MedResSales_Customers = median(m1_res)
                 , MedResComp = median(m2_res))

ggplot(cdf_res, aes(DaysSinceNewComp, MedResSales_Customers
                , col = as.factor(IsCompOpen))) +
  geom_smooth(se = FALSE, size = 1.5) +
  scale_x_continuous("Days before and after new competition") +
  scale_y_continuous("Avg Sales by customers residuals") +
  scale_color_discrete("Is competition open?") +
  geom_hline(yintercept = 0, lty = 2, col = "gray20") +
  geom_vline(xintercept = 0, lty = 2, col = "gray20") + 
  theme_light()

cdf_res1 <- summarize(group_by(cdf, 
                              DaysSinceNewComp, CompDistBin)
                     , NumObs = length(Date)
                     , IsCompOpen = max(IsCompOpen)
                     , MedResSales_Customers = median(m1_res)
                     , MedResComp = median(m2_res))

require(RColorBrewer)

ggplot(cdf_res1, aes(DaysSinceNewComp, MedResSales_Customers
                , col = as.factor(CompDistBin))) +
  stat_smooth(se = FALSE, size = 1.5) +
  scale_x_continuous("Days before and after new competition") +
  scale_y_continuous("Avg Sales by customers residuals") +
  scale_color_manual("Comp. Dist. Quintile"
                     , values = rev(brewer.pal(5, "Dark2"))) +
  geom_hline(yintercept = 0, lty = 2, col = "gray20") +
  geom_vline(xintercept = 0, lty = 2, col = "gray20") + 
  theme_light()

require(fBasics)

summary(store)

dagoTest(train$Sales)
# pval < a, reject H0
  # not normally distributed 