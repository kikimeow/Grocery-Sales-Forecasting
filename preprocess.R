# load library
library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(grid)

##
# read datasets for the first time
##

fulltrain <- fread('data/train.csv', showProgress = FALSE)
test <- fread('data/test.csv')
stores <- fread('data/stores.csv')
oil <- fread('data/oil.csv')
items <- fread('data/items.csv')
transactions <- fread('data/transactions.csv')
holidays <- fread('data/holidays_events.csv')

# modify data types
#glimpse(fulltrain)
#fulltrain[, date := as.Date(fast_strptime(date, format = "%Y-%m-%d"))]  # update by reference :=
#cols <- c('id', 'store_nbr', 'item_nbr')
#fulltrain[, (cols) := lapply(.SD, function(x) as.character(x)), .SDcols=cols]
#dat[, (cols) := lapply(.SD, function(x) as.integer(as.character(x))), .SDcols=cols]
#saveRDS(fulltrain, 'fullTrain.rds')

glimpse(stores)
stores$store_nbr <- as.character(stores$store_nbr)
stores$cluster <- as.character(stores$cluster)

glimpse(oil)
oil[, date := as.Date(fast_strptime(date, format = "%Y-%m-%d"))]
oil[1,2] <- 93.14
oil$dcoilwtico <- na.locf(x$dcoilwtico,  na.rm = TRUE, fromLast = FALSE)

glimpse(items)
items[, c('item_nbr', 'class') := lapply(.SD, function(x) as.character(x)), .SDcols=c('item_nbr', 'class')]

glimpse(holidays)
holidays[, date := as.Date(fast_strptime(date, format = "%Y-%m-%d"))]

glimpse(transactions)
transactions[, date := as.Date(fast_strptime(date, format = "%Y-%m-%d"))]
transactions$store_nbr <- as.character(transactions$store_nbr)


# sample dataset (1% of data)
set.seed(123)
index <- sample(1:nrow(fulltrain), nrow(fulltrain)*.01)
sampleTrain <- fulltrain[index,]
saveRDS(sampleTrain, "sampleTrain.rds")

# sample some data for plotting
# pick some item numbers
subsetItems <- subset(fulltrain, subset = item_nbr %in% c('1503844','1157329', '1926217', '1049595', '2105347', '514172', '317147', '507641', '464536', '1972587','1584353', '583868', '2081175'))
saveRDS(subsetItems, "subsetItems.rds")

# pick some store numbers
subsetStores <- subset(fulltrain, subset = store_nbr %in% c('44', '32'))
saveRDS(subsetStores, "subsetStores.rds")

##
# add date columns for grouping
##
train[, yymm:= as.yearmon(train$date)]
train[, yymm1:= as.Date(paste0(substring(train$date, 1, 7), '-01'))]
train[, yy:= year(train$date)]
train[, mm:= month(train$date)]
train[, day:= day(train$date)]
train[, weekday:= weekdays(train$date)]

##
# merge with other data sources
##
train <- merge(train, stores, by="store_nbr", all.x = TRUE)
train <- merge(train, items, by = "item_nbr", all.x = TRUE)

setkey(train, date, store_nbr)
setkey(transactions, date, store_nbr)
train <- merge(train, transactions)

setkey(train, date)
setkey(oil, date)
train <- merge(train, oil, all.x = TRUE)
train$dcoilwtico <- na.locf(train$dcoilwtico, fromLast = FALSE) 

# national holidays
nationals <- holidays[locale == "National"] %>%
  select(date, type, description, transferred) %>%
  filter(!duplicated(date))
colnames(nationals)[2:4] <- paste0('nat_', colnames(nationals)[2:4])

setDT(train)
setDT(nationals)
setkey(train, date)
setkey(nationals, date)

train <- merge(train, nationals, all.x = TRUE)

# local holidays
locals <- holidays[locale == "Local"] %>%
  select(date, type, description, transferred, locale_name) %>%
  filter(!duplicated(date))
colnames(locals)[2:4] <- paste0('local_', colnames(locals)[2:4])

train <- setDF(train) %>%
  left_join(setDF(locals), by = c("city" = "locale_name", "date" = "date"))

# regional holidays
regionals <- holidays[locale == "Regional"] %>%
  select(date, type, description, transferred, locale_name) %>%
  filter(!duplicated(date))
colnames(regionals)[2:4] <- paste0('regional_', colnames(regionals)[2:4])

train <- setDF(train) %>%
  left_join(setDF(regionals), by = c("state" = "locale_name", "date" = "date"))

# holiday flag
train$holidayFlag <- ifelse(
  ((is.na(train$nat_type) == TRUE) &
      (is.na(train$regional_type) == TRUE) &
      (is.na(train$local_type) == TRUE)),0,1)

# payday flag add 15th and last day of month 
lastDay <- seq(as.Date("2013-01-01"), length=60, by="1 month") - 1
train$payDay <- ifelse(
  ((train$day == 15) | 
  (train$date %in% lastDay)), 1, 0)

saveRDS(train, "train.rds")

##
# Apply transformations to subsetItems
##
subsetItems[, yymm:= as.yearmon(subsetItems$date)]
subsetItems[, yymm1:= as.Date(paste0(substring(subsetItems$date, 1, 7), '-01'))]
subsetItems[, yy:= year(subsetItems$date)]
subsetItems[, mm:= month(subsetItems$date)]
subsetItems[, day:= day(subsetItems$date)]
subsetItems[, weekday:= weekdays(subsetItems$date)]

##
# merge with other data sources
##
subsetItems <- merge(subsetItems, stores, by="store_nbr", all.x = TRUE)
subsetItems <- merge(subsetItems, items, by = "item_nbr", all.x = TRUE)

setkey(subsetItems, date, store_nbr)
setkey(transactions, date, store_nbr)
subsetItems <- merge(subsetItems, transactions)

setkey(subsetItems, date)
setkey(oil, date)
subsetItems <- merge(subsetItems, oil, all.x = TRUE)
subsetItems$dcoilwtico <- na.locf(subsetItems$dcoilwtico, fromLast = FALSE) 

#holiday

# national holidays
nationals <- holidays[locale == "National"] %>%
  select(date, type, description, transferred) %>%
  filter(!duplicated(date))
colnames(nationals)[2:4] <- paste0('nat_', colnames(nationals)[2:4])

setDT(subsetItems)
setDT(nationals)
setkey(subsetItems, date)
setkey(nationals, date)

subsetItems <- merge(subsetItems, nationals, all.x = TRUE)

# local holidays
locals <- holidays[locale == "Local"] %>%
  select(date, type, description, transferred, locale_name) %>%
  filter(!duplicated(date))
colnames(locals)[2:4] <- paste0('local_', colnames(locals)[2:4])

subsetItems <- setDF(subsetItems) %>%
  left_join(setDF(locals), by = c("city" = "locale_name", "date" = "date"))

# regional holidays
regionals <- holidays[locale == "Regional"] %>%
  select(date, type, description, transferred, locale_name) %>%
  filter(!duplicated(date))
colnames(regionals)[2:4] <- paste0('regional_', colnames(regionals)[2:4])

subsetItems <- setDF(subsetItems) %>%
  left_join(setDF(regionals), by = c("state" = "locale_name", "date" = "date"))

# holiday flag
subsetItems$holidayFlag <- ifelse(
  ((is.na(subsetItems$nat_type) == TRUE) &
     (is.na(subsetItems$regional_type) == TRUE) &
     (is.na(subsetItems$local_type) == TRUE)),0,1)

# add 15th and last day of month
lastDay <- seq(as.Date("2013-01-01"), length=60, by="1 month") - 1
subsetItems$payDay <- ifelse(
  ((subsetItems$day == 15) | 
     (subsetItems$date %in% lastDay)), 1, 0)

saveRDS(subsetItems, "subsetItems.rds")


##
# Apply transformations to subsetItems
##
subsetStores[, yymm:= as.yearmon(subsetStores$date)]
subsetStores[, yymm1:= as.Date(paste0(substring(subsetStores$date, 1, 7), '-01'))]
subsetStores[, yy:= year(subsetStores$date)]
subsetStores[, mm:= month(subsetStores$date)]
subsetStores[, day:= day(subsetStores$date)]
subsetStores[, weekday:= weekdays(subsetStores$date)]

##
# merge with other data sources
##
subsetStores <- merge(subsetStores, stores, by="store_nbr", all.x = TRUE)
subsetStores <- merge(subsetStores, items, by = "item_nbr", all.x = TRUE)

setkey(subsetStores, date, store_nbr)
setkey(transactions, date, store_nbr)
subsetStores <- merge(subsetStores, transactions)

setkey(subsetStores, date)
setkey(oil, date)
subsetStores <- merge(subsetStores, oil, all.x = TRUE)
subsetStores$dcoilwtico <- na.locf(subsetStores$dcoilwtico, fromLast = FALSE) 

#holiday

# national holidays
nationals <- holidays[locale == "National"] %>%
  select(date, type, description, transferred) %>%
  filter(!duplicated(date))
colnames(nationals)[2:4] <- paste0('nat_', colnames(nationals)[2:4])

setDT(subsetStores)
setDT(nationals)
setkey(subsetStores, date)
setkey(nationals, date)

subsetStores <- merge(subsetStores, nationals, all.x = TRUE)

# local holidays
locals <- holidays[locale == "Local"] %>%
  select(date, type, description, transferred, locale_name) %>%
  filter(!duplicated(date))
colnames(locals)[2:4] <- paste0('local_', colnames(locals)[2:4])

subsetStores <- setDF(subsetStores) %>%
  left_join(setDF(locals), by = c("city" = "locale_name", "date" = "date"))

# regional holidays
regionals <- holidays[locale == "Regional"] %>%
  select(date, type, description, transferred, locale_name) %>%
  filter(!duplicated(date))
colnames(regionals)[2:4] <- paste0('regional_', colnames(regionals)[2:4])

subsetStores <- setDF(subsetStores) %>%
  left_join(setDF(regionals), by = c("state" = "locale_name", "date" = "date"))

# holiday flag
subsetStores$holidayFlag <- ifelse(
  ((is.na(subsetStores$nat_type) == TRUE) &
     (is.na(subsetStores$regional_type) == TRUE) &
     (is.na(subsetStores$local_type) == TRUE)),0,1)

# add 15th and last day of month
lastDay <- seq(as.Date("2013-01-01"), length=60, by="1 month") - 1
subsetStores$payDay <- ifelse(
  ((subsetStores$day == 15) | 
     (subsetStores$date %in% lastDay)), 1, 0)


##
# Apply transformations to transactions
##

fun_date_features <- function(data){
  data[, yymm:= as.yearmon(data$date)]
  data[, yymm1:= as.Date(paste0(substring(data$date, 1, 7), '-01'))]
  data[, yy:= year(data$date)]
  data[, mm:= month(data$date)]
  data[, day:= day(data$date)]
  data[, weekday:= weekdays(data$date)]
}

fun_date_features(transactions)


##
# merge with other data sources
##

# merge with store info
transactions <- merge(transactions, stores, by="store_nbr", all.x = TRUE)

# merge with oil
setkey(transactions, date)
setkey(oil, date)
transactions <- merge(transactions, oil, all.x = TRUE)
transactions$dcoilwtico <- na.locf(transactions$dcoilwtico, fromLast = FALSE) 

# national holidays
nationals <- holidays[locale == "National"] %>%
  select(date, type, description, transferred) %>%
  filter(!duplicated(date))
colnames(nationals)[2:4] <- paste0('nat_', colnames(nationals)[2:4])

setDT(transactions)
setDT(nationals)
setkey(transactions, date)
setkey(nationals, date)

transactions <- merge(transactions, nationals, all.x = TRUE)

# local holidays
locals <- holidays[locale == "Local"] %>%
  select(date, type, description, transferred, locale_name) %>%
  filter(!duplicated(date))
colnames(locals)[2:4] <- paste0('local_', colnames(locals)[2:4])

transactions <- setDF(transactions) %>%
  left_join(setDF(locals), by = c("city" = "locale_name", "date" = "date"))

# regional holidays
regionals <- holidays[locale == "Regional"] %>%
  select(date, type, description, transferred, locale_name) %>%
  filter(!duplicated(date))
colnames(regionals)[2:4] <- paste0('regional_', colnames(regionals)[2:4])

transactions <- setDF(transactions) %>%
  left_join(setDF(regionals), by = c("state" = "locale_name", "date" = "date"))

# holiday flag
transactions$holidayFlag <- ifelse(
  ((is.na(transactions$nat_type) == TRUE) &
     (is.na(transactions$regional_type) == TRUE) &
     (is.na(transactions$local_type) == TRUE)),0,1)

# payday flag add 15th and last day of month 
lastDay <- seq(as.Date("2013-01-01"), length=60, by="1 month") - 1
transactions$payDay <- ifelse(
  ((transactions$day == 15) | 
     (transactions$date %in% lastDay)), 1, 0)

setDT(transactions)
transactions <- transactions[order(store_nbr, date)]
transactions[, movingAvg_7:= rollapply(transactions, 7, mean, fill = 'extend', align = 'right'), by = store_nbr]
transactions[, movingAvg_15:= rollapply(transactions, 15, mean, fill = 'extend', align = 'right'), by = store_nbr]
transactions[, movingAvg_30:= rollapply(transactions, 30, mean, fill = 'extend', align = 'right'), by = store_nbr]
transactions$transactions <- as.numeric(transactions$transactions)

saveRDS(subsetStores, "subsetStores.rds")
saveRDS(stores, "stores.rds")
saveRDS(oil, "oil.rds")
saveRDS(items, "items.rds")
saveRDS(transactions, 'transactions.rds')
saveRDS(holidays, 'holidays.rds')

