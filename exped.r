setwd("~/Google Drive/School/MKTG 366 Project/")

library(data.table)
library(caret)
library(dplyr)
library(gbm)



confusion <- function(predicted, actual){
  tbl <- table(predicted, actual)
  mis <- 1 - sum(diag(tbl))/sum(tbl)
  list(table = tbl, misclass.prob = mis)
}

exped = data.table::fread("train.csv")

## -----------------------------------------------------------------------------
# select a 10% sample of search id srch_id
unique_srch_id = unique(exped$srch_id)
total_num_srch_id = length(unique_srch_id)
sample_100_srch_id = sample(unique_srch_id, round(total_num_srch_id/100))
train_id = sample(sample_100_srch_id, round(length(sample_100_srch_id)/10)*8)
test_id = setdiff(sample_100_srch_id, train_id)
train_sample_100 = exped %>% filter(srch_id %in% train_id)
test_sample_100 = exped %>% filter(srch_id %in% test_id)


# select numeric variables
train_sample_100_numeric = train_sample_100 %>% select(srch_id, site_id, visitor_location_country_id, random_bool,
                                           prop_country_id, prop_id, prop_starrating, prop_review_score,
                                           prop_brand_bool, prop_location_score1, prop_location_score2,
                                           prop_log_historical_price, position, price_usd, srch_destination_id,
                                           srch_length_of_stay, srch_booking_window, srch_adults_count,
                                           srch_children_count, srch_room_count, orig_destination_distance, booking_bool)

# convert variables that were coerced to "character" to type "double" and "factor" as appropriate
numeric_names = c("prop_review_score", "prop_location_score2", "orig_destination_distance")
setDT(train_sample_100_numeric)[, (numeric_names) := lapply(.SD, as.numeric), .SDcols = numeric_names]

# bool_names = c("prop_brand_bool", "booking_bool", "random_bool")
# setDT(train_sample_100_numeric)[, (bool_names) := lapply(.SD, factor), .SDcols = bool_names]
# train_sample_100_numeric$booking_bool = as.factor(train_sample_100_numeric$booking_bool)

# train model using Bernoulli
fit.gbm = gbm(booking_bool ~ .,
              data = train_sample_100_numeric,
              distribution = "bernoulli",
              n.trees = 1000,
              shrinkage = 0.02,
              train.fraction = 0.4,
              verbose = TRUE)

gbm.perf(fit.gbm)

## -------------------------
## making predictions
# select numeric variables for test set
test_sample_100_numeric = test_sample_100 %>%
  select(srch_id, site_id, visitor_location_country_id, random_bool,
         prop_country_id, prop_id, prop_starrating, prop_review_score,
         prop_brand_bool, prop_location_score1, prop_location_score2,
         prop_log_historical_price, position, price_usd, srch_destination_id,
         srch_length_of_stay, srch_booking_window, srch_adults_count,
         srch_children_count, srch_room_count, orig_destination_distance)

test_response = test_sample_100$booking_bool

# convert variables that were coerced to "character" to type "double" and "factor" as appropriate
setDT(test_sample_100_numeric)[, (numeric_names) := lapply(.SD, as.numeric), .SDcols = numeric_names]



pred = predict(fit.gbm, test_sample_100_numeric, n.trees = 120, type = 'response') > 0.10 #n.trees chosen by gbm.perf
confusion(pred, test_response > 0)

# Result # ----------------------------------------
#
#        FALSE  TRUE
# FALSE 18472   366
# TRUE   1035   199
#
# $misclass.prob
# [1] 0.06979872
# -------------------------------------------------



## -----------------------------------------------------------------------------
## Using 1/10 of the data
# select a 10% sample of search id srch_id
unique_srch_id = unique(exped$srch_id)
total_num_srch_id = length(unique_srch_id)
sample_100_srch_id = sample(unique_srch_id, round(total_num_srch_id/10))
train_id = sample(sample_100_srch_id, round(length(sample_100_srch_id)/10)*8)
test_id = setdiff(sample_100_srch_id, train_id)
train_sample_100 = exped %>% filter(srch_id %in% train_id)
test_sample_100 = exped %>% filter(srch_id %in% test_id)


# select numeric variables
train_sample_100_numeric = train_sample_100 %>% select(srch_id, site_id, visitor_location_country_id, random_bool,
                                                       prop_country_id, prop_id, prop_starrating, prop_review_score,
                                                       prop_brand_bool, prop_location_score1, prop_location_score2,
                                                       prop_log_historical_price, position, price_usd, srch_destination_id,
                                                       srch_length_of_stay, srch_booking_window, srch_adults_count,
                                                       srch_children_count, srch_room_count, orig_destination_distance, booking_bool)

# convert variables that were coerced to "character" to type "double" and "factor" as appropriate
numeric_names = c("prop_review_score", "prop_location_score2", "orig_destination_distance")
bool_names = c("prop_brand_bool", "booking_bool", "random_bool")
setDT(train_sample_100_numeric)[, (numeric_names) := lapply(.SD, as.numeric), .SDcols = numeric_names]
# setDT(train_sample_100_numeric)[, (bool_names) := lapply(.SD, factor), .SDcols = bool_names]

train_sample_100_numeric$booking_bool = as.factor(train_sample_100_numeric$booking_bool)
typeof(train_sample_100_numeric$booking_bool)


# select numeric variables for test set
test_sample_100_numeric = test_sample_100 %>% select(srch_id, site_id, visitor_location_country_id, random_bool,
                                                     prop_country_id, prop_id, prop_starrating, prop_review_score,
                                                     prop_brand_bool, prop_location_score1, prop_location_score2,
                                                     prop_log_historical_price, position, price_usd, srch_destination_id,
                                                     srch_length_of_stay, srch_booking_window, srch_adults_count,
                                                     srch_children_count, srch_room_count, orig_destination_distance)

test_response = test_sample_100$booking_bool


# convert variables that were coerced to "character" to type "double" and "factor" as appropriate
numeric_names = c("prop_review_score", "prop_location_score2", "orig_destination_distance")
bool_names = c("prop_brand_bool", "booking_bool", "random_bool")
setDT(test_sample_100_numeric)[, (numeric_names) := lapply(.SD, as.numeric), .SDcols = numeric_names]
# setDT(test_sample_100_numeric)[, (bool_names) := lapply(.SD, factor), .SDcols = bool_names]

# train model using Bernoulli
fit.gbm1 = gbm(booking_bool ~ ., data = train_sample_100_numeric, distribution = "bernoulli", n.trees = 400, shrinkage = 0.05, train.fraction = 0.5, verbose = TRUE)
gbm.perf(fit.gbm1)
pred = predict(fit.gbm1, test_sample_100_numeric, n.trees = 349, type = 'response') > 0.10
confusion(pred, test_response > 0)


#### ---------------------------------------------------------------------------

# train model using LambdaMart
sample_100_srch_id = sample(unique_srch_id, round(total_num_srch_id/100))
train_id = sample(sample_100_srch_id, round(length(sample_100_srch_id)/10)*8)
test_id = setdiff(sample_100_srch_id, train_id)
train_sample_100 = exped %>% filter(srch_id %in% train_id)
test_sample_100 = exped %>% filter(srch_id %in% test_id)


# select numeric variables
train_sample_100_numeric = train_sample_100 %>% select(srch_id, site_id, visitor_location_country_id, random_bool,
                                                       prop_country_id, prop_id, prop_starrating, prop_review_score,
                                                       prop_brand_bool, prop_location_score1, prop_location_score2,
                                                       prop_log_historical_price, position, price_usd, srch_destination_id,
                                                       srch_length_of_stay, srch_booking_window, srch_adults_count,
                                                       srch_children_count, srch_room_count, orig_destination_distance, booking_bool)

# convert variables that were coerced to "character" to type "double" and "factor" as appropriate
numeric_names = c("prop_review_score", "prop_location_score2", "orig_destination_distance")
setDT(train_sample_100_numeric)[, (numeric_names) := lapply(.SD, as.numeric), .SDcols = numeric_names]

# bool_names = c("prop_brand_bool", "booking_bool", "random_bool")
# setDT(train_sample_100_numeric)[, (bool_names) := lapply(.SD, factor), .SDcols = bool_names]
# train_sample_100_numeric$booking_bool = as.factor(train_sample_100_numeric$booking_bool)

# train model using MLR
fit.gbm = gbm(booking_bool ~ .,
              data = train_sample_100_numeric,
              distribution = list(name='pairwise', metric="ndcg", group = 'srch_id'),
              n.trees = 1000,
              interaction.depth = 7,
              shrinkage = 0.005,
              train.fraction = 0.5,
              verbose = TRUE)

gbm.perf(fit.gbm)

## -------------------------
## making predictions
# select numeric variables for test set
test_sample_100_numeric = test_sample_100 %>%
  select(srch_id, site_id, visitor_location_country_id, random_bool,
         prop_country_id, prop_id, prop_starrating, prop_review_score,
         prop_brand_bool, prop_location_score1, prop_location_score2,
         prop_log_historical_price, position, price_usd, srch_destination_id,
         srch_length_of_stay, srch_booking_window, srch_adults_count,
         srch_children_count, srch_room_count, orig_destination_distance)

test_response = test_sample_100$booking_bool

# convert variables that were coerced to "character" to type "double" and "factor" as appropriate
setDT(test_sample_100_numeric)[, (numeric_names) := lapply(.SD, as.numeric), .SDcols = numeric_names]



pred = predict(fit.gbm, test_sample_100_numeric, n.trees = 400, type = 'response') > 0.30 #n.trees chosen by gbm.perf
confusion(pred, test_response > 0)
unique_srch_id = unique(test_sample_100_numeric$srch_id)
predid = data.frame(cbind(test_sample_100_numeric$srch_id, pred))
colnames(predid) = c("id", "prediction")

bool_booking = rep(0, length(unique_srch_id))
for (uid in unique_srch_id) {
  temp = predid %>% filter(id == uid)
  bool_booking[i] = sum(temp$prediction)
  print(sum(temp$prediction))
}