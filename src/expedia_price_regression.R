setwd("/Users/amitbhatnagar/Github/expedia-hotel-search/src")
library(data.table)
library(ggplot2)

 expediaData = data.table::fread("../Data/train.csv")
# sampleData <- expediaData[(expediaData$srch_id%%8 == 0),]
# write.csv(sampleData,file = "../Data/sampleData.csv")
# 
# # sampleData=data.table::fread("../Data/sampleData.csv")
# 
# sampleData$intl_travel <- (sampleData$visitor_location_country_id != sampleData$prop_country_id)
# sampleData=data.table::fread("../Data/sampleData.csv")
# attach(sampleData)

# randomData = sampleData[random_bool == 1]
# regularData = sampleData[random_bool == 0]

# US_data <- expediaData[prop_country_id=="219"]
# write.csv(US_data,file = "../Data/US_data.csv")

US_Data = data.table::fread("../Data/US_data.csv")

attach(US_Data)
US_Data$prop_location_score2 <- as.numeric(prop_location_score2)
US_Data$prop_location_score1 <- as.numeric(prop_location_score1)
US_Data$prop_review_score <- as.numeric(prop_review_score)

US_Data$srch_month <- factor(month(US_Data$date_time))
US_Data$travel_month <- factor(month(as.Date(date_time) + srch_booking_window))
US_Data$intl_travel <- ifelse(visitor_location_country_id!= 219,1,0)

summary(lm(price_usd~factor(prop_review_score)
           # + srch_children_count+srch_adults_count
           +promotion_flag + factor(prop_starrating) + prop_brand_bool
           + prop_location_score1
           + prop_location_score2
            + srch_saturday_night_bool 
            +  travel_month +    srch_month + srch_booking_window 
           + 
          , data = US_Data))

summary(lm(price_usd~srch_booking_window, data = US_Data))


















