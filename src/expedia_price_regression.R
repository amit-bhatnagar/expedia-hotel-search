setwd("/Users/amitbhatnagar/Github/expedia-hotel-search/src")
library(data.table)
library(ggplot2)

 # expediaData = data.table::fread("../Data/train.csv")
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

# attach(US_Data)

#Focusing only on non random data
US_Data <- US_Data [random_bool == 0]
#Focusing only page 1 (First 10 results)
US_Data <- US_Data [position <= 10]
  

  
US_Data$prop_location_score2 <- as.numeric(US_Data$prop_location_score2)
US_Data$prop_location_score1 <- as.numeric(US_Data$prop_location_score1)
US_Data$prop_review_score <- as.numeric(US_Data$prop_review_score)

#Imputing median prop_location_score2 value in place of missing value
US_Data$prop_location_score2[is.na(US_Data$prop_location_score2)] <- median(US_Data$prop_location_score2,na.rm = TRUE)

#Imputing median prop_review_score value in place of missing value or where rating is 0.0. 
#Since rating cannot be zero, we treat these as missing values too
US_Data$prop_review_score[is.na(US_Data$prop_review_score)] <- median(US_Data$prop_review_score,na.rm = TRUE)
US_Data$prop_review_score[(US_Data$prop_review_score==0)] <- median(US_Data$prop_review_score,na.rm = TRUE)
US_Data$prop_starrating[(US_Data$prop_starrating==0)] <- median(US_Data$prop_starrating,na.rm = TRUE)

US_Data$srch_month <- factor(month(US_Data$date_time))
US_Data$travel_month <- factor(month(as.Date(US_Data$date_time) + US_Data$srch_booking_window))
US_Data$intl_travel <- ifelse(US_Data$visitor_location_country_id!= 219,1,0)

 # US_Data$prop_review_score[is.na(US_Data$prop_review_score)]=0

reg1 <- lm(log(price_usd)~factor(prop_review_score)
   + srch_children_count+srch_adults_count + srch_room_count
   +promotion_flag + factor(prop_starrating) + prop_brand_bool
   + prop_location_score1
   + prop_location_score2
   + srch_saturday_night_bool 
   +  travel_month +    srch_month + srch_booking_window 
   +intl_travel + position 
   , data = US_Data)
summary(reg1)

# For copying easily to Excel 
#coef <- coefficients(reg1)
# coefNames <- names(round(coef[order(coef)],3))
# coefValues <- unname(round(coef[order(coef)],3))
# allCoef <- cbind(coefNames, coefValues)












