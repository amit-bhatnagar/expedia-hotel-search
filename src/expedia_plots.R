setwd("/Users/amitbhatnagar/Github/expedia-hotel-search/src")
library(data.table)
library(ggplot2)

 expediaData = data.table::fread("../Data/train.csv")
sampleData <- expediaData[(expediaData$srch_id%%8 == 0),]
 write.csv(sampleData,file = "../Data/sampleData.csv")
# 
# 
# sampleData=data.table::fread("../Data/sampleData.csv")

sampleData$intl_travel <- (sampleData$visitor_location_country_id != sampleData$prop_country_id)
sampleData=data.table::fread("../Data/sampleData.csv")
attach(sampleData)

randomData = sampleData[random_bool == 1]
regularData = sampleData[random_bool == 0]

#Analyze impact of position on CTR
position_effect_CTR_reg <- regularData[,.(CTR = sum(click_bool)/sum(srch_id >0)),by=position][order(position)]
position_effect_CTR_ran <-randomData[,.(CTR = sum(click_bool)/sum(srch_id >0)),by=position][order(position)]

position_CTR <- data.frame(position=position_effect_CTR_reg$position,
                 CTR_regular = position_effect_CTR_reg$CTR,
                 CTR_random = position_effect_CTR_ran$CTR)

position_CTR2 <- melt(data = position_CTR, id.vars = "position")

ggplot(data = position_CTR2, aes(x = position, y = value, colour = variable)) + 
  geom_line() +
  ggtitle("Impact of position on CTR") +
  labs(x="Position",y="Click Through Rate") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks=seq(0,40,5))
rm(position_CTR,position_CTR2,position_effect_CTR_reg,position_effect_CTR_ran)


#Analyze impact of position on booking rate
position_effect_BR_reg <- regularData[,.(BR = sum(booking_bool)/sum(srch_id >0)),by=position][order(position)]
position_effect_BR_ran <-randomData[,.(BR = sum(booking_bool)/sum(srch_id >0)),by=position][order(position)]

position_BR <- data.frame(position=position_effect_BR_reg$position,
                          Booking_rate_regular = position_effect_BR_reg$BR,
                          Booking_rate_random = position_effect_BR_ran$BR)

position_BR2 <- melt(data = position_BR, id.vars = "position")

ggplot(data = position_BR2, aes(x = position, y = value, colour = variable)) + 
  geom_line() +
  ggtitle("Impact of position on Booking rate") +
  labs(x="Position",y="Booking Rate") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks=seq(0,40,5))

rm(position_BR,position_BR2,position_effect_BR_reg,position_effect_BR_ran)



#Analyze probability of booking conditional on clickthrough
position_effect_SR_reg <- regularData[,.(SR = sum(booking_bool)/sum(click_bool)),by=position][order(position)]
position_effect_SR_ran <-randomData[,.(SR = sum(booking_bool)/sum(click_bool)),by=position][order(position)]

position_SR <- data.frame(position=position_effect_SR_reg$position,
                          Booking_rate_conditional_on_click_regular = position_effect_SR_reg$SR,
                          Booking_rate_conditional_on_click_random = position_effect_SR_ran$SR)

position_SR2 <- melt(data = position_SR, id.vars = "position")

ggplot(data = position_SR2, aes(x = position, y = value, colour = variable)) + 
  geom_line() +
  ggtitle("Impact of position on Booking rate conditional on click") +
  labs(x="Position",y="Booking rate conditional on click") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks=seq(0,40,5))
rm(position_SR,position_SR2,position_effect_SR_reg,position_effect_SR_ran)

#Analyze impact of location score1
regularData$LS1_rounded <- round(regularData$prop_location_score1)
randomData$LS1_rounded <- round(randomData$prop_location_score1)

LS1_effect_BR_reg <- regularData[,.(booking_rate = sum(click_bool)/sum(srch_id >0)),by=LS1_rounded][order(LS1_rounded)]
LS1_effect_BR_ran <-randomData[,.(booking_rate = sum(click_bool)/sum(srch_id >0)),by=LS1_rounded][order(LS1_rounded)]

regularData$LS2_rounded <- round(as.numeric(regularData$prop_location_score2))
randomData$LS2_rounded <- round(as.numeric(randomData$prop_location_score2))

LS2_effect_BR_reg <- regularData[,.(booking_rate = sum(click_bool)/sum(srch_id >0)),by=LS2_rounded][order(LS2_rounded)]
LS2_effect_BR_ran <-randomData[,.(booking_rate = sum(click_bool)/sum(srch_id >0)),by=LS2_rounded][order(LS2_rounded)]


LR_SR <- data.frame(position=position_effect_BR_reg$position,
                    booking_rate_regular = position_effect_SR_reg$booking_rate,
                    booking_rate_random = position_effect_SR_ran$booking_rate)

position_SR2 <- melt(data = position_SR, id.vars = "position")

ggplot(data = position_SR2, aes(x = position, y = value, colour = variable)) + 
  geom_line() +
  ggtitle("Impact of position on Booking rate conditional on click") +
  labs(x="Position",y="Booking rate conditional on click") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks=seq(0,40,5))
rm(position_SR,position_SR2,position_effect_SR_reg,position_effect_SR_ran)


US_data <- sampleData[prop_country_id=="219"]
attach(US_data)
US_data$prop_location_score2 <- as.numeric(prop_location_score2)
US_data$prop_location_score1 <- as.numeric(prop_location_score1)
US_data$prop_review_score <- as.numeric(prop_review_score)

US_data$srch_month <- factor(month(US_data$date_time))
US_data$travel_month <- factor(month(as.Date(date_time) + srch_booking_window))
summary(lm(price_usd~factor(prop_review_score)
           # + srch_children_count+srch_adults_count
           +promotion_flag + factor(prop_starrating) + prop_brand_bool
           + prop_location_score1
           + prop_location_score2
           + srch_saturday_night_bool +  travel_month + 
             +srch_month+ srch_booking_window
          , data = US_data))

summary(lm(price_usd~srch_booking_window, data = US_data))


















