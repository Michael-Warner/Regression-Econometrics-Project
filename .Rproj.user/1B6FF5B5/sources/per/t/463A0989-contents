####FULL VERSION
#Group member:
#Jing Fei XU
#Michael WARNER

#The dataset contains data from two different hotels, 
#one is a Resort hotel (H1) and the other is a City hotel (H2).
#It comprehends bookings due to arrive between the 1st of July of 2015 and
#the 31st of August 2017, including bookings that effectively arrived and bookings that were canceled.

#Upload the package needed
library(ggplot2)
library(data.table)
library(countrycode)
library(magrittr)
library(dplyr)
library(car)

#Import data
data_original <- read.csv("hotel_bookings.csv")

#Check the type of the variable
str(data_original)
#factor: is_canceled(booking was canceled (1) or not (0)); 
#arrival_date_year; 
#is_repeated_guest(repeated guest (1) or not (0))
data_original$is_canceled <- factor(data_original$is_canceled, levels = c(0,1), labels = c("Not Canceled", "Canceled"))
data_original$arrival_date_year <- as.factor(data_original$arrival_date_year)
data_original$is_repeated_guest <- factor(data_original$is_repeated_guest, levels = c(0,1), labels = c("Not Repeated Guest", "Repeated Guest"))
data_original$arrival_date_month <- factor(data_original$arrival_date_month, levels = month.name)
summary(data_original)
#Combined meal columns, SC and undefined are no meal
data_original$meal[data_original$meal == "Undefined"] <- "SC" 

#Data cleaning: delete unnecessary columns
#previous_cancellations; previous_bookings_not_canceled; agent; company; 
#days_in_waiting_list; required_car_parking_spaces; reservation_status; 
#reservation_status_date
data <- data_original[,-c(18,19,24,25,26,29,31,32)]
data_nodeposittype <- data[,-21]

#Check if there's any missing value
summary(data)
#There're 4 missing value in column 'children',
#so we replace the missing values in children column from the corresponding babies column
n <- length(data$children)
for (i in 1:n) {
  if (is.na(data$children[i]))
    data$children[i] <- data$babies[i]
}

#-------------------------------
#Exploratory Data Analysis (EDA)
#-------------------------------

summary(data)
#We can find that there are two kinds of hotels: City hotel(79330) and Resort 
#hotel(40060);
#44224 bookings(37%) have been canceled;
#in 2016, hotels had the largest number of guests(56707) and they get the most 
#guests in the summer;
#people usually book a hotel online;
#a very small percentage(3.19%) of guests have previously booked these two hotels;
#most of the guests at these two hotels have breakfast reservations(BB:92310) 
#and have chosen room A(85994);
#most bookings don’t require a deposit;
#most of the guests at the hotel are transient;
#the mean of adr(average daily rate) is 101.83 per night

#2 types of hotel:
#City hotel: 79330;   Resort hotel: 40060 
ggplot(data = data, aes(x = hotel)) +
  geom_bar(stat = "count") +
  labs(title = "Booking Request by Hotel type",
       x = "Hotel type",
       y = "Number of bookings") 
#Break down of each hotel
table(data$hotel)
prop.table(table(data$hotel))

#How many bookings were canceled?
table(data$is_canceled, data$hotel)
prop.table(table(data$is_canceled, data$hotel))
prop.table(table(data$is_canceled, data$hotel), margin = 2)
#From 2015 to 2017, 33102 bookings(27.72%) are canceled for city hotel 
#and 11122 cancellations(9.3%) for resort hotel

#Relationship between Hotel cancellation and lead time, 
#which is the time gap between booking made and the arrival date.
hist(data$lead_time)
boxplot(data$lead_time)

ggplot(data = data, aes(
  x = hotel,
  y = lead_time,
  fill = factor(is_canceled)
)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Cancellation By Hotel Type",
    subtitle = "Based on Lead Time",
    x = "Hotel Type",
    y = "Lead Time (Days)"
  ) +
  scale_fill_discrete(
    name = "Booking Status"
  ) 
#From the box plot, we can know that the more people book hotels in advance, the 
#more likely they are to cancel

#Which year had the most hotel reservations?
ggplot(data = data, aes(x = arrival_date_year)) +
  geom_bar(stat = "count") +
  labs(title = "Booking Request by Year",
       x = "Year",
       y = "Number of bookings") 

table(data$arrival_date_year)
#Hotels had the most customers in 2016 

#Which is the most busy month?
ggplot(data = data, aes(arrival_date_month, fill = hotel)) +
  geom_bar() +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

table(data$arrival_date_week_number)

#Which are the most busy week?
ggplot(data = data, aes(arrival_date_week_number, fill = hotel)) + 
  geom_bar() +
  labs(title = "Booking Status by Week",
       x = "Week",
       y = "Count")

#Which is the most busy day of the month?
ggplot(data = data, aes(arrival_date_day_of_month, fill = hotel)) + 
  geom_bar() +
  labs(title = "Booking Status by Day",
       x = "Day",
       y = "Count") 

#Most people booked the hotel for these four months: May, July, August and October,
#and the number of people who booked into the hotel on the last day of each month 
#was relatively small, the other days were more average


#Which month has the highest number of cancellations?
ggplot(data, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status") +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") 

table(data$is_canceled, data$arrival_date_month)
prop.table(table(data$is_canceled, data$arrival_date_month), margin = 2)

#Which Week has the highest number of cancellations?
ggplot(data, aes(arrival_date_week_number, fill = factor(is_canceled))) +
  geom_bar() + 
  coord_flip() + scale_fill_discrete(
    name = "Booking Status") +
  labs(title = "Booking Status by Week",
       x = "Week",
       y = "Count") 

table(data$is_canceled, data$arrival_date_week_number)
prop.table(table(data$is_canceled, data$arrival_date_week_number), margin = 2)

#Which day of the month has the highest number of cancellations?
ggplot(data, aes(arrival_date_day_of_month, fill = factor(is_canceled))) +
  geom_bar() + 
  coord_flip() + scale_fill_discrete(
    name = "Booking Status") +
  labs(title = "Cancellation Status by Day of Month",
       x = "Day",
       y = "Count") 

table(data$is_canceled, data$arrival_date_day_of_month)
prop.table(table(data$is_canceled, data$arrival_date_day_of_month), margin = 2)

#Across the 12 months we can also find a lot of cancellations, the month, week, 
#the daily percentage of cancellations mostly hover in the 30 to 40% range.


#Subset the data that only includes the guests who didn't cancel the booking
data_0 <- data[data$is_canceled == "Not Canceled", ]

# Visualize the guests by Country
# Subset the data to include the countries which has more than 1500 bookings
# otherwise including all the country with few or occasional request to avoid the 
#graph from being clumsy
sub_data <- data_0 %>% 
  group_by(country) %>% 
  filter(n() > 1500)

sub_data$county_name <- countrycode(sub_data$country, 
                                    origin = "iso3c",
                                    destination = "country.name")

# Guests by Country per hotel 
ggplot(sub_data, aes(county_name, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Booking Status by Country",
       x = "Country",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Most guests are coming from Portugal, followed by the UK, France and Germany
#And we can see that guests from the UK are more likely to book a resort hotel
#than those from other countries

# How long do people stay at the hotels?
ggplot(sub_data, aes(x=stays_in_weekend_nights,fill = hotel)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Stay Duration(In Weekend)",
       x = "Stays in nights",
       y = "Count") 

#Most people did not spend the weekend in a hotel
#and if they did, it was usually for one or two nights

ggplot(sub_data, aes(x=stays_in_week_nights,fill = hotel)) + 
  geom_bar(position = position_dodge()) +
  labs(title = "Stay Duration(In Week)",
       x = "Stays in nights",
       y = "Count") 
#Most guests stayed one to three nights
#and we can find that a large percentage of resort hotel reservations stayed six 
#nights in weeks

#Bookings by market segment of all vs non canceled bookings
ggplot(data, aes(market_segment, fill = hotel)) + 
  geom_bar(position = position_dodge()) + 
  labs(title = "Hotel Booking by Market Segment",
       x = "Market Type",
       y = "Count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sub_data, aes(market_segment, fill = hotel)) + 
  geom_bar(position = position_dodge()) + 
  labs(title = "Hotel Booking by Market Segment of non-cancellations",
       x = "Market Type",
       y = "Count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#Most guests booked their hotel online, it did not matter if they canceled or not

# Did allocation of different room lead to cancellation?
# keep only that data where reserved room is different than the room allocated

df <- subset(data, 
             as.character(data$reserved_room_type) != as.character(data$assigned_room_type))

table(df$is_canceled)
prop.table(table(df$is_canceled))
#We find that when the assigned room type differs from the reserved room type 
#due to hotel operation reasons (e.g. overbooking) or by customer request, 
#most guests will not cancel the booking(95%)

#Did the number of changes made to the booking impact the cancellations?
ggplot(data,aes(booking_changes, fill=is_canceled)) + 
  geom_bar(stat = "count", position = position_dodge())+
  labs(title = "Booking Status by Changes",
       x = "Number of Changes",
       y = "Count") +
  scale_fill_discrete(
    name = "Booking Status"
  ) 


ggplot(data[which(data$booking_changes<3),],aes(booking_changes, fill=is_canceled)) + 
  geom_bar(stat = "count", position = position_dodge())+
  labs(title = "Booking Status by Changes",
       x = "Number of Changes",
       y = "Count") +
  scale_fill_discrete(
    name = "Booking Status"
  ) 

table(data$is_canceled, data$booking_changes)
prop.table(table(data$is_canceled, data$booking_changes), margin = 2)

#Most of guests didn't make any changes to the booking 
#and the number of changes didn't affect the cancellations overall

#Does type of deposit impact the cancellation?
ggplot(data, aes(deposit_type,fill=is_canceled)) +
  geom_bar(position = position_dodge())+ 
  labs(title = "Booking Status by Deposit Type",
       x = "Deposit Type",
       y = "Count") +
  scale_fill_discrete(
    name = "Booking Status")

table(data$is_canceled, data$deposit_type)
prop.table(table(data$is_canceled, data$deposit_type),2)

#Most cancellations were no deposit, 
#but it’s a bit odd that many cancellations were non-refundable,
#even far more than refundable

#Data from the Non Refund category, not canceled and canceled, 
#was analyzed for errors

data_non_refund <- data[data$deposit_type == "Non Refund",] 
table(data_non_refund$arrival_date_month, data_non_refund$deposit_type)

ggplot(data_non_refund, aes(arrival_date_year,fill=is_canceled)) +
  geom_bar(position = position_dodge())+ 
  labs(title = "Cancelation by year for Non Refund Deposit Type",
       x = "Year",
       y = "Count") +
  scale_fill_discrete(
    name = "Booking Status")

ggplot(data_non_refund, aes(arrival_date_month,fill=is_canceled)) +
  geom_bar(position = position_dodge())+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Cancelation by month for Non Refund Deposit Type",
       x = "Month",
       y = "Count") +
  scale_fill_discrete(
    name = "Booking Status")

ggplot(data_non_refund, aes(arrival_date_day_of_month, fill=is_canceled)) +
  geom_bar(position = position_dodge())+ 
  labs(title = "Cancelation by day of month for Non Refund Deposit Type",
       x = "Day of Month",
       y = "Count") +
  scale_fill_discrete(
    name = "Booking Status")

summary(data_non_refund)

#No error is spotted in the data, cancelations are spread out  but this does not 
#logically make sense, as most people who put a deposit on something would not 
#want to lose that money and cancel it and our data shows that 99.36% of people
#who put a non-refundable deposit canceled.

#A closer look was taken to see if it matter from what country people were
#from for putting non refundable deposits and then canceling them. 

table(data_non_refund$is_canceled, data_non_refund$country)
format(round(prop.table(table(data_non_refund$is_canceled, 
                              data_non_refund$country)),4), nsmall = 4)

#Portugal is seen as having a the overall majority of all cancellations for non
#refundable deposits at 96.94% 

#It was checked if all Portugal reservations had to be a non refundable deposit
#or not.

table(data$country == c("PRT"), data$deposit_type)
prop.table(table(data$country == c("PRT"), data$deposit_type),2)

#It is not, as most of the Portugal reservations were no deposit required.

#It was checked to see if it matter from which hotel the cancellations were from

ggplot(data_non_refund, aes(hotel, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Non Refundable Deposit Status by Location",
       x = "Country",
       y = "Count")

#It was seen that most of Portugal's non refundable deposits cancellations were
#from City Hotel and not from Resort Hotel


#It was checked to see if the market segment or distribution channel had any
#effect on the distribution of the non refundable deposits.

ggplot(data_non_refund, aes(market_segment, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Non Refundable Deposit by Market Segment by Hotel",
       x = "Market Segment",
       y = "Count")

ggplot(data_non_refund, aes(distribution_channel, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Non Refundable Deposit Status by Distribution Channel",
       x = "Distribution Channel",
       y = "Count")

ggplot(data_non_refund, aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Non Refundable Deposit Status by Customer Type",
       x = "Customer Type",
       y = "Count")

table(data_non_refund$market_segment)
prop.table(table(data_non_refund$market_segment))

table(data_non_refund$distribution_channel)
prop.table(table(data_non_refund$distribution_channel))

table(data_non_refund$customer_type)
prop.table(table(data_non_refund$customer_type))

#It is seen that almost all non refundable deposits come from Portugal,
#in a real world circumstance the company would be contacted to see if there
#was an error in their data or is this a requirement with a certain subset 
#of Portuguese clients as the majority of Portuguese clients do not need a 
#non refundable deposit. 

#As this is not possible in this, two different models will be created in the 
#logistic model step. One with non refundable deposits kept in, and one with
#the non refundable deposits removed.

#Hotel Preference in relation customer type

ggplot(data, aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Hotel Preference by Customer Type",
       x = "Customer Type",
       y = "Count") +
  scale_fill_discrete(
    name = "Hotel")

#Hotel preference in relation to customer type who canceled
ggplot(data[which(data$is_canceled == "Canceled"),], aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Hotel Preference by Customer Type, Canceled Only",
       x = "Customer Type",
       y = "Count") +
  scale_fill_discrete(
    name = "Hotel")

#Most bookings were transient

table(data$is_canceled, data$customer_type)
prop.table(table(data$is_canceled, data$customer_type))
prop.table(table(data$is_canceled, data$customer_type),2)
# It seems that most cancellations come from transient clients

#Did the hotel charge differently for different customer type?
ggplot(data, aes(x = customer_type, y = adr, fill = hotel)) + 
  geom_boxplot(position = position_dodge()) + 
  ylim(-10, 525)+
  labs(title = "Price Charged by Hotel Type",
       subtitle = "for Customer Type",
       x = "Customer Type",
       y = "Price per night(EUR)") +
  scale_fill_discrete(
    name = "Hotel") 

#Yes, as customers who had the transient bookings but not associated to the 
#other transient booking, they are usually charged the highest hotel rates per 
#night

#Is the difference between the customer type who canceled vs customer type who
#did not cancel greatly different?

ggplot(data[which(data$is_canceled != "Canceled"),], aes(x = customer_type, 
                                                         y = adr, fill = hotel)) + 
  geom_boxplot(position = position_dodge()) + 
  ylim(-10, 525)+
  labs(title = "Price Charged by Hotel Type, and Not Canceled",
       subtitle = "for Customer Type",
       x = "Customer Type",
       y = "Price per night(EUR)") +
  scale_fill_discrete(
    name = "Hotel") 

#Canceled clients based on the charge rate per customer type
ggplot(data[which(data$is_canceled == "Canceled"),], aes(x = customer_type, y = adr, fill = hotel)) + 
  geom_boxplot(position = position_dodge()) + 
  ylim(-10, 525)+
  labs(title = "Price Charged by Hotel Type and Canceled",
       subtitle = "for Customer Type",
       x = "Customer Type",
       y = "Price per night(EUR)") +
  scale_fill_discrete(
    name = "Hotel") 

#There is no great difference between the hotel, client type and those clients 
#who canceled and those clients who did not.

#Did the number of special requests made by the customer impact the cancellations?
boxplot(data$total_of_special_requests)
ggplot(data,aes(total_of_special_requests,fill=is_canceled)) +
  geom_bar(stat = "count", position = position_dodge()) +
  labs(title = "Booking Status by Special Requests",
       x = "Number of Special Requests",
       y = "Count") +
  scale_fill_discrete(
    name = "Booking Status")

table(data$is_canceled, data$total_of_special_requests)
prop.table(table(data$is_canceled, data$total_of_special_requests))
prop.table(table(data$is_canceled, data$total_of_special_requests),2)

#Most of guests didn't have any special requests 
#but as the number of requests increases, the likelihood of cancellations decreases

#Do repeated guests cancel more often then non repeated guests?
ggplot(data, aes(is_repeated_guest, fill = is_canceled)) + 
  geom_bar(position = position_dodge()) + 
  labs(title = "Cancelation by Repeated Guests",
       x = "Guest",
       y = "Number of guests") +
  scale_fill_discrete(
    name = "Canceled") 

table(data$is_canceled, data$is_repeated_guest)
prop.table(table(data$is_canceled, data$is_repeated_guest),2)

#There does not seem to be a connection between the percentage of guests who
#are repeat guests and cancellations.

#Does meal affect if a guest will cancel or not?

ggplot(data, aes(meal, fill = is_canceled)) + 
  geom_bar(position = position_dodge()) + 
  labs(title = "Cancelation Based on Meals",
       x = "Meal Type",
       y = "Number of guests") +
  scale_fill_discrete(
    name = "Canceled") 

table(data$is_canceled, data$meal)
prop.table(table(data$is_canceled, data$meal),2)

#There might be some kind of connection with meal type and cancellation, although
#that is doubtful. It is interesting to see that people with full board are more 
#likely to cancel then those who don't.

#Investigation of the adult, children and babies 

ggplot(data, aes(adults, fill = is_canceled)) + 
  geom_bar(position = position_dodge()) + 
  labs(title = "Cancelation Based on Adults",
       x = "Number of Adults",
       y = "Number of Guests") +
  xlim(0, 4) + 
  scale_fill_discrete(
    name = "Canceled") 

ggplot(data, aes(children, fill = is_canceled)) + 
  geom_bar(position = position_dodge()) + 
  labs(title = "Cancelation Based on Children",
       x = "Number of Children",
       y = "Number of Guests") +
  xlim(0, 4) +
  scale_fill_discrete(
    name = "Canceled") 

ggplot(data, aes(babies, fill = is_canceled)) + 
  geom_bar(position = position_dodge()) + 
  labs(title = "Cancelation Based on Babies",
       xlim(0, 4),
       x = "Number of Babies",
       y = "Number of Guests") +
  xlim(0, 4) +
  
  scale_fill_discrete(
    name = "Canceled") 

#There is no real connection between cancellations and children and babies. The
#only notable thing is that most adult cancellations come from parties with two
#adults in them.

#-------------------------
#Logistic Regression Model #1 - Keeping deposit type as it is a significant figure
#-------------------------

#Run the logistic regression model using the glm() function
logit.model <- glm(is_canceled ~ ., family = binomial(link = 'logit'), data = data) 
options(max.print=100000)
summary(logit.model)

#Only kept variables with significant values
logit.model_1 <- glm(is_canceled ~ hotel + lead_time +
                       arrival_date_year +
                       arrival_date_month +
                       arrival_date_week_number +
                       arrival_date_day_of_month +
                       stays_in_weekend_nights + 
                       stays_in_week_nights + adults + children +
                       meal +
                       market_segment +
                       distribution_channel + is_repeated_guest + reserved_room_type +
                       assigned_room_type + booking_changes +
                       deposit_type + customer_type + adr + total_of_special_requests,
                     family = binomial(link = 'logit'), data = data) 
summary(logit.model_1)

#All values are significant so are left in, with an AIC value of 107526
#check for multicollinearity
#use the car package
vif(logit.model_1)

#Error in vif.default(logit.model_1),there are aliased coefficients in the model
#Aliased coefficients in model were looked for and removed
alias( glm(is_canceled ~ .,family = binomial(link = 'logit'), data = data) ) 

#Assigned room type was the aliased coefficient. It was removed from the updated
#model and ran again.

logit.model_2 <- glm(is_canceled ~ hotel + lead_time +
                       arrival_date_year +arrival_date_month +
                       arrival_date_week_number + arrival_date_day_of_month +
                       stays_in_weekend_nights + stays_in_week_nights + adults +
                       children + meal + market_segment + distribution_channel + 
                       is_repeated_guest + reserved_room_type + booking_changes +
                       deposit_type + customer_type + adr + total_of_special_requests,
                     family = binomial(link = 'logit'), data = data) 
summary(logit.model_2)
#AIC: 110099

#check for multicollinearity
#use the car package
vif(logit.model_2)

#Remove arrival_date_month and reserved_room_type

logit.model_3 <- glm(is_canceled ~ hotel + lead_time  +
                       arrival_date_year + arrival_date_week_number + 
                       arrival_date_day_of_month +
                       stays_in_weekend_nights + stays_in_week_nights + adults +
                       children + meal + market_segment + distribution_channel + 
                       is_repeated_guest + booking_changes +
                       deposit_type + customer_type + adr + total_of_special_requests,
                     family = binomial(link = 'logit'), data = data) 
summary(logit.model_3)
#arrival_date_week_number and arrival_date_day_of_month no longer significante 
#and removed

logit.model_4 <- glm(is_canceled ~ hotel + lead_time  +
                       arrival_date_year + stays_in_weekend_nights + stays_in_week_nights + adults +
                       children + meal + market_segment + distribution_channel + 
                       is_repeated_guest + booking_changes +
                       deposit_type + customer_type + adr + total_of_special_requests,
                     family = binomial(link = 'logit'), data = data) 
summary(logit.model_4) 

#AIC = 110510

vif(logit.model_4)

#market_segment is removed

logit.model_5 <- glm(is_canceled ~ hotel + lead_time +
                       stays_in_weekend_nights + stays_in_week_nights + adults +
                       children + meal + distribution_channel + 
                       is_repeated_guest + booking_changes +
                       deposit_type + customer_type + adr + total_of_special_requests,
                     family = binomial(link = 'logit'), data = data) 
summary(logit.model_5) 

#AIC = 113663

vif(logit.model_5)
## all the VIF are <5, so there is no multicollinearity


# check the accuracy of the model
#create two subsets: one to build a predictive model, one for testing
data.train <- data[1:96000, ]
data.test <- data[-c(1:96000), ]

logit.model.train <- glm(is_canceled ~ hotel + lead_time + stays_in_weekend_nights + stays_in_week_nights + adults +
                           children + meal + distribution_channel + 
                           is_repeated_guest + booking_changes +
                           deposit_type + customer_type + adr + total_of_special_requests,
                         family = binomial(link = 'logit'), data = data.train) 
summary(logit.model.train)

#predict cancellation using the model
#probability
data.test$cancellation.predict <- predict(logit.model.train, newdata = data.test, type = 'response')
head(data.test$cancellation.predict)

#value 0/1
data.test$cancellation.predict <- ifelse(data.test$cancellation.predict > 0.5, "Canceled", "Not Canceled")
head(data.test$cancellation.predict)

#calculate the prediction accuracy
misclassificationError <- mean(data.test$cancellation.predict != data.test$is_canceled)
misclassificationError <- round(misclassificationError, digits = 2 )
print(paste('Prediction accuracy =', 1 - misclassificationError)) 
#prediction accuracy = 51%, I think it's not so good, maybe it's because we have too many variables or something else

#create tables to better understand where the errors are from
tab <- table(data.test$is_canceled , data.test$cancellation.predict)
tab
prop.table(tab,1)
mosaicplot(tab, cex = 0.5, color = TRUE)
#We can see that 49.4% of the bookings that were not actually canceled were forecast to be canceled



#-------------------------
#Logistic Regression Model #2 - Removing deposit type 
#-------------------------

#the dataset without deposity type
data_nodeposittype <- data[,-21]
data.train_1 <- data_nodeposittype[1:96000, ]
data.test_1 <- data_nodeposittype[-c(1:96000), ]

logit.model.train_1 <- glm(is_canceled ~ hotel + lead_time + stays_in_weekend_nights + stays_in_week_nights + adults +
                             children + meal + distribution_channel + 
                             is_repeated_guest + booking_changes +
                             customer_type + adr + total_of_special_requests,
                           family = binomial(link = 'logit'), data = data.train_1) 
summary(logit.model.train_1)

#remove stays_in_weekend_nights, stays_in_week_nights,adults, children
logit.model.train_2 <- glm(is_canceled ~ hotel + lead_time + meal + distribution_channel + 
                             is_repeated_guest + booking_changes +
                             customer_type + adr + total_of_special_requests,
                           family = binomial(link = 'logit'), data = data.train_1) 
summary(logit.model.train_2)

#predict cancellation using the model
#probability
data.test_1$cancellation.predict <- predict(logit.model.train_2, newdata = data.test_1, type = 'response')
head(data.test_1$cancellation.predict)

#value 0/1
data.test_1$cancellation.predict <- ifelse(data.test_1$cancellation.predict > 0.5, "Canceled", "Not Canceled")
head(data.test_1$cancellation.predict)

#calculate the prediction accuracy
misclassificationError_1 <- mean(data.test_1$cancellation.predict != data.test_1$is_canceled)
misclassificationError_1 <- round(misclassificationError_1, digits = 2 )
print(paste('Prediction accuracy =', 1 - misclassificationError_1)) 
#prediction accuracy = 42%, the result is worse so we will keep the deposit type

#create tables to better understand where the errors are from
tab_1 <- table(data.test_1$is_canceled , data.test_1$cancellation.predict)
tab_1
prop.table(tab_1,1)


#predict a decision for an application
predictors <- data.frame(hotel="City Hotel", lead_time=60, stays_in_weekend_nights=0,
                         stays_in_week_nights=2, adults=2,  children=0, meal="BB",distribution_channel="TA/TO",
                         is_repeated_guest="Not Repeated Guest", booking_changes=0, deposit_type="No Deposit",
                         customer_type="Transient", adr=120, total_of_special_requests=0)

predict(logit.model.train,predictors,type = "response")
#The result = 0.62 > 0.5, so the guest will probably cancel this booking
