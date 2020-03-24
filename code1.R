#
df <- read.csv("hotel_bookings.csv")

#Exploratory Data Analysis(EDA)
head(df)
dim(df)
#
#[1] 119390     32
# In total, there are more than one hundered thousand
# observations and  32 varaibles and  of which one is the
# response variable(reservation_status).I am displayting
# only a glimpse of the variables. All of them I will
# discuss in more detail throught the project.
# str(df[,c(1:32)])
summary(df)
# from the summary, I found there are 4 missing values in
# children column. Because 4 is very small, therefore I just
# drop down them directly.
newdf <- df[complete.cases(df),]
summary(newdf$is_canceled)
summary(newdf$reservation_status)
newdf$arrival_date_month <- factor(newdf$arrival_date_month, levels = month.name)
#I create the plot, and I found the month is disorder, do I order the month chronological, form January to December.
newdf
str(newdf[,c(1:32)])

# from the dataset, we can easily found we have 2 levels for Hotel,
# so before we take the next step to Exploratory Data Analysis,
# I think it's a good try to have some information about the ratio
# of preference for City hotel and resort hotel.

resort_hotel <- newdf[which(newdf$reservation_status!="No-Show"& newdf$hotel == "Resort Hotel"),]
city_hotel <- newdf[which(newdf$reservation_status!="No-Show"&newdf$hotel == "City Hotel"),]

#!!!!!!!!!!!Exploring some of the most important variables 

#pie chart
# mytable <- table(resort_hotel$country)
# lbls <- paste(names(mytable),"\n",mytable,sep="")
# pie(mytable, label = lbls,main= "home country of hotel guest")

#the guest who are staying in these 2 hotels are from all over the world.
#Most of the guest are Most guests are from Portugal and other countries in Europe. 

#2
#.Both resort hotel and city hotel have different room types and different meal arrangements. Seasonal 
#factors are also important. So the prices vary a lot.

#to count the money guest should pay per night per person.
resort_hotel$adr_pp <- resort_hotel$adr/(resort_hotel$adults+resort_hotel$children)
city_hotel$adr_pp <- city_hotel$adr/(city_hotel$adults+city_hotel$children)

#ag means acutal_guest data"
all_ag <- subset(newdf,is_canceled!="1")
all_ag$adr_pp <- all_ag$adr/(all_ag$adults+all_ag$children)
room_price <- subset(all_ag,select=c(hotel,reserved_room_type,adr_pp))
#boxplot 
ggplot(data = room_price, aes(x =reserved_room_type, y = adr_pp,
                              fill=hotel))+ geom_boxplot()+ labs(title = "                          Price of room types per night and person",x = "Room type",y="Price")


#This figure shows the average price per room, depending on its type and the standard deviation.
#Note that due to data anonymization rooms with the same type letter may not necessarily be the same across hotels.

#????Q
#Q4:
#How does the price per night vary over the year?
#room_price_month_trend 
room_price_mt <- subset(all_ag,select=c(hotel,arrival_date_month,adr_pp,days_in_waiting_list))
room_price_mt$arrival_date_month<- factor(room_price_mt$arrival_date_month, levels = month.name)
ggplot(data = room_price_mt, aes(x = arrival_date_month)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + labs(title = "Month Wise Booking Request",
                      x = "Month",
                      y = "Count") +
  theme_classic()


# room_price_mt2<-subset(all_ag,select=c(hotel,arrival_date_month,adr_pp,is_canceled))
# room_price_mt2$arrival_date_month<- factor(room_price_mt$arrival_date_month, levels = month.name)
# ggplot(room_price_mt2, aes(arrival_date_month, fill = factor(is_canceled))) +
#   geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
#   coord_flip() + scale_fill_discrete(
#     name = "Booking Status",
#     breaks = c("0", "1"),
#     label = c("Cancelled", "Not Cancelled")
#   ) +
#   labs(title = "Booking Status by Month",
#        x = "Month",
#        y = "Count") + theme_bw()

# From the month wise booking analysis, we found out that most number of hotel booking request came
# in the month of July and August followed by May and October. One reason for this may be the weather
# impact as these are the months of pleasant weather in Europe.


#Q5:Visualize Hotel traffic on Monthly basis
ggplot(room_price_mt, aes(arrival_date_month, fill = hotel)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()




# MONTHRESV<-table(newdf$arrival_date_month)
# MONTHRESV<-data.frame(MONTHRESV)
# MONTHRESV$arrival_date_month<-factor(MONTHRESV$arrival_date_month, levels=month.name)
# ggplot(MONTHRESV, aes(x=arrival_date_month, y=Freq, group=1)) + geom_line(col="navy") + 
#   ggtitle("Reservations by Arrival Month") + ylab("Count") + xlab("Month")+
#   theme(axis.text.x=element_text(angle=40))

ggplot(room_price_mt,aes(x=arrival_date_month, y=days_in_waiting_list,group=1,fill=hotel)) + stat_summary(fun="mean", geom="line", col="hotpink1") + 
  ggtitle("Average Days on Waiting List by Arrival Month") + ylab("Average Days on WaitingList") + xlab("Month") +theme_bw() 


#room_price_mt$arrival_date_month <- factor(room_price_mt$arrival_date_month
#new_room_price_mt <- mutate(room_price_mt,factor(arrival_date_month,levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
#ggplot(new_room_price_mt, aes(arrival_date_month,adr_pp)) + geom_l()
# p <- ggplot(room_price_mt,aes(x = arrival_date_month,y = adr_pp,group = hotel))+geom_point()
# p
#I count adults and children as paying guest only,not including babies.
#the average prices are:


# Q6???
#Cost per night per person by Hotel

ggplot(room_price_mt, aes(x = adr_pp, fill = hotel, color = hotel)) + 
  geom_histogram(aes(y = ..density..), position = position_dodge(), binwidth = 20 ) +
  geom_density(alpha = 0.2) + 
  labs(title = "Cost per night per person by Hotel",
       x = "Hotel Price",
       y = "Count") + scale_color_brewer(palette = "Paired") + 
  theme_classic() + theme(legend.position = "top")


#Q7:Hotel Preference by customer type

# ggplot(room_price_mt, aes(customer_type, fill = hotel)) + 
#   geom_bar(stat = "count", position = position_dodge()) + 
#   labs(title = "Hotel Preference by Customer Type",
#        x = "Customer Type",
#        y = "Count") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         panel.background = element_blank())


# Q8:Cancelation

copy_newdf <- read.csv("hotel_bookings.csv",stringsAsFactors = T)
# numeric_vars
numeric_vars <-which(sapply(copy_newdf,is.numeric))
factor_vars <- which(sapply(copy_newdf, is.factor)) 
cat('There are', length(numeric_vars), 'numeric variables, and', length(factor_vars), 'categoric variables')

#There are 18 numeric variables, and 14 categoric variables
summary(newdf$reservation_status)
#It shows Canceled Check-Out   No-Show 
#43013     75166      1207

#the no-show is very small amount, so I create a new dataframe which doesn't choose the no-show status observation.

copy_newdf <-data.frame(newdf)
copy_newdf <- subset(copy_newdf,reservation_status!="No-Show")

#Continuous Variables
#ggplot(data = copy_newdf, aes(adr , color = reservation_status))+geom_freqpoly(binwidth = 5, size = 1)+xlim(0,300)
#Then we can find when adr=60, the frequency of cancel reservation is the highest



#below is just a try, try to find something is distribution

# ggplot(data = copy_newdf, aes(lead_time, color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1) 
# 
# ggplot(data = copy_newdf, aes(arrival_date_year, color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1) 
# ggplot(data = copy_newdf, aes(arrival_date_day_of_month , color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1) 
# ggplot(data = copy_newdf, aes(stays_in_weekend_nights , color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1) 
# ggplot(data = copy_newdf, aes(stays_in_week_nights  , color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1) 
# ggplot(data = copy_newdf, aes(adults , color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1) 
# ggplot(data = copy_newdf, aes( children  , color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1) 
# ggplot(data = copy_newdf, aes(is_repeated_guest, color = reservation_status))+
#   geom_freqpoly() 
# ggplot(data = copy_newdf, aes(previous_cancellations, color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1)
# ggplot(data = copy_newdf, aes(previous_bookings_not_canceled, color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1)
# ggplot(data = copy_newdf, aes(booking_changes , color = reservation_status))+
#   geom_freqpoly(binwidth = 5, size = 1)

# Split data for training and test
copy_newdf$adr_pp <- copy_newdf$adr/(copy_newdf$adults+copy_newdf$children)
sample_size = floor(0.8*nrow(copy_newdf))
train_ind = sample(seq_len(nrow(copy_newdf)), size = sample_size)
train = copy_newdf[train_ind,]
test = copy_newdf[-train_ind,]



#model3 <- glm(formula = is_canceled~lead_time+booking_changes+adr_pp+required_car_parking_spaces+total_of_special_requests+deposit_type,family ="binomial",data=train)
summary(model3)

#correlations:find the most relavant numerical varivales , which help me train the model later on.
all_numVar <- copy_newdf[, numeric_vars]
cor_numVar <- cor(all_numVar)
cor_sorted <- as.matrix(sort(cor_numVar[,'is_canceled'],decreasing = TRUE))
cor_sorted

#From this list it is apparent that lead_time, total_of_special_requests,
# required_car_parking_spaces, booking_changes and previous_cancellations 
# are the 5 most important numerical features.

# cor_sorted <- dfOrder(cor_sorted,1,absolute = False,ascending = FALSE)
# CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
# cor_numVar <- cor_numVar[CorHigh, CorHigh]
# corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
# just select the important feature


#Although the correlations are giving a good overview of the most important numeric variables and 
# , I wanted to get an overview of the most important variables
# including the categorical variables before moving on to visualization.
#As I only want to get an indication of the variable importance,I eventually decided to keep it simple
#and just use a quick and dirty Random Forest model with only 100 trees. 

drops <- c("company","country","adr_pp","company","reservation_status_date","agent","reservation_status")
copy_newdf<-copy_newdf[ , !(names(copy_newdf) %in% drops)]
copy_newdf$is_canceled<- as.factor(copy_newdf$is_canceled)

# set.seed(1)
# train1 <- sample(1:nrow(copy_newdf),nrow(copy_newdf)/2)
# bag.copy_newdf <- randomForest(is_canceled~.,data=copy_newdf,subset=train1,na.action=na.omit,mtry=27,importance=TRUE)
# bag.copy_newdf
# yhat.bag <- predict(bag.copy_newdf,newdata=copy_newdf[-train1,])
# test1 <-copy_newdf[-train1,"is_canceled"]
# mean((yhat.bag_test1)^2)



#Variable importance using quick random forest:

set.seed(2020)
quick_tree <- randomForest(is_canceled~.,data = copy_newdf, na.action=na.omit,ntree=100,importance=TRUE)
imp_RF <- importance(quick_tree)
imp_RF
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]
imp_DF


ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")


set.seed(2020)
data_RF = df[sample(1:nrow(copy_newdf),0.02*nrow(copy_newdf),replace = FALSE),]
data_RF = na.omit(data_RF)
data_RF$is_canceled<- as.factor(data_RF$is_canceled)
drops <- c("company","country","adr_pp","reservation_status_date","agent","reservation_status")
data_RF<-data_RF[ , !(names(data_RF) %in% drops)]
smp_size = floor(0.80*nrow(data_RF))
smp_size
train_ind = sample(seq_len(nrow(data_RF)),size=smp_size)
train1 = data_RF[train_ind,]
test1 = data_RF[-train_ind,]
# require(caTools)
# sample <- sample.split(data_RF,SplitRatio=0.80)
# train1 = subset(data_RF,sample==TRUE)
# test1 = subset(data_RF,sample==FALSE)



#because the random forest can't not Can not handle categorical predictors with more than 53 categories,so I drop some variables
#rfcv(data_RF,is_canceled_v , cv.fold=5, scale="log", step=0.5)

#random forest model1:
set.seed(2020)
RF_model1<- randomForest( is_canceled~.,data = data_RF, na.action=na.omit,ntree=5,importance=TRUE, proximity=TRUE,do.trace=T)
conf1 <- RF_model1$confusion
conf1
RF_model1$confusion[, 'class.error']
accuracy1 <- 1- mean(RF_model1$confusion[, 'class.error'])
accuracy1

#random forest model2:
set.seed(2020)
RF_model2<- randomForest( is_canceled~.,data = data_RF, na.action=na.omit,ntree=50,importance=TRUE, proximity=TRUE,do.trace=T)
conf2 <- RF_model2$confusion
conf2
accuracy2 <- 1- mean(RF_model2$confusion[, 'class.error'])
accuracy2

#random forest model3:
set.seed(2020)
RF_model3<- randomForest( is_canceled~.,data = data_RF, na.action=na.omit,ntree=100,importance=TRUE, proximity=TRUE,do.trace=T)
conf3 <- RF_model3$confusion
conf3
accuracy3 <- 1- mean(RF_model3$confusion[, 'class.error'])
accuracy3

#random forest model4:
set.seed(2020)
RF_model4<- randomForest( is_canceled~.,data = data_RF, na.action=na.omit,ntree=200,importance=TRUE, proximity=TRUE,do.trace=T)
conf4 <- RF_model4$confusion
conf4
accuracy4 <- 1- mean(RF_model4$confusion[, 'class.error'])
accuracy4




#random forest model5:
set.seed(2020)
RF_model5<- randomForest( is_canceled~.,data = data_RF, na.action=na.omit,ntree=300,importance=TRUE, proximity=TRUE,do.trace=T)
conf5 <- RF_model5$confusion
conf5
accuracy5 <- 1- mean(RF_model5$confusion[, 'class.error'])
accuracy5

#random forest model6:
set.seed(2020)
RF_model6<- randomForest( is_canceled~.,data = data_RF, na.action=na.omit,ntree=500,importance=TRUE, proximity=TRUE,do.trace=T)
conf6 <- RF_model6$confusion
conf6
accuracy6 <- 1- mean(RF_model6$confusion[, 'class.error'])
accuracy6

c_names <-c("5","50","100","200","300","500")
c_accuracy <- c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5,accuracy6)
plot(x=c_names,y=c_accuracy,type="p",main="Accuracy of random forest model",xlab= "number of tree", ylab = "Accurary",col="lightpink",pch=16,cex=4)



# new_train <- subset(train,select=c(is_canceled,lead_time,booking_changes,adr_pp,
# required_car_parking_spaces,total_of_special_requests,deposit_type))
# new_test <- subset(test,select=c(is_canceled,lead_time,booking_changes,adr_pp,
# required_car_parking_spaces,total_of_special_requests,deposit_type))

#At first step, just take a part of the data set to do the logistic regression.
set.seed(2020)
data_logistic = df[sample(1:nrow(copy_newdf),0.02*nrow(copy_newdf),replace = FALSE),]
data_logistic = na.omit(data_logistic)
head(data_logistic)

#model1:
lr_model1<- glm(is_canceled ~ lead_time +customer_type + hotel +deposit_type + adr +total_of_special_requests, data = data_logistic , family = "binomial")
summary(lr_model1)
data_logistic$predict <-predict(lr_model1, type = 'response')
p = mean(data_logistic$is_canceled)
p
# P = 0.3757935

#caculate the model accurary
data_logistic$predictions <- ifelse(data_logistic$predict >p , 1,0)
mean(data_logistic$is_canceled == data_logistic$predictions)
#0.7465087
#The model accuracy is 74.65 % and now we going to see the auc and roc parameters
ROC <- roc(data_logistic$is_canceled , data_logistic$predictions)
plot(ROC, col = 'blue', main = "ROC curve to hotel booking model1")
auc(ROC)
#Area under the curve: 0.7208
#model2: use the top6 numerical variables
lr_model2<- glm(is_canceled ~ lead_time +previous_cancellations + total_of_special_requests +required_car_parking_spaces+booking_changes+is_repeated_guest, data = data_logistic , family = "binomial")
summary(lr_model2)
data_logistic$predict2 <-predict(lr_model2, type = 'response')
data_logistic$predictions2 <- ifelse(data_logistic$predict2 >p , 1,0)
mean(data_logistic$is_canceled == data_logistic$predictions2)
# 0.7257723
#The model accuracy is 72.57 % and now we going to see the auc and roc parameters
ROC2 <- roc(data_logistic$is_canceled , data_logistic$predictions2)
plot(ROC2, col = 'blue', main = "ROC curve to hotel booking model2")
auc(ROC2)
#Area under the curve: 0.7228
#model3:
lr_model3<- glm(is_canceled ~ total_of_special_requests +arrival_date_month+lead_time+required_car_parking_spaces, data = data_logistic , family = "binomial")
summary(lr_model3)
data_logistic$predict3 <-predict(lr_model3, type = 'response')
data_logistic$predictions3 <- ifelse(data_logistic$predict3 >p , 1,0)
mean(data_logistic$is_canceled == data_logistic$predictions3)
#0.6796445
ROC3 <- roc(data_logistic$is_canceled , data_logistic$predictions3)
plot(ROC3, col = 'blue', main = "ROC curve to hotel booking model3")
auc(ROC3)
#Area under the curve: 0.6842
