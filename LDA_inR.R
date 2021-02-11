#LDA analysis in R
setwd("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets")

library(tidyverse)
library(lubridate)

dat2018<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/processedtweets2018.csv")
dat2019<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/processedtweets2019.csv") %>% filter(PUBLISH_DATE!= "")
dat2020<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/processedtweets2020.csv") %>% filter(PUBLISH_DATE!= "")
datNEW<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/processedtweetsNEW.csv") %>% filter(PUBLISH_DATE!= "")
allData<- rbind(dat2018, dat2019, dat2020, datNEW) %>% mutate(newDate=as.Date(PUBLISH_DATE, "%m/%d/%Y")) %>%
  mutate(tweetYear= year(newDate), tweetMonth= month(newDate), tweetDay=day(newDate)) %>%
  group_by(tweetYear) %>% add_count(tweetYear, name="totalYearlyTweets") %>%
  group_by(tweetMonth) %>% add_count(tweetMonth, name= "totalMonthlyTweets") %>%
  ungroup() %>%
  mutate(tweetWeek= week(newDate)) %>% add_count(tweetWeek, name= "totalWeeklyTweets") %>% #isoweek instead of week is probably needed for Trends data
  distinct(ARTICLE_URL, .keep_all = TRUE) #when we brought in the new data there was overlap but we didnt remove it in the code because we needed identical dictionaries
  
allData <- allData %>% mutate(tweetYear= as.factor(tweetYear), tweetMonth= as.factor(tweetMonth), tweetWeek = as.factor(tweetWeek))
noSpam <- allData %>% filter(Dominant_Topic %in% c(8,4,7,6,0,1))

#redo all graphs that include 4Q 2020, we had a minor error there.

#Pull out the top 200 tweets in each topic to aid in naming the topics
representativeTweets<- allData %>% group_by(Dominant_Topic) %>% top_n(200, Perc_Contribution)
data.table::fwrite(representativeTweets, file="representativeTweets.csv")

#do it just for 2018 
representativeTweets2018<- dat2018 %>% group_by(Dominant_Topic) %>% top_n(200, Perc_Contribution)
data.table::fwrite(representativeTweets2018, file="representativeTweets2018.csv")

#Categories we want to keep:
#8- Personal Ex
#4- E-cig
#7- Advice/Success
#6- Clinics/Services
#0- Health Changes
#1- Need to Quit


##############################################################################################################
# PLOTS ######################################################################################################
##############################################################################################################


#total daily tweets with "quit smoking" over time
ggplot(allData, aes(x=newDate, group=1)) + 
  geom_line(stat="count", color="blue") + 
  ylab("Daily Tweets") + 
  xlab("Date") +
  labs(title="Total Daily Tweets by Date") +
  theme(plot.title = element_text(hjust = 0.5))


#total daily tweets with "quit smoking" over time
ggplot(noSpam, aes(x=newDate, group=1)) + 
  geom_line(stat="count", color="blue") + 
  ylab("Daily Tweets") + 
  xlab("Date") +
  labs(title="Total Daily Tweets of 6 Topics by Date") +
  theme(plot.title = element_text(hjust = 0.5))


#overlapped monthly
ggplot(allData, aes(x=tweetMonth, group=tweetYear)) + 
  geom_line(stat="count", size=1.5, aes(color=tweetYear)) + 
  geom_point(stat="count", size=2, aes(shape=tweetYear)) + 
  ylab("Monthly Tweets") +
  xlab("Month") +
  labs(title="Total Tweets per Month, Stratified by Year", color="Tweet Year", shape="Tweet Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks=1:12, labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D"))

#bar graph monthly
ggplot(allData, aes(x=tweetMonth, group=tweetYear)) + 
  geom_bar(stat="count", position="dodge", aes(fill=tweetYear)) +
  ylab("Monthly Tweets") +
  xlab("Month") + 
  labs(title="Monthly Total Tweets by Year", fill= "Year") +
  theme(plot.title = element_text(hjust = 0.5))

###########################################################################################################
#Scaled Versions of the above graphs will require some data manipulation ##################################
###########################################################################################################

reducedData <- allData %>% group_by(tweetYear) %>% count(tweetMonth, name="monthlyTweets")  %>% mutate(yearlyMax= max(monthlyTweets)) %>%
  mutate(scaledProp= (monthlyTweets/yearlyMax)*100)

data.table::fwrite(reducedData, file="monthlyTotalTweets.csv")

#overlapped monthly
ggplot(reducedData, aes(x=tweetMonth, y= scaledProp, group=tweetYear)) + 
  geom_line(stat="identity", size=1, aes(color=tweetYear)) + 
  geom_point(stat="identity", size=3, aes(shape=tweetYear)) + 
  ylab("Proportion of Max Monthly Tweets") +
  xlab("Month") +
  labs(title="Scaled Tweets per Month, Stratified by Year", color="Tweet Year", shape="Tweet Year") +
  theme(plot.title = element_text(hjust = 0.5)) 

#bar graph monthly
ggplot(reducedData, aes(x=tweetMonth, y= scaledProp, group=tweetYear)) + 
  geom_bar(stat="identity", position="dodge", aes(fill=tweetYear)) +
  ylab("Proportion of Max Monthly Tweets") +
  xlab("Month") +
  labs(title="Scaled Tweets per Month", fill="Tweet Year") +
  theme(plot.title = element_text(hjust = 0.5)) 

##########################################################################################################
# Topics #################################################################################################
##########################################################################################################


filteredData <- allData %>% filter(Dominant_Topic %in% c(8,4,7,6,0,1)) %>% group_by(tweetYear, tweetMonth) %>% count(Dominant_Topic) %>%
  group_by(Dominant_Topic) %>% mutate(topicMax= max(n)) %>% mutate(scaledProp= (n/topicMax)*100) %>%
  mutate(newTweetMonth= ifelse(tweetYear==2019, as.numeric(tweetMonth) + 12, ifelse(tweetYear==2020, as.numeric(tweetMonth) +24, as.numeric(tweetMonth)))) %>%
  mutate(newTweetMonth = as.factor(newTweetMonth), Dominant_Topic = as.factor(Dominant_Topic))

reformatted<- filteredData %>% mutate(newTopicNames= ifelse(Dominant_Topic==0, "Health Ch",
                                                            ifelse(Dominant_Topic==1, "Need to Qt", 
                                                                   ifelse(Dominant_Topic==4, "E-Cig", 
                                                                          ifelse(Dominant_Topic==6, "Clinics/Services",
                                                                                 ifelse(Dominant_Topic==7, "Advice/Success",
                                                                                        ifelse(Dominant_Topic==8, "Personal Exp", "NA")))))))


#scaled

ggplot(reformatted, aes(x=newTweetMonth, y=scaledProp, group=newTopicNames)) + 
  geom_point(stat="identity", size=3, aes(shape=newTopicNames)) + 
  geom_line(stat="identity", aes(color=newTopicNames)) + 
  labs(title="Scaled Topic Trends", shape="Topic", color="Topic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Month") +
  ylab("Scaled Tweets") + 
  scale_x_discrete(breaks=1:36, labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D"))+
  scale_shape_manual(values=c(0,1,15,16,8,2)) +
  geom_vline(xintercept = c(12.5,24.5))

#not scaled



ggplot(reformatted, aes(x=newTweetMonth, y=n, group=newTopicNames)) + 
  geom_point(stat="identity", size=3, aes(shape=newTopicNames)) + 
  geom_line(stat="identity", aes(color=newTopicNames)) + 
  labs(title="Selected Topic Trends", shape="Topic", color="Topic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Month") +
  ylab("Total Tweets") + 
  scale_x_discrete(breaks=1:36, labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D"))+
  scale_shape_manual(values=c(0,1,15,16,8,2)) +
  ylim(0,3600) +
  geom_vline(xintercept = c(12.5,24.5))

#Let's look at weeks instead of months

weeklyTopicData<- allData %>% filter(Dominant_Topic %in% c(8,4,7,6,0,1)) %>% group_by(tweetYear, tweetWeek) %>% count(Dominant_Topic) %>%
  group_by(Dominant_Topic) %>% mutate(topicMax= max(n)) %>% mutate(scaledProp= (n/topicMax)*100) %>%
  mutate(newTweetWeek= ifelse(tweetYear==2019, as.numeric(tweetWeek) + 53, ifelse(tweetYear==2020, as.numeric(tweetWeek) +106, as.numeric(tweetWeek)))) %>%
  mutate(newTweetWeek = as.factor(newTweetWeek), Dominant_Topic = as.factor(Dominant_Topic))

reformattedWeekly<- weeklyTopicData %>% mutate(newTopicNames= ifelse(Dominant_Topic==0, "Health Ch",
                                                                     ifelse(Dominant_Topic==1, "Need to Qt", 
                                                                            ifelse(Dominant_Topic==4, "E-Cig", 
                                                                                   ifelse(Dominant_Topic==6, "Clinics/Services",
                                                                                          ifelse(Dominant_Topic==7, "Advice/Success",
                                                                                                 ifelse(Dominant_Topic==8, "Personal Exp", "NA")))))))

#not scaled
ggplot(reformattedWeekly, aes(x=newTweetWeek, y=n, group=newTopicNames)) + 
  geom_point(stat="identity", size=3, aes(shape=newTopicNames)) + 
  geom_line(stat="identity", aes(color=newTopicNames)) + 
  labs(title="Selected Topic Trends", shape="Topic", color="Topic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Week") +
  ylab("Total Tweets") +
  scale_x_discrete(breaks=seq(0,156,5)) +
  scale_shape_manual(values=c(0,1,15,16,8,2)) +
  ylim(0,2000) +
  geom_vline(xintercept = c(53,106))

# scaled
ggplot(reformattedWeekly, aes(x=newTweetWeek, y=scaledProp, group=newTopicNames)) + 
  geom_point(stat="identity", size=3, aes(shape=newTopicNames)) + 
  geom_line(stat="identity", aes(color=newTopicNames)) + 
  labs(title="Scaled Topic Trends", shape="Topic", color="Topic") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Week") +
  ylab("Total Tweets") +
  scale_x_discrete(breaks=seq(0,156,5)) +
  scale_shape_manual(values=c(0,1,15,16,8,2)) +
  geom_vline(xintercept = c(53,106))
  
  
data.table::fwrite(filteredData, file="goodTopicTables.csv")
#############################################################################################################################
#Google Trends Data is weird to work with because of weeks ##################################################################
#############################################################################################################################

# trendsData<- read.csv("C:/Users/matth/Full_Study/trendsData.csv") %>% mutate(Week= as.Date(Week, "%m/%d/%Y"))
# weeklyCount<- allData %>% group_by(tweetYear) %>% count(newDate) %>% mutate(testWeek = 1)
# 
# #Because the Trends data has each week being seven days and carrying over through a january first we need to re-week the twitter data to match...
# 
# for(i in 1:length(trendsData$Week)) {
#   for (j in 1:length(weeklyCount$newDate)) {
#     if (weeklyCount$newDate[j] > trendsData$Week[i]) {
#       weeklyCount$testWeek[j] = i+1
#     }
#   }
# }
# 
# finalTwitter<- weeklyCount %>% group_by(testWeek) %>% count(wt=n, name="weeklySum") %>% filter(testWeek != 131) %>% 
#   mutate(scaledProp= (weeklySum/4881)*100)
# 
# trendsData<- trendsData %>% mutate(testWeek= 1:130)
# 
# FINALTRENDS<- left_join(finalTwitter, trendsData)
# data.table::fwrite(FINALTRENDS, file="twitterTrends_Combined.csv")

twitterTrends_Combined<- read.csv("C:/Users/matth/Full_Study/twitterTrends_Combined.csv") %>% mutate(scaledProp = floor(scaledProp))

ggplot(twitterTrends_Combined, aes(x=testWeek)) + 
  geom_point(stat="identity", aes(y=scaledProp), shape=1) +
  geom_line(stat="identity", aes(y=scaledProp), color="blue") +
  geom_point(stat="identity", aes(y=RSF)) +
  geom_line(stat="identity", aes(y=RSF), color="red") +
  scale_x_continuous(breaks=seq(0,130,10))

#group it so we can get a legend
Google<- twitterTrends_Combined %>% select(testWeek, RSF) %>% rename(scaledProp= RSF) %>% mutate(source="Google")
Twitter<- twitterTrends_Combined %>% select(testWeek, scaledProp) %>% mutate(source="Twitter")
newfile<- rbind(Google, Twitter)

ggplot(newfile, aes(x=testWeek, y=scaledProp, group=source)) +
  geom_point(stat="identity", size=2, aes(shape=source)) +
  geom_line(stat="identity", size=1, aes(color=source)) +
  scale_x_continuous(breaks=seq(0,130,26), labels=c("Jan18", "Jul18", "Jan19", "Jun19", "Jan20", "Jun20")) +
  scale_color_manual(values=c("blue", "red")) +
  scale_shape_manual(values=c(1,15)) +
  xlab("Week") +
  ylab("Scaled Proportion of Max Activity") +
  labs(title="Comparison of Scaled Google and Twitter Activity") +
  theme(plot.title = element_text(hjust = 0.5)) 

