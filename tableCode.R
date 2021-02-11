################################################################################
#Make Tables of Tweets                                                         #
################################################################################

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


#add a column for each individual search term as well as an indicator if either is present
allData$coronavirus <- grepl("coronavirus", allData$CONTENT, ignore.case = TRUE)
allData$covid <- grepl("covid", allData$CONTENT, ignore.case = TRUE)
allData$hasCOVID <- ifelse(allData$coronavirus == TRUE | allData$covid == TRUE, 1, 0)

#2018 quarterly all tweets
all_2018_1Q <- allData %>% filter(tweetYear==2018, tweetMonth %in% c(1,2,3)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==8)
all_2018_2Q <- allData %>% filter(tweetYear==2018, tweetMonth %in% c(4,5,6)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==8)
all_2018_3Q <- allData %>% filter(tweetYear==2018, tweetMonth %in% c(7,8,9)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==8)
all_2018_4Q <- allData %>% filter(tweetYear==2018, tweetMonth %in% c(10,11,12)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==8)

#2019 quarterly all tweets
all_2019_1Q <- allData %>% filter(tweetYear==2019, tweetMonth %in% c(1,2,3)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==8)
all_2019_2Q <- allData %>% filter(tweetYear==2019, tweetMonth %in% c(4,5,6)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==8)
all_2019_3Q <- allData %>% filter(tweetYear==2019, tweetMonth %in% c(7,8,9)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==8)
all_2019_4Q <- allData %>% filter(tweetYear==2019, tweetMonth %in% c(10,11,12)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==8)

#2020 quarterly all tweets
all_2020_1Q <- allData %>% filter(tweetYear==2020, tweetMonth %in% c(1,2,3)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==1)
all_2020_2Q <- allData %>% filter(tweetYear==2020, tweetMonth %in% c(4,5,6)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==1)
all_2020_3Q <- allData %>% filter(tweetYear==2020, tweetMonth %in% c(7,8,9)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==1)
all_2020_4Q <- allData %>% filter(tweetYear==2020, tweetMonth %in% c(10,11,12)) %>% filter(hasCOVID == TRUE) %>% filter(Dominant_Topic==1)

#only our six topics we felt were non-spammy
filteredData <- allData %>% filter(Dominant_Topic %in% c(8,4,7,6,0,1)) %>% group_by(tweetYear, tweetMonth) %>% count(Dominant_Topic)

#Categories we want to keep:
#8- Personal Ex
#4- E-cig
#7- Advice/Success
#6- Clinics/Services
#0- Health Changes
#1- Need to Quit


