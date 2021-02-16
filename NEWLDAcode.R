################################################################################
#Updated the data for new analyses                                             #
################################################################################

#LDA analysis in R
setwd("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets")

library(tidyverse)
library(lubridate)

dat2018<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/processedWide2018.csv")
dat2019<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/processedWide2019.csv") 
dat2020<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/processedWide2020.csv")
datNEW<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets/processedWideNEW.csv") 

wideData<- rbind(dat2018, dat2019, dat2020, datNEW) %>% mutate(newDate=as.Date(PUBLISH_DATE, "%m/%d/%Y")) %>%
  mutate(tweetYear= year(newDate), tweetMonth= month(newDate), tweetDay=day(newDate)) %>%
  group_by(tweetYear) %>% add_count(tweetYear, name="totalYearlyTweets") %>%
  group_by(tweetMonth) %>% add_count(tweetMonth, name= "totalMonthlyTweets") %>%
  ungroup() %>% 
  mutate(tweetWeek= week(newDate)) %>% add_count(tweetWeek, name= "totalWeeklyTweets") %>% #isoweek instead of week is probably needed for Trends data
  distinct(ARTICLE_URL, .keep_all = TRUE) #when we brought in the new data there was overlap but we didnt remove it in the code because we needed identical dictionaries
 

#Separate out the topics and percentages, along with the date for graphing and the unique ID variable (ARTICLE_URL)
A <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, zero_top, zero_perc) %>% rename(Topic = zero_top, Probability = zero_perc)
B <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, one_top, one_perc) %>% filter(is.na(one_top)==FALSE) %>% rename(Topic = one_top, Probability = one_perc)
C <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, two_top, two_perc) %>% filter(is.na(two_top)==FALSE) %>% rename(Topic = two_top, Probability = two_perc)
D <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, three_top, three_perc) %>% filter(is.na(three_top)==FALSE) %>% rename(Topic = three_top, Probability = three_perc)
E <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, four_top, four_perc) %>% filter(is.na(four_top)==FALSE) %>% rename(Topic = four_top, Probability = four_perc)
G <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, five_top, five_perc) %>% filter(is.na(five_top)==FALSE) %>% rename(Topic = five_top, Probability = five_perc)
H <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, six_top, six_perc) %>% filter(is.na(six_top)==FALSE) %>% rename(Topic = six_top, Probability = six_perc)
I <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, seven_top, seven_perc) %>% filter(is.na(seven_top)==FALSE) %>% rename(Topic = seven_top, Probability = seven_perc)
J <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, eight_top, eight_perc) %>% filter(is.na(eight_top)==FALSE) %>% rename(Topic = eight_top, Probability = eight_perc)
K <- wideData %>% select(ARTICLE_URL, CONTENT, newDate, tweetYear, tweetMonth, tweetWeek, nine_top, nine_perc) %>% filter(is.na(nine_top)==FALSE) %>% rename(Topic = nine_top, Probability = nine_perc)

longData<- rbind(A,B,C,D,E,G,H,I,J,K) 

monthlySummedLong<- longData %>% group_by(tweetYear,tweetMonth) %>% count(Topic, wt=Probability) %>%
  mutate(tweetMonth = case_when(tweetYear == 2018 ~ (tweetMonth + 0),
                                   tweetYear == 2019 ~ (tweetMonth + 12),
                                   tweetYear == 2020 ~ (tweetMonth +24)))  %>% 
  mutate(Topic = case_when(Topic == 0 ~ "Health Ch",
                           Topic == 1 ~ "Need 2 Qt",
                           Topic == 4 ~ "E-Cig",
                           Topic == 6 ~ "Clinics/Serv",
                           Topic == 7 ~ "Advice/Suc",
                           Topic == 8 ~ "Personal Ex")) %>% #note: other topics are NA unless we assign them something
  mutate(tweetMonth = as.factor(tweetMonth))

monthlyNoSpamSummedLong<- monthlySummedLong %>% filter(is.na(Topic)==FALSE)

ggplot(monthlyNoSpamSummedLong, aes(x=tweetMonth, y=n, group=Topic)) + 
  geom_line(aes(color=Topic)) +
  geom_point(aes(color=Topic, shape=Topic)) +
  scale_x_discrete(breaks=1:36, labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D","J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D","J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N","D"))+
  geom_vline(xintercept = c(12.5,24.5)) +
  ylab("Weighted Total Tweets") +
  xlab("Month")
