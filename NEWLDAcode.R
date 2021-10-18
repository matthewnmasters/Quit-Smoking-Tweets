################################################################################
#Updated the data for new analyses                                             #
################################################################################

#LDA analysis in R
setwd("C:/Users/matth/Documents/GitHub/QS2020/QuitSmokingTweets")

library(tidyverse)
library(lubridate)
library(gridExtra)
library(lsmeans)

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

#add coronavirus mentions
longData$corona <- grepl("coronavirus", longData$CONTENT, ignore.case = TRUE)
longData$covid <- grepl("covid", longData$CONTENT, ignore.case = TRUE)
longData$hasCovid <- ifelse(longData$corona == TRUE | longData$covid == TRUE, 1, 0)

###########################################################################################################
#Lee wanted yearly average daily tweets and standard deviations
leeData<- longData %>% mutate(Topic = case_when(Topic == 0 ~ "Health Ch",
                                            Topic == 1 ~ "Need 2 Qt",
                                            Topic == 4 ~ "E-Cig",
                                            Topic == 6 ~ "Clinics/Serv",
                                            Topic == 7 ~ "Advice/Suc",
                                            Topic == 8 ~ "Personal Ex")) %>%
  filter(is.na(Topic)==FALSE) %>% group_by(tweetYear) %>% count(newDate, wt=Probability) %>%summarise(mean=mean(n), sd=sd(n))



monthlySummedLong<- longData %>% group_by(tweetYear,tweetMonth) %>% count(Topic, wt=Probability) %>%
  mutate(Topic = case_when(Topic == 0 ~ "Health Ch",
                           Topic == 1 ~ "Need 2 Qt",
                           Topic == 4 ~ "E-Cig",
                           Topic == 6 ~ "Clinics/Serv",
                           Topic == 7 ~ "Advice/Suc",
                           Topic == 8 ~ "Personal Ex")) %>% #note: other topics are NA unless we assign them something
  mutate(tweetMonth = as.factor(tweetMonth)) %>%
  ungroup() %>%
  mutate(tweetMonth = as.factor(tweetMonth)) %>%
  mutate(Quarter = case_when(tweetMonth %in% c(1,2,3) ~ "Q1",
                             tweetMonth %in% c(4,5,6) ~ "Q2",
                             tweetMonth %in% c(7,8,9) ~ "Q3",
                             tweetMonth %in% c(10,11,12) ~ "Q4"))

CVmonthlySummedLong<- longData %>% filter(hasCovid==1) %>% group_by(tweetYear,tweetMonth) %>% count(Topic, wt=Probability) %>%
  mutate(Topic = case_when(Topic == 0 ~ "Health Ch",
                           Topic == 1 ~ "Need 2 Qt",
                           Topic == 4 ~ "E-Cig",
                           Topic == 6 ~ "Clinics/Serv",
                           Topic == 7 ~ "Advice/Suc",
                           Topic == 8 ~ "Personal Ex")) %>% #note: other topics are NA unless we assign them something
  mutate(tweetMonth = as.factor(tweetMonth)) %>%
  ungroup() %>%
  mutate(tweetMonth = as.factor(tweetMonth)) %>%
  mutate(Quarter = case_when(tweetMonth %in% c(1,2,3) ~ "Q1",
                             tweetMonth %in% c(4,5,6) ~ "Q2",
                             tweetMonth %in% c(7,8,9) ~ "Q3",
                             tweetMonth %in% c(10,11,12) ~ "Q4"))

#######################################################################################################################
#For most of the tables
#######################################################################################################################

dailySummedLong<- longData %>% group_by(tweetYear,tweetMonth,newDate) %>% count(Topic, wt=Probability) %>%
  mutate(Topic = case_when(Topic == 0 ~ "Health Ch",
                           Topic == 1 ~ "Need 2 Qt",
                           Topic == 4 ~ "E-Cig",
                           Topic == 6 ~ "Clinics/Serv",
                           Topic == 7 ~ "Advice/Suc",
                           Topic == 8 ~ "Personal Ex")) %>% #note: other topics are NA unless we assign them something
  ungroup() %>%
  mutate(tweetMonth = as.factor(tweetMonth)) %>%
  mutate(Quarter = case_when(tweetMonth %in% c(1,2,3) ~ "Q1",
                             tweetMonth %in% c(4,5,6) ~ "Q2",
                             tweetMonth %in% c(7,8,9) ~ "Q3",
                             tweetMonth %in% c(10,11,12) ~ "Q4"))

#for coronavirus table
QuarterlyNoSpamTotal2020<- monthlySummedLong %>% filter(is.na(Topic)==FALSE, tweetYear==2020) %>%
  group_by(tweetYear) %>% count(Quarter, wt=n)

QuarterlyNoSpamTotalTopics2020<- monthlySummedLong %>% filter(is.na(Topic)==FALSE, tweetYear==2020) %>%
  group_by(tweetYear,Topic) %>% count(Quarter, wt=n)

#same as above but only corona

QuarterlyNoSpamTotal2020CV<- CVmonthlySummedLong %>% filter(is.na(Topic)==FALSE, tweetYear==2020) %>%
  group_by(tweetYear) %>% count(Quarter, wt=n)

QuarterlyNoSpamTotalTopics2020CV<- CVmonthlySummedLong %>% filter(is.na(Topic)==FALSE, tweetYear==2020) %>%
  group_by(tweetYear,Topic) %>% count(Quarter, wt=n)

################################################

dailyNoSpamSummedLong<- dailySummedLong %>% filter(is.na(Topic)==FALSE)

SupTable <- dailyNoSpamSummedLong %>% group_by(tweetYear, Topic) %>% count(Quarter, wt=n)

###########################################################################################################
#Create tables with daily mean topic tweets per quarter                                                   #
###########################################################################################################

DailyAllTopic <- dailyNoSpamSummedLong %>% group_by(tweetYear, Quarter) %>% count(newDate, wt=n)
Q1 <- DailyAllTopic %>% filter(Quarter=="Q1") %>% mutate(tweetYear = as.factor(tweetYear))
Q2 <- DailyAllTopic %>% filter(Quarter=="Q2") %>% mutate(tweetYear = as.factor(tweetYear))
Q3 <- DailyAllTopic %>% filter(Quarter=="Q3") %>% mutate(tweetYear = as.factor(tweetYear))
Q4 <- DailyAllTopic %>% filter(Quarter=="Q4") %>% mutate(tweetYear = as.factor(tweetYear))

aovQ1 <- aov(n ~ tweetYear, data = Q1)
summary(aovQ1)
model.tables(aovQ1, "means")
summary.lm(aovQ1)
with(Q1, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q1, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p1<- ggplot(Q1, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(50,1000))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q1") + theme(axis.text.y = (element_text(angle=90, hjust = 0.5)) ,plot.title = element_text(size = 10, hjust = 0.5))
Q1 %>% group_by(tweetYear) %>% summarise(sd = sd(n))
lsmeans(aovQ1, specs="tweetYear")


aovQ2 <- aov(n ~ tweetYear, data = Q2)
summary(aovQ2)
model.tables(aovQ2, "means")
summary.lm(aovQ2)
with(Q2, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q2, pairwise.wilcox.test(n, tweetYear, p.adjust.method = "bonf"))
p2<- ggplot(Q2, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(50,1000))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q2") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q2 %>% group_by(tweetYear) %>% summarise(sd = sd(n))
lsmeans(aovQ2, specs="tweetYear")


aovQ3 <- aov(n ~ tweetYear, data = Q3)
summary(aovQ3)
model.tables(aovQ3, "means")
summary.lm(aovQ3)
with(Q3, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q3, pairwise.wilcox.test(n, tweetYear, p.adjust.method = "bonf"))
p3<- ggplot(Q3, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(50,1000))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q3") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q3 %>% group_by(tweetYear) %>% summarise(sd = sd(n))
lsmeans(aovQ3, specs="tweetYear")


aovQ4 <- aov(n ~ tweetYear, data = Q4)
summary(aovQ4)
model.tables(aovQ4, "means")
summary.lm(aovQ4)
with(Q4, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q4, pairwise.wilcox.test(n, tweetYear, p.adjust.method = "bonf"))
p4<- ggplot(Q4, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(50,1000))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q4") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q4 %>% group_by(tweetYear) %>% summarise(sd = sd(n))
lsmeans(aovQ4, specs="tweetYear")



grid.arrange(p1,p2,p3,p4, nrow=1, top="All Topics", left="Daily Tweets")

DailyEachTopic <- dailyNoSpamSummedLong %>% group_by(tweetYear, Quarter, Topic) %>% count(newDate, wt=n)
#Doing a two-way ANOVA didn't give us p values for each topic*year, just an overall... so we need to split it into topics
Q1ADV <- DailyEachTopic %>% filter(Quarter=="Q1", Topic == "Advice/Suc") %>% mutate(tweetYear = as.factor(tweetYear))
Q1CLI <- DailyEachTopic %>% filter(Quarter=="Q1", Topic == "Clinics/Serv") %>% mutate(tweetYear = as.factor(tweetYear))
Q1ECI <- DailyEachTopic %>% filter(Quarter=="Q1", Topic == "E-Cig") %>% mutate(tweetYear = as.factor(tweetYear))
Q1HEA <- DailyEachTopic %>% filter(Quarter=="Q1", Topic == "Health Ch") %>% mutate(tweetYear = as.factor(tweetYear))
Q1NEE <- DailyEachTopic %>% filter(Quarter=="Q1", Topic == "Need 2 Qt") %>% mutate(tweetYear = as.factor(tweetYear))
Q1PER <- DailyEachTopic %>% filter(Quarter=="Q1", Topic == "Personal Ex") %>% mutate(tweetYear = as.factor(tweetYear))
Q2ADV <- DailyEachTopic %>% filter(Quarter=="Q2", Topic == "Advice/Suc") %>% mutate(tweetYear = as.factor(tweetYear))
Q2CLI <- DailyEachTopic %>% filter(Quarter=="Q2", Topic == "Clinics/Serv") %>% mutate(tweetYear = as.factor(tweetYear))
Q2ECI <- DailyEachTopic %>% filter(Quarter=="Q2", Topic == "E-Cig") %>% mutate(tweetYear = as.factor(tweetYear))
Q2HEA <- DailyEachTopic %>% filter(Quarter=="Q2", Topic == "Health Ch") %>% mutate(tweetYear = as.factor(tweetYear))
Q2NEE <- DailyEachTopic %>% filter(Quarter=="Q2", Topic == "Need 2 Qt") %>% mutate(tweetYear = as.factor(tweetYear))
Q2PER <- DailyEachTopic %>% filter(Quarter=="Q2", Topic == "Personal Ex") %>% mutate(tweetYear = as.factor(tweetYear))
Q3ADV <- DailyEachTopic %>% filter(Quarter=="Q3", Topic == "Advice/Suc") %>% mutate(tweetYear = as.factor(tweetYear))
Q3CLI <- DailyEachTopic %>% filter(Quarter=="Q3", Topic == "Clinics/Serv") %>% mutate(tweetYear = as.factor(tweetYear))
Q3ECI <- DailyEachTopic %>% filter(Quarter=="Q3", Topic == "E-Cig") %>% mutate(tweetYear = as.factor(tweetYear))
Q3HEA <- DailyEachTopic %>% filter(Quarter=="Q3", Topic == "Health Ch") %>% mutate(tweetYear = as.factor(tweetYear))
Q3NEE <- DailyEachTopic %>% filter(Quarter=="Q3", Topic == "Need 2 Qt") %>% mutate(tweetYear = as.factor(tweetYear))
Q3PER <- DailyEachTopic %>% filter(Quarter=="Q3", Topic == "Personal Ex") %>% mutate(tweetYear = as.factor(tweetYear))
Q4ADV <- DailyEachTopic %>% filter(Quarter=="Q4", Topic == "Advice/Suc") %>% mutate(tweetYear = as.factor(tweetYear))
Q4CLI <- DailyEachTopic %>% filter(Quarter=="Q4", Topic == "Clinics/Serv") %>% mutate(tweetYear = as.factor(tweetYear))
Q4ECI <- DailyEachTopic %>% filter(Quarter=="Q4", Topic == "E-Cig") %>% mutate(tweetYear = as.factor(tweetYear))
Q4HEA <- DailyEachTopic %>% filter(Quarter=="Q4", Topic == "Health Ch") %>% mutate(tweetYear = as.factor(tweetYear))
Q4NEE <- DailyEachTopic %>% filter(Quarter=="Q4", Topic == "Need 2 Qt") %>% mutate(tweetYear = as.factor(tweetYear))
Q4PER <- DailyEachTopic %>% filter(Quarter=="Q4", Topic == "Personal Ex") %>% mutate(tweetYear = as.factor(tweetYear))

#Advice
aovQ1ADV <- aov(n ~ tweetYear, data = Q1ADV)
summary(aovQ1ADV)
model.tables(aovQ1ADV, "means")
with(Q1ADV, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q1ADV, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p1<- ggplot(Q1ADV, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q1") + theme(axis.text.y = (element_text(angle=90, hjust = 0.5)) ,plot.title = element_text(size = 10, hjust = 0.5))
Q1ADV %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ2ADV <- aov(n ~ tweetYear, data = Q2ADV)
summary(aovQ2ADV)
model.tables(aovQ2ADV, "means")
with(Q2ADV, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q2ADV, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p2<- ggplot(Q2ADV, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q2") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q2ADV %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ3ADV <- aov(n ~ tweetYear, data = Q3ADV)
summary(aovQ3ADV)
model.tables(aovQ3ADV, "means")
with(Q3ADV, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q3ADV, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p3<- ggplot(Q3ADV, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q3") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q3ADV %>% group_by(tweetYear) %>% summarise(sd = sd(n))

aovQ4ADV <- aov(n ~ tweetYear, data = Q4ADV)
summary(aovQ4ADV)
model.tables(aovQ4ADV, "means")
with(Q4ADV, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q4ADV, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p4<- ggplot(Q4ADV, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q4") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q4ADV %>% group_by(tweetYear) %>% summarise(sd = sd(n))

grid.arrange(p1,p2,p3,p4, nrow=1, top="Advice", left="Daily Tweets")


#Clinics
aovQ1CLI <- aov(n ~ tweetYear, data = Q1CLI)
summary(aovQ1CLI)
model.tables(aovQ1CLI, "means")
with(Q1CLI, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q1CLI, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p1<- ggplot(Q1CLI, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q1") + theme(axis.text.y = (element_text(angle=90, hjust = 0.5)) ,plot.title = element_text(size = 10, hjust = 0.5))
Q1CLI %>% group_by(tweetYear) %>% summarise(sd = sd(n))

aovQ2CLI <- aov(n ~ tweetYear, data = Q2CLI)
summary(aovQ2CLI)
model.tables(aovQ2CLI, "means")
with(Q2CLI, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q2CLI, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p2<- ggplot(Q2CLI, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q2") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q2CLI %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ3CLI <- aov(n ~ tweetYear, data = Q3CLI)
summary(aovQ3CLI)
model.tables(aovQ3CLI, "means")
with(Q3CLI, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q3CLI, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p3<- ggplot(Q3CLI, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q3") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q3CLI %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ4CLI <- aov(n ~ tweetYear, data = Q4CLI)
summary(aovQ4CLI)
model.tables(aovQ4CLI, "means")
with(Q4CLI, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q4CLI, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p4<- ggplot(Q4CLI, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q4") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q4CLI %>% group_by(tweetYear) %>% summarise(sd = sd(n))

grid.arrange(p1,p2,p3,p4, nrow=1, top="Clinics", left="Daily Tweets")

#ECIG
aovQ1ECI <- aov(n ~ tweetYear, data = Q1ECI)
summary(aovQ1ECI)
model.tables(aovQ1ECI, "means")
with(Q1ECI, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q1ECI, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p1<- ggplot(Q1ECI, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q1") + theme(axis.text.y = (element_text(angle=90, hjust = 0.5)) ,plot.title = element_text(size = 10, hjust = 0.5))
Q1ECI %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ2ECI <- aov(n ~ tweetYear, data = Q2ECI)
summary(aovQ2ECI)
model.tables(aovQ2ECI, "means")
with(Q2ECI, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q2ECI, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p2<- ggplot(Q2ECI, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q2") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q2ECI %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ3ECI <- aov(n ~ tweetYear, data = Q3ECI)
summary(aovQ3ECI)
model.tables(aovQ3ECI, "means")
with(Q3ECI, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q3ECI, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p3<- ggplot(Q3ECI, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q3") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q3ECI %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ4ECI <- aov(n ~ tweetYear, data = Q4ECI)
summary(aovQ4ECI)
model.tables(aovQ4ECI, "means")
with(Q4ECI, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q4ECI, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p4<- ggplot(Q4ECI, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q4") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q4ECI %>% group_by(tweetYear) %>% summarise(sd = sd(n))

grid.arrange(p1,p2,p3,p4, nrow=1, top="Ecigs", left="Daily Tweets") #Q3 has values as high as 427...


#Health Ch
aovQ1HEA <- aov(n ~ tweetYear, data = Q1HEA)
summary(aovQ1HEA)
model.tables(aovQ1HEA, "means")
with(Q1HEA, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q1HEA, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p1<- ggplot(Q1HEA, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q1") + theme(axis.text.y = (element_text(angle=90, hjust = 0.5)) ,plot.title = element_text(size = 10, hjust = 0.5))
Q1HEA %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ2HEA <- aov(n ~ tweetYear, data = Q2HEA)
summary(aovQ2HEA)
model.tables(aovQ2HEA, "means")
with(Q2HEA, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q2HEA, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p2<- ggplot(Q2HEA, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q2") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q2HEA %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ3HEA <- aov(n ~ tweetYear, data = Q3HEA)
summary(aovQ3HEA)
model.tables(aovQ3HEA, "means")
with(Q3HEA, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q3HEA, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p3<- ggplot(Q3HEA, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q3") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q3HEA %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ4HEA <- aov(n ~ tweetYear, data = Q4HEA)
summary(aovQ4HEA)
model.tables(aovQ4HEA, "means")
with(Q4HEA, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q4HEA, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p4<- ggplot(Q4HEA, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q4") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q4HEA %>% group_by(tweetYear) %>% summarise(sd = sd(n))

grid.arrange(p1,p2,p3,p4, nrow=1, top="Health", left="Daily Tweets") #Q3 has values as high as 427...


#Need to Quit
aovQ1NEE <- aov(n ~ tweetYear, data = Q1NEE)
summary(aovQ1NEE)
model.tables(aovQ1NEE, "means")
with(Q1NEE, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q1NEE, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p1<- ggplot(Q1NEE, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q1") + theme(axis.text.y = (element_text(angle=90, hjust = 0.5)) ,plot.title = element_text(size = 10, hjust = 0.5))
Q1NEE %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ2NEE <- aov(n ~ tweetYear, data = Q2NEE)
summary(aovQ2NEE)
model.tables(aovQ2NEE, "means")
with(Q2NEE, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q2NEE, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p2<- ggplot(Q2NEE, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q2") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q2NEE %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ3NEE <- aov(n ~ tweetYear, data = Q3NEE)
summary(aovQ3NEE)
model.tables(aovQ3NEE, "means")
with(Q3NEE, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q3NEE, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p3<- ggplot(Q3NEE, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q3") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q3NEE %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ4NEE <- aov(n ~ tweetYear, data = Q4NEE)
summary(aovQ4NEE)
model.tables(aovQ4NEE, "means")
with(Q4NEE, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q4NEE, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p4<- ggplot(Q4NEE, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q4") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q4NEE %>% group_by(tweetYear) %>% summarise(sd = sd(n))

grid.arrange(p1,p2,p3,p4, nrow=1, top="Need to Quit", left="Daily Tweets") 


#Personal Ex
aovQ1PER <- aov(n ~ tweetYear, data = Q1PER)
summary(aovQ1PER)
model.tables(aovQ1PER, "means")
with(Q1PER, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q1PER, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p1<- ggplot(Q1PER, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q1") + theme(axis.text.y = (element_text(angle=90, hjust = 0.5)) ,plot.title = element_text(size = 10, hjust = 0.5))
Q1PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ2PER <- aov(n ~ tweetYear, data = Q2PER)
summary(aovQ2PER)
model.tables(aovQ2PER, "means")
with(Q2PER, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q2PER, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p2<- ggplot(Q2PER, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q2") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q2PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ3PER <- aov(n ~ tweetYear, data = Q3PER)
summary(aovQ3PER)
model.tables(aovQ3PER, "means")
with(Q3PER, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q3PER, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p3<- ggplot(Q3PER, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q3") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q3PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovQ4PER <- aov(n ~ tweetYear, data = Q4PER)
summary(aovQ4PER)
model.tables(aovQ4PER, "means")
with(Q4PER, pairwise.t.test(n, tweetYear, p.adjust.method = "bonf"))
with(Q4PER, pairwise.wilcox.test(n, tweetYear, p.adjust.method="bonf"))
p4<- ggplot(Q4PER, aes(x=tweetYear, y=n, fill=tweetYear)) + geom_boxplot(width=0.5, notch = TRUE, show.legend = FALSE) + scale_y_continuous(limits = c(0,150))+ xlab(NULL)  + ylab(NULL) + labs(title = "Q4") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

grid.arrange(p1,p2,p3,p4, nrow=1, top="Personal Exp", left="Daily Tweets") 


###########################################################################################################
#Now for ANOVAs between quarters within each topic
#2020

dailyTotal <- DailyEachTopic %>% ungroup() %>% filter(tweetYear==2020) %>% group_by(Quarter) %>% count(newDate, wt=n)

aovALL <- aov(n ~ Quarter, data = dailyTotal)
summary(aovALL)
model.tables(aovALL, "means")
with(dailyTotal, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(dailyTotal, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

ADV <- DailyEachTopic %>% filter(tweetYear==2020, Topic == "Advice/Suc") 
CLI <- DailyEachTopic %>% filter(tweetYear==2020, Topic == "Clinics/Serv") 
ECI <- DailyEachTopic %>% filter(tweetYear==2020, Topic == "E-Cig") 
HEA <- DailyEachTopic %>% filter(tweetYear==2020, Topic == "Health Ch")
NEE <- DailyEachTopic %>% filter(tweetYear==2020, Topic == "Need 2 Qt")
PER <- DailyEachTopic %>% filter(tweetYear==2020, Topic == "Personal Ex")

aovADV <- aov(n ~ Quarter, data = ADV)
summary(aovADV)
model.tables(aovADV, "means")
with(ADV, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(ADV, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

aovCLI <- aov(n ~ Quarter, data = CLI)
summary(aovCLI)
model.tables(aovCLI, "means")
with(CLI, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(CLI, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovECI <- aov(n ~ Quarter, data = ECI)
summary(aovECI)
model.tables(aovECI, "means")
with(ECI, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(ECI, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovHEA <- aov(n ~ Quarter, data = HEA)
summary(aovHEA)
model.tables(aovHEA, "means")
with(HEA, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(HEA, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovNEE <- aov(n ~ Quarter, data = NEE)
summary(aovNEE)
model.tables(aovNEE, "means")
with(NEE, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(NEE, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovPER <- aov(n ~ Quarter, data = PER)
summary(aovPER)
model.tables(aovPER, "means")
with(PER, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(PER, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

####################################################################
#2019

dailyTotal <- DailyEachTopic %>% ungroup() %>% filter(tweetYear==2019) %>% group_by(Quarter) %>% count(newDate, wt=n)

aovALL <- aov(n ~ Quarter, data = dailyTotal)
summary(aovALL)
model.tables(aovALL, "means")
with(dailyTotal, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(dailyTotal, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

ADV <- DailyEachTopic %>% filter(tweetYear==2019, Topic == "Advice/Suc") 
CLI <- DailyEachTopic %>% filter(tweetYear==2019, Topic == "Clinics/Serv") 
ECI <- DailyEachTopic %>% filter(tweetYear==2019, Topic == "E-Cig") 
HEA <- DailyEachTopic %>% filter(tweetYear==2019, Topic == "Health Ch")
NEE <- DailyEachTopic %>% filter(tweetYear==2019, Topic == "Need 2 Qt")
PER <- DailyEachTopic %>% filter(tweetYear==2019, Topic == "Personal Ex")

aovADV <- aov(n ~ Quarter, data = ADV)
summary(aovADV)
model.tables(aovADV, "means")
with(ADV, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(ADV, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

aovCLI <- aov(n ~ Quarter, data = CLI)
summary(aovCLI)
model.tables(aovCLI, "means")
with(CLI, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(CLI, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovECI <- aov(n ~ Quarter, data = ECI)
summary(aovECI)
model.tables(aovECI, "means")
with(ECI, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(ECI, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovHEA <- aov(n ~ Quarter, data = HEA)
summary(aovHEA)
model.tables(aovHEA, "means")
with(HEA, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(HEA, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovNEE <- aov(n ~ Quarter, data = NEE)
summary(aovNEE)
model.tables(aovNEE, "means")
with(NEE, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(NEE, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovPER <- aov(n ~ Quarter, data = PER)
summary(aovPER)
model.tables(aovPER, "means")
with(PER, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(PER, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

###################################################################
#2018

dailyTotal <- DailyEachTopic %>% ungroup() %>% filter(tweetYear==2018) %>% group_by(Quarter) %>% count(newDate, wt=n)

aovALL <- aov(n ~ Quarter, data = dailyTotal)
summary(aovALL)
model.tables(aovALL, "means")
with(dailyTotal, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(dailyTotal, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

ADV <- DailyEachTopic %>% filter(tweetYear==2018, Topic == "Advice/Suc") 
CLI <- DailyEachTopic %>% filter(tweetYear==2018, Topic == "Clinics/Serv") 
ECI <- DailyEachTopic %>% filter(tweetYear==2018, Topic == "E-Cig") 
HEA <- DailyEachTopic %>% filter(tweetYear==2018, Topic == "Health Ch")
NEE <- DailyEachTopic %>% filter(tweetYear==2018, Topic == "Need 2 Qt")
PER <- DailyEachTopic %>% filter(tweetYear==2018, Topic == "Personal Ex")

aovADV <- aov(n ~ Quarter, data = ADV)
summary(aovADV)
model.tables(aovADV, "means")
with(ADV, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(ADV, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))

aovCLI <- aov(n ~ Quarter, data = CLI)
summary(aovCLI)
model.tables(aovCLI, "means")
with(CLI, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(CLI, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovECI <- aov(n ~ Quarter, data = ECI)
summary(aovECI)
model.tables(aovECI, "means")
with(ECI, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(ECI, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovHEA <- aov(n ~ Quarter, data = HEA)
summary(aovHEA)
model.tables(aovHEA, "means")
with(HEA, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(HEA, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovNEE <- aov(n ~ Quarter, data = NEE)
summary(aovNEE)
model.tables(aovNEE, "means")
with(NEE, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(NEE, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))


aovPER <- aov(n ~ Quarter, data = PER)
summary(aovPER)
model.tables(aovPER, "means")
with(PER, pairwise.t.test(n, Quarter, p.adjust.method = "bonf"))
with(PER, pairwise.wilcox.test(n, Quarter, p.adjust.method="bonf"))
Q4PER %>% group_by(tweetYear) %>% summarise(sd = sd(n))