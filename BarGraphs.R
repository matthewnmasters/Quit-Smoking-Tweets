#Bar graphs for Lee
library(ggplot2)
library(tidyverse)

tbl<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/tbl.csv") %>%
  pivot_longer(cols=c("Avg2018","Avg2019","Avg2020"),
               names_to="variable",
               values_to = "value") %>%
  select(Topic, Quarter, variable, value) %>%
  mutate(tweetYear= case_when(
    variable == "Avg2018" ~"2018",
    variable == "Avg2019" ~"2019",
    variable == "Avg2020" ~"2020"
  ))

tbl2<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/tbl.csv") %>%
  pivot_longer(cols=c("SE2018","SE2019","SE2020"),
               names_to="variable2",
               values_to = "value2") %>%
  select(Topic, Quarter, variable2, value2) %>%
  mutate(tweetYear= case_when(
    variable2 == "SE2018" ~"2018",
    variable2 == "SE2019" ~"2019",
    variable2 == "SE2020" ~"2020"
  ))

tbl3<- read.csv("C:/Users/matth/Documents/GitHub/QS2020/tbl.csv") %>%
  pivot_longer(cols=c("pt1820","pt1920"),
               names_to="variable3",
               values_to = "value3") %>%
  select(Topic, Quarter, variable3, value3) %>%
  mutate(tweetYear= case_when(
    variable3 == "pt1820" ~"2018",
    variable3 == "pt1920" ~"2019"
  ))

fin <- left_join(tbl, tbl2) %>% left_join(., tbl3) %>%
  mutate(ptag = case_when(
    is.na(value3) ~ "",
    value3 > 0.05 ~ "",
    value3 <= 0.05 ~"*"
  )) %>%
  mutate(Quarter = factor(Quarter, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"), ordered = TRUE))

alltopics <- fin %>% filter(Topic== "All Quitting Related")
advice <- fin %>% filter(Topic== "Advice/Success")
clinics <- fin %>% filter(Topic== "Clinics/Services")
ecigs <- fin %>% filter(Topic== "E-cigarettes")
health <- fin %>% filter(Topic== "Health Changes")
need <- fin %>% filter(Topic== "Need to Quit")
personal <- fin %>% filter(Topic== "Personal Experience")

ggplot(alltopics, aes(x=Quarter, y=value, group=tweetYear)) + geom_col(aes(fill=tweetYear), width=0.75, position=position_dodge(0.85)) + 
  geom_errorbar(aes(ymin=value-value2, ymax=value+value2), width=0.3, stat="identity", position=position_dodge(0.85)) +
  geom_text(aes(label=ptag), position= position_dodge(0.85), vjust=-0.75, size=8) + 
  labs(fill="Year", title="All Topics" , y="Mean Daily Tweets", x= NULL) +
  theme(plot.title = element_text(hjust=0.5), plot.caption= element_text(hjust=0.5), legend.position = "bottom") +
  scale_fill_manual(values=c("blue", "dark grey", "red")) + scale_y_continuous(limits = c(0,200))

#Originally we had all of our graphs with the same y axis [limits=c(0,60)] but Lee wanted them different to better highlight differences

p4<- ggplot(advice, aes(x=Quarter, y=value, group=tweetYear)) + geom_col(aes(fill=tweetYear), width=0.75, position=position_dodge(0.85)) + 
  geom_errorbar(aes(ymin=value-value2, ymax=value+value2), width=0.3, stat="identity", position=position_dodge(0.85)) +
  geom_text(aes(label=ptag), position= position_dodge(0.85), vjust=-0.85, size=8) + 
  labs(fill="Year", title="1d) Quitting Advice/Success Stories" , y="Mean Daily Tweets", x= NULL) +
  theme(plot.title = element_text(hjust=0.5, size=10), plot.caption= element_text(hjust=0.5), legend.position = "bottom") +
  scale_fill_manual(values=c("light blue", "dark grey", "red")) + scale_y_continuous(limits = c(0,30)) 

p6<- ggplot(clinics, aes(x=Quarter, y=value, group=tweetYear)) + geom_col(aes(fill=tweetYear), width=0.75, position=position_dodge(0.85)) + 
  geom_errorbar(aes(ymin=value-value2, ymax=value+value2), width=0.3, stat="identity", position=position_dodge(0.85)) +
  geom_text(aes(label=ptag), position= position_dodge(0.85), vjust=-0.25, size=8) + 
  labs(fill="Year", title="1f) Support Services for Quitting from Clinics/Services" , y="Mean Daily Tweets", x= NULL) +
  theme(plot.title = element_text(hjust=0.5, size=10), plot.caption= element_text(hjust=0.5), legend.position = "bottom") +
  scale_fill_manual(values=c("light blue", "dark grey", "red")) + scale_y_continuous(limits = c(0,20))

p3<- ggplot(ecigs, aes(x=Quarter, y=value, group=tweetYear)) + geom_col(aes(fill=tweetYear), width=0.75, position=position_dodge(0.85)) + 
  geom_errorbar(aes(ymin=value-value2, ymax=value+value2), width=0.3, stat="identity", position=position_dodge(0.85)) +
  geom_text(aes(label=ptag), position= position_dodge(0.85), vjust=-1.3, size=8) + 
  labs(fill="Year", title="1c) Electronic Cigarettes" , y="Mean Daily Tweets", x= NULL) +
  theme(plot.title = element_text(hjust=0.5, size=10), plot.caption= element_text(hjust=0.5), legend.position = "bottom") +
  scale_fill_manual(values=c("light blue", "dark grey", "red")) + scale_y_continuous(limits = c(0,55))

p5<- ggplot(health, aes(x=Quarter, y=value, group=tweetYear)) + geom_col(aes(fill=tweetYear), width=0.75, position=position_dodge(0.85)) + 
  geom_errorbar(aes(ymin=value-value2, ymax=value+value2), width=0.3, stat="identity", position=position_dodge(0.85)) +
  geom_text(aes(label=ptag), position= position_dodge(0.85), vjust=-1, size=8) + 
  labs(fill="Year", title="1e) Quitting as Part of General Health Behavior Change" , y="Mean Daily Tweets", x= NULL) +
  theme(plot.title = element_text(hjust=0.5, size=10), plot.caption= element_text(hjust=0.5), legend.position = "bottom") +
  scale_fill_manual(values=c("light blue", "dark grey", "red")) + scale_y_continuous(limits = c(0,35))

p1<- ggplot(need, aes(x=Quarter, y=value, group=tweetYear)) + geom_col(aes(fill=tweetYear), width=0.75, position=position_dodge(0.85)) + 
  geom_errorbar(aes(ymin=value-value2, ymax=value+value2), width=0.3, stat="identity", position=position_dodge(0.85)) +
  geom_text(aes(label=ptag), position= position_dodge(0.85), vjust=-0.25, size=8) + 
  labs(fill="Year", title="1a) Expressions about Need to Quit Smoking" , y="Mean Daily Tweets", x= NULL) +
  theme(plot.title = element_text(hjust=0.5, size=10), plot.caption= element_text(hjust=0.5), legend.position = "bottom") +
  scale_fill_manual(values=c("light blue", "dark grey", "red")) + scale_y_continuous(limits = c(0,20))

p2<- ggplot(personal, aes(x=Quarter, y=value, group=tweetYear)) + geom_col(aes(fill=tweetYear), width=0.75, position=position_dodge(0.85)) + 
  geom_errorbar(aes(ymin=value-value2, ymax=value+value2), width=0.3, stat="identity", position=position_dodge(0.85)) +
  geom_text(aes(label=ptag), position= position_dodge(0.85), vjust=-0.65, size=8) + 
  labs(fill="Year", title="1b) Personal Experiences with Quitting Smoking" , y="Mean Daily Tweets", x= NULL) +
  theme(plot.title = element_text(hjust=0.5, size=10), plot.caption= element_text(hjust=0.5), legend.position = "bottom") +
  scale_fill_manual(values=c("light blue", "dark grey", "red")) + scale_y_continuous(limits = c(0,45))

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1, p3, p5, p2, p4, p6, cols=2)
