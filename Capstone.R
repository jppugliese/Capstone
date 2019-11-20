################################################################################
# Kickstarter Project, data collected from www.kaggle.com and stored locally.
# The dataset has been stored in Github too.
# Author: JP Pugliese
# ----------------------------------------------------------
# Description: This is the final assignment 
# for the Harvard Data Science Professional Program 


# Install all needed libraries if it is not present
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gbm)) install.packages("gbm")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(xgboost)) install.packages("xgboost")
if(!require(e1071)) install.packages("e1071")
if(!require(class)) install.packages("class")
if(!require(ROCR)) install.packages("ROCR")
if(!require(randomForest)) install.packages("randomForest")
if(!require(PRROC)) install.packages("PRROC")
if(!require(reshape2)) install.packages("reshape2")
if(!require(knitr)) install.packages("knitr")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(lubridate)) install.packages("lubridate")
if(!require(pROC)) install.packages("pROC")
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(tree)) install.packages("tree")

# Loading all needed libraries
library(dplyr)
library(knitr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(ggplot2)
library(gbm)
library(caret)
library(xgboost)
library(e1071)
library(class)
library(ROCR)
library(randomForest)
library(PRROC)
library(reshape2)
library(ggthemes)
library(lubridate)
library(pROC)
library(rpart)      
library(rpart.plot) 
library(tree)

#Let's load in the data (here located on local HD) and check to see if it was read in correctly.
setwd("~/projects/Capstone/Capstone")
data <- read_csv("ks-projects-201801.csv")

#Structure of the data
str(data)
summary(data)

#dimensions of the dataset
dim(data)

#Information and column's headers
head(data, 5)
tail(data, 5)

#Everything looks okay so far. Now let's check to see if there are any N/A values that we have to clean up.
sapply(data, function(x) sum(is.na(x)))
is.na(data$`usd pledged`)

#Looking at all the different state of a Kickstarter campaign. We can see that some **undefined state* are present, which requires fixing.
table(data$state)

#There appears to only be N/A values for *usd.pledged*. We actually don't need that column since we'll be using *usd_pledged_real* instead, so let's get rid of it and rename *usd_pledged_real* to *usd_pledged*. Likewise, we'll do the same with *usd_goal_real* and simply name it *usd_goal*.
data <- data[,-13]
colnames(data)[13] <- "usd_pledged"
colnames(data)[14] <- "usd_goal"

#Checking the data once more
head(data)

#Removing at the projects set as **successful** but having 0 backers, which is strange
data <- filter(data, backers != 0 | state != "successful")

#Number of **undefined** state 
sum(data$state=="undefined")

#Cleansing the **undefined* state from the dataset
data <- data %>% filter(state!="undefined")

#Addind a new column called **launched_year**
data$launched_year <- year(data$launched)

#What types of projects are most popular?
#We'll answer this question in the perspective of the project-starter and based on two levels: category (called *main_category* in the dataset) and subcategory (called *category* in the dataset). We first begin by examining the number of projects by category. 

cat.freq <- data %>%
  group_by(main_category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
#Ranking of the **main_category**
cat.freq

cat.freq$main_category <- factor(cat.freq$main_category, levels=cat.freq$main_category)


ggplot(cat.freq, aes(main_category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Category") + xlab("Project Category") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#*Film & Video* appears to be the most popular project category and *Dance* the least popular.  Now we'll do the same thing for subcategories. There are 159 subcategory levels, which is far too many to plot individually, so we'll just plot the ten subcategories with the greatest number of projects.

subcat.freq <- data %>%
  group_by(category) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
#Ranking of the **category**
subcat.freq

subcat.freq$category <- factor(subcat.freq$category, levels=subcat.freq$category)

ggplot(head(subcat.freq, 10), aes(category, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Subcategory") + xlab("Project Subcategory") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#*Product Design* is the most popular subcategory here, stemming from the category of *Design*.
#What types of projects are being funded?
#This question is akin to the first question but phrased from the perspective of the backers. In other words, the most funded projects are the most popular projects in the perspective of the backers.
#Let's start by taking a look at the top 15 highest funded projects.
head(data[order(-data$usd_pledged), c(1,2,3,13)], 15)

#A lot of the projects here fall under the *Product Design* subcategory.
#Similarly, let's list the top 15 most backed projects (i.e. projects with the most backers).
head(data[order(-data$backers), c(2,3,11)], 15)

#The most common subcategory here appears to be *Video Games*.
#Now let's determine what types of projects funding is going towards. We'll do this by aggregating the amount of funds pledged for each category, providing us with the total amount pledged for each category.
pledged.tot <- data %>%
  group_by(main_category) %>%
  summarize(total=sum(usd_pledged)) %>%
  arrange(desc(total))

pledged.tot$main_category <- factor(pledged.tot$main_category, levels=pledged.tot$main_category)

ggplot(pledged.tot, aes(main_category, total/1000000, fill=total)) + geom_bar(stat="identity") + 
  ggtitle("Total Amount Pledged by Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD millions)") + 
  geom_text(aes(label=paste0("$", round(total/1000000,1))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#Games, Design, and Technology are the highest grossing categories by far.
#It’s important to take the number of backers into account as well, so let’s determine the average amount pledged per backer for each category. We’ll calculate this by taking the total amount pledged for each category and dividing it by the total number of backers for each category.
pledged.avg <- data %>%
  group_by(main_category) %>%
  summarize(pledged=sum(usd_pledged), backers=sum(backers)) %>%
  mutate(avg=pledged/backers) %>%
  arrange(desc(avg))

pledged.avg$main_category <- factor(pledged.avg$main_category, levels=pledged.avg$main_category)

ggplot(pledged.avg, aes(main_category, avg, fill=avg)) + geom_bar(stat="identity") + 
  ggtitle("Average Amount Pledged per Backer") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  geom_text(aes(label=paste0("$", round(avg,2))), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#Technology has the highest average amount pledged, whereas Comics has the lowest average amount pledged. An interesting note here is that the average amount pledged for Technology is nearly double that of Games, even though Games had the higher aggregate amount pledged as shown in the previous graph.
#Next we’ll examine the distribution of amounts pledged for individual projects using box plots. There are no doubt a lot of projects that received little to no funding as well as huge outliers, which will cause the box plots to appear “squished” near the bottom.
ggplot(data, aes(main_category, usd_pledged, fill=main_category)) + geom_boxplot() + 
  ggtitle("Amount Pledged vs. Project Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + theme_economist()+ 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,20000))

#Technology has an incredibly high upper quartile and median. Although not nearly as high, Design and Food also have relatively high upper quartile and median values as well. 
#The average project goal for these two categories was lower than that of Journalism and Film & Video, but they have higher median and upper quartile values, 
#suggesting that the former must have many projects with smaller goal amounts, the latter must have many high outliers, or both.

#######
#Having looked at both the amount pledged and goal for different kinds of projects, let’s see how the distribution of each compares to one another. Since we can expect both distributions to be heavily right-skewed due to many projects that received little to no funding and extremely high outliers, we will use a log transformation on both variables to better visualize their distributions.
#usd.amounts <- gather(data, type, amount, usd_pledged, usd_goal, factor_key=T) %>% filter(state %in% c("successful", "failed"))

#ggplot(usd.amounts, aes(log(amount+1), fill=type)) + 
 # geom_histogram(alpha=0.7, binwidth =30, position="identity") + theme_economist() +  
  #ggtitle("Distribution of log(USD Pledged) vs. log(USD Goal)") + xlab("log(USD + 1)") + 
  #ylab("Frequency") + scale_fill_discrete("Type", labels=c("USD Pledged", "USD Goal"))

#USD Goal appears to be relatively normally distributed. USD Pledged on the other hand has a bimodal distribution, with a tall left peak, which would represent projects that received either little or no funding. The approximate centre of the distribution of USD Pledged is located to the left side of the distribution of USD Goal, illustrating how for the most part, many projects did not receive the required funding they were looking for.

#quantile(usd.amounts$pledged, probs = seq(from = 0, to = 1, by = .2))

#ggplot(usd.amounts, aes(log(pledged), fill=state)) + 
#  geom_histogram(alpha=0.7, binwidth =30, position="identity") + theme_economist() +  
#  ggtitle("Distribution of log(USD Pledged)") + xlab("log(USD + 1)") + 
#  ylab("Frequency") + scale_fill_discrete("State", labels=c("Successful", "Failed"))
#####

#What types of projects were successful and unsuccessful?
#Let’s break down the number of projects by their status (e.g. successful, failed, cancelled, etc.).
state.freq <- data %>%
  group_by(state) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

state.freq$state <- factor(state.freq$state, levels=state.freq$state)

ggplot(state.freq, aes(state, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Status") + xlab("Project Status") + ylab("Frequency") + 
  geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

#More projects failed than succeeded. It seems most projects don’t see the light of day. We can further group the projects into two different categories, “complete” projects (projects that have reached their deadline, i.e. successful and failed pojects) and “incomplete” projects (projects that have not reached their deadline, i.e. live, cancelled, or suspended projects). Let’s do this and look at the project status proportion for each group.
state.grp <- data %>%
  filter(state!="undefined") %>%
  mutate(grp=ifelse(state %in% c("successful", "failed"), "complete", "incomplete")) %>%
  group_by(grp, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(grp, desc(state))

state.grp$state <- factor(state.grp$state, levels=state.grp$state)

ggplot(state.grp, aes(grp, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Project Status by Completion") + xlab("Project Completion") + ylab("Percentage") + 
  scale_x_discrete(labels=c("Complete", "Incomplete")) + 
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_brewer(name="Project Status", 
                    labels=c("Successful", "Failed", "Suspended", "Live", "Cancelled"), 
                    palette="Set1") + 
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="white", size=5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold"))

#Now we know that approximately 60% of completed projects failed and only approximately 40% succeeded. Approximately 90% of incomplete projects were cancelled.
state.pct <- data %>%
  filter(state %in% c("successful", "failed")) %>%
  group_by(main_category, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state), pct)

state.pct$main_category <- factor(state.pct$main_category, 
                                  levels=state.pct$main_category[1:(nrow(state.pct)/2)])

cat1 <- ggplot(state.pct, aes(main_category, pct, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Project Category") + 
  xlab("Project Category") + ylab("Percentage") + scale_y_continuous(labels=scales::percent) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failure")) + 
  geom_text(aes(label=paste0(round(pct*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="white", size=3) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold")) + coord_flip()

cat2 <- ggplot(state.pct, aes(main_category, count, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Project Category") + 
  xlab("Project Category") + ylab("Number of Projects") + scale_y_continuous(labels=scales::comma) + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failed")) + 
  geom_text(aes(label=paste0(round(count,1),"")), position=position_stack(vjust=0.5), 
            colour="white", size=3) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold")) + coord_flip()

gridExtra::grid.arrange(cat1, cat2, ncol = 2)

#Dance, Theater, and Comics have the highest success rates and Technology, Journalism, and Crafts have the lowest. This agrees with the box plots for amounts pledged and project goal amounts above as Dance and Comics both had high median amounts pledged and low median goals, with Theater having a low median goal as well. Technology, Journalism, and Crafts had low median amounts pledged, with Technology having a high median goal. In general, the higher the amount pledged and/or the lower the goal, the more likely a project will be successful. Interestingly enough, Crafts, having a low median amount pledged, also has a low success rate despite having a low median goal, which may indicate that people generally are not as interested in this category as a whole.

#Does project length affect success rate?
#The maximum project duration according to Kickstarter’s rules is 60 days. Kickstarter also recommends that projects be set to 30 days or less. Their reasoning is that projects that have not been funded within 30 days are not likely to be funded by their deadline either.
#We can find the length of each project by taking the difference between the project deadline and the project launch date, then dividing by the number of whole days. Now let’s plot the success rate of projects based on their length in days.

data$length <- interval(ymd_hms(data$launched), ymd(data$deadline)) %/% days(1)
#head(data$length)
length.pct <- data %>%
  filter(state %in% c("successful", "failed"), length <= 61) %>%
  group_by(length, state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count))

ggplot(length.pct[length.pct$state=="successful",], aes(length, pct)) + 
  geom_point(colour="royalblue4", size=2.5) + ggtitle("Success Rate vs. Project Length") + 
  xlab("Project Length (Days)") + ylab("Success Rate (%)") + 
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) + geom_vline(xintercept=30, colour="red") + 
  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"))
#It seems that overall, projects exceeding 30 days have a lower success rate than projects lasting fewer than 30 days, so there is some truth to what Kickstarter says. In fact, there seems to be a negative relationship between success rate and project length for projects exceeding 30 days. This begs the question, are projects following Kickstarter’s recommendation of setting their length to be less than 30 days? Let’s answer this by examining the distribution of projects by project length.

#Projects by year
#Let’s examine the number of projects and their success/failure rate by launch year.
year.freq <- data %>%
  filter(year(launched)!="1970", state %in% c("successful", "failed")) %>%
  group_by(year=year(launched)) %>%
  summarize(count=n())

plot1 <- ggplot(year.freq, aes(year, count, fill=count)) + geom_bar(stat="identity") + 
  ggtitle("Number of Projects by Launch Year") + xlab("Year") + ylab("Frequency") + 
  scale_x_discrete(limits=c(2009:2018)) + 
  geom_text(aes(label=paste0(count)), vjust=-0.5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="null") + 
  scale_fill_gradient(low="skyblue1", high="royalblue4")

state.pct2 <- data %>%
  filter(year(launched)!="1970", state %in% c("successful", "failed")) %>%
  group_by(year=year(launched), state) %>%
  summarize(count=n()) %>%
  mutate(pct=count/sum(count)) %>%
  arrange(desc(state))

plot2 <- ggplot(state.pct2, aes(year, count, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Success vs. Failure Rate by Year Launched") + 
  xlab("Year") + ylab("Frequency") + scale_x_discrete(limits=c(2009:2018)) + 
  scale_y_continuous() + 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failed")) + 
  geom_text(aes(label=paste0(count,"")), position=position_stack(vjust=0.5), 
            colour="white", size=3) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold"))

gridExtra::grid.arrange(plot1, plot2, ncol = 2)

#Projects by country
#It would be nice to know what countries Kickstarter projects are originating from.

countries.freq <- data %>%
  filter(country!='N,0"', state %in% c("successful", "failed")) %>%
  group_by(country, state) %>%
  summarize(count=n())%>%
  mutate(pct2=count/sum(count)) %>%
  arrange(desc(count), pct2)


pp1 <- ggplot(head(countries.freq,10), aes(country,count, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Country") + 
  xlab("Country") + ylab("Frequency") + scale_y_continuous(labels=scales::comma)+ 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Success", "Failed")) + 
  geom_text(aes(label=paste0(round(pct2*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="black", size=5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold"))


currency.freq <- data %>%
  filter(country!='N,0"', state %in% c("successful", "failed")) %>%
  group_by(currency, state) %>%
  summarize(count=n())%>%
  mutate(pct3=count/sum(count)) %>%
  arrange(desc(count), pct3)


pp2 <- ggplot(head(currency.freq,10), aes(currency,count, fill=state)) + geom_bar(stat="identity") + 
  ggtitle("Projects by Currency") + 
  xlab("Currency") + ylab("Frequency") + scale_y_continuous(labels=scales::comma)+ 
  scale_fill_discrete(name="Project Status", breaks=c("successful", "failed"),
                      labels=c("Successful", "Failed")) + 
  geom_text(aes(label=paste0(round(pct3*100,1),"%")), position=position_stack(vjust=0.5), 
            colour="black", size=5) + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12), legend.position="bottom", 
        legend.title=element_text(size=12, face="bold"))

gridExtra::grid.arrange(pp1, pp2, ncol = 2)


#backers supports in function of the success of the project.
#quantile(data$backers, probs = seq(from = 0, to = 1, by = .2))

#frequency.backer <- data %>%
#  filter(backers!="0", state %in% c("successful", "failed")) %>%
#  group_by(backers, state) 
#frequency.backer

#ggplot(frequency.backer, aes(log(backers), fill=state)) + 
#  geom_histogram(alpha=0.9, binwidth = 30, position="identity") + theme_economist() +  
#  ggtitle("Distribution of log(Backers)") + xlab("log(USD)") + 
#  ylab("Frequency") + scale_fill_discrete("State", labels=c("Failed","Successful"))


#Project goals supports in function of the success of the project.
#quantile(data$goal, probs = seq(from = 0, to = 1, by = .2))

frequency.goal <- data %>%
  filter(goal!="0", state %in% c("successful", "failed")) %>%
  group_by(goal, state) 
#frequency.goal

#ggplot(frequency.goal, aes(log(usd_goal), fill=state)) + 
#  geom_histogram(alpha=1, binwidth = 30, position="identity") + theme_economist() +  
#  ggtitle("Distribution of log(USD goals)") + xlab("log(goals)") + 
#  ylab("Frequency") + scale_fill_discrete("State", labels=c("Failed","Successful"))

#Correlating exploration
#backers in function of usd pledged
ggplot(frequency.goal, aes(log(backers),log(usd_pledged), color=state)) + 
  geom_point(position="identity") + theme_economist() +  
  ggtitle("Backers in function of pledges") + xlab("log(Backers)") + 
  ylab("USD Pledged")

#Goals in function of usd pledged
ggplot(frequency.goal, aes(log(usd_goal),log(usd_pledged), color=state)) + 
  geom_point(position="identity") + theme_economist() +  
  ggtitle("Goals in function of Pledges") + xlab("log(USD Goals)") + 
  ylab("USD Pledged")

#Goals in function of backers
ggplot(frequency.goal, aes(log(backers),log(usd_goal), color=state)) + 
  geom_point(position="identity") + theme_economist() +  
  ggtitle("Backers in function of Goals") + xlab("log(Backers)") + 
  ylab("USD Pledged")


#Pledge's length in function of backers
ggplot(frequency.goal, aes((length),log(usd_goal), color=state)) + 
  geom_point(position="identity") + theme_economist() +  
  ggtitle("Pledge's length in function of USD Goals") + xlab("length") + 
  ylab("USD Pledged")


#Pledge's length in function of backers
ggplot(frequency.goal, aes(year(launched), fill=state)) + 
  geom_bar() + theme_economist() +  facet_wrap( ~ main_category) +
  ggtitle("Pledges in function of years") + xlab("Category") + 
  ylab("USD Pledged")

#Correlation among backers, usd_pledge, usd_goal and launched_year
cor(data[,c(11,13:15)])

##############################################################################################################################################
##### Modelling Part
#select the features we believe have an impact on project success or failure
subset1 <- data %>% select(state,  main_category, usd_goal, country, backers, usd_pledged, length, launched_year)

#filter the state variable to just successful or failed projects.  (Other states exist like live, cancelled, etc.) 
subset1 <- subset1 %>% filter(., state=="successful" | state =="failed")

#Factorize categorical varibles:
subset1$state <- as.factor(subset1$state)
subset1$main_category <- as.factor(subset1$main_category)
subset1$country <- as.factor(subset1$country)
subset1$launched <- as.factor(subset1$launched_year)


#omit NA values
subset1 = na.omit(subset1)

#Binarize state variables
#subset1$state <- ifelse(subset1$state=="successful", 1, 0)

#Creating partition for test and training datasets
set.seed(77777777, sample.kind="Rounding")
test_index <- createDataPartition(y = subset1$state, times = 2, p = 0.1, list = FALSE)
trainset <- subset1[-test_index,]
testset <- subset1[test_index,]


####################### Model exploration
# knn, svm, rf and nv are taking too long here.
set.seed(11)
fit.lda <- train(state~., data=trainset, method="lda")
# CART
set.seed(111)
fit.cart <- train(state~., data=trainset, method="rpart")
# kNN takes too long
#set.seed(1111)
#fit.knn <- train(state~., data=trainset, method="knn")
# SVM takes too long
#set.seed(11111)
#fit.svm <- train(state~., data=trainset, method="svmRadial")
# Random Forest takes too long
#set.seed(111111)
#fit.rf <- train(state~., data=trainset, method="rf")
# Naive Bayse takes too long
# nb
#set.seed(111111)
#fit.nb <- train(state~., data=trainset, method="nb")


# summarize accuracy of used models
results <- resamples(list(lda=fit.lda, cart=fit.cart))
summary(results)



#Tree model selection view - showing the most significant predictors
tree1 <- tree(state ~ . , data = trainset)
summary(tree1)
plot(tree1)
text(tree1)
prediction <- predict(tree1, testset)
validation <- data.frame( v_state = testset$state, pred_state = prediction)
validation$check <- ifelse(validation$pred_state.successful < 0.5, 0, 1)

table(validation$v_state, validation$check)

auc(validation$v_state, validation$check)


















































































