# Kaggle: Bike Sharing Demand
# Annie Eby
# 11/21/2018
#
#
# Download Data: https://www.kaggle.com/c/bike-sharing-demand/data
# Tutorials: http://brandonharris.io/kaggle-bike-sharing/ 
#            http://nonsensical.in/datascience/bike-sharing-kaggle/ 
#            file:///Users/mymac/Downloads/Tutorial_kaggle_Bikesharing_randomForest.pdf 



##################################################################################
################################### PREPARE FILE ##################################
##################################################################################

# load libraries using pacman to make the file more easily shareable
if(!require(pacman)){install.packages('pacman')}
pacman::p_load(dplyr, tidyr, lubridate, ggplot2, caret,randomForest, rpart, 
               rpart.plot, RColorBrewer, rattle, party, Hmisc)

# set working directory and files; read.csv automatically makes strings into factors; override this.
setwd('~/Documents/Programming Projects/Data Sets/bike_all/')
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

# Plot helper-functions
posn.d <- position_dodge(width = 0.1)
posn.jd <- position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)
posn.j <- position_jitter(width = 0.2)

##################################################################################
################################ OVERVIEW OF DATA ################################ 
##################################################################################

# take a quick look at the dataframe
str(train)
head(train)
summary(train)

# See how count varies over entire timeframe
plot(train$count, main="User Count Over Time", sub="Full Training Data Set",
     xlab="Observations", ylab="Count")

# Use the pairs command to plot all variables against eachother
# Use slice to plot only the first 100 observations.	
pairs(train %>% slice(1:100))

##################################################################################
################################ INITIAL INSIGHTS ################################ 
##################################################################################

# Registered user behavior may differ from casual user behavior.

# Day of week (workday vs weekend) may yield different usage.

# Hour of day may yield different usage. Combine with day of week for more refined usage info.

# Windspeed, humidity, and temperature may yield different usage.

# Temp and atemp may be tandem or else hypothesis is atemp may be more important.

# Overall usage trends rise from 2011 to 2012.

# Wind speed == 0 can be consider missing data. Could use random forest to predict these values.


##################################################################################
################################## DATA CLEANING AND #############################
################################# FEATURE ENGINEERING  ###########################
##################################################################################

# view our three dependent variables
counts <- train %>%
  slice(sample(1:nrow, train, 100))%>%
  select("casual", "registered", "count")
# Question: Error in slice_impl(.data, dots) : Evaluation error: NA/NaN argument.
# Still allows counts to run below, but gives the same data as if I just do slice(1:100)

# view table
counts

# make a table of the proportions of casual, registered, and total (count), users 
prop.table(table(counts))
# Question: How do I get a one-row summary of the proportions of each variable?

################################# COMBINE DATAFRAMES ############################# 

# Make a new dataframe called “combi” with all the same rows as the original two datasets, stacked in the order in which we specified: train first, and test second.
# Use bind_rows(), which automatically creates new columns with NA values, as needed
combi <- bind_rows(train %>% mutate(datetime), 
                  test %>% mutate(datetime))

# Note: I do not have to use mutate(datetime = as.character(datetime)) because I
# prevented them from becoming factors with train = read.csv("train.csv",
# stringsAsFactors = FALSE)

# Observe combi
str(combi)

#################################### DATE/TIME ###################################

# Use lubridate with datetime to create new variables: month, day, and hour

combi = combi %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"),
         month = month(datetime),
         day = weekdays(datetime),
         hour = hour(datetime)) 

head(combi)

################################## CREATE DAYPARTS ################################

# Note: I did not end up using this in the Random Forest model; simply used hour instead.

# Use a visual to determine distinct dayparts.

# when using sample(), set seed to get the same random sample each time the code is run. 
# this helps reproducing the same results when the code is shared with someone.
set.seed(10012)

# Plot
ggplot(train %>% 
      slice(sample(1:nrow(train), 5000)), # slice for selecting a sample of rows
      aes(x = hour, y = count, col = workingday)) +
      geom_point(size=3, alpha = 0.1, position = posn.jd)

#create daypart column with five levels
#6AM - 10AM = 1
combi$daypart[(combi$hour < 10) & (combi$hour >= 6)] <- 1
#10AM - 5PM = 2
combi$daypart[(combi$hour < 17) & (combi$hour >= 10)] <- 2
#5PM - 8PM = 3
combi$daypart[(combi$hour < 20) & (combi$hour >= 17)] <- 3
#8PM - 12AM
combi$daypart[(combi$hour < 24) & (combi$hour >= 20)] <- 4
#12AM - 6AM
combi$daypart[(combi$hour < 6) & (combi$hour >= 0)] <- 5


##################################### FACTOR ####################################

# Factor categorical variables  
combi$weather <- factor(combi$weather)
combi$season <- factor(combi$season)
combi$holiday <- factor(combi$holiday)
combi$workingday <- factor(combi$workingday)
combi$day <- factor(combi$day)
combi$daypart <- factor(combi$daypart)

glimpse(combi)  
glimpse(combi$daypart)

############################## SPLIT DATAFRAMES ################################

# Split the test and training sets back into their original states
train <- combi[1:10886,]
test <- combi[10887:17379,]



##################################################################################
################################# MANUAL ANALYSIS ################################ 
##################################################################################


# User behavior shows positive correlation with good weather

ggplot(train, aes(x = temp, y = count, col = weather)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)

# Registered user rentals appear to peak during commute hours

ggplot(train, aes(x = hour, y = count, col = registered)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)

# Casual:Registered user proportion appears to be higher during holidays

ggplot(train, aes(x = holiday, y = count, col = registered)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)

# Hourly usage differs for working-days vs non-working-days

ggplot(train, aes(x = hour, y = count, col = workingday)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)

# Monday holidays are popular for bike rentals

ggplot(train, aes(x = day, y = count, col = holiday)) +
  geom_point(size=3, alpha = 0.05, position = posn.jd)

# There are approximately zero users on Level 4 "weather" days

ggplot(train, aes(x = temp, col = weather, fill = weather))+
  geom_freqpoly()

# Sunday is lowest use day, nearly 10% lower than Thursday, Friday, Saturday
# (highest days); Monday, Tuedsay, Wednesday are in-between, and about equal

aggregate(train[,"count"],list(train$day),mean)
summary(aggregate(train[,"count"],list(train$day),mean))



##################################################################################
################################## RANDOM FOREST ################################# 
##################################################################################

# Set random seed so results are reproducible (otherwise different each time you run). The number inside isn't important.
set.seed(415)

# create formula
fit <- randomForest(count ~ season + holiday + workingday + weather + workingday + weather + temp + atemp + humidity + windspeed + day + hour,
                    data=train, 
                    mtry = 3, 
                    importance=TRUE, 
                    ntree=250, 
                    do.trace = TRUE)

# plot
varImpPlot(fit)

# run model against test data set
predict.tree <- predict(fit, test)

# build a dataframe with results
submit.tree <- data.frame(datetime = test$datetime, count=predict.tree)

# write results to .csv for submission
write.csv(submit.tree, file="rforest_modelA.csv",row.names=FALSE)

# Kaggle score = 0.61941

##################################################################################
############################ CONDITIONAL INFERENCE TREE ########################## 
##################################################################################

# Note: Model A was dysfunctinal, hence we begin with Model B.

################################### MODEL B #####################################

# create formula
fit <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart

# build model
fit.ctree <- ctree(fit, data=train)

# examine model for variable importance
#fit.ctree
# Note: this tree is large and takes significance computing power

# run model against test data set
predict.ctree <- predict(fit.ctree, test)

#build a dataframe with results
submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)

#write results to .csv for submission
write.csv(submit.ctree, file="ctree_modelB.csv",row.names=FALSE)

# Kaggle score = 0.61165

##################################################################################
################################### CONCLUSIONS ################################## 
##################################################################################

# Insert conclusions here


##################################################################################
################################### SCRATCHPAD ################################### 
##################################################################################

# These plots are for practice, and are not included in the main "Manual Analysis" 
# because they are, for the most part, indecipherable.

ggplot(train, aes(x = hour,  y = temp))+
  geom_point(position = position_jitter(0.2))

ggplot(train, aes(x = hour,  y = temp))+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1))

ggplot(train, aes(x = daypart,  y = count))+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1))

ggplot(train, aes(x = daypart,  y = count))+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.3)

smean.cl.normal() 
mean_cl_normal()
#not running

ggplot(train, aes(x = hour,  y = count, col = daypart))+
  geom_point(alpha=.7) + 
  geom_smooth()

ggplot(train, aes(x = temp,  y = count, col = weather))+
  geom_point(alpha=.7) + 
  geom_smooth()

ggplot(train, aes(x = temp,  y = count, col = weather))+
  geom_point(alpha=.7) + 
  geom_smooth(se = FALSE)
# se = FALSE

ggplot(train, aes(x = temp,  y = count, col = weather))+
  geom_point(alpha=.7) + 
  stat_smooth(se = FALSE)
# when to use stat_smooth vs geom_smooth?

ggplot(train, aes(x = temp,  y = count, col = weather))+
  geom_point(alpha=.7) + 
  stat_smooth(method = "lm")
# method = "lm"; other argument options:  fullrange = TRUE

ggplot(train, aes(x = humidity,  y = count, col = weather))+
  geom_point(alpha=.7) + 
  geom_smooth(se = FALSE, span = 0.01)
# not good with this data, but...
# span argument controls alpha, degree of smoothing; smaller spans are more noisy

ggplot(train, aes(x = temp,  y = humidity, col = season))+
  geom_point(alpha=.7) + 
  geom_smooth()
# Question: How can I change plot colors so hotter seasons are associated with warmer colors, 
# and colder seasons are associated with cooler colors?

ggplot(train, aes(x = casual, y = count, col = holiday))+
  geom_point(alpha=.7) + 
  geom_smooth()

ggplot(train, aes(x = hour,  y = count, col = daypart))+
  geom_line() + 
  coord_fixed(0.02)

##################################################################################

# Question: How to get a numeric proportion of users in weather 1:2:3:4

ggplot(train, aes(x = temp, col = weather, fill = weather))+
  geom_histogram()

ggplot(train, aes(x = temp, col = weather, fill = weather))+
  geom_histogram()+
  geom_freqpoly(col = "white")

ggplot(train, aes(x = temp, col = weather, fill = weather))+
  geom_bar()

ggplot(train, aes(x = temp, col = weather, fill = weather))+
  stat_bin()
