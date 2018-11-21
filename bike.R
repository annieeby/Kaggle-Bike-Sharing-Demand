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
               rpart.plot, RColorBrewer, rattle, party)

# set working directory and files; read.csv automatically makes strings into factors; override this.
setwd('~/Documents/Programming Projects/Data Sets/bike_all/')
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)



##################################################################################
################################ OVERVIEW OF DATA ################################ 
##################################################################################

# take a quick look at the dataframe
str(train)
head(train)
summary(train)

# See how count varies over entire timeframe
plot(train$count, main="Count of Users Over Time", sub="Full Train Data Set",
     xlab="Observations", ylab="Count")

# Use the	pairs	command to plot	all	the	variables	of the data set against each	other. 
# Use slice to plot only the first 100 observations.	
pairs(train %>% slice(1:100))



##################################################################################
################################ INITIAL INSIGHTS ################################ 
##################################################################################

# Question: Create plots with which to observe the following:	

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



##################################################################################
################################## DATA CLEANING AND #############################
################################# FEATURE ENGINEERING  ###########################
##################################################################################

# view our three dependent variables
counts <- train %>%
  slice(sample(1:nrow, train, 100))%>%
  select("casual", "registered", "count")
# Question: Error in slice_impl(.data, dots) : Evaluation error: NA/NaN argument.
# Still allows counts to run below, but gives the same data as slice(1:100)

# view table
counts

# make a table of the proportions of casual, registered, and total (count), users 
# prop.table(table(counts))
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

# Use lubridate with datetime to create new variables: month, weekday, and hour

combi = combi %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"),
         month = month(datetime),
         day = weekdays(datetime),
         hour = hour(datetime)) 

head(combi)

################################## CREATE DAYPARTS ################################

# Note: I did not end up using this in the Random Forest model; simply used hour instead.

# Use a visual to determine distinct dayparts.

# Plot helper-functions
# when using sample(), set seed to get the same random sample each time the code is run. 
# this helps reproducing the same results when the code is shared with someone.
set.seed(10012)
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# Plot
ggplot(train %>% 
      slice(sample(1:nrow(train), 100)), # slice for selecting a sample of rows
      aes(x = hour, y = count, col = workingday)) +
      geom_point(size=3, alpha = 0.1, position = posn.jd)
# Question: Error: Don't know how to automatically pick scale for object of type
# function. Defaulting to continuous. Error: Column `x` must be a 1d atomic
# vector or a list Call `rlang::last_error()` to see a backtrace

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

# Count by day
aggregate(train[,"count"],list(train$day),mean)
summary(aggregate(train[,"count"],list(train$day),mean))

################################### OBSERVATIONS ################################# 

# Sunday is lowest day, nearly 10% lower than Thursday, Friday, Saturday
# (highest days); Monday, Tuedsay, Wednesday are in-between, and about equal



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
