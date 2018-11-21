# Download Data: https://www.kaggle.com/c/bike-sharing-demand/data
# Tutorials: http://brandonharris.io/kaggle-bike-sharing/ 
#            http://nonsensical.in/datascience/bike-sharing-kaggle/ 
#            file:///Users/mymac/Downloads/Tutorial_kaggle_Bikesharing_randomForest.pdf 

##################################################################################
################################### PREPARE FILE ##################################
##################################################################################

# load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(party)

# set working directory and files
setwd('~/Documents/Programming Projects/Data Sets/bike_all/')
train = read.csv("train.csv")
test = read.csv("test.csv")

##################################################################################
################################ OVERVIEW OF DATA ################################ 
##################################################################################

# take a quick look at the dataframe
str(train)
head(train)
summary(train)

# See how count varies over entire timeframe
plot(train$count)

# Use the	pairs	command to plot	all	the	variables	of the data set against each	other.	
pairs(train)

# Question: what other plot methods can I use to get a good overview of the data?

##################################################################################
################################ INITIAL INSIGHTS ################################ 
##################################################################################


# Borrowed observation from another user. Question: How would I observe this?
# There	are	some loose connections to the weather variables:	
# Extreme	weather	seems	to	reduce	the	bike rentals.	But	no	variable
# in the graph shows a linear	connection to	the	count.

# Registered user behavior may differ from and casual user behavior.

# Day of week (workday vs weekend) will yield different usage.

# Hour of day will yield different usage. Combine with day of week for more refined usage info.

# Windspeed, humidity, and temperature may yield different usage.

# Temp and atemp may be tandem or else hypothesis is atemp may be more important.

# Overall usage trends rise from 2011 to 2012



##################################################################################
########################### DATA CLEANING AND ORGANIZING #########################
##################################################################################

# view our three dependent variables
# counts <- train %>%
#  select("casual", "registered", "count")

# Question: what is wrong with this? For one thing, it takes about three minutes to appear

# view table
#counts

# make a table of dependent variables
# prop.table(table(counts))

################################# COMBINE DATAFRAMES ############################# 
# To rbind dataframes, they have to have the same columns as each other. 
# Since we lack some columns in our test set, create variables full of missing values (NAs)

test$casual <- NA
test$registered <- NA
test$count <- NA

# Make a new dataframe called “combi” with all the same rows as the original two datasets, stacked in the order in which we specified: train first, and test second.
combi <- rbind(train, test)

# Observe combi
str(combi)
# Question: What's up with this weird structure at the bottom?

#################################### DATE/TIME ###################################

# Use lubridate with datetime to create new variables: month, weekday, and hour
# Question: I do not think I am actually using lubridate here?
combi$datetime <- strptime(combi$datetime, format="%Y-%m-%d %H:%M:%S")
combi$month <- month(combi$datetime)         # Question: Error: could not find function "month"
combi$day <- day(combi$datetime)
combi$hour <- combi$datetime$hour

# Question: why doesn't this work?
# combi%>%
#   datetime <- strptime(datetime, format="%Y-%m-%d %H:%M:%S")
#   month <- month(datetime)
#   weekday <- weekdays(datetime)
#   hour <- datetime$hour

################################## CREATE DAYPARTS ################################

# Use a visual to determine distinct dayparts
ggplot(train, aes(x = hour, y = count, col = workingday)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)

#create daypart column with five levels
#6AM - 10AM = 1
combi$daypart[(combi$hour < 10) & (combi$hour >= 6)] <- "1"
#10AM - 5PM = 2
combi$daypart[(combi$hour < 5) & (combi$hour >= 10)] <- "2"
#5PM - 8PM = 3
combi$daypart[(combi$hour < 8) & (combi$hour >= 5)] <- "3"
#8PM - 12AM
combi$daypart[(combi$hour < 12) & (combi$hour >= 8)] <- "4"
#12AM - 6AM
combi$daypart[(combi$hour < 6) & (combi$hour >= 12)] <- "5"


##################################### FACTOR ####################################

# Factor categorical variables  
combi$weather <- factor(combi$weather)
combi$season <- factor(combi$season)
combi$holiday <- factor(combi$holiday)
combi$workingday <- factor(combi$workingday)
combi$day <- factor(combi$day)
combi$daypart <- factor(combi$daypart)
glimpse(combi)  

# Question: Why does combi$daypart have only 2 levels and a bunch of NA's? I
# tried defining dayparts as both integers and strings.
glimpse(combi$daypart)

############################## COMBINE DATAFRAMES ################################

# Split the test and training sets back into their original states
train <- combi[1:10886,]
test <- combi[10887:17379,]

##################################################################################
############################## FEATURE ENGINEERING ###############################
##################################################################################

# Question: None of these feature engineering ideas have been executed - want to learn how.

# Group registered and casual users by time, day, holiday, etc. 
# Create better plots to show different usage for registered vs casual users. 
ggplot(train, aes(x = hour, y = count, col = registered)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)
ggplot(train, aes(x = holiday, y = count, col = registered)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)

# Use dayparts for workingdays vs non-workingdays. See plot from above:
ggplot(train, aes(x = hour, y = count, col = workingday)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)

# Group by temperature. 
# Create a better plot to show weather-related variables
ggplot(train, aes(x = temp, y = count, col = weather)) +
  geom_point(size=3, alpha = 0.1, position = posn.jd)

# Plot helper-functions
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# Use holidays vs non-holidays.
# Create a better plot to show holiday variables.
ggplot(train, aes(x = day, y = count, col = holiday)) +
  geom_point(size=3, alpha = 0.05, position = posn.jd)

# Wind speed == 0 can be consider missing. Use random forest to predict these values.

# If we include month, a problem can be that the rentals in for example January 2011 and 
# January 2012 may not be related. But the sales of the previous months would incorporate 
# the growth, hence including quarters in our model would make sense.


##################################################################################
################################# MANUAL ANALYSIS ################################ 
##################################################################################

# Question: insert which variables?
# aggregate(count ~ [insert which variables?], data=train, FUN=sum)
# aggregate(count ~ [insert which variables?], data=train, FUN=length)
# aggregate(count ~ [insert which variables?], data=train, FUN= function(x) {sum(x)/length(x)})

# Count by day
aggregate(train[,"count"],list(train$day),mean)
summary(aggregate(train[,"count"],list(train$day),mean))

# Observations: Sunday is lowest day, nearly 10% lower than Thursday, Friday, Saturday
# (highest days); Monday, Tuedsay, Wednesday are in-between, and about equal
# What do we do with this information? This blog says: "Why don’t we create a
# ‘sunday’ variable that tells our model that this may be an important predictor
# (and we’ll also make it a factor)."
# http://brandonharris.io/kaggle-bike-sharing/


##################################################################################
################################# DECISION TREE ################################## 
##################################################################################

# rpart stands for “Recursive Partitioning and Regression Trees” and uses the CART decision tree algorithm. 

################################### MODEL A #####################################

fit <- rpart(count ~ season + holiday + workingday + weather + workingday + weather + temp + atemp + humidity + windspeed + day + hour,
             data=train,
             method="class")
# Question: Error in eval(expr, envir, enclos) : objects 'day', 'hour' not found

#plot
rpart.plot(fit)
# Question: Error: box.palette: c("#F7FCF5", "#EEF8EA", "#E5F5E0",
# "#D6EFD0", "#C7E9C0", "#B4E1AD", "#A1D99B", "#8ACE88", "#74C476") is neither a
# color nor a palette. Try something like box.palette="blue" or
# box.palette="Blues". The predefined palettes are (with an optional "-"
# prefix): Grays Greys Greens Blues Browns Oranges Reds Purples Gy Gn Bu Bn Or
# Rd Pu (alternative names for the above palettes) BuGn BuBn GnRd etc.
# (two-color diverging palettes: any combination of two palettes) RdYlGn GnYlRd
# BlGnYl YlGnBl (three color palettes)

plot(fit)
# Error in plot.rpart(fit) : fit is not a tree, just a root
text(fit)
# Question: Error in text.rpart(fit) : fit is not a tree, just a root

# build model
fit.tree <- forest(fit, data=train)
# Question: Error: could not find function "forest"

# run model against test data set
predict.tree <- predict(fit.tree, test)

#build a dataframe with results
submit.tree <- data.frame(datetime = test$datetime, count=predict.tree)

#write results to .csv for submission
write.csv(submit.tree, file="submit_tree_modelA.csv",row.names=FALSE)


# Kaggle score = [...]

################################### MODEL B #####################################

# Override the defaults to make the minimum-to-split-buckets 2 and unleash the cp parameter, the metric that stops splits that aren’t deemed important enough, by reducing it to zero.
fit <- rpart(count ~ season + holiday + workingday + weather + workingday + weather + temp + atemp + humidity + windspeed + day + hour,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
rpart.plot(fit)

# build model
fit.tree <- forest(fit, data=train)

# run model against test data set
predict.tree <- predict(fit.tree, test)

#build a dataframe with results
submit.tree <- data.frame(datetime = test$datetime, count=predict.tree)

#write results to .csv for submission
write.csv(submit.tree, file="submit_tree_modelB.csv",row.names=FALSE)

# Kaggle score = [...]

##################################################################################
################################## RANDOM FOREST ################################# 
##################################################################################

# Question: 

# Set random seed so results are reproducible (otherwise different each time you run). The number inside isn't important.

# set seed for reproducibility (otherwise random each run)
set.seed(415)

# create formula
fit <- randomForest(as.factor(count) ~ season + holiday + workingday + weather + workingday + weather + temp + atemp + humidity + windspeed + day + hour,
              data=train, 
              importance=TRUE, 
              ntree=2000)
# Question: Error:
# Error in eval(expr, envir, enclos) : object 'day' not found

# plot
varImpPlot(fit)
# Question: Error: 
# Error in varImpPlot(fit) : 
# This function only works for objects of class `randomForest'

# build model
fit.tree <- forest(fit, data=train)
# Question: Error: could not find function "forest"

# run model against test data set
predict.tree <- predict(fit.tree, test)

#build a dataframe with results
submit.tree <- data.frame(datetime = test$datetime, count=predict.tree)

#write results to .csv for submission
write.csv(submit.tree, file="submit_ctree_v1.csv",row.names=FALSE)

# Kaggle score = [...]

##################################################################################
############################ CONDITIONAL INFERENCE TREE ########################## 
##################################################################################

################################### MODEL A #####################################

# Question: What's wrong with the fit formula?

# set seed for reproducibility (otherwise random each run)
set.seed(415)

# create formula
fit <- count ~ season + holiday + workingday + weather + workingday + weather + temp + atemp + humidity + windspeed + weekday + hour,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3)

# build model
fit.ctree <- cforest(fit, data=train)

# examine model for variable importance
#fit.ctree
# Note: this tree is large and takes significance computing power

# run model against test data set
predict.ctree <- predict(fit.ctree, test)

#build a dataframe with results
submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)

#write results to .csv for submission
write.csv(submit, file = "submit_ctree_modelA.csv", row.names = FALSE)

# Kaggle score = [...]

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
################################## CONCLUSIONS ################################### 
##################################################################################

# How can I observe the trees I've created to draw conclusions?

##################################################################################
################################ RESOURCES & TIPS ################################ 
##################################################################################

############################## DATA MANIPULATION ##################################

# Hint:	If	you	just	want	to	mess	around	or	are	unsure	about	the	syntax	of	complex
# prediction	methods like	randomForest,	use	only	a	few	rows	of	the	data	until	you	are
# sure	your	script	returns	usable	results.	This	will	save	you	hours	of	your	life.	Use
# read.csv("train.csv",	nrows=1000)	instead	of	the above	commands.

#################################### MODELS ######################################

# DT (supposedly superior for this data set: https://www.kaggle.com/c/bike-sharing-demand/discussion/64386)
# XGBoost
# Randomforest Explanation: file:///Users/mymac/Downloads/Tutorial_kaggle_Bikesharing_randomForest.pdf 
      # The	variable	ntree	sets	how	large	your	Random	Forest	is	going	to	be.	Or	in	other	words,
      # how	many	trees	should	be	contained	in	your	ensemble.	We	use	500	here	to	strike	some
      # balance	between	fitness	and	computation	time. myNtree	=	500
      # The	variable	mtry	controls	the	number	of	variables	randomly	sampled	at	each	split.	The
      # random	value	here	is	one	third	of	all	the	variables	given	to	randomForest.	In	our
      # example	we	use	the	value	5: myMtry	=	5
      # Hint:	More	trees		and	more	variables	don’t	always	mean	“better	fit”.	But	they	do	always
      # mean	a	lot	of	additional	computation	time.	Try	and	play	around	with	the	values	but
      # don’t	panic	when	your	R	session	gets	busy	for	10	–
      # 20	minutes.	This	stuff	is	intense!
      # Should	importance	of	predictors	be	assessed?	Yes,	please,	since	we	aren’t	so	sure	about
      # that	ourselves. myImportance	=	TRUE
      # And	at	last	we	set	a	seed	for	the	random	sampling	generator	so	we	make
      # our	results
      # comparable	and	not	tainted	by	different	random	samples	(Feel	free	to	use	any	integer
      # value	you	like	here). set.seed(415)
      # Now	we	can	start	making	sense	of	the	data	using	randomForest.
      # testTrain	<- subset(train,	select	=	-c(datetime,	count,	registered))
      # testFit	<- randomForest(casual	~	.,	data=testTrain,	ntree=myNtree,	mtry=myMtry,	
      #                 importance=myImportance)
