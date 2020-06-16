# 2020.06.01
# Introduction to Data Science (UCLA Extension)
# Final Class Project
# Student: Sasima Deesriwong
# Instructor: Daniel D. Gutierrez

#Topic 'California Housing data'

library(corrplot)
library(ggplot2)
library(GGally)
install.packages("ggthemes") # Install 
library(ggthemes)
library(randomForest)

# 1. Data access: download the data set into your R environment.
#Reading csv file Data
cahousing<-read.csv("housing.csv", header=T)


#Check summary of data in each column 
summary(cahousing)
str(cahousing)

#There are 20640 rows with 10 variables
#There are  9 numeric variables and 1 categorical variables
#There are 207 NA in total bedrooms data #NA which I will manage it later

#longitude         latitude     housing_median_age  total_rooms   
#Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2  
#1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448  
#Median :-118.5   Median :34.26   Median :29.00      Median : 2127  
#Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636  
#3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148  
#Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320  

#total_bedrooms     population      households     median_income    
#Min.   :   1.0   Min.   :    3   Min.   :   1.0   Min.   : 0.4999  
#1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634  
#Median : 435.0   Median : 1166   Median : 409.0   Median : 3.5348  
#Mean   : 537.9   Mean   : 1425   Mean   : 499.5   Mean   : 3.8707  
#3rd Qu.: 647.0   3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432  
#Max.   :6445.0   Max.   :35682   Max.   :6082.0   Max.   :15.0001  
#NA's   :207                                                        
#median_house_value   ocean_proximity
# Min.   : 14999     <1H OCEAN :9136  
# 1st Qu.:119600     INLAND    :6551  
# Median :179700     ISLAND    :   5  
# Mean   :206856     NEAR BAY  :2290  
# 3rd Qu.:264725     NEAR OCEAN:2658  
# Max.   :500001              

head(cahousing)
#to see first 6 row of data
# latitude housing_median_age total_rooms total_bedrooms population households
#1   -122.23    37.88                 41         880            129        322        126
#2   -122.22    37.86                 21        7099           1106       2401       1138
#3   -122.24    37.85                 52        1467            190        496        177
#4   -122.25    37.85                 52        1274            235        558        219
#5   -122.25    37.85                 52        1627            280        565        259
#6   -122.25    37.85                 52         919            213        413        193

#median_income median_house_value ocean_proximity
#1        8.3252             452600        NEAR BAY
#2        8.3014             358500        NEAR BAY
#3        7.2574             352100        NEAR BAY
#4        5.6431             341300        NEAR BAY
#5        3.8462             342200        NEAR BAY
#6        4.0368             269700        NEAR BAY

tail(cahousing)
#to see the last 6 row of data
#longitude latitude housing_median_age total_rooms total_bedrooms population households
#20635   -121.56    39.27                 28        2332            395       1041        344
#20636   -121.09    39.48                 25        1665            374        845        330
#20637   -121.21    39.49                 18         697            150        356        114
#20638   -121.22    39.43                 17        2254            485       1007        433
#20639   -121.32    39.43                 18        1860            409        741        349
#20640   -121.24    39.37                 16        2785            616       1387        530
#median_income median_house_value ocean_proximity
#20635        3.7125             116800          INLAND
#20636        1.5603              78100          INLAND
#20637        2.5568              77100          INLAND
#20638        1.7000              92300          INLAND
#20639        1.8672              84700          INLAND
#20640        2.3886              89400          INLAND

----------------------------------------------------------------
#Data Visualization
  #Histogram of all variables
  
      #Longitude
  hist(cahousing$longitude, col=rgb(1,0,0,0.5),
       main = "Histogram of Longtitude",xlab = " Longtitude")
  
  ggplot(cahousing, aes(x = longitude)) + 
    geom_histogram(binwidth = 1, fill="black", col="grey")+
    ggtitle("Histogram of Longitude")+
    theme(plot.title = element_text(hjust = 0.5))
  
      #latitude 
  ggplot(cahousing, aes(x = latitude )) + 
    geom_histogram(binwidth = 1, fill="gray39", col="grey")+
    ggtitle("Histogram of Latitude")+
    theme(plot.title = element_text(hjust = 0.5))
  
      #housing_median_age 
  ggplot(cahousing, aes(x = housing_median_age )) + 
    geom_histogram(binwidth = 5, fill="midnightblue", col="grey")+
    ggtitle("Histogram of Housing median age")+
    theme(plot.title = element_text(hjust = 0.5))
  
      #total_rooms 
  ggplot(cahousing, aes(x = total_rooms )) + 
    geom_histogram(binwidth = 1000, fill="steelblue4", col="grey")+
    ggtitle("Histogram of Total rooms")+
    theme(plot.title = element_text(hjust = 0.5))
    
  
      #total_bedrooms  
  ggplot(cahousing, aes(x = total_bedrooms )) + 
    geom_histogram(binwidth = 100, fill="maroon4", col="grey")+
    ggtitle("Histogram of Total bedrooms")+
    theme(plot.title = element_text(hjust = 0.5))
  
      #population
  ggplot(cahousing, aes(x = population )) + 
    geom_histogram(binwidth = 1000, fill="yellow4", col="grey")+
    ggtitle("Histogram of population")+
    theme(plot.title = element_text(hjust = 0.5))
  
      #households 
  ggplot(cahousing, aes(x = households  )) + 
    geom_histogram(binwidth = 100, fill="mediumpurple4", col="grey")+
    ggtitle("Histogram of Households ")+
    theme(plot.title = element_text(hjust = 0.5))
  
      #median_income
  ggplot(cahousing, aes(x = median_income )) + 
    geom_histogram(binwidth = 1, fill="brown3", col="grey")+
    ggtitle("Histogram of Median income (10K USD)")+
    theme(plot.title = element_text(hjust = 0.5))
  
      #median_house_value 
  options(scipen = 999)
  ggplot(cahousing, aes(x = median_house_value )) + 
    geom_histogram(binwidth = 10000, fill="lightpink3", col="grey")+
    ggtitle("Histogram of Median house value ")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  #Density plot of Med house value by Ocean proximity
  ggplot(cahousing, aes( x=median_house_value,fill=ocean_proximity, color=ocean_proximity)) +
    geom_density(alpha=0.3)+
    labs(title = "Median house value by Ocean proximity")+ 
    theme(plot.title = element_text(hjust = 0.5))
    
  #Density plot of Med  Income by Ocean proximity
  ggplot(cahousing, aes( x=median_income,fill=ocean_proximity, color=ocean_proximity)) +
    geom_density(alpha=0.3)+
    labs(title = "Median income by Ocean proximity")+ 
    theme(plot.title = element_text(hjust = 0.5))
  
 #Map latitude Longitude +pop + median house value
  ggplot(cahousing, aes( x=longitude, y= latitude, fill=median_house_value, color=median_house_value)) +
    geom_point(aes(size = population),alpha=0.3)
  
  #Ocean proxi
  
  ggplot(cahousing, aes(x = ocean_proximity)) +
    geom_bar(stat = "count", color = "black", fill = "navyblue")
  
   ------------------------------------------------------
    
    #Data Transformation
    #Manipulate NA data for total_bedrooms
summary(cahousing)
#Summary of bedrooms before filling NA
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #1.0   296.0   435.0   537.9   647.0  6445.0     207 
    
  summary(cahousing$total_bedrooms)
#Find out what is the Median value of total_bedrooms that do not hv NA
 Medianbed<-median(cahousing$total_bedrooms, na.rm = T)
 Medianbed
#Median of data = 435
# >  Medianbed
 #[1] 435
 
# Filling NA data in total_badrooms with median data
  cahousing$total_bedrooms[which(is.na(cahousing$total_bedrooms))] <- median(cahousing$total_bedrooms, na.rm = T)
#See summary after filling data
   summary(cahousing$total_bedrooms)

   #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 1.0   297.0   435.0   536.8   643.2  6445.0 
   
#(list = ls())
#ls()   

   
   #split ocean_proximity to binary categorical
   
   class(cahousing$ocean_proximity)
  summary(cahousing$ocean_proximity)
  
  ocp_house <- levels(cahousing$ocean_proximity)
ocp_house  
binaryOcp <- function(c) {return(cahousing$ocean_proximity == c)}
binaryOcp("NEAR BAY")

newVars <- sapply(ocp_house, binaryOcp)
newVars [1:10,]
class(newVars)

cahousing_new<-cahousing
cahousing_new$ONEH_OCEAN <- newVars[,1] 
cahousing_new$INLAND <- newVars[,2] 
cahousing_new$ISLAND<- newVars[,3] 
cahousing_new$NEAR_BAY <- newVars[,4] 
cahousing_new$NEAR_OCEAN <- newVars[,5] 

head(cahousing_new)
summary(cahousing_new)
str(cahousing_new)

#delete Ocean_proximity column
cahousing_new <- subset(cahousing_new, select = -c(ocean_proximity) )
head(cahousing_new)
summary(cahousing_new)

#convert logical to binary 0 and 1

cols <- sapply(cahousing_new, is.logical)
cols
cahousing_new[,cols] <- lapply(cahousing_new[,cols], as.numeric)
head(cahousing_new)
summary(cahousing_new)

##Find mean of total bedrooms & total rooms
cahousing_new$mean_number_bedrooms <- (cahousing_new$total_bedrooms/ cahousing_new$households)
cahousing_new$mean_number_rooms <- (cahousing_new$total_rooms/ cahousing_new$households)

summary(cahousing_new)
#delete total bedrooms & total rooms
cahousing_new <- subset(cahousing_new, select = -c(total_bedrooms, total_rooms) )


#Scale all numeric variable except median_house_value
cahousing_new[,c(1:6,13:14)]<-scale(cahousing_new[,c(1:6,13:14)])
summary(cahousing_new)

ls()
ls(cahousing_new)
#[1] "households"           "housing_median_age"   "INLAND"               "ISLAND"              
#[5] "latitude"             "longitude"            "mean_number_bedrooms" "mean_number_rooms"   
#[9] "median_house_value"   "median_income"        "NEAR_BAY"             "NEAR_OCEAN"          
#[13] "ONEH_OCEAN"           "population"   

#---------------------------------------------------------------
# Correlation Matrix all numeric variables
#---------------------------------------------------------------
cormat <-(cor(cahousing_new[,c(1:6,13:14,7)]))
corrplot(cormat,tl.cex = 0.7,tl.col = "black", number.cex = 0.6,method = "number")
cor(cahousing_new[,c(1:6,13:14,7)])
plot(cahousing_new[,c(1:6,13:14,7)])     

plot(cahousing_new$median_income, cahousing_new$median_house_value)  
------------------------------
  
  #create training data set
  cahousing_new <- na.omit(cahousing_new)
# Split data set into training set and test set
n <- nrow(cahousing_new) # Number of observations 20640
ntrain <- round(n*0.75)  # 75% for training set
ntrain
#[1] 15480
set.seed(333)    # Set seed for reproducible results

tindex <- sample(n, ntrain)   # Create an index

train_cahousenew <- cahousing_new[tindex,]   # Create training set
test_cahousenew <- cahousing_new[-tindex,]   # Create test set
 summary(train_cahousenew)
#Subset column
train_x <-subset(train_cahousenew, select=c(1:6,8:14))
str(train_x)
class(train_x)
train_y<-subset(train_cahousenew, select=c(7))
length(train_x)
str(train_y)
length(train_y)
class(train_y)
# ---------------------------------------------------------------
# Train randomForest to predict Species using all predictors

rf<-randomForest(x=train_x, y=train_y, ntree=500, importance = TRUE)

names(rf)


#Error in randomForest.default(x = train_x, y = train_y, ntree = 500, importance = TRUE) : 
#length of response must be the same as predictors
#In addition: Warning message:
#In randomForest.default(x = train_x, y = train_y, ntree = 500, importance = TRUE) :
#The response has five or fewer unique values.  Are you sure you want to do regression?

#I struck at this and try to search the right solution, but cannot figure out
#I decided to do normal regression based on relationship with median house value


#---------------------------------------------------------------
# Correlation Matrix all numeric variables
#---------------------------------------------------------------
cormat <-(cor(cahousing_new[,c(1:6,13:14,7)]))
corrplot(cormat,tl.cex = 0.7,tl.col = "black", number.cex = 0.6,method = "number")
cor(cahousing_new[,c(1:6,13:14,7)])
plot(cahousing_new[,c(1:6,13:14,7)])     

plot(cahousing_new$median_income, cahousing_new$median_house_value)    #>> positive linear line
plot(cahousing_new$median_house_value, cahousing_new$mean_number_rooms) #>> no pattern

# From corrplot shown that median income is only variable that correlated with med house value
#Do linear regression

plot(cahousing_new$median_income, cahousing_new$median_house_value) 
smooth <- smooth.spline(cahousing_new$median_income, cahousing_new$median_house_value, spar=1)
lines(smooth, col='red', lwd = 2, lty = 2)
modelMC1 <- lm(median_house_value~median_income, data=cahousing_new)
summary(modelMC1)

plot(modelMC1)

plot(cahousing_new$median_income, cahousing_new$median_house_value)  
modelMC2 <- lm(median_house_value~median_income+longitude+ latitude+housing_median_age+population+households+ONEH_OCEAN+INLAND+ ISLAND +NEAR_BAY+ mean_number_bedrooms   , data=cahousing_new)
summary(modelMC2)

plot(modelMC2)


plot(cahousing_new$median_income, cahousing_new$median_house_value)  
modelMC2 <- lm(median_house_value~median_income+longitude+ latitude+housing_median_age+population+households+total_rooms + total_bedrooms+ONEH_OCEAN+INLAND+ ISLAND +NEAR_BAY   , data=cahousing_new)
summary(modelMC2)

plot(modelMC2)

#remove multicolinearity
plot(cahousing_new$median_income, cahousing_new$median_house_value)  
modelMC2 <- lm(median_house_value~median_income+housing_median_age++total_rooms +ONEH_OCEAN+INLAND+ ISLAND +NEAR_BAY , data=cahousing_new)
summary(modelMC2)

plot(modelMC2)


