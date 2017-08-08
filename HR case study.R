############################# HR CASE STUDY SOLUTION ###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Based on the past and current employee information
# the management has contracted an HR analytics firm to understand what factors they should focus on, in order to curb attrition
# The dataset has 6 files and has various employee related information , viz, Age,Attrition,BusinessTravel,Department,DistanceFromHome etc


## AIM:

# The aim is to model the probability of attrition using a logistic regression
# To predict the factors which influences the employee attrition 
# Whether an employee will leave the company or not depends on data from the following  buckets:

# 1. Employee survey data
# 2. General data
# 3. Manager survey data
# 4. In time and out time data

##################################################################

################################################################

### Data Understanding

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("lubridate")
#install.packages("data.table")
#install.packages("dplyr")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)

#Loading the required files for analysis:

emp_survey_data <- read.csv("employee_survey_data.csv",stringsAsFactors = F) 
general_data    <- read.csv("general_data.csv",stringsAsFactors = F)
manager_survey_data  <- read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time   <- read.csv("in_time.csv",stringsAsFactors = F)
out_time  <- read.csv("out_time.csv",stringsAsFactors= F)


str(emp_survey_data) #4410 obs. of  4 variables
str(general_data)   #4410 obs. of  24 variables
str(manager_survey_data)  #4410 obs. of  3 variables
str(in_time) #4410 obs. of  262 variables
str(out_time) #4410 obs. of  262 variables

#Renaming the X column name in the in_time and out_time dataframes to EmployeeID(primary key)
names(in_time)[1] <- "EmployeeID"
names(out_time)[1] <- "EmployeeID"

# Collate the data together in one single file
length(unique(emp_survey_data$EmployeeID)) #4410, confirming the EmployeeID is the key
length(unique(manager_survey_data$EmployeeID))  #4410, confirming the EmployeeID is the key
length(unique(general_data$EmployeeID)) #4410, confirming the EmployeeID is the key
length(unique(in_time$EmployeeID)) #4410, confirming the EmployeeID is the key
length(unique(out_time$EmployeeID)) #4410, confirming the EmployeeID is the key


setdiff(emp_survey_data$EmployeeID,general_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(emp_survey_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(emp_survey_data$EmployeeID,in_time$EmployeeID)  # Identical EmployeeID across these datasets
setdiff(emp_survey_data$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these datasets


employee <- merge(general_data,emp_survey_data,by="EmployeeID", all = F)
employee <- merge(employee,manager_survey_data,by="EmployeeID",all=F)
employee <- merge(employee,in_time,by="EmployeeID",all=F)
employee <- merge(employee,out_time,by="EmployeeID",all=F)


#we observe that the in_time and out_time have NA's in all rows, lets identify them and remove it
na_count <- sapply(employee,function(x) sum(is.na(x))) #get the count of NA's of all columns
col_names_NA <- which(na_count == nrow(employee)) #getting the column names which have all NA's
length(col_names_NA)  #There are 24 columns with all NA's , lets remove them

#Eliminating columns with all NA's from employee dataframe
employee <- employee[,-col_names_NA]


#Treating the date and time format:
#a) the date column starts from X2015.01.02.x and goes upto X2015.12.31.y for all employees
#b) getting the column number for the these ranges
which(names(employee) == "X2015.01.02.x") #30
which(names(employee) == "X2015.12.31.y") #527

#c) subsetting only the date/time columns to convert to date-time format
employee_date_time <- employee[,30:527]  #there are 498 columns
employee[,30:527] <- lapply(employee_date_time,function(x) ymd_hms(x))
type_of <- sapply(employee[,30:527],typeof) # Columns 30 to 527 are converted to double type
class_of<- sapply(employee[,30:527],class)  # Columns 30 to 527 are converted to "POSIXt POSIXct"


#Now that we have successfully treated the date and time, lets aggregate the day wise hours to monthly hours
#The in time and out time are synchronous, as in if there is an NA on a particualr row in in_time df, there is a corresponding NA in Out_time df
sum(is.na(employee[279:527]))  == sum(is.na(employee[30:278])) #TRUE

actuals <- employee[279:527]-employee[30:278] #Getting the number of actual working hours per day/month wise per employee
actuals <- data.frame(sapply(actuals,function(x) round(x,2)))#rounding to 2 decimal places

#Grouping the columns by months
library(dplyr)
#colname            #grouping                       #creating a dataframe by month
jan <- names(select(actuals, contains("X2015.01"))); jan_hours <- actuals[,jan] 
feb <- names(select(actuals, contains("X2015.02"))); feb_hours <- actuals[,feb]
mar <- names(select(actuals, contains("X2015.03"))); mar_hours <- actuals[,mar]
apr <- names(select(actuals, contains("X2015.04"))); apr_hours <- actuals[,apr]
may <- names(select(actuals, contains("X2015.05"))); may_hours <- actuals[,may]
jun <- names(select(actuals, contains("X2015.06"))); jun_hours <- actuals[,jun]
jul <- names(select(actuals, contains("X2015.07"))); jul_hours <- actuals[,jul]
aug <- names(select(actuals, contains("X2015.08"))); aug_hours <- actuals[,aug]
sep <- names(select(actuals, contains("X2015.09"))); sep_hours <- actuals[,sep]
oct <- names(select(actuals, contains("X2015.10"))); oct_hours <- actuals[,oct]
nov <- names(select(actuals, contains("X2015.11"))); nov_hours <- actuals[,nov]
dec <- names(select(actuals, contains("X2015.12"))); dec_hours <- actuals[,dec]


#Cumulating the number of actual working hours an employee has clocked by month
#first creating a list of all month wise dataframes
month_list <- list(jan_hours,feb_hours,mar_hours,apr_hours,may_hours,jun_hours,jul_hours,aug_hours,sep_hours,oct_hours,nov_hours,dec_hours)
actuals2 <- data.frame(sapply(month_list,function(x) rowSums(x,na.rm =T)))  #getting the cumulative working hours for each employee by month

#now that we have aggregated the actual working hours per day by month, lets merge it with main dataframe

employee1 <- employee
#lets remove columns 30 to 527 , as we have already aggregated the hours above
employee1 <- employee1[,-(30:527)]

#merging the actuals2 dataframe which has the cumulative working hours/month/employee
employee1 <- cbind(employee1,actuals2)

#rename the generic columns from X1 to X12 with sequence of months from Jan to dec
library(data.table)
setnames(employee1,old=names(employee1[,30:41]),new= paste(rep(month.abb,1),"_hours"))
