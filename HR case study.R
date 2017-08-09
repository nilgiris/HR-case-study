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
library(grid)
library(gridExtra)

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

#-------------------------------------------------------------------------------------------------------------------------------
#                             DATA PREPARATION AND DERIVED METRICS FOR IN TIME AND OUT TIME DATA
#--------------------------------------------------------------------------------------------------------------------------------

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

#Lets capture the sum of NA's in each of the months, these can be interpreted as the total number of leaves
number_of_leaves <- data.frame(no_of_leaves=rowSums(is.na(actuals)))

#now that we have aggregated the actual working hours per day by month, lets merge it with main dataframe

employee1 <- employee
#lets remove columns 30 to 527 , as we have already aggregated the hours above
employee1 <- employee1[,-(30:527)]

#merging the actuals2 dataframe which has the cumulative working hours/month/employee
employee1 <- cbind(employee1,actuals2)

#rename the generic columns from X1 to X12 with sequence of months from Jan to dec
library(data.table)
setnames(employee1,old=names(employee1[,30:41]),new= paste(rep(month.abb,1),"_hours"))

#Appending the number of leaves derived metric to employee 1 DF
employee1 <- cbind(employee1,number_of_leaves)



#-------------------------------------------------------------------------------------------------------------------------------------------
#                                   DATA PREPARATION AND EDA FOR THE ENTIRE DATASET (employee1 dataframe)
#-------------------------------------------------------------------------------------------------------------------------------------------

#1. The rows and columns are consistent in terms of numbers. There are no missing rows and columns, 
#   summary rows,misaligned rows,extra rows,missing column name
#--------------------------------------------------------------------------------------------------------------
#2. Dealing with missing values.
sum(is.na(employee1)) #there are now 111 NA's in the entire dataframe,lets check which columns contain the NA's

na_columns <- sapply(employee1,function(x) sum(is.na(x)))
na_columns #NumCompaniesWorked has 19 NA's,EnvironmentSatisfaction has 25 NA's,JobSatisfaction has 20 NA's, WorkLifeBalance has 38 NA's,TotalWorkingYears has 9 NA's

#lets examine the structure of above variables
#a) numberofcompaniesworked
summary(employee1$NumCompaniesWorked) #median is 2, lets replace the missing values with 2

#replacing missing values with 2
employee1[which(is.na(employee1$NumCompaniesWorked)),15] <- 2

#b.EnvironmentSatisfaction
#converting from numeric to categorical type
employee1$EnvironmentSatisfaction <- as.factor(employee1$EnvironmentSatisfaction)
summary(employee1$EnvironmentSatisfaction) #the most frequently occuring value is 3, lets impute the NA's with 3
employee1[which(is.na(as.numeric(employee1$EnvironmentSatisfaction))),25] <- 3
summary(employee1$EnvironmentSatisfaction) #there are no NA's

#c.JobSatisfaction
#converting from numeric to categorical type
employee1$JobSatisfaction <- as.factor(employee1$JobSatisfaction)
summary(employee1$JobSatisfaction) #the most frequently occuring value is 4, lets impute the NA's with 4
employee1[which(is.na(as.numeric(employee1$JobSatisfaction))),26] <- 4
summary(employee1$JobSatisfaction) 

#d.Worklifebalance
#converting from numeric to categorical type
employee1$WorkLifeBalance <- as.factor(employee1$WorkLifeBalance) 
summary(employee1$WorkLifeBalance) #the most frequently occuring value is 3, lets impute the NA's with 3
employee1[which(is.na(as.numeric(employee1$WorkLifeBalance))),27] <- 3
summary(employee1$JobSatisfaction) 

#e.TotalWorkingYears
summary(employee1$TotalWorkingYears) #median value is 10, lets impute the missing values with 10
employee1[which(is.na(employee1$TotalWorkingYears)),20] <- 10

#Now that we have imputed all the missing values, lets verify the same
sum(is.na(employee1)) #0 , implies no missing values
#------------------------------------------------------------------------------------------------------------------

#3. Dealing with blanks
employee_blanks <- sapply(employee1, function(x) length(which(x == ""))) # checking for blank "" values
employee_blanks                #There are no blank values

#-------------------------------------------------------------------------------------------------------------------

#4. Checking for duplicated rows.
sum(duplicated(employee1$EmployeeID))  #0,There are not duplicate ID's

#-------------------------------------------------------------------------------------------------------------------

#5. Standardising Numbers.

#a. All the observations have the same consistent units

#b Ensuring digits as factors are not numeric,if not converting to factor type variables

str(employee1)
employee1$Education <- as.factor(employee1$Education)
employee1$JobLevel <- as.factor(employee1$JobLevel)
employee1$StockOptionLevel <- as.factor(employee1$StockOptionLevel)
employee1$JobInvolvement <- as.factor(employee1$JobInvolvement)
employee1$PerformanceRating <- as.factor(employee1$PerformanceRating)

#c  Checking for outliers and dealing with the same for all continous variables

#getting all the columns which are numeric
#lets knock off the employee ID ,StandardHours,EmployeeCount as its of no significance to the analysis
employee1 <- employee1[,-c(1,9,18)]

employee_numeric <- sapply(employee1,function(x)is.numeric(x))
employee_numeric_df <- employee1[,employee_numeric]

outlier_df <- data.frame(sapply(employee_numeric_df,function(x) quantile(x,seq(0,1,0.01))))
outlier_df #by inspection of the outlier dataframe and the progression of numbers by quantiles, it appears there are no outliers and is within the data range

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# 6. Converting character type variables to factor type

str(employee1)
employee_char <- sapply(employee1,is.character)
employee_char_df <- employee1[,employee_char]
employee1[,employee_char] <- lapply(employee_char_df,function(x) as.factor(x))
summary(employee1) #All the character type variables are converted to factor type successfully

#Eliminating the column named "Over18"
employee1 <- employee1[,-which(names(employee1)=="Over18")]

#----------------------------------------------------------------------------------------------------------------------------------------
#                                 EXPLORATORY DATA ANALYSIS(UNIVARIATE,BIVARIATE,CORRELATION ANALYSIS,DERIVED METRICS)
#--------------------------------------------------------------------------------------------------------------------------


#1. Lets analyze the categorical features in a bar chart grid view as follows:



Businesstravel_plot <- ggplot(employee1, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()
department_plot <-  ggplot(employee1, aes(x=Department,fill=Attrition))+ geom_bar() 
educationfield_plot  <-   ggplot(employee1, aes(x=EducationField,fill=Attrition))+ geom_bar()
gender_plot <- ggplot(employee1, aes(x=Gender,fill=Attrition))+ geom_bar()
job_role_plot <- ggplot(employee1, aes(x=JobRole,fill=Attrition))+ geom_bar()
marital_status_plot <- ggplot(employee1, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()
grid.arrange(Businesstravel_plot,department_plot,educationfield_plot,gender_plot,ncol=2,top = "Fig 1a")        
grid.arrange(job_role_plot,marital_status_plot,ncol=1,top = "Fig 1b")        

eduPlot <- ggplot(employee1, aes(x=Education,fill=Attrition))+ geom_bar()
joblevelPlot <- ggplot(employee1, aes(x=JobLevel,fill=Attrition))+ geom_bar()
stockoptionLevelPlot <- ggplot(employee1, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()
EnvironmentSatisfactionplot <-ggplot(employee1, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()
grid.arrange(eduPlot,joblevelPlot,stockoptionLevelPlot,EnvironmentSatisfactionplot ,ncol=2,top = "Fig 1c")


job_satisfaction_Plot <- ggplot(employee1, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position="dodge")
WorkLifeBalance_plot <- ggplot(employee1, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position="dodge")
JobInvolvement_plot <- ggplot(employee1, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position="dodge")
PerformanceRating_plot <-ggplot(employee1, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(position="dodge")
grid.arrange(job_satisfaction_Plot,WorkLifeBalance_plot,JobInvolvement_plot,PerformanceRating_plot ,ncol=2,top = "Fig 1d")



#Reveals the following insights with respect to Attrition
#1. High for those who travelled rarely
#2. High for Research and Development Department
#3. High for educational fields as Life sciences and Medical
#4. High for gender male
#5. High for job roles as Research scientist, Technician and sales
#6. High for marital status single




#-----------------------------------------------------------------------------------------------------------------------------------

#2. Lets analyze the prominent continous variables using histogram and barcharts


agePlot <- ggplot(employee1,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
distPlot <- ggplot(employee1,aes(DistanceFromHome,fill=Attrition))+geom_bar()
monthly_income_Plot <- ggplot(employee1,aes(MonthlyIncome,fill=Attrition))+geom_density()
number_of_companies_plot <- ggplot(employee1,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()
grid.arrange(agePlot,distPlot,monthly_income_Plot,number_of_companies_plot,ncol=2,top = "Fig 2a")

#Observations:
#1.Age: Majority of employees leaving the organization are aged around 30 years
#2.Distance from home: Interestingly, employees who have left the company stay closer to office
#3.Monthly income: Attrition is high in employees of lower income groups
#4.Number of companies worked: Clearly people who have worked in one company before have higher attrition rate


salary_hike_Plot <- ggplot(employee1,aes(PercentSalaryHike,Attrition))+geom_point(size=4,alpha = 0.01)
total_working_Years_Plot <- ggplot(employee1,aes(TotalWorkingYears,fill = Attrition))+geom_bar()
Training_Times_Plot <- ggplot(employee1,aes(TrainingTimesLastYear,fill = Attrition))+geom_bar()
grid.arrange(salary_hike_Plot,total_working_Years_Plot,Training_Times_Plot,ncol=2,top = "Fig 2b")

#Observations:
#1.Percent Salary Hike: Employees with less than 15% hike have more chances to leave.
#2. Total Working Years: larger proportions of employees with 1 year of experiences quitting the company also in bracket of 1-10 Years.
#3.Traning Times Last Year: This indicates the no of trainings the employee has attended. People who have been trained 2-4 times is a problematic area

YearAtCom_Plot <- ggplot(employee1,aes(YearsAtCompany,fill = Attrition))+geom_bar()
YearsSinceProm_Plot <- ggplot(employee1,aes(YearsSinceLastPromotion,fill = Attrition))+geom_bar()
YearsCurrManP_Plot <- ggplot(employee1,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()
grid.arrange(YearAtCom_Plot,YearsSinceProm_Plot,YearsCurrManP_Plot,ncol=2,top = "Fig 2c")

#Observations:
# 1.Years at Company: Larger proportion of new comers are quitting the organization. Which sidelines the recruitment efforts of the organization.
# 2.Years Since Last Promotion: Larger proportion of people who have been promoted recently have quit the company.
# 3.Years With Current Manager: As expected a new Manager is a strong cause for quitting.

ggplot(employee1,aes(x="Number of leaves",y=employee1$no_of_leaves,fill = Attrition))+ geom_boxplot()
# Contrary to the belief, the median number of leaves by proportion of people who have left the company is lower than people who have not left

#Boxplots for actual working hours for each months
jan_plot <-ggplot(employee1,aes(x="Jan _hours",y=employee1$`Jan _hours`,fill = Attrition))+ geom_boxplot()
feb_plot <- ggplot(employee1,aes(x="Feb _hours",y=employee1$`Feb _hours`,fill = Attrition))+ geom_boxplot()
mar_plot <- ggplot(employee1,aes(x="Mar _hours",y=employee1$`Mar _hours`,fill = Attrition))+ geom_boxplot()
apr_plot <- ggplot(employee1,aes(x="Apr _hours",y=employee1$`Apr _hours`,fill = Attrition))+ geom_boxplot()
may_plot <- ggplot(employee1,aes(x="May _hours",y=employee1$`May _hours`,fill = Attrition))+ geom_boxplot()
jun_plot <- ggplot(employee1,aes(x="Jun _hours",y=employee1$`Jun _hours`,fill = Attrition))+ geom_boxplot()
jul_plot <- ggplot(employee1,aes(x="Jul _hours",y=employee1$`Jul _hours`,fill = Attrition))+ geom_boxplot()
aug_plot <- ggplot(employee1,aes(x="Aug _hours",y=employee1$`Aug _hours`,fill = Attrition))+ geom_boxplot()
sep_plot <- ggplot(employee1,aes(x="Sep _hours",y=employee1$`Sep _hours`,fill = Attrition))+ geom_boxplot()
oct_plot <- ggplot(employee1,aes(x="Oct _hours",y=employee1$`Oct _hours`,fill = Attrition))+ geom_boxplot()
nov_plot <- ggplot(employee1,aes(x="Nov _hours",y=employee1$`Nov _hours`,fill = Attrition))+ geom_boxplot()
dec_plot <- ggplot(employee1,aes(x="Dec _hours",y=employee1$`Dec _hours`,fill = Attrition))+ geom_boxplot()
grid.arrange(jan_plot,feb_plot,mar_plot,apr_plot,may_plot,jun_plot,jul_plot,aug_plot,sep_plot,
             oct_plot,nov_plot,dec_plot,ncol=3,top = "Fig 2cd")

#Observation
#While the individual months doesnt give any insight, one thing thats evident is people who
#left the company have a higher median working hours per month than those who have not

#--------------------------------------------------------------------------------------------------------------------------------
#3. Lets do a correlation analysis among various numeric variables

employee_numeric <- sapply(employee1,function(x)is.numeric(x))
employee_numeric_df <- employee1[,employee_numeric]
employee_numeric_df <- employee_numeric_df[,-c(11:22)] #Excluding the actual hrs across months

employee_corr <- cor(employee_numeric_df) #storing correlation matrix in employee_corr variable

#Applying a proper theme for the corrplot to get a an easily readable correlation matrix
library(corrplot)
corrplot(employee_corr, order="AOE", method= "square", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         p.mat = 1-abs(employee_corr), sig.level=0.50, insig = "blank") 

#Observation from correlation analysis
#1.Age is correlated to Total working years ( 68% correlation)
#2. Total working years is correlated to years at company (61% correlation)
#3. Yearsatcompany is correlated to yearssincelastpromotion (62% correlation)
#4. Yearsatcompany is correlated to yearswithcurrentmanager (77% correlation)
#5. Yearssincelastpromotion is correlated to yearswithcurrentmanager (51% correlation)


#------------------------------------------------------------------------------------------------------------------------------------
#4. Derived metrics : lets derive few metrics based on our understanding of problem statement and dataset

#Lets derive the following metrics :
# a. Tenure per job: Employeed who have worked with many companies for smaller duration tend to leave 
#company in search of others.
#b. Years without change : This variable will be created to analyse the stagnation period of an employee,as in how long is the employee in same role/grade without change
#c. Compa Ratio: Compa Ratio is the ratio of the actual pay of an Employee to the midpoint of a salary range. The salary range can be that of his/her department or organization or role. The benchmark numbers can be a organization’s pay or Industry average.
#Here, we use the Comapny pay information to calculate our Compa Ratio at Department Level & Organiation Level.

#Calculating tenure per job
employee1$tenureperjob <- round(ifelse(employee1$NumCompaniesWorked!=0, employee1$TotalWorkingYears/employee1$NumCompaniesWorked,0),1)

#Calculating years without change
employee1$yrswithoutchange <- employee1$TotalWorkingYears - employee1$YearsSinceLastPromotion

#Lets plot the above 2 variables to see the impact on attrition
tenure_Plot <- ggplot(employee1,aes(tenureperjob))+geom_density()+facet_grid(~Attrition)
change_Plot <- ggplot(employee1,aes(yrswithoutchange))+geom_density()+facet_grid(~Attrition)
grid.arrange(tenure_Plot,change_Plot,ncol=2,top = "Derived metrics 1a")

#c.Calculating Compa ratio
#Aggregating by department and getting median salary by department
dept_median_salary <- data.frame(group_by(employee1,Department) %>% summarise(median_Sal=median(MonthlyIncome)))
hr_med_salary <- dept_median_salary[which(dept_median_salary$Department=="Human Resources"),2]
research_med_salary <- dept_median_salary[which(dept_median_salary$Department=="Research & Development"),2]
sales_med_salary <- dept_median_salary[which(dept_median_salary$Department=="Sales"),2]


