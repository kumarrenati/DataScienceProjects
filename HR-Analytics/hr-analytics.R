# ************************************************************************
# HR Analytics case study: 
# Logistic regression and evaluation of the model
# ************************************************************************
# Group Members:
# 1. Kumar Renati
# *********************************************************************
# Business understanding
# *****************************************************************
# A company XYZ, at a given point of time has 4000 employees.
# Every year around 15% leave the company. This level of
# attrition is very bad for the company.
# 
# Goal:
# To model the probability of attrition using logistic
# regression.  It will be used to understand the factors the
# company should focus on, in order to curb attrition. Also,
# which of these variables are most important and needs to be
# addressed right away.
#
# **************** Load the required libraries ***************
# clear the working environment
rm(list = ls())

pkg_check <- function(pkg) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
  
}
# Installing the packages if not installed
packages <- c("data.table", "ggplot2", "ggthemes", "tidyverse", "rvest",
              "lubridate", "dplyr", "MASS", "RColorBrewer", "stats", 
              "reshape2", "gains", "car", "ROCR", "e1071", "GGally", 
              "Information", "devtools", "rstudioapi", "ggcorrplot")
pkg_catalog <- pkg_check(packages)

install.packages('caret', dependencies = c('Depends', 'Suggests'))
library(caret)

# ********************************************************
# Set current working directory to the current location
# ********************************************************
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# *********************************************************

# Loading Input files
general_data <- fread(input = "general_data.csv", stringsAsFactors = TRUE, 
                      data.table = FALSE)
employee_survey_data <- fread(input = "employee_survey_data.csv", 
                              stringsAsFactors = TRUE, data.table = FALSE)
manager_survey_data <- fread(input = "manager_survey_data.csv", 
                             stringsAsFactors = TRUE, data.table = FALSE)

# Warning: Don't change fread function, if you change it then you 
# should consider modifying the code in the Data preparation section
# while renaming the column 1 of "in_time" and "out_time" dataframes
# By using fread the 1st column name was defaulted to "V1"
# In case of read.csv() the 1st column name would be empty
# check.names : default is FALSE. If TRUE then the names of the variables
# in the data.table are checked to ensure that they are
# syntactically valid variable names
# To avoid variables like X2015.01.01 instead to keep the date as is
in_time_initial <- fread(input = "in_time.csv", stringsAsFactors = FALSE, 
                         check.names = FALSE, data.table = FALSE)
out_time_initial <- fread(input = "out_time.csv", stringsAsFactors = FALSE, 
                          check.names = FALSE, data.table = FALSE)


#Retaining initial versions
in_time  <- in_time_initial
out_time <- out_time_initial

# viewing the dataframe
head(general_data)
str(general_data) # 4410 obs of 24 variables with the target variable

head(employee_survey_data)
str(employee_survey_data) # 4410 obs of 4 variables

head(manager_survey_data)
str(manager_survey_data) # 4410 obs of 3 variables

head(in_time)
str(in_time)  # has time in character data type AND bunch of NAs

head(out_time)
str(out_time)  # has time in character data type AND bunch of NAs

# ********************************************************
# DATA UNDERSTANDING - Data Preparation
# ********************************************************

# Assumption: renaming the V1 variable of in_time, out_time with EmployeedID 
colnames(in_time)[which(colnames(in_time) == "V1")] <- "EmployeeID"
colnames(out_time)[which(colnames(out_time) == "V1")] <- "EmployeeID"

# Check for duplicates in EmployeeID Variable
# This confirms EmployeeID is the key
sapply(list(general_data, employee_survey_data, manager_survey_data,
            in_time, out_time), function(x) sum(duplicated(x$EmployeeID)))

# Inferences,:
# There are no duplicates
# Thus the assumption that EmployeeID holds true for in_time and out_time dataframes
# EmployeeID has to be converted to factors: 
# This is done after merging all 5 dataframes

# ********************************************************************
# in_time & out_time dataframe analysis
# ********************************************************************

# To find the regularity of an employee
# (Assumption: NA's in individual observations are 
# considered as vacations or leaves)
# Leaves/Vacation taken by employee
# 
# Assumption: 
# 1. If both in_time and out_time columns with complete NA's matches 
#    then they are considered as 'public holidays'

# Check if the column names of the data.frames are identifical.
sum(colnames(in_time) != colnames(out_time))

# Inference: All column names are identical in_time and out_time dataframe.

# *******************************************************************
# Creation of in_out_hr data.frame using in_time, out_time data.frame
# *******************************************************************

# Empty matrix for in_out_hr
in_out_hr <- as.data.frame(matrix(data = 0,nrow = nrow(in_time), ncol = ncol(in_time)-1))
colnames(in_out_hr) <- c(colnames(in_time[,-which(colnames(in_time) == "EmployeeID")]))

# Also remove EmployeeID from in_out_hr dataframe
# in_out_hr matrix: stores the calculated difference of the work timings
# work timings = out_time[,i] - in_time[,i]
for (i in 1:(ncol(in_time)-1)) {
  x <- in_time[,-which(colnames(in_time)=="EmployeeID")]
  y <- out_time[,-which(colnames(out_time)=="EmployeeID")]
  time1 <- ymd_hms(x[,i])
  time2 <- ymd_hms(y[,i])
  in_out_hr[,i] <- interval(time1, time2) %>% as.period() %>% 
                    as.numeric("hours") %>% round(2)
}

# replacing the NAs with 0s
in_out_hr[is.na(in_out_hr)] <- 0

head(in_out_hr)

# Assumption: Columns with only zeroes are public holidays

all_zero_col <- which(colSums(in_out_hr == 0) == nrow(in_out_hr))
# colnames which are all zeroes
colnames_all_zero_col <- colnames(in_out_hr[all_zero_col])
colnames_all_zero_col
length(colnames_all_zero_col)
# Inference: There are 12 public holidays
# Removing 12 columns which are public holidays
in_out_hr_no_holiday <- in_out_hr[,-which(names(in_out_hr) %in% colnames_all_zero_col)]

# *************************************************
# Derived metrics(DM) of in_out_hr_no_holiday dataframe
# *************************************************
# DM-1: Vacation of each employee
emp_vacation <- rowSums(in_out_hr_no_holiday == 0)
general_data$vacations <- emp_vacation

# DM-2: Mean working hour without considering the vacations, 
# public holidays and weekends
# Mean is taken over all the columns. Therefore ncol(in_out_hr_no_holiday) is used
general_data$mean_attendance <- apply(in_out_hr_no_holiday, 1, 
                                      function(x) mean(x[1:ncol(in_out_hr_no_holiday)][x > 0],
                                                       na.rm = TRUE) %>% round(2))

# DM-3: Work regularity check
# Assumption: Employee is considered irregular if he/she works for 
# less than 7 hrs on a working day excluding vacation, public holidays and weekends
# Those who are not in this range are regular
num_regular_days_count <- rowSums((in_out_hr_no_holiday > 0) & (in_out_hr_no_holiday < 7))
regularity_df <- data.frame(num_regular_days_count)

# Histogram: Plot regularity distribution of the data
cbind(regularity_df, Attrition = general_data$Attrition) %>% 
  filter(num_regular_days_count > 0) %>%  
  ggplot(aes(num_regular_days_count,fill = Attrition)) + 
  geom_histogram(binwidth = 2) +
  labs(title = "Regularity to Work of Employees in 1 Year",
       x = "Daily Work distribution in 1 Year")+
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8,0.8))

dir.create("plots")
ggsave("plots/Regularity to Work of Employees in 1 Year.png", 
       width = 125, height = 100, units = "mm")


# Studying the distribution of data in regularity_df dataframe
table(regularity_df)
quantile(regularity_df$num_regular_days_count,seq(0,1,0.25))

# Work Regularity Classification: Classifying the employees into 3 levels 
# 0: "regular", 
# 1: "irregular", 
# 2: "chronic_irregular"

# Inference: 
# Total days in a year 261
# Working days excluding public holidays = 261 - 12 = 249  
# 50% of employees are irregular for less than 20 days out of 249 working days, 
# 75 % percentile of data is 205, 
# 100% percentile is 245

class(num_regular_days_count) # It is numeric need not convert
general_data$work_regularity <- regularity_df$num_regular_days_count %>% 
                                  cut(breaks = c(0,20,205,245),
                                      labels = c(0,1,2),
                                      include.lowest = T)


# DM-4: Work Load Check
# Assumption: Employee is considered overloaded if he/she works for 
# more than 9 hrs intermittently, on-off continuously, and consistently
# on a working day excluding vacation, public holidays and weekends

num_workload_days_count <- rowSums((in_out_hr_no_holiday > 9))
workload_df <- data.frame(num_workload_days_count)

# Histogram: Plotting the workload
cbind(workload_df,Attrition = general_data$Attrition) %>% 
  filter(num_workload_days_count > 0) %>% 
  ggplot(aes(num_workload_days_count,fill = Attrition)) + 
  geom_histogram(binwidth = 2) + 
  labs(title = "Workload Distribution of Employees in 1 Year",
       x = "Daily Work distribution in 1 Year") +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8,0.8))

ggsave("plots/Workload Distribution of Employees in 1 Year.png", 
       width = 125, height = 100, units = "mm")


# Studying the distribution of data in workload_df dataframe
table(workload_df)
quantile(workload_df$num_workload_days_count ,seq(0,1,0.25))
quantile(workload_df$num_workload_days_count, 0.78)

# Inference:
# 50 Percetile have 0 days overworked
# 75 percentile have 3 days overworked
# 78 percentile have 24 days overworked
# Rest fall under 100% overworked.

# Work Load Classification: Employees with heavy work load working overtime
# Classification of Overtime: Based on aggregated number of overtime incidents
# 0: "Normal" # Count: 0-3
# 1: "Heavy"  # Count: 3-24 (Approx: A month overload in a year excluding weekends)
# 2: "Severe" # Count: 24-248

class(num_workload_days_count) # It is numeric need not convert
general_data$workLoad <- workload_df$num_workload_days_count %>% 
                          cut( breaks = c(0,3,24,248), 
                               labels = c(0,1,2),
                               include.lowest = T)

# **********************************************************************
# MERGING OF DATAFRAMES: 
# general_data, manager_survey_data, employee_survey_data, 
# derived metrics from in_time, out_time
# **********************************************************************

# checking if the EmployeeIDs in the general_data, manager_survey_data, 
# employee_survey_data is unique

# All are identical no difference in employee id
setdiff(manager_survey_data$EmployeeID, employee_survey_data$EmployeeID)        
# All are identical no difference in employee id
setdiff(general_data$EmployeeID, employee_survey_data$EmployeeID)
# All are identical no difference in employee id
setdiff(in_time$EmployeeID, employee_survey_data$EmployeeID) 

# Merge-1: employee survey data and manager_survey_data
employee_survey_data <- merge(x = employee_survey_data,
                              y = manager_survey_data,
                              by = intersect(colnames(employee_survey_data),
                                             colnames(manager_survey_data)))

# Removing fields which are not useful for analysis
# EmployeeCount, Over18, StandardHours 
general_data %>% sapply(function(x) length(unique(x))==1) %>% which() %>% data.frame()
general_data_dummy_cols <- general_data %>% sapply(function(x) length(unique(x))==1) %>% 
                            which() %>% as.vector()
general_data <- general_data[,-general_data_dummy_cols]

# Merge-2: general_data and employee_survey_data dataframes
emp_master <-  merge(x = general_data,
                     y = employee_survey_data,
                     by = intersect(colnames(general_data),
                                    colnames(employee_survey_data)))

# *************************************
# Data quality check
# *************************************

# Total Working years less than Year at Company
sum(emp_master$TotalWorkingYears < emp_master$YearsAtCompany,na.rm = T)
# Total Working years less than YearsWith CurrManager
sum(emp_master$TotalWorkingYears < emp_master$YearsWithCurrManager,na.rm = T)
# Years At Company Less than Years with Current Manager
sum(emp_master$YearsAtCompany < emp_master$YearsWithCurrManager,na.rm = T )
# Monthly Income is Zero
sum(emp_master$MonthlyIncome == 0)
# Total Years At Company is less than YearsSince Last Promotion
sum(emp_master$YearsAtCompany < emp_master$YearsSinceLastPromotion,na.rm = T )

# **********************************************
# Imputation
# **********************************************
#Checking for individual fields
sapply(emp_master, function(x) length(which(is.na(x)))) 

#No blank spaces found
sapply(emp_master, function(x) length(which(x == ""))) 

#As there are around 19 NA's in NumCompaniesWorked  and 9 in TotalWorkingYears
# Imputation: NumCompaniesWorked:
#1. NA - can be replaced with 2 if TotalWorkingYears and YearsAtCompany is not same 
#   and difference between them is just one years or else 
#2. NA's are replaced with 1 if difference between TotalWorkingYears and 
#   YearsAtCompany is 0 or else NA's are left as is 
#3. zero's are replaced with 2 if difference between TotalWorkingYears and 
#   YearsAtCompany is 1 assuming that the employee wouldn't be possibly 
#   changing the company more than once in an year as most of the companies have
#   probationary period of 1 year with contract or else


sum(is.na(emp_master$NumCompaniesWorked)) #Returns 19
NumCompaniesWorked_is_na <- emp_master$NumCompaniesWorked %>% is.na() %>% which()

emp_master$diff_wrking_yrs <- emp_master$TotalWorkingYears - emp_master$YearsAtCompany
emp_master$NumCompaniesWorked [NumCompaniesWorked_is_na] <- 
  ifelse(emp_master$diff_wrking_yrs [NumCompaniesWorked_is_na] == 1, 2, 
         ifelse(emp_master$diff_wrking_yrs [NumCompaniesWorked_is_na] == 0, 1, NA))

sum(is.na(emp_master$NumCompaniesWorked)) #Returns 9

# Returns 586
length(emp_master$NumCompaniesWorked [(which(emp_master$NumCompaniesWorked == 0))])
emp_master$NumCompaniesWorked [(which(emp_master$NumCompaniesWorked == 0))] <- 
  ifelse(emp_master$diff_wrking_yrs [(which((emp_master$NumCompaniesWorked == 0)))] == 1, 
         2, 0)

#Removing difference field of total working years and years at company
emp_master <- emp_master[ , -which(colnames(emp_master) == "diff_wrking_yrs")]

# Imputation: TotalWorkingYears
#1. NA - replace it with YearsAtCompany if NumCompaniesWorked is 0 or 1 

sum(is.na(emp_master$TotalWorkingYears))  #Returns 9
TotalWorkingYears_is_na <- emp_master$TotalWorkingYears %>% is.na() %>% which()

emp_master$TotalWorkingYears [TotalWorkingYears_is_na] <- 
  ifelse(emp_master$NumCompaniesWorked [TotalWorkingYears_is_na] == 0, 
         emp_master$YearsAtCompany [TotalWorkingYears_is_na],
         ifelse(emp_master$NumCompaniesWorked [TotalWorkingYears_is_na] == 1,
                emp_master$YearsAtCompany [TotalWorkingYears_is_na],NA))

sum(is.na(emp_master$TotalWorkingYears))  #Returns 7

# **************************************************
# Identifying and Deleting the rows that have NAs
# **************************************************
# After the imputation
# NumCompaniesWorked, TotalWorkingYears, EnvironmentSatisfaction, Jobsatisfaction, 
# WorklifeBallance of an employee is subjective. 
# It doesn't make sense to replace the NA's with median or mean of the respective variable. 
# Thus deleting the records of the employees that do not have enough data is 
# one among the better solutions. 

#we have total 101 Missing values.
sum(is.na(emp_master))
# Lets check the number of records which has missing values.
nrow(emp_master[!complete.cases(emp_master),])
# We have 98 rows with NA values, so it is about 100/4410 = 2.27 %, 
# so we can omit the records.
emp_master_clean <- na.omit(emp_master)

# calculating the data loss as a result of missing
paste(100 - (round(nrow(emp_master_clean)*100/nrow(emp_master),2)),"%")
# Inference: Thus approximately 2.22% of data is lost as a result of removing the "NAs"

str(emp_master_clean)
# Converting categorical variables to factors
colnames(emp_master_clean)
int_to_factors <- c("Attrition","BusinessTravel","Department",
                    "Education","EducationField","Gender","JobLevel","JobRole",
                    "MaritalStatus", "StockOptionLevel","JobInvolvement",
                    "PerformanceRating","EnvironmentSatisfaction","JobSatisfaction",
                    "WorkLifeBalance","work_regularity","workLoad","NumCompaniesWorked",
                    "TrainingTimesLastYear")


# converting integer columns to factors
emp_master_clean[int_to_factors] <- lapply(emp_master_clean[int_to_factors], factor)

# Checking unique factors in the dataframe
sapply(emp_master_clean, unique)
# Inference: 
# 1. There are no duplicates in the categories
# 2. There are no spelling mistakes

# duplicate the emp_master_clean
emp_master_duplicate <- emp_master_clean

# ****************************************
# Exploratory Data Analysis
# ****************************************

# ******************
# OUTLIER TREATMENT
# ******************

str(emp_master_clean)
# separating integer and numeric variables for outlier treatment
int_num_variables <- sapply(emp_master_clean, is.numeric) | sapply(emp_master_clean, is.integer)

# Visualizing the outliers of continuous variables using boxplots
melt(data = emp_master_clean[int_num_variables],id.vars = "EmployeeID") %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")

# Check for outliers in integer and numeric variables:
sapply(emp_master_clean[int_num_variables], function (x) {boxplot.stats(x)$out } )
# Inference: These outliers are not errors in measurements. 
# They are valid values. So no outlier treatment is done.

# Correlation Plot against all Numerical features.
ggcorrplot(cor(emp_master_clean[int_num_variables], use = "complete.obs"),
           hc.order = TRUE, type = "lower", 
           lab = TRUE, lab_size = 3, method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Plotting correlations between variables", ggtheme=theme_bw)
ggsave("plots/Plotting correlations between variables.png",
       width = 250, height = 200, units = "mm")

write.csv(emp_master_clean,"emp_master_clean.csv")

# *************************************************************
# Univariate and several plots has to be done
# *************************************************************

# ************************
# 1. Continuos variables 
# ************************

emp_master_clean %>%
  subset(select = c("DistanceFromHome","Age","Attrition","vacations", 
                    "mean_attendance","YearsSinceLastPromotion", 
                    "YearsWithCurrManager")) %>%
  melt(id.vars = "Attrition") %>%
  ggplot(aes(x = value, fill = Attrition)) +
  geom_histogram(binwidth = 2, aes(y = ..density..)) +
  geom_density(aes(alpha = .001,col = Attrition)) + 
  scale_color_brewer(palette = "Set1",direction = 1) + 
  scale_fill_manual(values = c("grey69", "#006600")) + 
  scale_alpha_identity(guide = "none") + 
  facet_wrap(facets = ~variable,scales = "free",ncol = 2) +
  labs(title = "Percentage Attrition of Continuos variables",
       y = "Density distribution") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(linetype = "dotted",fill = NA)) 

ggsave("plots/Percentage Attrition of Continuos variables.png",
       width = 125, height = 100, units = "mm")

# ********************************************************************
# 2. Nominal variables: Categorical variables without any natural variable
# ********************************************************************

# Subetting the nominal categories
emp_master_clean %>% 
  subset(select = c(c("Attrition","BusinessTravel", "Department","JobRole", 
                      "MaritalStatus","EducationField", "Gender"))) %>%
  lapply(as.character) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  melt(id.vars = "Attrition") %>%
  group_by(variable,value,Attrition) %>%
  summarise(value_count = n()) %>%
  mutate(percent = paste0(round(value_count*100/sum(value_count)),"%")) %>%
  ggplot(aes(x = factor(reorder(value,-value_count)),
             y = value_count, fill = Attrition)) +
  geom_bar(position = "fill",stat = "identity") + 
  geom_text(aes(label = percent),position = position_fill(vjust = 0.5),size = 3) +
  coord_flip() +
  facet_wrap(facets = ~variable,scales = "free",ncol = 3) +
  labs(title = "Percentage Attrition by each Nominal category") +
  theme(axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "grey80"),
    panel.border = element_rect(linetype = "dotted",fill = NA)) + 
  scale_fill_manual(values=c("#FFFFCC", "#E69F00"))
  
ggsave("plots/Percentage Attrition by each Nominal category.png",
       width = 250, height = 200, units = "mm")

# ********************************************************************
# 3. Ordinal variables: Categorical variables with an order
# ********************************************************************
emp_master_clean %>%
  subset(select = c("NumCompaniesWorked","Education","StockOptionLevel",
             "work_regularity","workLoad","JobLevel", "Attrition")) %>%
  lapply(as.character) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  melt(id.vars = "Attrition") %>%
  group_by(variable,value,Attrition) %>%
  summarise(value_count = n()) %>%
  mutate(percent = paste0(round(value_count*100/sum(value_count)),"%")) %>%
  ggplot(aes(x = factor(reorder(value,-value_count)),
             y = value_count, fill = Attrition)) +
  geom_bar(position = "fill",stat = "identity") + 
  geom_text(aes(label = percent),position = position_fill(vjust = 0.5),size = 3) +
  coord_flip() +
  facet_wrap(facets = ~variable,scales = "free",ncol = 3) +
  labs(title = "Percentage Attrition by each Ordinal category 1") +
  theme(axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(linetype = "dotted",fill = NA)) + 
  scale_fill_manual(values=c("#FFFFCC", "#E69F00"))

ggsave("plots/Percentage Attrition by each Ordinal category1.png",
       width = 125, height = 100, units = "mm")

emp_master_clean %>%
  subset(select = c("JobSatisfaction","WorkLifeBalance","EnvironmentSatisfaction",
                    "TrainingTimesLastYear", "JobInvolvement","PerformanceRating",
                    "Attrition")) %>%
  lapply(as.character) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  melt(id.vars = "Attrition") %>%
  group_by(variable,value,Attrition) %>%
  summarise(value_count = n()) %>%
  mutate(percent = paste0(round(value_count*100/sum(value_count)),"%")) %>%
  ggplot(aes(x = factor(reorder(value,-value_count)),
             y = value_count, fill = Attrition)) +
  geom_bar(position = "fill",stat = "identity") + 
  geom_text(aes(label = percent),position = position_fill(vjust = 0.5),size = 3) +
  coord_flip() +
  facet_wrap(facets = ~variable,scales = "free",ncol = 3) +
  labs(title = "Percentage Attrition by each Ordinal category 2") +
  theme(axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(linetype = "dotted",fill = NA)) + 
  scale_fill_manual(values=c("#FFFFCC", "#E69F00"))

ggsave("plots/Percentage Attrition by each Ordinal category2.png",
       width = 125, height = 100, units = "mm")

# *******************************************
# **************Model Building***************
# *******************************************

# Remove EmployeeID column from emp_master_clean
emp_model_prep <- emp_master_clean[,-which(colnames(emp_master_clean)=="EmployeeID")]

# Lets scale all the integer and numeric varaibles, i.e, continous variables.
non_fact_variables <- sapply(emp_model_prep, is.numeric) | sapply(emp_model_prep, is.integer)
factor_variables <- sapply(emp_model_prep, is.factor)

emp_model_prep[non_fact_variables] <- lapply(emp_model_prep[non_fact_variables], scale)


# creating dummy variables for factor variables
dummy_variables <- dummyVars(formula = ~ .,data = emp_model_prep,sep = ".", 
                             levelsOnly = FALSE, fullRank = TRUE)
HR_Analytics_Model <- data.frame(predict(dummy_variables, emp_model_prep))

# *********************************************
# SAMPLING OF datasets - test and train data set
# *********************************************
set.seed(50)

# index of randowm numbers
index <- sample(x = 1:nrow(HR_Analytics_Model),size = 0.7 * nrow(HR_Analytics_Model))

# Test and training Datasets
train <- HR_Analytics_Model[index,]
test <- HR_Analytics_Model[-index,]

# *********************************************
# Model creation
# *********************************************
# Creating the initial model
model_01 <- glm(formula = Attrition.Yes ~ .,family = "binomial",data = train)
# Stepwise selection
model_02 <- stepAIC(model_01, direction="both")
 
summary(model_02)
# Removing multicollinearity through vif check

sort(vif(model_02))

model_03 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences + JobLevel.2 + 
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Married + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + StockOptionLevel.1 + StockOptionLevel.3 + 
                  TotalWorkingYears + TrainingTimesLastYear.1 + TrainingTimesLastYear.6 + 
                  YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
                  work_regularity.1 + work_regularity.2 + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 + JobRole.Laboratory.Technician, family = "binomial", 
                data = train)
summary(model_03)

# Removing multicollinearity through VIF check
sort(vif(model_03))

# Removing mean.attendence as it has VIF > 5 and p-value > 0.05
model_04 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences + JobLevel.2 + 
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Married + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + StockOptionLevel.1 + StockOptionLevel.3 + 
                  TotalWorkingYears + TrainingTimesLastYear.1 + TrainingTimesLastYear.6 + 
                  YearsSinceLastPromotion + YearsWithCurrManager +  
                  work_regularity.1 + work_regularity.2 + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 + JobRole.Laboratory.Technician, family = "binomial", 
                data = train)
summary(model_04)

# Removing multicollinearity through VIF check
sort(vif(model_04))

# Removing TrainingTimesLastYear.1 due to its insignficance p-vale > 0.05
model_05 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences + JobLevel.2 + 
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Married + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + StockOptionLevel.1 + StockOptionLevel.3 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 + work_regularity.2 + 
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 + JobRole.Laboratory.Technician, family = "binomial", 
                data = train)
summary(model_05)

# Removing multicollinearity through VIF check
sort(vif(model_05))

# Removing work_regularity.2 p-value = 0.089 > 0.05 i.e., Insignificant
model_06 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences + JobLevel.2 + 
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Married + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + StockOptionLevel.1 + StockOptionLevel.3 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 + JobRole.Laboratory.Technician, family = "binomial", 
                data = train)
summary(model_06)

# Removing multicollinearity through VIF check
sort(vif(model_06))

# Removing JobLevel.2 p-value = 0.094 > 0.05 i.e., Insignificant
model_07 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences +  
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Married + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + StockOptionLevel.1 + StockOptionLevel.3 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 + JobRole.Laboratory.Technician, family = "binomial", 
                data = train)
summary(model_07)

# Removing multicollinearity through VIF check
sort(vif(model_07))

# Removing JobRole.Laboratory.Technician p-value = 0.085 > 0.05 i.e., Insignificant
model_08 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences +  
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Married + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + StockOptionLevel.1 + StockOptionLevel.3 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 , family = "binomial", 
                data = train)
summary(model_08)

# Removing multicollinearity through VIF check
sort(vif(model_08))

# Removing MaritalStatus.Married p-value = 0.078 > 0.05 i.e., Insignificant
model_09 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences +  
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive +  
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + StockOptionLevel.1 + StockOptionLevel.3 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 , family = "binomial", 
                data = train)
summary(model_09)

# Removing multicollinearity through VIF check
sort(vif(model_09))

# Removing StockOptionLevel.1 p-value = 0.063 > 0.05 i.e., Insignificant
model_10 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences +  
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive +  
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + StockOptionLevel.3 + TotalWorkingYears + 
                  TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 , family = "binomial", 
                data = train)
summary(model_10)

# Removing multicollinearity through VIF check
sort(vif(model_10))

# Removing StockOptionLevel.3 p-value = 0.077 > 0.05 i.e., Insignificant
model_11 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences +  
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive +  
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 + 
                  JobInvolvement.3 , family = "binomial", 
                data = train)
summary(model_11)

# Removing multicollinearity through VIF check
sort(vif(model_11))

 # Removing JobInvolvement.3 p-value = 0.047 i.e., Less significant
model_12 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + EducationField.Life.Sciences +  
                  JobLevel.5 + JobRole.Manufacturing.Director + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive +  
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 , 
                  family = "binomial", 
                data = train)
summary(model_12)

# Removing multicollinearity through VIF check
sort(vif(model_12))

# Removing EducationField.Life.Sciences p-value = 0.026 i.e., Less significant
model_13 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + JobLevel.5 + JobRole.Manufacturing.Director + 
                  JobRole.Research.Director + JobRole.Research.Scientist + 
                  JobRole.Sales.Executive + MaritalStatus.Single + 
                  NumCompaniesWorked.2 + NumCompaniesWorked.3 + NumCompaniesWorked.5 + 
                  NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 , 
                family = "binomial", 
                data = train)
summary(model_13)

# Removing multicollinearity through VIF check
sort(vif(model_13))

# Removing PercentSalaryHike p-value = 0.016 i.e., Less significant
model_14 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + JobLevel.5 + JobRole.Manufacturing.Director + 
                  JobRole.Research.Director + JobRole.Research.Scientist + 
                  JobRole.Sales.Executive + MaritalStatus.Single + 
                  NumCompaniesWorked.2 + NumCompaniesWorked.3 + NumCompaniesWorked.5 + 
                  NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 , 
                family = "binomial", 
                data = train)
summary(model_14)

# Removing multicollinearity through VIF check
sort(vif(model_14))

# Removing JobRole.Manufacturing.Director p-value = 0.013 i.e., Less significant
model_15 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + JobLevel.5 + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.5 + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 , 
                family = "binomial", 
                data = train)
summary(model_15)

# Removing multicollinearity through VIF check
sort(vif(model_15))

# Removing NumCompaniesWorked.5 p-value = 0.011 i.e., Less significant
model_16 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + JobLevel.5 + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 , 
                family = "binomial", 
                data = train)
summary(model_16)

# Removing multicollinearity through VIF check
sort(vif(model_16))

# Removing JobLevel.5 p-value = 0.006 i.e., Less significant
model_17 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager +  work_regularity.1 +  
                  workLoad.1 + workLoad.2 + EnvironmentSatisfaction.2 + 
                  EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
                  JobSatisfaction.2 + JobSatisfaction.3 + JobSatisfaction.4 + 
                  WorkLifeBalance.2 + WorkLifeBalance.3 + WorkLifeBalance.4 , 
                family = "binomial", 
                data = train)
summary(model_17)

# Removing multicollinearity through VIF check
sort(vif(model_17))

# Removing work_regularity.1 p-value = 0.007 i.e., Less significant
model_18 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  BusinessTravel.Travel_Rarely + Department.Research...Development + 
                  Department.Sales + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
                  EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
                  JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
                  WorkLifeBalance.4 , family = "binomial", 
                data = train)
summary(model_18)

# Removing multicollinearity through VIF check
sort(vif(model_18))

# Removing BusinessTravel.Travel_Rarely p-value = 0.001 i.e., Less significant
model_19 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  Department.Research...Development + 
                  Department.Sales + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.2 + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
                  EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
                  JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
                  WorkLifeBalance.4 , family = "binomial", 
                data = train)
summary(model_19)

# Removing multicollinearity through VIF check
sort(vif(model_19))

# Removing NumCompaniesWorked.2 p-value = 0.001 i.e., Less significant
model_20 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  Department.Research...Development + 
                  Department.Sales + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.3 + 
                  NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
                  EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
                  JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
                  WorkLifeBalance.4 , family = "binomial", 
                data = train)
summary(model_20)

# Removing multicollinearity through VIF check
sort(vif(model_20))

# Removing NumCompaniesWorked.3 p-value = 0.004 i.e., Less significant
model_21 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  Department.Research...Development + 
                  Department.Sales + JobRole.Research.Director + 
                  JobRole.Research.Scientist + JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
                  EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
                  JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
                  WorkLifeBalance.4 , family = "binomial", 
                data = train)
summary(model_21)

# Removing multicollinearity through VIF check
sort(vif(model_21))

# Removing JobRole.Research.Director p-value = 0.003 i.e., Less significant
model_22 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  Department.Research...Development + Department.Sales +  
                  JobRole.Research.Scientist + JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
                  EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
                  JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
                  WorkLifeBalance.4 , family = "binomial", 
                data = train)
summary(model_22)

# Removing multicollinearity through VIF check
sort(vif(model_22))

# Removing JobRole.Research.Scientist p-value = 0.010 i.e., Less significant
model_23 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  Department.Research...Development + Department.Sales +  
                  JobRole.Sales.Executive + 
                  MaritalStatus.Single + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
                  EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
                  JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
                  WorkLifeBalance.4 , family = "binomial", 
                data = train)
summary(model_23)

# Removing multicollinearity through VIF check
sort(vif(model_23))

# Removing JobRole.Sales.Executive p-value = 0.020 i.e., Less significant
model_24 <- glm(formula = Attrition.Yes ~ Age + BusinessTravel.Travel_Frequently + 
                  Department.Research...Development + Department.Sales +  
                  MaritalStatus.Single + NumCompaniesWorked.7 + NumCompaniesWorked.9 + 
                  TotalWorkingYears + TrainingTimesLastYear.6 + YearsSinceLastPromotion + 
                  YearsWithCurrManager + workLoad.1 + workLoad.2 + 
                  EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
                  EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
                  JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
                  WorkLifeBalance.4 , family = "binomial", 
                data = train)
summary(model_24)

# Removing multicollinearity through VIF check
sort(vif(model_24))

# With ALL significant variables in the model, lets treat model_24
final_model <- model_24

# ******************************
# Model Evaluation
# ******************************
# Test Data : predict probabilities of Attrition.Yes for test data

test_pred <- predict(final_model, type = "response", newdata = test)
summary(test_pred)

test$prob <- test_pred
head(test)

# CHARACTERISITCS OF THE MODEL AT CUTOFF

# Actual attrition
test_actual_Attrition <- factor(ifelse(test$Attrition.Yes==1,"Yes","No"))
# Let's use the probability cutoff of 50%.
test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))

table(test_actual_Attrition,test_pred_Attrition)

#At 0.40 
test_pred_Attrition_1 <- factor(ifelse(test_pred >= 0.40, "Yes", "No")) 
test_conf2 <- confusionMatrix(test_pred_Attrition_1, test_actual_Attrition, positive = "Yes")
test_conf2

#At 0.30
test_pred_Attrition_2 <- factor(ifelse(test_pred >= 0.30, "Yes", "No")) 
test_conf3 <- confusionMatrix(test_pred_Attrition_2, test_actual_Attrition, positive = "Yes")
test_conf3

#At 0.25
test_pred_Attrition_3 <- factor(ifelse(test_pred >= 0.25, "Yes", "No")) 
test_conf4 <- confusionMatrix(test_pred_Attrition_3, test_actual_Attrition, positive = "Yes")
test_conf4

# ***********************************************************************
# Calculate cutoff to have a balance between sensitivity and specificitiy
# finding the optimal probalility cutoff value
# ***********************************************************************

# Cutoff functions which will calculate the sensitivity, specificity 
# and accuracy for given range(0.1 to 0.8) of cutoff
# Function to evaluate Cutoff
perform_fn <- function(cutoff) {
  hr_attri_pred <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(hr_attri_pred, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)
# Sequence and Empty Matrix to Store the Probabilities
s <- seq(.01,.80,length=100)
OUT <- matrix(0,100,3)

for(i in 1:100) {
  OUT[i,] = perform_fn(s[i])
} 

colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
head(OUT)
# create an OUT matrix data.frame
OUT <- data.frame(OUT)
head(OUT)

# For this we can also use a OptimalCutpoints package
max_cutoff <- abs(OUT$sensitivity + OUT$specificity) %>% which.max()
max_cutoff <- s[max_cutoff]
min_cutoff <- abs(OUT$sensitivity - OUT$specificity) %>% which.min()
min_cutoff <- s[min_cutoff] 

cutoff <- (abs(OUT$sensitivity - OUT$specificity) < 0.01) %>% which()
cutoff <- s[cutoff]

# Cutoff is equal to min_cutoff
cutoff == min_cutoff

# PLOTTING THE SENSITIVITY, SPECIFICITY, AND ACCURACY
ggplot(OUT) +
  geom_line(aes(x = s,y = OUT$sensitivity,col = "black")) + 
  geom_line(aes(x = s,y = OUT$specificity,col = "blue")) +
  geom_line(aes(x = s,y = OUT$accuracy,col = "green")) +
  scale_color_discrete(labels = c("Sensitivity","Specificity","Accuracy")) +
  geom_vline(xintercept = c(min_cutoff,cutoff,max_cutoff),
             linetype = "dotted") +
  labs(title = "Sensitiviy, Specificity, and Accuracy",
       x = "Predicted Probability") + 
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave("plots/Sensitiviy, Specificity, and Accuracy.png",
       width = 125, height = 100, units = "mm")

OUT$s <- s
head(OUT)
cutoff
# Inference: Cutoff at 0.169596
# Cutoff Of sensitiviy, specificity and accuracy reaches optimum at: s:0.169596
predicted_attrition_cutoff <- factor(ifelse(test$prob > cutoff,"Yes","No"))
actual_attrition <- factor(ifelse(test$Attrition.Yes == 1, "Yes","No"))

# confusion Matrix at Cutoff
conf_cutoff <- confusionMatrix(predicted_attrition_cutoff,
                               actual_attrition,positive = "Yes")

# confusion matrix element
conf_cutoff$table

# Accuracy: 0.754
conf_cutoff$overall[1]
# sensitivity: 0.762
conf_cutoff$byClass[1]
# specificity: 0.752
conf_cutoff$byClass[2]

# Adding the CutOff Predicted attrition to TEST
test$predicted_attr_cutoff <- ifelse(predicted_attrition_cutoff == "Yes",1,0) 

# ***************************************************
# KS Statistic: ROC Curve(Reciever operating characteristics)
# ***************************************************

# Comparing the predicting power and performance
predictHR <- prediction(test_pred, test$Attrition.Yes)
# fpr: False Positive rate
# tpr: True positive rate
perfHR <- performance(prediction.obj = predictHR, measure = "tpr", x.measure = "fpr")

ks_statistic <- max(attr(perfHR,'y.values')[[1]] - attr(perfHR,'x.values')[[1]])
print(ks_statistic)
# 0.5474982

# Area under the curve: 0.8291104
performance(predictHR,"auc")@y.values
# Inference: Model is scaling well as the area under the curve is closer to 1

# *********************************************************************
# GAIN CHART LIFT CHART: To cross validate the model 
# *********************************************************************
hr_analyt_gains <- gains(actual = test$Attrition,
                         predicted = test_pred,
                         groups = 10,ties.method = c("max"),
                         conf = c("normal"),conf.level = 0.95,
                         optimal = TRUE)

# Plotting Gain Chart
ggplot(data.frame(dec = hr_analyt_gains[[1]], 
                  cumm_gain = hr_analyt_gains[[6]]),
       aes(x = dec,y = cumm_gain)) + 
  geom_point() + 
  geom_text(aes(label = paste0(round(cumm_gain*100,1),"%")),
                           nudge_y = 0.05) +
  geom_line(linetype = "dotted") + 
  geom_line(aes(x = dec,y = seq(0.1,1,0.1)),linetype = "dashed") +
  labs(title = "Gain Chart", x= "decile", y = "Cummulative Gain") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plots/Gain Chart.png", width = 125, height = 100, units = "mm" )

# Plotting the lift

ggplot(data.frame(dec = hr_analyt_gains[[1]], 
                  lift = hr_analyt_gains[[6]]*100/hr_analyt_gains[[1]]),
       aes(x = dec,y = lift)) + 
  geom_point() +
  geom_line(linetype = "dotted") + 
  labs(title = "Lift Chart using Gains",
       x= "decile", y = "Lift") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plots/Lift Chart.png", width = 125, height = 100, units = "mm" )

# ************************
# Model Analysis
# ************************

# coefficients values in the model
final_model$coefficients


# Attrition predictor variables are: 

# Based on the sign of the coefficients in the final model, 
# key factors impacting attrition are as follows:

# Positive impact (that is, additive increase in these factors produce a 
# multiplicative increase in odds of attrition):
# 1. BusinessTravel: Employees who travle frequently for business purposes have higher attrition
# 2. MaritalStatus: Employees who are single have higher attrition
# 3. NumCompaniesWorked:(In this model it is 7 and 9) Employees worked in more companies have higher attrition
# 4. YearsSinceLastPromotion: Higher the delay in promotion higher is the attrition
# 5. workLoad: Employees who have "Normal" and "Heavy" workload have higher attrition 
names(which(final_model$coefficients > 0))


# Negative impact (that, additive increase in these factors produce a 
# multiplicative decrease in odds of attrition)
names(which(final_model$coefficients < 0))

# 6. Age: Higher the age lower is the attrition. As they may be looking for stability
# 7. Department: (R&D, Sales) Employees are happy and have lower attrition. 
# 8. TotalWorkingYears: Higher is the experience lower is the attrition
# 9. TrainingTimesLastYear: (6 Times) Increase in this factor tends to decrease in attrition
# 10.YearsWithCurrManager: Higher the employees work with same manager. Lesser is the attrition
# 11.EnvironmentSatisfaction:(Levels 2,3,4) Increase in this factor tends to decrease in attrition
# 12.JobSatisfaction:(Levels 2,3,4) Increase in this factor tends to decrease in attrition
# 13.WorkLifeBalance:(Levels 2,3,4) Increase in this factor tends to decrease in attrition

