## 2020 Introduction to data science
## Class Final Project
## Joseph Park
## Using COVID-19 data that include medical information such as hospitalization, ICU admission, underlying medical condition and death,
## we are trying to find a logistic regression model that can predict death possibility of the patient.

## Getting current directory
getwd()
# [1] "D:/UCLA/UCLA Extension/Introduction to Data Science/Homework"

## Save source URL
SourceURL <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD"

## Download the data set
#download.file(SourceURL, destfile="./COVID-19_Case_Surveillance_Public_Use_Data.csv")

## Verify the data set landed properly
list.files("./")
#[1] "CLASS PROJECT.pdf"                              "Class Project.R"                               
#[3] "COVID-19_Case_Surveillance_Public_Use_Data.csv" 

## Read the data set into R. 
## Convert all "unknown" values to NA from the data set.
COVID <- read.csv("./COVID-19_Case_Surveillance_Public_Use_Data.csv",
                  stringsAsFactors = FALSE,
                  na.strings = c("Unknown",""))
## Take a look to see if data looks reasonable
head(COVID)
#             current_status cdc_report_dt  sex     age_group pos_spec_dt hosp_yn icu_yn death_yn medcond_yn   onset_dt
#1 Laboratory-confirmed case    2020/03/16 <NA> 10 - 19 Years  2020/03/13      No   <NA>     <NA>         No       <NA>
#2 Laboratory-confirmed case    2020/03/17 Male 10 - 19 Years  2020/03/13    <NA>   <NA>     <NA>       <NA>       <NA>
#3 Laboratory-confirmed case    2020/03/23 Male 10 - 19 Years  2020/03/13      No     No       No         No 2020/03/07
#4 Laboratory-confirmed case    2020/03/20 Male 10 - 19 Years  2020/03/13      No   <NA>     <NA>       <NA> 2020/03/13
#5 Laboratory-confirmed case    2020/03/27 Male 10 - 19 Years  2020/03/13      No     No       No         No 2020/03/12
#6 Laboratory-confirmed case    2020/03/19 Male 10 - 19 Years  2020/03/13    <NA>   <NA>     <NA>       <NA>       <NA>
## Names of the data set
names(COVID)
#[1] "current_status" "cdc_report_dt"  "sex"            "age_group"      "pos_spec_dt"    "hosp_yn"        "icu_yn"        
#[8] "death_yn"       "medcond_yn"     "onset_dt"

## Dimension of the original data set
dim(COVID)
#[1] 1111016      10

## Data Munging
## Remove all NA values from data set. Data set is quite big. 
## Therefore, we are going to remove rows of the data set if it includes at least one NA value.
COVID.has.na <- apply(X=COVID, MARGIN=1, FUN=function(x){any(is.na(x))})
sum(COVID.has.na)
#[1] 1042012
## Remove rows using index
COVID_trimmed <- COVID[!COVID.has.na,]
## Dimension of the data set
dim(COVID_trimmed)
#[1] 44328    10
head(COVID_trimmed)
#              current_status cdc_report_dt  sex     age_group pos_spec_dt hosp_yn icu_yn death_yn medcond_yn   onset_dt
#3  Laboratory-confirmed case    2020/03/23 Male 10 - 19 Years  2020/03/13      No     No       No         No 2020/03/07
#5  Laboratory-confirmed case    2020/03/27 Male 10 - 19 Years  2020/03/13      No     No       No         No 2020/03/12
#16 Laboratory-confirmed case    2020/03/23 Male 10 - 19 Years  2020/03/13      No     No       No         No 2020/03/15
#47 Laboratory-confirmed case    2020/03/24 Male 20 - 29 Years  2020/03/14      No     No       No         No 2020/03/12
#72 Laboratory-confirmed case    2020/03/27 Male 20 - 29 Years  2020/03/14      No     No       No         No 2020/03/09
#77 Laboratory-confirmed case    2020/03/18 Male 20 - 29 Years  2020/03/14      No     No       No         No 2020/03/11

## Check number of cases
table(COVID_trimmed$current_status)
#Laboratory-confirmed case             Probable Case 
#                    44138                       190

## Deal only with laboratory confirmed cases
COVID_trimmed <- COVID_trimmed[COVID_trimmed$current_status=="Laboratory-confirmed case",]
## Renumber the row
row.names(COVID_trimmed) <- 1:nrow(COVID_trimmed)
## Dimension of the data set
dim(COVID_trimmed)
##[1] 44138    10

## Change the date information into POSIXct format
#install.packages("lubridate")
library(lubridate)

## class before changing date format
class(COVID_trimmed$cdc_report_dt)
#[1] "character"
class(COVID_trimmed$pos_spec_dt)
#[1] "character"
class(COVID_trimmed$onset_dt)
#[1] "character"

## Use parse_date_time() from lubridate
COVID_trimmed$cdc_report_dt <- parse_date_time(COVID_trimmed$cdc_report_dt, "%y/%m/%d")
COVID_trimmed$pos_spec_dt <- parse_date_time(COVID_trimmed$pos_spec_dt, "%y/%m/%d")
COVID_trimmed$onset_dt <- parse_date_time(COVID_trimmed$onset_dt, "%y/%m/%d")

## class after changing date format
class(COVID_trimmed$cdc_report_dt)
#[1] "POSIXct" "POSIXt
class(COVID_trimmed$pos_spec_dt)
#[1] "POSIXct" "POSIXt
class(COVID_trimmed$onset_dt)
#[1] "POSIXct" "POSIXt

## Exploratory Data Analysis

## Gender distribution
table(COVID_trimmed$sex)
#Female   Male 
# 22690  21448

## Pie chart for COVID_19 Gender distribution 
slices <- table(COVID_trimmed$sex)
lbls <- c("Female", "Male")
pie(slices, labels = lbls, main="COVID_19: Gender distribution")

prop.table(table(COVID_trimmed$sex))
#   Female      Male
#0.5142574 0.4857426

## Check hospitalization rate
table(COVID_trimmed$hosp_yn)
#   No   Yes 
#29352 14786
prop.table(table(COVID_trimmed$hosp_yn))
#       No       Yes 
#0.6660801 0.3339199

## Check ICU admission rate among hospitalized patient.
table(COVID_trimmed$icu_yn)
#   No  Yes 
#38665 5473

## Check death rate
prop.table(table(COVID_trimmed$death_yn))
#       No       Yes 
#0.8592086 0.1407914

##-------------- Gender vs. Death --------------
## Check percentage of the patient gender with respect to death.
gender_death <- data.frame(unclass(table(COVID_trimmed$death_yn,COVID_trimmed$sex)))
gender_death
#    Female  Male
#No   20160 17745
#Yes   2530  3703

##-------------- Age group vs. Death --------------
## Count number of patient for each age group  with respect to death.
age_death <- table(COVID_trimmed$age_group, COVID_trimmed$death_yn)
age_death
#                No  Yes
#0 - 9 Years    435    6
#10 - 19 Years 1117    5
#20 - 29 Years 5251   45
#30 - 39 Years 6032  135
#40 - 49 Years 6661  356
#50 - 59 Years 7781  891
#60 - 69 Years 5748 1347
#70 - 79 Years 2761 1278
#80+ Years     2119 2170

##ggplot for COVID-19: Number of deaths versus recuperators by age group
age_death <- data.frame(table(COVID_trimmed$age_group, COVID_trimmed$death_yn))
names(age_death)<-c("Age_group","Death","Count")
age_death
#       age_group  YN     Count
#1  0  -  9 Years  No       435
#2  10 - 19 Years  No      1117
#3  20 - 29 Years  No      5251
#4  30 - 39 Years  No      6032
#5  40 - 49 Years  No      6661
#6  50 - 59 Years  No      7781
#7  60 - 69 Years  No      5748
#8  70 - 79 Years  No      2761
#9  80+     Years  No      2119
#10 0  -  9 Years Yes         6
#11 10 - 19 Years Yes         5
#12 20 - 29 Years Yes        45
#13 30 - 39 Years Yes       135
#14 40 - 49 Years Yes       356
#15 50 - 59 Years Yes       891
#16 60 - 69 Years Yes      1347
#17 70 - 79 Years Yes      1278
#18 80+     Years Yes      2170
library(ggplot2)
ggplot(age_death, aes(x=Age_group, y= Count)) + 
  geom_bar(aes(color = Death, fill = Death),
           stat = "identity", position = position_stack()
  ) +
  scale_color_manual(values = c("green", "red"))+
  scale_fill_manual(values = c("green", "red"))+
  ggtitle("Number of Deaths vs. Recuperators by Age Group")

##-------------- ICU vs. Death --------------
## Count number of ICU patient with respect to death.
icu_death <- data.frame(unclass(table(COVID_trimmed$icu_yn, COVID_trimmed$death_yn)))
#       No   Yes
#No  35887  2965
#Yes  2200  3276

## Convert age_group into numbers that can represent the age groups.
COVID_Score <- COVID_trimmed
convert_age_group <- function(x){if(x == "0 - 9 Years"){result<-0}
  else if(x == "10 - 19 Years"){result<-1}
  else if(x == "20 - 29 Years"){result<-2}
  else if(x == "30 - 39 Years"){result<-3}
  else if(x == "40 - 49 Years"){result<-4}
  else if(x == "50 - 59 Years"){result<-5}
  else if(x == "60 - 69 Years"){result<-6}
  else if(x == "70 - 79 Years"){result<-7}
  else {result<-8}
  return(result)
}
age_group_number <- apply(X=data.frame(COVID_Score$age_group), MARGIN=1, FUN=convert_age_group)
COVID_Score <- cbind(COVID_Score,age_group_number)

##-------------- Risk Score vs. Death --------------
## COVID-19: A Score for Predicting Risk of Death
## Convert Yes/No to 1/0, then calculate summation(score) of multiple categories.

convert_yn <- function(x){
  if(x=="Yes"){
    result<-1}
  else{
    result<-0}
  return(result)}

COVID_Score$hosp_yn <- apply(X=data.frame(COVID_trimmed$hosp_yn), MARGIN=1, FUN=convert_yn)
COVID_Score$icu_yn <- apply(X=data.frame(COVID_trimmed$icu_yn), MARGIN=1, FUN=convert_yn)
COVID_Score$medcond_yn <- apply(X=data.frame(COVID_trimmed$medcond_yn), MARGIN=1, FUN=convert_yn)
COVID_Score$death_yn <-apply(X=data.frame(COVID_trimmed$death_yn), MARGIN=1, FUN=convert_yn)

COVID_Sum <- apply(X=COVID_Score[,c(6,7,9,11)], MARGIN=1, FUN=sum)
COVID_Score <- cbind(COVID_Score,COVID_Sum)
summary(COVID_Score)
#current_status     cdc_report_dt                     sex             age_group          pos_spec_dt                 
#Length:44138       Min.   :2020-01-22 00:00:00   Length:44138       Length:44138       Min.   :2020-01-19 00:00:00  
#Class :character   1st Qu.:2020-04-04 00:00:00   Class :character   Class :character   1st Qu.:2020-03-29 00:00:00  
#Mode  :character   Median :2020-04-14 00:00:00   Mode  :character   Mode  :character   Median :2020-04-08 00:00:00  
#                   Mean   :2020-04-15 00:33:32                                         Mean   :2020-04-10 02:55:56  
#                   3rd Qu.:2020-04-27 00:00:00                                         3rd Qu.:2020-04-23 00:00:00  
#                   Max.   :2020-05-14 00:00:00                                         Max.   :2020-05-27 00:00:00  
#
#      hosp_yn          icu_yn         death_yn        medcond_yn        onset_dt                     COVID_Sum     
#Min.   :0.000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :2020-01-05 00:00:00   Min.   :0.0000  
#1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:2020-03-23 00:00:00   1st Qu.:0.0000  
#Median :0.000   Median :0.000   Median :0.0000   Median :0.0000   Median :2020-04-03 00:00:00   Median :1.0000  
#Mean   :0.335   Mean   :0.124   Mean   :0.1412   Mean   :0.4883   Mean   :2020-04-05 02:36:43   Mean   :0.9473  
#3rd Qu.:1.000   3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:2020-04-18 00:00:00   3rd Qu.:2.0000  
#Max.   :1.000   Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :2020-05-27 00:00:00   Max.   :3.0000  



## Boxplots for the Scores vs.Death 
boxplot(COVID_Score$COVID_Sum ~ as.factor(COVID_trimmed$death_yn),
        main = "COVID-19: A Score for Predicting Risk of Death",
        xlab = "Death",
        ylab = "Risk Score",
        col = c("Green","Red"),
        border = "brown",
        horizontal = FALSE,
        notch = TRUE)

## Stastical Learning Algorithm for Predicting scores vs. deaths of COVID-19
## logistic regression - Multiple predictors:

# Split data set into training set and test set
n <- nrow(COVID_Score)  # Number of observations
ntrain <- round(n*0.6)  # 60% for training set
set.seed(100)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create an index

train_COVID <- COVID_Score[tindex,]   # Create training set
test_COVID <- COVID_Score[-tindex,]   # Create test set

formula <- COVID_Score$death_yn ~ COVID_Score$age_group_number +
                                  COVID_Score$hosp_yn +
                                  COVID_Score$icu_yn +
                                  COVID_Score$medcond_yn
                                

glm2 <- glm(formula, data=train_COVID, family="binomial")

summary(glm2)
#Call:
#  glm(formula = formula, family = "binomial", data = train_COVID)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.1207  -0.3722  -0.1534  -0.0601   3.5542  
#
#Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)                  -7.65943    0.09514  -80.50   <2e-16 ***
#  COVID_Score$age_group_number  0.67254    0.01341   50.16   <2e-16 ***
#  COVID_Score$hosp_yn           1.20502    0.04562   26.41   <2e-16 ***
#  COVID_Score$icu_yn            2.08210    0.04406   47.25   <2e-16 ***
#  COVID_Score$medcond_yn        1.12906    0.05217   21.64   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 35943  on 44137  degrees of freedom
#Residual deviance: 20533  on 44133  degrees of freedom
#AIC: 20543
#
#Number of Fisher Scoring iterations: 7
# ---------------------------------------------------------------
coef(glm2)
#(Intercept) COVID_Score$age_group_number          COVID_Score$hosp_yn           COVID_Score$icu_yn       COVID_Score$medcond_yn 
# -7.6594309                    0.6725408                    1.2050218                    2.0820995                    1.1290579 
# ---------------------------------------------------------------
prob <- predict(glm2, newdata=test_COVID, type="response")
prob <- round(prob,3)*100   # Get percentages
prob <- data.frame(prob)
names(prob)<-"Probability"
COVID_Result <- cbind(COVID_Score,prob)
head(COVID_Result)
#             current_status cdc_report_dt  sex     age_group pos_spec_dt hosp_yn icu_yn death_yn medcond_yn   onset_dt age_group_number COVID_Sum Probability
#1 Laboratory-confirmed case    2020-03-23 Male 10 - 19 Years  2020-03-13       0      0        0          0 2020-03-07                1         1         0.1
#2 Laboratory-confirmed case    2020-03-27 Male 10 - 19 Years  2020-03-13       0      0        0          0 2020-03-12                1         1         0.1
#3 Laboratory-confirmed case    2020-03-23 Male 10 - 19 Years  2020-03-13       0      0        0          0 2020-03-15                1         1         0.1
#4 Laboratory-confirmed case    2020-03-24 Male 20 - 29 Years  2020-03-14       0      0        0          0 2020-03-12                2         2         0.2
#5 Laboratory-confirmed case    2020-03-27 Male 20 - 29 Years  2020-03-14       0      0        0          0 2020-03-09                2         2         0.2
#6 Laboratory-confirmed case    2020-03-18 Male 20 - 29 Years  2020-03-14       0      0        0          0 2020-03-11                2         2         0.2
tail(COVID_Result)
#                 current_status cdc_report_dt    sex age_group pos_spec_dt hosp_yn icu_yn death_yn medcond_yn   onset_dt age_group_number COVID_Sum Probability
#44133 Laboratory-confirmed case    2020-03-18 Female 80+ Years  2020-03-15       1      0        1          1 2020-03-13                8        10        51.4
#44134 Laboratory-confirmed case    2020-03-20 Female 80+ Years  2020-03-15       1      0        0          1 2020-03-10                8        10        51.4
#44135 Laboratory-confirmed case    2020-04-09 Female 80+ Years  2020-03-15       1      0        1          1 2020-03-15                8        10        51.4
#44136 Laboratory-confirmed case    2020-03-23 Female 80+ Years  2020-03-15       1      0        1          1 2020-03-14                8        10        51.4
#44137 Laboratory-confirmed case    2020-03-18 Female 80+ Years  2020-03-15       1      0        1          1 2020-03-14                8        10        51.4
#44138 Laboratory-confirmed case    2020-03-30 Female 80+ Years  2020-03-15       1      1        1          1 2020-03-11                8        11        89.4