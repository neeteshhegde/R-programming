###########################################################
## Author:   Neetesh Hegde
## Date:     2018-09-03
## Title:    PS1.R
## Purpose:  R Code for Assignment 1 of BUAN 6356 course
###########################################################

rm(list=ls()) #drop all variables

#call the required packages and set the working directory
library(data.table)
library(DBI)           
library(RSQLite)
library(tidyverse)
setwd("D:/MSBA/Fall 18/BUAN 6356 Business Analytics with R/Lec1")

###########################################################

## 1.1
## Read the WAGE1.RAW table data into the variable wage1
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
wage1 <- con %>% dbReadTable('wage1') %>% data.table
con %>% dbReadTable('wage1_labels')
con %>% dbDisconnect

## Data description from wage1_labels
# index variable.name  type format                  variable.label
# 1      0          wage float  %8.2g         average hourly earnings
# 2      1          educ  byte  %8.0g              years of education
# 3      2         exper  byte  %8.0g      years potential experience
# 4      3        tenure  byte  %8.0g     years with current employer
# 5      4      nonwhite  byte  %8.0g                  =1 if nonwhite
# 6      5        female  byte  %8.0g                    =1 if female
# 7      6       married  byte  %8.0g                   =1 if married
# 8      7        numdep  byte  %8.0g            number of dependents
# 9      8          smsa  byte  %8.0g              =1 if live in SMSA
# 10     9      northcen  byte  %8.0g =1 if live in north central U.S
# 11    10         south  byte  %8.0g   =1 if live in southern region
# 12    11          west  byte  %8.0g    =1 if live in western region
# 13    12      construc  byte  %8.0g  =1 if work in construc. indus.
# 14    13       ndurman  byte  %8.0g  =1 if in nondur. manuf. indus.
# 15    14      trcommpu  byte  %8.0g  =1 if in trans, commun, pub ut
# 16    15         trade  byte  %8.0g    =1 if in wholesale or retail
# 17    16      services  byte  %8.0g        =1 if in services indus.
# 18    17      profserv  byte  %8.0g     =1 if in prof. serv. indus.
# 19    18       profocc  byte  %8.0g    =1 if in profess. occupation
# 20    19       clerocc  byte  %8.0g    =1 if in clerical occupation
# 21    20       servocc  byte  %8.0g     =1 if in service occupation
# 22    21         lwage float  %9.0g                       log(wage)
# 23    22       expersq   int  %9.0g                         exper^2
# 24    23       tenursq   int  %9.0g                        tenure^2

#(i)
	educ_mean <- mean(wage1$educ)
	educ_mean #12.56274
	#The average education level in the sample is 12.56 years

	educ_min <- min(wage1$educ)
	educ_min
	#The lowest education level in the sample is 0 years

	educ_max <- max(wage1$educ)
	educ_max
	#The highest education level in the sample is 18 years

#(ii)
	wage_mean <- mean(wage1$wage)
	wage_mean #5.908992
	#The average hourly wage in the sample is $5.91
	#As per the current standards the hourly wage is really low

#(iii)
	cpi <- fread('ERP-2011-table60.csv') #Source: Economic report of President (2011),"Table B60. Consumer price indexes for major expenditure classes, 1967-2010". Excel file has been modified into a csv

	cpi_1976 <- cpi[cpi$Year == 1976, `All items`]
	cpi_1976
	#Consumer Price Index for 1976 was 56.9

	cpi_2010 <- cpi[cpi$Year == 2010, `All items`]
	cpi_2010
	#Consumer Price index for 2010 was 218.056

#(iv)
	wage_mean_2010 <- (wage_mean * cpi_2010)/cpi_1976
	wage_mean_2010 #22.64484
	#The average hourly wage in 2010 dollars for the sample is $22.64. The average hourly wage now seems reasonable.

#(v)
	nrow(wage1) #Total count of sample = 526
	unique(wage1$female) #Checking unique values for the field female
	
	female_count <- sum(wage1$female)
	female_count
	#There are 252 women in the sample

	male_count <- sum(wage1$female-1)*(-1)
	male_count
	#There are 274 men in the sample

###########################################################################################
###########################################################################################

## 1.2
## Read the MEAP01.RAW table data into the variable meap01
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
meap01 <- con %>% dbReadTable('meap01') %>% data.table
con %>% dbReadTable('meap01_labels')
con %>% dbDisconnect

## Data description from meap01_labels
# index variable.name  type format                                variable.label
# 1      0         dcode float  %9.0g                                 district code
# 2      1         bcode   int  %9.0g                                 building code
# 3      2         math4 float  %9.0g       % students satisfactory, 4th grade math
# 4      3         read4 float  %9.0g    % students satisfactory, 4th grade reading
# 5      4         lunch float  %9.0g % students eligible for free or reduced lunch
# 6      5        enroll   int  %9.0g                             school enrollment
# 7      6        expend float  %9.0g                             total spending, $
# 8      7         exppp float  %9.0g         expenditures per pupil: expend/enroll
# 9      8       lenroll float  %9.0g                                   log(enroll)
# 10     9       lexpend float  %9.0g                                   log(expend)
# 11    10        lexppp float  %9.0g                                    log(exppp)

#(i)
  math4_max <- max(meap01$math4)
  math4_max #100
  #Largest value of math4 is 100%

  math4_min <- min(meap01$math4)
  math4_min #0
  #Smallest value of math4 is 0%
  #The largest value of math4 makes sense since it is common for a school to achieve 100% students getting satisfactory grade
  #The smallest value of math4, 0%, should ideally be very rare as it is not common for a school to have no sudents achieving a satisfactory grade in math for the 4th grade. This can also be verified by checking for the % of schools with 0 math4, which is just 0.05%.
  #sum(as.numeric(meap01$math4==0))*100/nrow(meap01)

#(ii)
  math4_100 <- sum(as.numeric(meap01$math4==100))
  math4_100 #38
  #38 schools in the sample have a perfect pass rate on the math test

  math4_100_pcntg <- sum(as.numeric(meap01$math4==100))*100/nrow(meap01)
  math4_100_pcntg
  #2.08% of the schools in the sample have perfect pass rate on the math test 

#(iii)
  math4_50 <- sum(as.numeric(meap01$math4==50))
  math4_50 #17
  #17 schools in the sample have math pass rate of exactly 50%

#(iv)
  math4_mean <- mean(meap01$math4)
  math4_mean #71.909
  read4_mean <- mean(meap01$read4) 
  read4_mean #60.06188

  #Since the average pass rate for reading is lower than that for math, the reading test is the harder exam to pass 

#(v)
  cor_math_read <- cor(meap01$math4,meap01$read4)
  cor_math_read #0.8427281
  #The correlation between math and reading pass rate is 0.8427281 which is very close to 1 hence they are highly correlated
  #Hence it can be concluded that as math score increases or decreases, the same can be expected from the reading score, and vice versa.

#(vi)
  expp_mean <- mean(meap01$exppp)
  expp_mean
  #The average expenditure per pupil is $5194.87

  expp_sd <- sd(meap01$exppp)
  expp_sd
  #Standard deviation for expenditure per pupil is $1091.89
  #Since the standard deviation is almost 20% of the average expenditure per pupil, the variation (square of standard deviation) will be very high

#(vii)
  expp_sA <- 6000
  expp_sB <- 5500
  percent_diff <- (expp_sA-expp_sB)*100/expp_sB
  percent_diff #9.090909 ~ 9.09%

  percent_diff_log <- 100*(log(6000)-log(5500))
  percent_diff_log #8.701138 ~ 8.7%

###########################################################################################
###########################################################################################

## 1.3
## Read the 401K.RAW table data into the variable k401
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
k401 <- con %>% dbReadTable('401k') %>% data.table
con %>% dbReadTable('401k_labels')
con %>% dbDisconnect

## Data description from 401k_labels
# index variable.name  type format                  variable.label
# 1     0         prate float  %7.0g     participation rate, percent
# 2     1         mrate float  %7.0g            401k plan match rate
# 3     2       totpart float  %7.0g         total 401k participants
# 4     3        totelg float  %7.0g    total eligible for 401k plan
# 5     4           age  byte  %7.0g                age of 401k plan
# 6     5        totemp float  %7.0g  total number of firm employees
# 7     6          sole  byte  %7.0g = 1 if 401k is firm's sole plan
# 8     7       ltotemp float  %9.0g                   log of totemp

#(i)
  prate_mean <- mean(k401$prate)
  prate_mean #87.36291
  #Average participation rate is 87.36%

  mrate_mean <- mean(k401$mrate)
  mrate_mean #0.7315124
  #Average match rate is 73.15%

#(ii)
  model_3_3 <- lm(prate~mrate, data=k401)   #does ordinary least squares in one line
  summary(model_3_3)
  nrow(k401)
  #B0 = 83.0755
  #B1 = 5.8611
  #sample size = 1534
  #R Squared = 0.0747 or 7.47%

#(iii) The intercept in the equation (B0) is 83.0755 which means that even if the firm doesn't match any percent of the contribution from the worker, i.e. mrate = 0, then it is predicted that 83.0755% of the workers will participate in the 401K plan
  
#(iv)
  mrate_4 <-3.5
  prate_pred <- 83.0755 + 5.8611*mrate_4
  prate_pred #103.5894
  #The predicted prate is 103.59%
  #The predicted prate is cannot be possible as it's over 100%. The model is a mathematical estimate hence doesn't take into account real life limitations. Hence the predicted value will have a residual error.

#(v) Since the R-squared value is 0.0747, 7.47% of the variation in prate is explained by mrate. This is very less and many other variables are required to predict variations in prate.
  
###########################################################################################
###########################################################################################

## 1.4
## Read the CEOSAL2.RAW table data  into the variable ceosal2
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
ceosal2 <- con %>% dbReadTable('ceosal2') %>% data.table
con %>% dbReadTable('ceosal2_labels')
con %>% dbDisconnect

## Data description from ceosal2_labels
# index variable.name  type format                 variable.label
# 1      0        salary   int  %9.0g      1990 compensation, $1000s
# 2      1           age  byte  %9.0g                       in years
# 3      2       college  byte  %9.0g         =1 if attended college
# 4      3          grad  byte  %9.0g =1 if attended graduate school
# 5      4        comten  byte  %9.0g             years with company
# 6      5        ceoten  byte  %9.0g      years as ceo with company
# 7      6         sales float  %9.0g      1990 firm sales, millions
# 8      7       profits   int  %9.0g         1990 profits, millions
# 9      8        mktval float  %9.0g market value, end 1990, mills.
# 10     9       lsalary float  %9.0g                    log(salary)
# 11    10        lsales float  %9.0g                     log(sales)
# 12    11       lmktval float  %9.0g                    log(mktval)
# 13    12      comtensq   int  %9.0g                       comten^2
# 14    13      ceotensq   int  %9.0g                       ceoten^2
# 15    14      profmarg float  %9.0g          profits as % of sales

#(i)
  salary_mean <- mean(ceosal2$salary)
  salary_mean #865.8644
  #Average salary in the sample is $865864.4

  ceoten_mean <- mean(ceosal2$ceoten)
  ceoten_mean #7.954802
  #Average tenure of CEOs in the sample is 7.95 years

#(ii)
  sum(as.numeric(ceosal2$ceoten==0)) #5
  #5 CEOs are in their first year as CEO

  max(ceosal2$ceoten) #37
  #The longest tenure as a CEO is 37 years

#(iii)
  model_4_3 <- lm(log(salary)~ceoten, data=ceosal2)
  summary(model_4_3)
  #log(salary) = 6.505498 + 0.009724 * ceoten + u
  #The predicted percentage increase in salary given one more year is 0.9724% ~ 1%

###########################################################################################
###########################################################################################

## 1.5
## Read the WAGE2.RAW table data  into the variable wage2
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
wage2 <- con %>% dbReadTable('wage2') %>% data.table
con %>% dbReadTable('wage2_labels')
con %>% dbDisconnect

## Data description from wage2_labels
# index variable.name  type format                variable.label
# 1      0          wage   int  %9.0g              monthly earnings
# 2      1         hours  byte  %9.0g          average weekly hours
# 3      2            IQ   int  %9.0g                      IQ score
# 4      3           KWW  byte  %9.0g knowledge of world work score
# 5      4          educ  byte  %9.0g            years of education
# 6      5         exper  byte  %9.0g      years of work experience
# 7      6        tenure  byte  %9.0g   years with current employer
# 8      7           age  byte  %9.0g                  age in years
# 9      8       married  byte  %9.0g                 =1 if married
# 10     9         black  byte  %9.0g                   =1 if black
# 11    10         south  byte  %9.0g           =1 if live in south
# 12    11         urban  byte  %9.0g            =1 if live in SMSA
# 13    12          sibs  byte  %9.0g            number of siblings
# 14    13       brthord  byte  %9.0g                   birth order
# 15    14         meduc  byte  %9.0g            mother's education
# 16    15         feduc  byte  %9.0g            father's education
# 17    16         lwage float  %9.0g           natural log of wage

#(i)
  wage_mean <- mean(wage2$wage)
  wage_mean #957.9455
  #The average salary in the sample is $957.95

  IQ_mean <- mean(wage2$IQ)
  IQ_mean #101.2824
  #Average IQ in the sample is 101.28

  IQ_sd <- sd(wage2$IQ)
  IQ_sd #15.05264
  #The sample standard deviation of IQ is 15.05

#(ii)
  model_5_2 <- lm(wage~IQ, data=wage2)
  summary(model_5_2)
  #wage = 116.9916 + 8.3031 * IQ
  #Multiple R-Squared = 0.09554
  
  wage_predinc = 8.3031 * 15
  wage_predinc
  #For an IQ increase of 15 points the predicted increase in wage is $124.55
  #Since the Multiple R-Squared value is 9.55% for this model, which is very low, IQ does not explain most of the variation in wage

#(iii)
  model_5_3 <- lm(log(wage)~IQ, data = wage2)
  summary(model_5_3)
  #log(wage) = 5.8869943 + 0.0088072 * IQ
  
  wage_percentinc <- 0.0088072 * 15 * 100
  wage_percentinc #13.2108
  #For an IQ increase of 15 points the predicted wage increases by approximately 13.21%
  
###########################################################################################
###########################################################################################

## 1.6
## Read the MEAP93.RAW table data into the variable meap93
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
meap93 <- con %>% dbReadTable('meap93') %>% data.table
con %>% dbReadTable('meap93_labels')
con %>% dbDisconnect

## Data description from meap93_labels
#     index variable.name  type format                  variable.label
# 1      0       lnchprg float  %9.0g  perc of studs in sch lnch prog
# 2      1        enroll   int  %9.0g               school enrollment
# 3      2         staff float  %9.0g         staff per 1000 students
# 4      3        expend   int  %9.0g             expend. per stud, $
# 5      4        salary float  %9.0g          avg. teacher salary, $
# 6      5      benefits   int  %9.0g        avg. teacher benefits, $
# 7      6      droprate float  %9.0g       school dropout rate, perc
# 8      7      gradrate float  %9.0g    school graduation rate, perc
# 9      8        math10 float  %9.0g    perc studs passing MEAP math
# 10     9         sci11 float  %9.0g perc studs passing MEAP science
# 11    10       totcomp float  %9.0g               salary + benefits
# 12    11      ltotcomp float  %9.0g                    log(totcomp)
# 13    12       lexpend float  %9.0g                   log of expend
# 14    13       lenroll float  %9.0g                     log(enroll)
# 15    14        lstaff float  %9.0g                      log(staff)
# 16    15        bensal float  %9.0g                 benefits/salary
# 17    16       lsalary float  %9.0g                     log(salary)

#(i)
  model_6_1 <- lm(math10~expend, data=meap93)
  summary(model_6_1)
  #B0 = 1.336e+1
  #B1 = 2.456e-3
  # Although each additional dollar spent increases the math pass rate, the % increase in the pass rate diminishes as every additional dollar increases the pass rate by a constant amount but as the pass rate increases the % by which the pass rate increases diminishes.

#(ii) math10 = B0 + B1 * log(expend) + u
  # As per the model, unit increase in expenditure per studen should result in B1 percentage point increase in math10
  # Hence, 10% increase or 1/10 unit increase in expenditure per student will result in B1/10 percentage point increase in math10

#(iii)
  model_6_3 <- lm(math10~log(expend), data = meap93)
  summary(model_6_3)
  nrow(meap93)
  # math10 = -69.341 + 11.164 * log(expend)
  # R-squared = 0.02966 or 2.97%
  # Sample size = 408
  
#(iv)
  # As mentioned in (ii), 10% increase in expenditure per student will result in B1/10 percentage point in math10
  math10_inc = 11.164/10
  math10_inc #1.1164
  # Increasing spending by 10% will result in approximately 1.12 percentage point increase in math pass rate
  
#(v)
  summary(meap93$math10) #Max = 66.70
  # Although the regression analysis can produce fitted values for math10 greater that 100, this wouldn't be a problem for this data set as the maximum value already present in the data is 66.7. Also for 100% increase in expenditure per student, the math pass rate increases only 11.16 percentage points. Hence to produce a value over 100 for math10 will require the expenditure per student to increase by a very large value.
  
###########################################################################################
###########################################################################################

## 1.7
## Read the HPRICE1.RAW table data  into the variable hprice1
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
hprice1 <- con %>% dbReadTable('hprice1') %>% data.table
con %>% dbReadTable('hprice1_labels')
con %>% dbDisconnect

## Data description from hprice1_labels
#     index variable.name  type format               variable.label
# 1      0         price float  %9.0g          house price, $1000s
# 2      1        assess float  %9.0g       assessed value, $1000s
# 3      2         bdrms  byte  %9.0g              number of bdrms
# 4      3       lotsize float  %9.0g   size of lot in square feet
# 5      4         sqrft   int  %9.0g size of house in square feet
# 6      5      colonial  byte  %9.0g =1 if home is colonial style
# 7      6        lprice float  %9.0g                   log(price)
# 8      7       lassess float  %9.0g                   log(assess
# 9      8      llotsize float  %9.0g                 log(lotsize)
# 10     9        lsqrft float  %9.0g                   log(sqrft)

#(i)
  model_7_1 <- lm(price~sqrft+bdrms, data=hprice1)
  summary(model_7_1)
  # price = -19.315 + 0.1284*sqrft + 15.1982*bdrms + u

#(ii)
  1000*15.1982*1
  # Estimated increase in price with 1 extra bedroom holding square footage constant is $15198.2

#(iii) 
  1000*(15.1982*1 + 140*0.1284)
  # Estimated increase in price with 1 extra bedroom and 140 sq ft extra space is $33174.2

#(iv) Multiple R Squared is 0.6319 hence 63.19% of variation in price is explained by square footage and number of bedrooms

#(v)
  price_pred <- 1000*(-19.3150 + 0.1284*2438 + 15.1982*4)
  price_pred #354517
  #The predicted price for the house with 2438 sqrft and 4 bedrooms is $354517

#(Vi)
  price_actual <- 300000
  price_residual <- price_pred - price_actual
  price_residual #54517
  # Residual for this house is $54517
  # Buyer underpaid as the actual price is lower than the predicted price
  
###########################################################################################
###########################################################################################

## 1.8
## Read the CEOSAL2.RAW table data into the variable ceosal2
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
ceosal2 <- con %>% dbReadTable('ceosal2') %>% data.table
con %>% dbReadTable('ceosal2_labels')
con %>% dbDisconnect

## Data description from ceosal2_labels
# index variable.name  type format                 variable.label
# 1      0        salary   int  %9.0g      1990 compensation, $1000s
# 2      1           age  byte  %9.0g                       in years
# 3      2       college  byte  %9.0g         =1 if attended college
# 4      3          grad  byte  %9.0g =1 if attended graduate school
# 5      4        comten  byte  %9.0g             years with company
# 6      5        ceoten  byte  %9.0g      years as ceo with company
# 7      6         sales float  %9.0g      1990 firm sales, millions
# 8      7       profits   int  %9.0g         1990 profits, millions
# 9      8        mktval float  %9.0g market value, end 1990, mills.
# 10     9       lsalary float  %9.0g                    log(salary)
# 11    10        lsales float  %9.0g                     log(sales)
# 12    11       lmktval float  %9.0g                    log(mktval)
# 13    12      comtensq   int  %9.0g                       comten^2
# 14    13      ceotensq   int  %9.0g                       ceoten^2
# 15    14      profmarg float  %9.0g          profits as % of sales

#(i)
  model_8_1 <- lm(log(salary)~log(sales)+log(mktval),data = ceosal2)
  summary(model_8_1)
  #log(salary) = 4.62092 + 0.16213 * log(sales) + 0.10671 * log(mktval)
  #R-Squared = 0.2991

#(ii)
  model_8_2 <- lm(log(salary)~log(sales)+log(mktval)+log(profits),data = ceosal2) #In log(profits) : NaNs produced
  summary(ceosal2$profits)
  # Profits can't be used in the log form because there are negative values(losses)

  model_8_2 <- lm(log(salary)~log(sales)+log(mktval)+profits, data = ceosal2)
  summary(model_8_2)
  # log(salary) = 4.687e+00 + 1.614e-01 * log(sales) + 9.753e-02 * log(mktval) + 3.566e-05 * profits
  # Since multiple R-squared is 0.2993 it means that these firm performance variables aren't enough to explain most of the variations in the CEO salaries

#(iii)
  model_8_3 <- lm(log(salary)~log(sales)+log(mktval)+profits+ceoten,ceosal2)
  summary(model_8_3)
  coef(model_8_3)
  # log(salary) = 4.558e+00 + 1.622e-01 * log(sales) + 1.018e-01 * log(mktval) + 2.905e-05 * profits + 1.168e-02 * ceoten
  # Estimated percentage increase in salary for another year of CEO tenure is approximately 1.17%, holding others factors fixed

#(iv)
  cor(log(ceosal2$mktval),ceosal2$profits) #0.7768976
  # Sample correlation coefficient between log(mktval) and profits is 77.69%
  # Since the correlation is really high the OLS estimators won't be the best way to model for salary
  
###########################################################################################
###########################################################################################

## 1.9
## Read the ATTEND.RAW table data into the variable attend
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
attend <- con %>% dbReadTable('attend') %>% data.table
con %>% dbReadTable('attend_labels')
con %>% dbDisconnect

## Data description from attend_labels
# index variable.name  type format               variable.label
# 1      0        attend  byte  %8.0g   classes attended out of 32
# 2      1       termGPA float  %9.0g                 GPA for term
# 3      2        priGPA float  %9.0g cumulative GPA prior to term
# 4      3           ACT  byte  %8.0g                    ACT score
# 5      4         final  byte  %8.0g             final exam score
# 6      5       atndrte float  %9.0g     percent classes attended
# 7      6         hwrte float  %9.0g   percent homework turned in
# 8      7         frosh  byte  %8.0g               =1 if freshman
# 9      8          soph  byte  %8.0g              =1 if sophomore
# 10     9        missed  byte  %9.0g     number of classes missed
# 11    10       stndfnl float  %9.0g            (final - mean)/sd

#(i)
  min(attend$atndrte) #minimum attendance rate = 6.25%
  max(attend$atndrte) #maximum attendance rate = 100%
  mean(attend$atndrte) #average attendance rate = 81.71%

  min(attend$priGPA) #minimum prior GPA = 0.857
  max(attend$priGPA) #maximum prior GPA = 3.93
  mean(attend$priGPA) #average prior GPA = 2.59

  min(attend$ACT) #minimum ACT score = 13
  max(attend$ACT) #maximum ACT score = 32
  mean(attend$ACT) #average ACT score = 22.51

#(ii)
  model_9_2 <- lm(atndrte~priGPA+ACT, data = attend)
  summary(model_9_2)
  #atndrte = 75.7 + 17.261 * priGPA - 1.717 * ACT + u
  #The intercept here is 75.7 which does not make sense as it means if a student has 0 prior GPA and 0 ACT score then he will have a 75.7% attendance this semester. Even accounting for a new student who could have 0 priGPA, to get admitted into the school, the student would have to have a non-zero ACT score.

#(iii) It is surprising to see a negative coefficient for ACT as the model predicts that as the student has a better score in ACT score, he will attend lesser classes. That means the better students attend lesser classes, which is contrary to popular expectation.

#(iv)
  patndrte <- (75.7 + 17.26 * 3.65 - 1.72 * 20)
  patndrte #104.3
  # The predicted attendance rate, for a student with priGPA = 3.65 and ACT = 20, is 104.3% which is not possible as 100 can be the maximum value for attendance rate.

  sum(as.numeric(attend$priGPA==3.65 & attend$ACT == 20)) #1
  attend[priGPA == 3.65, ACT == 20, atndrte]  #87.5
  # There is 1 student in the sample with priGPA = 3.65 and ACT = 20 and the attendance rate is 87.5%

#(v)
  Pdiff_atndrte <- 17.261 * (3.1-2.1) - 1.717 * (21-26)
  Pdiff_atndrte #25.846
  #Predicted difference in attendance rate is 25.846%
  
###########################################################################################
###########################################################################################

## 1.10
## Read the HTV.RAW table data into the variable htv
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
htv <- con %>% dbReadTable('htv') %>% data.table
con %>% dbReadTable('htv_labels')
con %>% dbDisconnect

## Data description from htv_labels
# index variable.name  type format                  variable.label
# 1      0          wage float  %9.0g               hourly wage, 1991
# 2      1          abil float  %9.0g abil. measure, not standardized
# 3      2          educ  byte  %9.0g highest grade completed by 1991
# 4      3            ne  byte  %9.0g        =1 if in northeast, 1991
# 5      4            nc  byte  %9.0g        =1 if in nrthcntrl, 1991
# 6      5          west  byte  %9.0g             =1 if in west, 1991
# 7      6         south  byte  %9.0g            =1 if in south, 1991
# 8      7         exper  byte  %9.0g            potential experience
# 9      8      motheduc  byte  %9.0g           highest grade, mother
# 10     9      fatheduc  byte  %9.0g           highest grade, father
# 11    10      brkhme14  byte  %9.0g       =1 if broken home, age 14
# 12    11          sibs  byte  %9.0g              number of siblings
# 13    12         urban  byte  %9.0g       =1 if in urban area, 1991
# 14    13          ne18  byte  %9.0g             =1 if in NE, age 18
# 15    14          nc18  byte  %9.0g             =1 if in NC, age 18
# 16    15       south18  byte  %9.0g          =1 if in south, age 18
# 17    16        west18  byte  %9.0g           =1 if in west, age 18
# 18    17       urban18  byte  %9.0g     =1 if in urban area, age 18
# 19    18        tuit17 float  %9.0g         college tuition, age 17
# 20    19        tuit18 float  %9.0g         college tuition, age 18
# 21    20         lwage float  %9.0g                       log(wage)
# 22    21       expersq   int  %9.0g                         exper^2
# 23    22         ctuit float  %9.0g                 tuit18 - tuit17

# (i)
  range(htv$educ)
  # Range of highest education completed by working men in 1991 is 14 years (6 years to 20 years)

  sum(as.numeric(htv$educ==12))/nrow(htv)
  # 41.63% men in the sample have completed 12th grade but no higher grade

  mean(htv$educ)
  #average level of higher education for working men is 13.04 years

  mean(htv$motheduc)
  #average level of higher education for mother is 12.18 years
  mean(htv$fatheduc)
  #average level of higher education for father is 12.45 years
  #Since average level of higher education for both  mothers and fathers is lesser than the men, the men on average have higher levels of education than their parents.

#(ii)
  model_10_2 <- lm(educ~motheduc+fatheduc, data = htv)
  summary(model_10_2)
  #educ = 6.96435 + 0.30420 * motheduc + 0.19029 * fatheduc + u
  #Since R-squared = 0.2493, 24.93% of the sample variation in educ is explained by parents' eduction
  #As per the model an increase of 1 level of education for the mother will result in a predicted increase of 0.3 level of education for the worker

#(iii)
  model_10_3 <- lm(educ~motheduc+fatheduc+abil, data = htv)
  summary(model_10_3)
  #educ = 8.44869 + 0.18913 * motheduc + 0.11109 * fatheduc + 0.50248 * abil + u or 1.784
  #Since the R-squared has increased from 0.2493 to 0.4275, adding abil has improved the model to explain the effect on variation but still not enough to explain it to a great extent as the value of R-squared is still not high enough.

#(iv)
  htv$abilq <- (htv$abil)^2
  model_10_4 <- lm(educ~motheduc+fatheduc+abil+abilq, data = htv)
  summary(model_10_4)
  #educ = 8.240226 + 0.190126 * motheduc + 0.108939 * fatheduc + 0.401462 * abil + 0.050599 * abil^2
  #R-Squared = 0.4444

  #abil* = -B3/2*B4
  abilmin <- -0.401462/(2*0.050599)
  abilmin #-3.967094
  #Holding parents' education constant, education level is minimized when abil is -3.967094

#(v)
  sum(as.numeric(htv$abil< (-3.967094)))*100/nrow(htv) # 1.219512
  #Only 1.22% of the mean have ability less than the value calculated in (iv). This is important because as per the model only a really small percentage of men will have their ability lesser than the ability required for the minimum education level keeping parents' education constant

#(vi)
  # educ = 8.240226 + 0.190126 * 12.18 + 0.108939 * 12.45 + 0.401462 * abil + 0.050599 * abil^2
  
  #Plotting scatter plot for educ vs abil
  plot_10_basic <- htv %>% ggplot(aes(x=abil,y=educ)) + geom_point() 
  plot_10_basic <- plot_10_basic + scale_x_continuous(name="Ability") + scale_y_continuous(name="Education Level")
  plot_10_basic

  #Plotting the regression line from model in (iv)
  plot_10 <- plot_10_basic + geom_line(aes(x=abil,y=(8.240226 + 0.190126*12.18 + 0.108939*12.45 + 0.401462 * abil + 0.050599 * abil^2)),color="blue",size=2)
  plot_10

# References: 
# 1. Economic report of President 2011,  "Table B-60. Consumer price indexes for major            expenditure classes, 1967-2010", Economic report from Government Publishing office US Website  