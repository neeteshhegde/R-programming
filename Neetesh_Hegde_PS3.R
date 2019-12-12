###########################################################
## Author:   Neetesh Hegde
## Date:     2018-09-24
## Title:    PS3.R
## Purpose:  R Code for Assignment 3 of BUAN 6356 course
###########################################################

rm(list=ls()) #drop all variables

#call the required packages and set the working directory
library(data.table)
library(DBI)           
library(RSQLite)
library(tidyverse)
library(lmtest) 
library(sandwich)
library(tseries)
library(broom)
library(forecast)
library(TSA)
library(lubridate)


################################################################################
############################### Question 3.1 ###################################

# Q3.1(i): The null hypothesis that catchers and outfielders, on average, earn the same salaries will happen when being a catcher doesn't make a difference in the salaries. This means that coefficient for catcher variable should be statistically insignificant. The t-ratio for the catcher variable is 1.931. Since this is lesser than 1.96 the coefficient for catcher variable is statistically insignificant for a 2-sided alternative.

# Q3.1(ii):  the values of F statistic is 1.7774 (with 339 df and 5 variables) and its p value is 0.1168 which shows that it is not significant for 11%. However, the F stat is still significant for 5% which is less than 11% so hypothesis can be rejected.

# Q3.1(iii): Both models show similar results. The first one though gives a slightly higher amount earned by catcher when compared to other outfielders but not by much though

con <- SQLite() %>% dbConnect('wooldridge.db')
mlb1 <- con %>% dbReadTable('mlb1') %>% data.table
con %>% dbDisconnect
model_1_1 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+shrtstop+catcher,data=mlb1)
summary(model_1_1)
model_1_2 <- lm(salary~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+shrtstop+catcher,data=mlb1)
summary(model_1_2)
anova(model_1_1,model_1_2, test = "F")


################################################################################
############################### Question 3.2 ###################################

# Q3.2(i) Since Hperc is percentile from top, we can estimate that the B3 < 0, since the GPA decreases with increase. Also we can assume that students with better SAT scores have higher GPA so B4 > 0. Also we can assume that being an athlete will result in lower GPA, hence B6 < 0. Gender's and graduating class size's effect on GPA is unclear. 

# Q3.2(ii): colgpa = 1.241 - 0.05685*hsize + 0.004675*hsize^2 - 0.01321*hsperc + 0.001646*sat + 0.1549*female + 0.1693*athlete. Difference between athlete and non-athlete is 0.1693. It is statistically significant as the t-value for the coefficient is 3.998 which is higher than 1.96

# Q3.2(iii) On dropping SAT from the model the coefficient of athlete drops to 0.0054487 and it becomes statistically insignificant as the t-value is 0.122. This could be because athletes score lesser on SAT and since we do not control for SAT score there is no difference. Once we account for SAT score like in (ii) then athlete has a statistical significance

# Q3.2(iv) Taking female non athlete as base set and null hypothesis that there is no difference between female athletes and non athletes, we get that the coeff of female athletes is 0.1751 meaning female athletes have higher GPA than non-athletes. Null hypothesis can be rejected since t value = 2.084 for p value < 0.05 is significant at 5%.

# Q3.2(v) The effect of sat on colgpa does not differ by gender as can be seen in model_2_4. The t value for the variable female*sat is 0.397 which is very low compared to 1.96 hence it is statistically insignificant

con <- SQLite() %>% dbConnect('wooldridge.db')
gpa2 <- con %>% dbReadTable('gpa2') %>% data.table
con %>% dbDisconnect
model_2_1 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete, data = gpa2)
summary(model_2_1)
model_2_2 <- lm(colgpa~hsize+I(hsize^2)+hsperc+female+athlete, data = gpa2)
summary(model_2_2)
model_2_3 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+I(female*athlete)+I((1-female)*athlete)+I((1-female)*(1-athlete)),data = gpa2)
summary(model_2_3)
model_2_4 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete+I(female*sat),data = gpa2)
summary(model_2_4)


################################################################################
############################### Question 3.3 ###################################

# Q3.3(i) If there is discrimination against minorities then, with the appropriate factors being controlled for, B1 will be positive

# Q3.3(ii): approve = 0.70779 + 0.20060 * white. The t-value for coefficient of white is high (10.11) hence it is statistically significant. Also a white applicant has 20% more likelihood of being approved hence it is practically large.

# Q3.3(iii) Even though the coefficient of white reduces 0.20060 to 0.128820, reducing it's effect on the approval rate, the t-valye is still high (6.529) which means it is still statistically significant.

# Q3.3(iv) Since the t-value for the interaction variable white*obrat is higher than 1.96 it is statistically significant

# Q3.3(v) 

con <- SQLite() %>% dbConnect('wooldridge.db')
loanapp <- con %>% dbReadTable('loanapp') %>% data.table
con %>% dbDisconnect
model_3_1 <- lm(approve~white, data = loanapp)
summary(model_3_1)
model_3_2 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp)
summary(model_3_2)
model_3_3 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+I(white*obrat), data = loanapp)
summary(model_3_3)
model_3_4 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+I(white*(obrat-32)), data = loanapp)
summary(model_3_4)
confint(model_3_4)


################################################################################
############################### Question 3.4 ###################################

#Q3.4(i): price = -21.7703086 + 0.0020677 * lotsize + 0.1227782 * sqrft + 13.8525219 * bdrms
# from Standard error on using coeftest we get Standard error for lotsize has almost doubled which makes lotsize less significant. t value for sqrft also slightly decreases. Bdrms becomes slightly more significant.

#Q3.4(ii): The difference between the standard errors is comparitively less

#Q3.4(iii): Heteroskadisticity is reduced due to the usage of the log transformation.

con <- SQLite() %>% dbConnect('wooldridge.db')
hprice1 <- con %>% dbReadTable('hprice1') %>% data.table
con %>% dbDisconnect
model_4_1 <- lm(price~lotsize+sqrft+bdrms, data=hprice1)
summary(model_4_1)
coeftest(model_4_1,vcov.=vcovHC)
model_4_2 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice1)
summary(model_4_2)
coeftest(model_4_2,vcov.=vcovHC)


################################################################################
############################### Question 3.5 ###################################

# Q3.5(i) colGPA = 1.35651 + 0.41295*hsGPA + 0.01334*ACT - 0.07103*skipped + 0.12444*PC

# Q3.5(ii): residue2 = -0.321837 + (0.219599 * fit_val) + (0.002946 * fit_val^2). F stat value= 3.581 for 2 and 138 df for a pval= 0.03045. Hetroskedasticity volume = 0.007126816

#3.5(iii): h hat values (h) are all positive. colGPA = 1.401564 + 0.402506*hsGPA + 0.013162*ACT - 0.076365*skipped + 0.126005*PC. R^2 = 0.3062. R-multiple^2 = 0.2858. n= 141. The difference in the Standard errors in the OLS and weighted model are very minimal. PC variable is not as significant as skipped variable having an influence on colGPA

#3.5(iv): Hetroskedasticity volume: 0.007070506. Using whites test, we get robust standard errors which are greater than the weighted least square standard errors

con <- SQLite() %>% dbConnect('wooldridge.db')
gpa1 <- con %>% dbReadTable('gpa1') %>% data.table
con %>% dbDisconnect
model_5_1 <- lm(colGPA~hsGPA+ACT+skipped+PC,gpa1)
summary(model_5_1)
res_error <- residuals(model_5_1)
res_error2 <- res_error^2
ols_se <- tidy(coeftest(model_5_1))[,3]
white_se <- tidy(coeftest(model_5_1,vcov.=vcovHC))[,3]
diff <- ols_se - white_se
sum(diff^2)/sum(ols_se^2)
fit_val <- predict(model_5_1)
model_5_1_fit <- lm(res_error2~fit_val+I(fit_val^2),data=gpa1)
summary(model_5_1_fit)

h_hat <- predict(model_5_1_fit)
summary(h_hat)

model_5_2 <- lm(colGPA~hsGPA+ACT+skipped+PC,weights = 1/h_hat,data = gpa1)
summary(model_5_2)
res_error_2 <- residuals(model_5_2)
res_error2_2 <- res_error_2^2
ols_se_2 <- tidy(coeftest(model_5_2))[,3]
white_se_2 <- tidy(coeftest(model_5_2,vcov.=vcovHC))[,3]
ols_se_2 - white_se_2


################################################################################
############################### Question 3.6 ###################################

# Q3.6.4
data <- fread('PS3_3.6_Data.csv')
summary(data)
ts.plot(cbind(data$BTC,data$EU,data$GOLD,data$OIL,data$SP))

# Q3.6.5
model_6_1 <- lm(BTC~EU+GOLD+OIL+SP, data = data)
summary(model_6_1)
# The r squared seems to be very high 

# Q3.6.6
kpss.test(data$BTC,null="Level")
kpss.test(data$BTC,null="Trend")
kpss.test(diff(data$BTC),null="Level")
kpss.test(diff(data$BTC),null="Trend")
kpss.test(diff(diff(data$BTC)),null="Level")
kpss.test(diff(diff(data$BTC)),null="Trend") #2 for Bitcoin

kpss.test(data$EU,null="Level")
kpss.test(data$EU,null="Trend")
kpss.test(diff(data$EU),null="Level")
kpss.test(diff(data$EU),null="Trend") #1 for EU exchange

kpss.test(data$GOLD,null="Level")
kpss.test(data$GOLD,null="Trend")
kpss.test(diff(data$GOLD),null="Level")
kpss.test(diff(data$GOLD),null="Trend") #1 for Gold bullion

kpss.test(data$OIL,null="Level")
kpss.test(data$OIL,null="Trend")
kpss.test(diff(data$OIL),null="Level")
kpss.test(diff(data$OIL),null="Trend") #1 for oil price

kpss.test(data$SP,null="Level")
kpss.test(data$SP,null="Trend")
kpss.test(diff(data$SP),null="Level")
kpss.test(diff(data$SP),null="Trend") #1 for S&P500

# Q3.6.7
model_6_2 <- lm(diff(BTC)~diff(EU)+diff(GOLD)+diff(OIL)+diff(SP), data = data)
summary(model_6_2)

# Q3.6.8
data$Date <- as.Date(data$Date)
sub_data <- subset(data,data$Date > "2016-12-31") 
sub_data

# Q3.6.9
kpss.test(sub_data$BTC,null="Level")
kpss.test(sub_data$BTC,null="Trend")
kpss.test(diff(sub_data$BTC),null="Level")
kpss.test(diff(sub_data$BTC),null="Trend")

acf(diff(sub_data$BTC))
pacf(diff(sub_data$BTC))

# Q3.6.10
maxa <- 5
outp <- matrix(0L,(maxa+1)^2,3)
ndx <- 1
for(i in 0:maxa){
  print(paste(as.character(round(i*(maxa+1)/(maxa+1)^2*100,digits=2)),'%...',sep=''))
  for(j in 0:maxa) {
    tryCatch({aic<-sub_data$BTC%>%arima(c(i,1,j))%>%AIC},error=function(err){aic<-9999.99})
    outp[ndx,1:3] <- c(i,j,aic)
    ndx <- ndx + 1
  }
}
rm(sub_data$BTC,ndx) 
outp <- data.table(outp)
colnames(outp) <- c('p','q','AIC')
outp[order(AIC)]

model_6_3 <- arima(sub_data$BTC, c(5,1,4))
model_6_3

# Q3.6.11
steps <- 30
future <- forecast(model_6_3,h=steps)
plot(future)

# Q3.6.12
periodogram(sub_data$BTC)
periodogram(diff(sub_data$BTC))
# yes , seasonality in data is seen for the values of diff()

# Q3.6.13
class(sub_data$Date)
sub_data$day <- wday(sub_data$Date)
summary(sub_data$day)

sub_data$sunday[sub_data$day==1]<- 1
sub_data$sunday[sub_data$day!=1]<- 0

sub_data$monday[sub_data$day==2]<- 1
sub_data$monday[sub_data$day!=2]<- 0

sub_data$tuesday[sub_data$day==3]<- 1

sub_data$tuesday[sub_data$day!=3]<- 0

sub_data$wednesday[sub_data$day==4]<- 1
sub_data$wednesday[sub_data$day!=4]<- 0

sub_data$thursday[sub_data$day==5]<- 1
sub_data$thursday[sub_data$day!=5]<- 0

sub_data$friday[sub_data$day==6]<- 1
sub_data$friday[sub_data$day!=6]<- 0

sub_data$saturday[sub_data$day==7]<- 1
sub_data$saturday[sub_data$day!=7]<- 0

model_6_13 <- lm(BTC~monday+tuesday+wednesday+thursday+friday, data= sub_data)
summary(model_6_13)

res_6_13 <- residuals(model_6_13)
summary(res_6_13)

periodogram(res_6_13)

