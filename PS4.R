###########################################################
## Author:   Neetesh Hegde
## Date:     2018-09-05
## Title:    PS4.R
## Purpose:  R Code for Assignment 4 of BUAN 6356 course
###########################################################

rm(list=ls())

#Packages
install.packages("data.table")
install.packages("sandwich")
install.packages("lmtest")
install.packages("tseries")
install.packages("DBI")
install.packages("RSQLite")
install.packages("tidyverse")
install.packages("broom")
install.packages("plm")
install.packages("margins")

#Libraries
library(data.table)
library(sandwich)
library(lmtest) 
library(tseries) 
library(DBI)
library(RSQLite)
library(tidyverse)
library(broom)
library(plm)
library(margins)

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables

################################################################################
############################### Question 4.1 ###################################

hprice <- con %>% dbReadTable('hprice1') %>% data.table
summary(step(lm(price~(bdrms+lotsize+sqrft+colonial)^2+I(bdrms^2)+I(lotsize^2)+I(sqrft^2),data = hprice)))
summary(step(lm(price~(bdrms+lotsize+sqrft+colonial)^2+I(bdrms^2)+I(lotsize^2)+I(sqrft^2),data = hprice), k = log(nrow(hprice))))

#Running the step wise models, we get an optimal model with the best AIC and BIC 
# AIC: 678.46, BIC: 697.58

################################################################################
############################### Question 4.2 ###################################

gpa2 <- con %>% dbReadTable('gpa2') %>% data.table
summary(step(lm(colgpa~(sat+hsize+hsperc+athlete+hsrank+tothrs+female+white)^2+I(sat^2)+I(hsize^2),data = gpa2)))
summary(step(lm(colgpa~(sat+hsize+hsperc+athlete+hsrank+tothrs+female+white)^2+I(sat^2)+I(hsize^2),data = gpa2), k = log(nrow(gpa2))))

#Running the stepwise models, we get an optimal model with the best AIC and BIC values 
# AIC: -5157.94, BIC: -5047.93

################################################################################
############################### Question 4.3 ###################################

mlb <- con %>% dbReadTable('mlb1') %>% data.table
View(mlb)
summary(step(lm(lsalary~(years+nl+games+atbats+runs+hits+doubles+triples+hruns+rbis+bavg+bb+so+sbases+fldperc+scndbase+shrtstop+thrdbase+outfield+catcher+allstar+slugavg+hispan+black)^2, data = mlb)))
summary(step(lm(lsalary~(years+nl+games+atbats+runs+hits+doubles+triples+hruns+rbis+bavg+bb+so+sbases+fldperc+scndbase+shrtstop+thrdbase+outfield+catcher+allstar+slugavg+hispan+black)^2, data = mlb), k = log(nrow(mlb))))

#Running the stepwise models, we get an optimal model with the best AIC and BIC values
# AIC = -692.05, BIC = -7.24

################################################################################
############################### Question 4.4 ###################################

rental <- con %>% dbReadTable('rental') %>% data.table
prental <- pdata.frame(rental, index = c('city','year'))
mr <- plm(lrent~y90+lpop+lavginc+pctstu,model="pooling",data=prental) 
summary(mr)
mr_diff <- plm(lrent~y90+lpop+lavginc+pctstu,model="fd",data=prental)
summary(mr_diff)
mr1 <- plm(lrent~y90+lpop+lavginc+pctstu,model="within",data=prental) 
summary(mr1)
mr_diff1 <- lm(diff(lrent)~diff(lpop)+diff(lavginc)+diff(pctstu),data=prental)
summary(mr_diff1)

#Q4.4(i): log(rent) = -0.568 + 0.262*Y90t + 0.04*log(pop)+0.571*log(avginc)+0.005*pctstu
# R-squared: 0.861
#The estimate on the 1990 dumy variable is 0.262, i.e. through the years, the rent has increased by 26% (approx.)
# The variable is highly significant. 
#pcstu coefficient: 0.005 -  A 1% increase in the pcstu causes the rent to increase by 0.5%. 
#Q4.4(ii): The standard errors reported in part (i) may not be valid as the ai term is not in the model and this may be part of the standard error term
#Q4.4(iii): The coefficient on pctstu is much higher after the first differencing and it is also statistically significant.
#Relative size of the student population appears to affect the rental prices
#Q4.4(iv): After running the fixed effects model, we see that we are getting idetical estimates and standard errors to those in part (iii)

################################################################################
############################### Question 4.5 ###################################

murder <- con %>% dbReadTable('murder') %>% data.table
pmurder <- pdata.frame(murder, index = c('state','year'))
model_murder <- plm(mrdrte~as.factor(year)+exec+unem,model="pooling",data = pmurder %>% subset(year == 90 | year == 93))
summary(model_murder)
model_murder_1 <- plm(mrdrte~as.factor(year)+exec+unem,model="fd",data = pmurder %>% subset(year == 90 | year == 93))
summary(model_murder_1)
coeftest(model_murder) 
coeftest(model_murder,vcov.=vcovHC)
ols_se_murder <- tidy(coeftest(model_murder))[,3]
white_se_murder <- tidy(coeftest(model_murder,vcov.=vcovHC))[,3]
diff_murder <- ols_se_murder - white_se_murder
vol_hetero_murder <- sum(diff_murder^2)/sum(ols_se_murder^2)
vol_hetero_murder
murder[year == 93][order(exec)]
murder_drop_TX <- murder[murder$state!="TX"]
pmurder_drop_TX <- pdata.frame(murder_drop_TX, index = c('state','year'))
model_murder_drop_TX <- plm(mrdrte~as.factor(year)+exec+unem,model="fd",data = pmurder_drop_TX %>% subset(year == 90 | year == 93))
summary(model_murder_drop_TX)
coeftest(model_murder_drop_TX) 
coeftest(model_murder_drop_TX,vcov.=vcovHC)
ols_se_murder_drop_TX <- tidy(coeftest(model_murder_drop_TX))[,3]
white_se_murder_drop_TX <- tidy(coeftest(model_murder_drop_TX,vcov.=vcovHC))[,3]
diff_murder_drop_TX <- ols_se_murder_drop_TX - white_se_murder_drop_TX
vol_hetero_murder_drop_TX <- sum(diff_murder_drop_TX^2)/sum(ols_se_murder_drop_TX^2)
vol_hetero_murder_drop_TX
model_murder_2 <- plm(mrdrte~as.factor(year)+exec+unem,model="within",data = pmurder)
summary(model_murder_2)
model_murder_3 <- plm(mrdrte~as.factor(year)+exec+unem,model="within",data = pmurder %>% subset(year == 90 | year == 93))
summary(model_murder_3)
                      
#Q4.5(i): b1 should have a negative sign as executions is expected to lower the murder rates over time.
# b2 should have a positive sign as unemployment is expected to get the murder rates higher. 
#Q4.5(ii): There is no evidence of a deterrent effect after running the model by pooled OLS 
#Q4.5(iii): After first differencing, there is evidence of a deterrent effect for execution 
# and the effect is not very strong 
#Q4.5(iv): The heteroskedasticity-robut standard error for part (ii) is 0.224 
#Q4.5(v): The state with the highest number of executions in 1993 was Texas and it is much higher than the next highest value that is 11. 
#Q4.5(vi): After dropping Texas from the analysis, we see that effect of exec on mrdte has decreased as TX was the state with the highest number 
# of executions. After computing for heteroskedasticity, we see that it has drastically reduced after dropping Texas. 
#Q4.5(vii): After adding all the year variables as well as Texas to the model, we see that the effect of exec on the mrdrte
# has increased that is it has become more negative as compared to that of just 1990 and 1993 
# The size of the deterrent effect is higher in the case with all the year variables and the coefficient on the variables are not very significant

################################################################################
############################### Question 4.6 ###################################

airfare <- con %>% dbReadTable('airfare') %>% data.table
pairfare <- pdata.frame(airfare, index = c('id','year'))
model_airfare <- plm(log(fare)~as.factor(year)+concen+ldist+ldistsq,model="pooling",data = pairfare)
summary(model_airfare)
confint(model_airfare)
coeftest(model_airfare) 
coeftest(model_airfare,vcov.=vcovHC)  
confint(model_airfare,vcov.=vcovHC)
summary(airfare)
airfare_deriv <- expression(6.209 + 0.0211*y98 + 0.037*y99 + 0.099*y00 +0.36*concen - 0.901*ldist + 0.103*ldist^2)
airfare_deriv1 <- D(airfare_deriv, 'ldist')
airfare_deriv1
model_airfare_1 <- plm(log(fare)~as.factor(year)+concen+ldist+ldistsq,model="within",data = pairfare)
summary(model_airfare_1)

#Q4.6(i): D(log(fare))/D(concen) = b1 
# D(log(fare)) = 0.3011 * 0.1 = 0.03011, which roughly means that for D(concen) = 0.10, we get a change of 3% in the fare. 
#Q4.6(ii): The confidence interval for concen is 0.3011 and 0.419 
# It is probably not reliable because of the presence of heteroskedasticity 
# After computing the robust errors, we see that the 95% CI is much different in this case
#Q4.6(iii): The quadratic in log(dist) is a positive value, which shows that as the dist increases, there is not a diminishing 
# effect on the fares and in fact, it increases. 
# After taking derivative, we see that for ldist value of 4.37, the relationship between log(fare) and dist becomes positive. 
# The turning point is outside the range of the data. 
#Q4.6(iv): After running the model using fixed effects, the coefficient on concen is 0.1688, which means that for a unit increase in the 
# concentration of the route, the fare increases by 16.88% 
#Q4.6(v): The other  characteristics of a route that are captured by ai would be the time taken to travel between the stops which may not be 
# correlated with the concen variable and the other would be whether or not there is turbulence on that route, that could be 
# correlated with the concen variable 
#Q4.6(vi): It is convincing from the above models that a higher concentration on a route increases the airfare of the route. 

################################################################################
############################### Question 4.7 ###################################

loanapp <- con %>% dbReadTable('loanapp') %>% data.table
model_loanapp <- glm(approve~white,family=binomial,data=loanapp)
summary(model_loanapp)
data_white <- data.frame(white = 1)
data_non_white <- data.frame(white = 0)
predict(model_loanapp, data_white, type = "response")
predict(model_loanapp, data_non_white, type = "response")
model_loanapp_lpm <- lm(approve~white,data=loanapp)
summary(model_loanapp_lpm)
predict(model_loanapp_lpm, data_white)
predict(model_loanapp_lpm, data_non_white)
model_loanapp_1 <- glm(approve~white+hrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family=binomial,data=loanapp)
summary(model_loanapp_1)

#Q4.7(i): After running the logit as well as the lpm models, we find that the probability of loan approval for whites and non-whites
# is not very different. They are almost the same in this case. 
#Q4.7(ii): After runnign the model with all the variables added, we find that the being a white does cause the likelihood of being 
# approved to increase and in this case, the coefficient on white is statistically significant and this shows that 
# there is statistically significant evidence of discrimination against nonwhites. 

################################################################################
############################### Question 4.8 ###################################

alcohol <- con %>% dbReadTable('alcohol') %>% data.table
length(which(alcohol$employ==1))/nrow(alcohol)
length(which(alcohol$abuse==1))/nrow(alcohol)
model_alcohol_1 <- lm(employ~abuse, data = alcohol)
coeftest(model_alcohol_1) 
coeftest(model_alcohol_1,vcov.=vcovHC)
model_alcohol_2 <- glm(employ~abuse,family=binomial, data = alcohol)
coeftest(model_alcohol_2) 
data_abuse <- data.frame(abuse = 1)
data_non_abuse <- data.frame(abuse = 0)
margins(model_alcohol_2)
margins(model_alcohol_1)
predict(model_alcohol_1, data_abuse)
predict(model_alcohol_1, data_non_abuse)
predict(model_alcohol_2, data_abuse,type="response")
predict(model_alcohol_2, data_non_abuse, type = "response")
model_alcohol_3 <- lm(employ~abuse+age+agesq+educ+educsq+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, data = alcohol)
coeftest(model_alcohol_3) 
coeftest(model_alcohol_3,vcov.=vcovHC)
model_alcohol_4 <- glm(employ~abuse+age+agesq+educ+educsq+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3,family=binomial, data = alcohol)
coeftest(model_alcohol_4) 
coeftest(model_alcohol_4,vcov.=vcovHC)
margins(model_alcohol_3)
margins(model_alcohol_4)

#Q4.8(i): Fraction of the sample empolyed : 0.898
# Fraction of sample abused alchohol : 0.0991
#Q4.8(ii): As per the estimated regression equation, a person who has abused alchohol, the chances of him being employed decreases by a value of 0.028. 
# This is the expected relationship. It is statistically significant at 5% significance and not at 1% significance. 
#Q4.8(iii): After running the glm logit model, we get the same sign and a higher statistical significance. The average marginal effect for the logit is 
# almost the same as that for the linear model 
#Q4.8(iv): After running the fitted models, when abuse = 0 and abuse = 1, we get the same values for both the logit and linear models and this is because 
# both the bariables are binary, and they get the same prediction for this reason. 
#Q4.8(v): The coefficient on abuse is almost the same as the previous models, but it is not statistically significant at the 5% significance level 
#Q4.8(vi): The estimated marginal effect of abuse on employ for a logit model is not the same as that for the linear model. They are close. 
#Q4.8(vii): The health variable should be included as a control as that would be a major factor in deciding whether the person would be employed or not
# as a person with overall good health would be in a better shape to get employed than one who is not.
#Q4.8(viii): Abuse could be thought of as endogenous in the employ equation as it may be correlated with one of those terms. 
# mothalc and fathalc are sensible intrumental variables for abuse. 


################################################################################
############################### Question 4.9 ###################################

fertil1 <- con %>% dbReadTable('fertil1') %>% data.table
model_fertil1_1 <- glm(kids~educ+age+agesq+black+east+northcen+west+farm+othrural+town+smcity+y74+y76+y78+y80+y82+y84,family=poisson, data = fertil1)
summary(model_fertil1_1)
exp(0.36)-1
fertil1_predict_model <- predict.glm(model_fertil1_1)
corr <- cor(fertil1$kids,fertil1_predict_model)
corrsqr <- corr^2
corrsqr
fertil1_model_lm <- lm(kids~educ+age+agesq+black+east+northcen+west+farm+othrural+town+smcity+y74+y76+y78+y80+y82+y84, data=fertil1)
summary(fertil1_model_lm) 

#Q4.9(i): The coefficient on y82 states that on an average, a woman is 19.2% less fertile than a woman in 1972. 
#Q4.9(ii): The percentage difference in fertility between a black woman and a non-black woman is 43.3%. 
#Q4.9(iii): The square of the correlation value is close to the R-squared value of the linear regression model. 



#################################################################################
#################################################################################

