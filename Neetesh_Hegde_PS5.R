###########################################################
## Author:   Neetesh Hegde
## Date:     2018-11-13
## Title:    Neetesh_Hegde_PS5.R
## Purpose:  R Code for Assignment 5 of BUAN 6356 course
###########################################################

rm(list=ls())

#call the required packages
library(data.table)
library(dplyr)
library(partykit)
library(plm)

n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

data_table <- merge(dt1    ,dt2, all=TRUE)
data_table <- merge(data_table ,dt3, all=TRUE)
data_table$group <- as.factor(data_table$group)
data_table$iid <- 1:1500

pooled_model <- lm(sat~income,data=data_table)
within_model <- lm(sat~income+group-1,data=data_table)
model1 <- lm(sat~income,data=data_table[group==1])
model2 <- lm(sat~income,data=data_table[group==2])
model3 <- lm(sat~income,data=data_table[group==3])

summary(pooled_model)
summary(within_model)
summary(model1)
summary(model2)
summary(model3)

ptable <- pdata.frame(data_table,index=c('group','iid'))
lm(sat~income+group-1,data=data_table)


###################################################################################
############################### Question 5.1(a) ###################################

# Random number sequence is generated and then the variable, z is generated which is normally distributed based on the random set of generated. Based on z values, we set the y variable as the sat value and the x variable as the income. Using z variable, the income has been generated separate for each group. The 3 tables each with different groups are  merged together in order to create a table with all the groups and to spread the data evenly in order to make right inferences after modelling the data. 

###################################################################################
############################### Question 5.1(b) ###################################

# When we run the pooled OLS model, we get a positive value for income. We are getting the value as whole for all 3 groups combined. When we run the within model, we are grouping the model based on the different groups, in which the parameters are fixed and hence, we are ontaining the parameters different from that of the pooled OLS model. When we run the model for the 3 different groups. the incomes vary for each group and hence we get a different result in that case as well. 

###################################################################################
############################### Question 5.1(c) ###################################

#As per the above recursive partitioning models, we can understand how the data is spread and based on the 3 different models, we can understand the for each node, the distribution of the data that is, the boxplot figure accurately shows the variance of the data.

tree1 <- ctree(sat~income,data=data_table)
tree1 %>% plot
tree2 <- ctree(sat~group,data=data_table)
tree2 %>% plot
tree3 <- ctree(sat~income+group,data=data_table)
tree3 %>% plot

###################################################################################
############################### Question 5.1(d) ###################################

glmtree(sat~income,data=data_table)
glmtree(sat~income|group,data=data_table)
glmtree(sat~income+group,data=data_table)
glmtree(sat~group|income,data=data_table)

#tree model with income, income & group are having multiple nodes. Hence, they are over fit models. Model with groups seems to have nodes better to model for the data.

###################################################################################
############################### Question 5.1(e) ###################################

set.seed(5)
runif(10)

kmeans.wss <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  return(wss)
}
eratio <- function(wss) {
  n <- NROW(wss)
  dss <- -diff(wss) 
  dss <- c(wss[1]/log(n),dss) 
  erat <- dss[1:(n-1)]/dss[2:n] 
  gss <- log(1+dss/wss)
  grat <- gss[1:(n-1)]/gss[2:n] 
  return(c(which.max(erat),which.max(grat))) 
}
plot.wss <- function(wss) {
  plot(1:NROW(wss), wss, type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}
dtable_value <- data_table[,-c(1,4:5)]
dtable_data <- data_table[,-c(1,4:5)]
wss <- dtable_value %>% kmeans.wss
plot.wss(wss)
eratio(wss)

# Knowing the correct number of clusters to use, run the model
set.seed(1)
model <- kmeans(scale(dtable_value),centers=2,nstart=10)
model$centers
groupsk <- model$cluster
groupsk
table(groupsk)

#As per the model, the optimal number of groups we get using the k-means estimation is 2.

###################################################################################
############################### Question 5.1(f) ###################################

hclust.wss <- function(data,model=hclust(dist(data)),maxclu=10) {
 
  wss <- rep(NA,maxclu)
  for(i in 1:maxclu){
    gps <- cutree(model,i) 
    means <- data[,lapply(.SD,mean),by=gps] 
    demeaned <- data-means[gps,2:(ncol(data)+1)] 
    wss[i] <- sum(demeaned^2) 
  }
  return(wss)
}

# Main hierarchical clustering approach
wss <- dtable_value %>% hclust.wss
plot.wss(wss)
eratio(wss)

# Alternative choices for distances between clusters
plot.wss(hclust.wss(dtable_value,model=hclust(dist(dtable_value),method="complete")))
plot.wss(hclust.wss(dtable_value,model=hclust(dist(dtable_value),method="average")))
plot.wss(hclust.wss(dtable_value,model=hclust(dist(dtable_value),method="median")))
plot.wss(hclust.wss(dtable_value,model=hclust(dist(dtable_value),method="ward.D")))
plot.wss(hclust.wss(dtable_value,model=hclust(dist(dtable_value),method="ward.D2")))
plot.wss(hclust.wss(dtable_value,model=hclust(dist(dtable_value),method="mcquitty")))
plot.wss(hclust.wss(dtable_value,model=hclust(dist(dtable_value)^2,method="centroid")))

# Model dendrogram
model <- hclust(dist(dtable_value))
plot(model)
rect.hclust(model,k=7,border="red")
rect.hclust(model,k=6,border="purple")
rect.hclust(model,k=5,border="blue")
rect.hclust(model,k=4,border="green")
rect.hclust(model,k=3,border="yellow")
rect.hclust(model,k=2,border="orange")
cutree(model,k=4)  # Finding clusters from the model

#We are not able to often identify the cluster of data correctly as we are getting only 2 clusters in this case. K-means does not do a good job here. 
#When we use heirarchical clustering, we are getting 4 cluster points, which in this case would produce a better fit. 

###################################################################################
############################### Question 5.1(g) ###################################

dtable_value$kmean <- kmeans(dtable_value,centers=2,nstart=10)$cluster
dtable_value$hier <- cutree(hclust(dist(dtable_value)),k=4)
dtable_value[order(kmean),lapply(.SD,mean),by=kmean] 
dtable_value[order(hier),lapply(.SD,mean),by=hier]
table(dtable_value$hier,dtable_value$kmean) 

# Create dummies
dtable_value$kmean1 <- as.numeric(dtable_value$kmean==1)
dtable_value$kmean2 <- as.numeric(dtable_value$kmean==2)
dtable_value$hier1 <- as.numeric(dtable_value$hier==1)
dtable_value$hier2 <- as.numeric(dtable_value$hier==2)
dtable_value$hier3 <- as.numeric(dtable_value$hier==3)
dtable_value$hier4 <- as.numeric(dtable_value$hier==4)

# k-means clustering models
# Pooled:
lm(sat~income,data=dtable_value) %>% summary
# Within:
lm(sat~as.factor(kmean)+income,data=dtable_value) %>% summary

# hierarchical clustering models
# Pooled:
lm(sat~income,data=dtable_value) %>% summary
# Within:
lm(sat~as.factor(hier)+income,data=dtable_value) %>% summary

#In the case of the pooled model for k-means clustering, we are getting similar results alongside the groups variable.For the within groups model, we are able to find the relationships that exist from the data generating process. 

###################################################################################
############################### Question 5.1(h) ###################################

wss_income <- dtable_value$income %>% kmeans.wss
plot.wss(wss_income)
eratio(wss_income)

set.seed(1)
model1 <- kmeans(scale(dtable_value),centers=3,nstart=10)
model1$centers
groupsk1 <- model1$cluster
groupsk1
table(groupsk1)

#When we run the k-means estimation model using only the income variable, we get 3 cluster variables. The estimation in this case is much more accurate as we have 3 groups and we are getting 3 different clusters. We would be able to accurately check based on the groups, which value goes to which cluster. 

###################################################################################
############################### Question 5.1(i) ###################################

wss_scale <- dtable_data %>% scale %>% kmeans.wss
plot.wss(wss_scale)
eratio(wss_scale)

set.seed(1)
model1 <- kmeans(scale(dtable_data),centers=3,nstart=10)
model1$centers
groupsk1 <- model1$cluster
groupsk1
table(groupsk1)

#After scaling, we are getting a much better estimation as we are getting 3 clusters and the number of variables in each cluster is more or less spread out equally, thus, showing more accurate results. 