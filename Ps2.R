###########################################################
## Author:   Neetesh Hegde
## Date:     2018-09-05
## Title:    PS2.R
## Purpose:  R Code for Assignment 2 of BUAN 6356 course
###########################################################

rm(list=ls()) #drop all variables

#call the required packages and set the working directory
library(data.table)
library(DBI)           
library(RSQLite)
library(tidyverse)
setwd("D:/MSBA/Fall 18/BUAN 6356 Business Analytics with R/Lec1")

###########################################################

##2.1
## Read the VOTE1.RAW table data into the variable vote1
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
vote1 <- con %>% dbReadTable('vote1') %>% data.table
con %>% dbReadTable('vote1_labels')
con %>% dbDisconnect

## Data description from vote1_labels
#    index variable.name  type format                  variable.label
# 1      0         state  str2    %9s               state postal code
# 2      1      district  byte  %3.0f          congressional district
# 3      2        democA  byte  %3.2f             =1 if A is democrat
# 4      3         voteA  byte  %5.2f              percent vote for A
# 5      4       expendA float  %8.2f     camp. expends. by A, $1000s
# 6      5       expendB float  %8.2f     camp. expends. by B, $1000s
# 7      6      prtystrA  byte  %5.2f            % vote for president
# 8      7      lexpendA float  %9.0g                    log(expendA)
# 9      8      lexpendB float  %9.0g                    log(expendB)
# 10     9        shareA float  %5.2f 100*(expendA/(expendA+expendB))

#(i) B1 is the number of percentage points by which the percentage of votes for A changes for a 1% change in the campaign expenditure of A, holding B's campaign expenditure and party strength for A constant

#(ii) 0 = n B1 + n B2 -> B1 + B2 = 0
# modify model to change variables but keep r squared same.
# B1 + B2 = Theta1 = 0
# voteA = B0 + (theta1 - B2) (log(expendA)) + (B2)(log(expendB)) + B3 partystrA
# voteA = B0 + Theta1 (log(expendA)) + B2[log(expendB)-log(expendA)] + B3 partystrA

summary(lm(voteA~log(expendA)+I(log(expendB)-log(expendA))+prtystrA,data=vote1))

#t is more than -1.96 and below 1.96 hence hypothesis is correct

confint(lm(voteA~log(expendA)+I(log(expendB)-log(expendA))+prtystrA,data=vote1))

#(iii)
  model_1_3 <- lm(voteA~log(expendA)+log(expendB)+prtystrA,data=vote1)
  summary(model_1_3)
  # voteA = 45.08788 + 6.08136*log(expendA) - 6.61563*log(expendB) + 0.15201*prtystrA + u
  # A's campaign expenditure has a positive imapact on the vote percent for A. Every 1% increase in expenditure by A will result in 6.08 percentage point increase in vote percent for A, holding B's expenditure constant
  # B's campaign expenditure has a negative impact on the vote percent for A. Every 1% increase in expenditure by B will result in 6.61 percentage point decrease in vote percent for A, holding A's expenditure constant
#Theta1 = 6.08136-6.61563 = -0.53427
  
#(iv)
  
###########################################################################################
###########################################################################################

##2.2

#(i)
  
#(ii)

#(iii)
  
#(iv)
  
###########################################################################################
###########################################################################################
  
##2.3
  
#(i)
  
#(ii)
  
#(iii)
  
###########################################################################################
###########################################################################################
  
##2.4
  
#(i)
  
#(ii)
  
###########################################################################################
###########################################################################################
  
##2.5
## Read the 401KSUbS.RAW table data into the variable subs401k
con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
subs401k <- con %>% dbReadTable('401ksubs') %>% data.table
con %>% dbReadTable('401ksubs_labels')
con %>% dbDisconnect
  
## Data description from 401ksubs_labels
#    index variable.name  type format               variable.label
# 1      0         e401k  byte  %9.0g     =1 if eligble for 401(k)
# 2      1           inc float  %9.0g        annual income, $1000s
# 3      2          marr  byte  %9.0g                =1 if married
# 4      3          male  byte  %9.0g        =1 if male respondent
# 5      4           age  byte  %9.0g                     in years
# 6      5         fsize  byte  %9.0g                  family size
# 7      6        nettfa float  %9.0g net total fin. assets, $1000
# 8      7         p401k  byte  %9.0g  =1 if participate in 401(k)
# 9      8          pira  byte  %9.0g               =1 if have IRA
# 10     9         incsq float  %9.0g                        inc^2
# 11    10         agesq   int  %9.0g                        age^2

#(i)
count_sph <- sum(as.numeric(subs401k$fsize==1))
count_sph
# There are 2017 single-person households

#(ii)
model_5_2 <- lm(nettfa ~ inc + age, data=subs401k,subset = (fsize==1))
summary(model_5_2)
#nettfa = -43.03981 + 0.79932*inc + 0.84266*age + u

#(iii)
