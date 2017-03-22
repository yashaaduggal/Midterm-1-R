# Assume that you are preparing the data for analysis that 
# will be used to report on the most dangerous places to work in Massachusetts

install.packages("foreign")
install.packages('data.table')

# Load required packages for project
# Load foreign package open DBF files
require(foreign)

# Load data.table to use data.tables
require(data.table)

# Set working directory
# setwd("~/Desktop/Midterm_Project_R/MA615-midterm-spring-17")

# read in all files
accid <- data.table(read.dbf('accid.DBF')) # details about accident victims
hazsub <- data.table(read.dbf('hazsub.DBF')) # hazardous substances involved
viol <- data.table(read.dbf('viol.DBF')) #violations from each inspection
osha <- data.table(read.dbf('osha.DBF')) # main table with company, address, date of inspection, etc.
admpay <- data.table(read.dbf('admpay.DBF')) # a record of collecting administrative fees/penalties


# number of companies
length(levels(osha$ESTABNAME))


# see what columns are given
colnames(osha)
osha

# explore given information
table(osha$SFTYCONST)
table(osha$SAFETYMANF)
table(osha$VIOLS_)
table(osha$EVENT_)
table(osha$HAZSUB_)
table(osha$ADMPAY_)

#load dplyr and tidyr
library(dplyr)
library(tidyr)

# I noticed that activity number overlaps between some of the files, 
# so I will be able to map the files by activity number

# I will parse down the osha file just to contain matching activity numbers from the accident file

# make a map between company names and activity number
estab.activity <- osha %>% select(ACTIVITYNO, ESTABNAME)


# add establishment name to accident reports/violations/adminpay/hazsub

osha 

osha.accid <- data.table(osha[osha$ACTIVITYNO %in% accid$ACTIVITYNO])
osha.viol <- data.table(osha[osha$ACTIVITYNO %in% viol$ACTIVITYNO])
osha.hazsub <- data.table(osha[osha$ACTIVITYNO %in% hazsub$ACTIVITYNO])
osha.admpay <- data.table(osha[osha$ACTIVITYNO %in% admpay$ACTIVITYNO])

## see if overlap between accid and hazsub since they are smallest
table(osha.accid$ACTIVITYNO %in% osha.hazsub$ACTIVITYNO)
# only 24 overlap

table(osha.accid$ACTIVITYNO %in% osha.admpay$ACTIVITYNO)
# 1061 overlap between these two .. accident and adminstrative payments

head(accid)
head(viol)

# SELECT COLUMNS THAT SOUND INTERESTING TOWARDS VIOLENT WORK SITES
viol <- viol %>% select(ACTIVITYNO, VIOLTYPE, VIOLTYPEA, STD_LOOKUP)
accid <- accid %>% select(ACTIVITYNO,
                          SITESTATE,
                          SEX,
                          DEGREE,
                          NATURE,
                          BODYPART,
                          SOURCE,
                          EVENT,
                          ENVIRON,
                          HUMAN,
                          TASK,
                          AGE,
                          OCC_CODE)

osha <- osha %>% select(ACTIVITYNO,
                        REPORTID,
                        ESTABNAME,
                        INSPTYPE,
                        INSPSCOPE,
                        EMPCOUNT,
                        EMPCOVERED,
                        UNION,
                        TOTPENLTY,
                        TOTALFTA, 
                        TOTALVIOLS,
                        TOTSERIOUS,
                        DEBT_,
                        VIOLS_,
                        EVENT_,
                        HAZSUB_,
                        ACCID_,
                        ADMPAY_,
                        SITEZIP,
                        SITECITY,
                        SITECNTY)

std <- data.table(read.dbf("./lookups/STD.dbf"))


# check overlap between activity numbers of hazsub and accid
table(accid$ACTIVITYNO %in% viol$ACTIVITYNO)

# 1604 overlap


# now lets grab some data from the violations

accid.osha <- accid %>% inner_join(osha, by="ACTIVITYNO")  %>% select(ESTABNAME, everything()) %>% mutate(DEGREE=as.numeric(as.character(DEGREE))) %>% data.table()

# we have a dataset accid.osha -- that has 2147 rows and 33 columns, but some of these still have missing data ...

# what about accidents where the incident was fatal?
accid.dead <- accid.osha[DEGREE==1]


accid.dead[max(accid.dead$TOTALVIOLS)]
length(accid.dead[DEGREE==1])


# check
class(accid.osha$DEGREE)




accid.osha.narm <- accid.osha %>% na.omit() %>% select(ESTABNAME, everything()) %>% data.table()

# after removing NAs we check the dimensions
dim(accid.osha.narm)

# we have 1416 rows now ... so that means we lost 731 observations by removing NAs 
# .... some of this could have been interesting data ... 

Degrees
# 1 - fatal
# 2 - hospitalized
# 3 - non-hospitalized

class(accid.osha$DEGREE)
table(accid.osha$DEGREE)

# lets see if we can see what most dangerous company to work for is
table(accid.osha$EVENT) # Event type ... classification of different ways get to injured

# The most common way to get injured is by fall from elevation with 554 injured. 

# total violations looks interesting... lets see how the range of violations
table(accid.osha$TOTALVIOLS)
range(accid.osha$TOTALVIOLS)

accid.osha[max(accid.osha$TOTALVIOLS)]$ESTABNAME
# [1] MODERN CONTINENTAL CONSTRUCTIO have the largest number of violations in a data set where NA's werent removed and [1] BATTERY ENGINEERING, INCORPORA has 
# the largest number of violations in a clean dataset(W/O NA's)

# total penalities ... who paid the most
table(accid.osha$TOTPENLTY)

accid.osha <- data.table(accid.osha)
accid.osha[accid.osha$TOTPENLTY==max(accid.osha$TOTPENLTY)]

accid.osha[accid.osha$TOTPENLTY == 210000]
# J.M. CASHMAN, INCORPORATED - unkown degree, but paid $210000 in a clean data set (Without NA's) where we can see that this company also had 11 violations. 

## Graphics
# distribution of total penalities across all companies
boxplot(log(accid.osha$TOTPENLTY+0.01,base=10),
        main="Boxplot of OSHA Penalities by Company")

# here we can see the outliers where we know that companies with much higer penalities do exist. What compaies are those?

# lets find which companies have total penalty > 4
accid.osha[log10(accid.osha$TOTPENLTY+0.01) > 4.5,]
accid.osha[accid.osha$TOTPENLTY > 190000]

# We can see that the top three companies with the greatest penalities known are J.M. CASHMAN, INCORPORATED, SAUGUS CONSTRUCTION COMPANY, and 
# KIEWIT/ATKINSON/KENNY, A JOINT, two of which teh degree of injury was 1, meaning fatal. 

# sort by amount paid by company
accid.osha <- accid.osha[order(accid.osha$TOTPENLTY, decreasing = T)]
head(accid.osha)$ESTABNAME

# average penalty a company pays
mean(accid.osha$TOTPENLTY)
# [1] 5905.371 versus say how much a company like J.M. CASHMAN, INCORPORATED pays. 

# plot number of occurrences
hist(accid.osha$TOTALVIOLS,
     main="Histogram of Accident Violations by Company",
     col="gray")

#install.packages("ggplot2")
library(ggplot2)
qplot(accid.osha$AGE, 
      accid.osha$TOTALVIOLS,
      xlab = "Age",
      ylab="Total Violations",
      main="Age vs. Total Violations")


table(accid.osha$SITECITY)
table(osha.viol$ESTABNAME)
table(accid.osha.narm$TOTPENLTY)

# We are able to specify compaines overall that have the greatest penalties and violations where most of the dangerous scenarios take place in Massachusettes. 
# As an example, as previously discussed... J.M. CASHMAN, INCORPORATED. 



#Osha.dbf - main table with company name, address, date of inspection, etc. 
#If you get the entire country, there's a number after the 
#word OSHA, since it's too big to put on one CD.
#Viol.dbf - violations from each inspection. If you get the entire 
#country, there's a number after the word VIOL, since it's 
#too big to put on one CD.
#Accid.dbf - details about accident victims
#Hazsub.dbf - hazardous substances involved
#Debt.dbf - status of debt
#History.dbf - outlines a history of any changes in penalty
#Admpay.dbf - a record of collecting administrative fees or penalties
#Prog.dbf - special programs the inspection might be involved in
#Relact.dbf - whether the inspection is related to another inspection or other action
#Optinfo.dbf - optional information



