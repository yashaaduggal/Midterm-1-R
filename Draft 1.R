install.packages("foreign")
require(foreign)
library(foreign)

# set working directory

# install data.table
install.packages("data.table")

# load data.table
require(data.table)

# read in all files

accid <- data.table(read.dbf("accid.DBF")) # details about accident victims
hazsub <- data.table(read.dbf("hazsub.DBF")) # hazardous substances involved
viol <- data.table(read.dbf("viol.DBF")) # volations from each inspection
osha <- data.table(read.dbf("osha.DBF")) # main table with company, address, date of inspectoin, etc.
admpay <- data.table(read.dbf("admpay.DBF")) # a record of collection adminitrative fees/penalties

# number of companies
length(levels(osha$ESTABNAME))

colnames(osha)
osha$
  osha

table(osha$SFTYCONST)
table(osha$SAFETYMANF)
table(osha$VIOLS_)
table(osha$EVENT_)
table(osha$HAZSUB_)
table(osha$ADMPAY_)

library(dplyr)
library(tidyr)

## I noticed that activity number overlaps between some of the files, so I will be able to map the
# files by activity number

# make map between company names and activity number
estab.activity <- osha %>% select(ACTIVITYNO, ESTABNAME)

head(accid)
head(viol)

# Selected columns that sounded interesting.
viol <- viol %>% select(ACTIVITYNO, SITESTATE, VIOLTYPE, VIOLTYPEA, STD_LOOKUP)
accid <- accid %>% select(ACTIVITYNO, SITESTATE, SEX, DEGREE, NATURE, BODYPART, SOURCE, EVENT, ENVIRON, HUMAN, TASK, AGE, OCC_CODE)


# check overlap between activity numbers of hazsub and accid
table(accid$ACTIVITYNO %in% viol$ACTIVITYNO)
nrow(accid.hazsub)

# getting data from violations

accid.vio <- accid.hazsub %>% inner_join(viol, by="ACTIVITYNO")

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
                        ADMPAY,
                        SITEZIP,
                        SITECITY,
                        SITECNTY)

std <- data.table(read.dbf("./lookups/STD.dbf"))

# check overlap between activity numbers of hazsub and accid
table(accid$ACTIVITYNO %in% viol$ACTIVITYNO)

accid.osha <- accid %>% inner_join(osha, by="ACTIVITYNO")

acciaccid.osha.narm <- accid.osha %>% na.omit() %>% select(ESTABNAME, everything())                      

class(accid.osha.narm$DEGREE)
#convert to numeric


#lookups <- list.files(path="~/Desktop/Midterm Project R/MA615-midterm-spring-17/", pattern=".dbf")
#lookups <- lapply(lookup.files, read.dbf)
