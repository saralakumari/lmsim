####
# summarizes the sample according to
# various dimensions of the sampling frame:
# urban/town/rural, ethnic-Mongol/not, Mongolian-speaking/not
# writes 2 tables, one for current and one for childhood residence
####

rm(list=ls())
library(dplyr)
library(tidyr)

# read in data
source("inread.r")
almost_everything <- inread('data/2015_lmsim_data.csv') # demographics
cp <- inread("data/mbc_kid_cp.csv") # language

# language variables
lg <- cp %>% select(Subject_Code, self.MBC) 
lg$plusM <- (lg$self.MBC=="M" | lg$self.MBC=="B")

# combine language, ethnicity and residence data
df <- almost_everything %>% 
		select(Subject_Code, Urban_Status.x, Urban_Status.y, Ethnicity) %>% 
		inner_join(lg, by = "Subject_Code")
df$Mongol <- df$Ethnicity=="MONGOL"
df$Ethnicity <- NULL
df$self.MBC <- NULL

# refine residence variable
orderXZC <- function(myvar){
  x <- factor(myvar, 
  		levels = c("XIANG_RURAL","ZHEN_TOWN","CHENG_CITY","UNKNOWN"),
        labels=c("Rural","Town","Urban","Unknown"), ordered=TRUE)
  return(x)
}
df$Urban_Status.x <- orderXZC(df$Urban_Status.x)
df$Urban_Status.y <- orderXZC(df$Urban_Status.y)

# make contingency tables for each time point
then <- ftable(df, 
		row.vars = "Mongol", 
		col.vars = c("plusM", "Urban_Status.y"))

now <- ftable(df, 
		row.vars = "Mongol", 
		col.vars = c("plusM", "Urban_Status.x"))

# write out
write.ftable(then, file="analysis/sampling_gps_childhood.txt")
write.ftable(now, file="analysis/sampling_gps_current.txt")
