###
#  transition matrix analysis
#  predictor: child's age cohort
#  6 tables for 6 subsets of data
###

rm(list=ls())
library(dplyr)
library(tidyr)
source("inread.r")

# read in data
df <- inread("data/mbc_kid_cp.csv") # language
Subjects <- inread("data/2015_lmsim_data.csv") # demographics
# join birthyear data to language data
df <- Subjects %>% 
		select(Subject_Code, Birthyear) %>% 
		right_join(df, by = "Subject_Code")
rm(Subjects)
df <- df %>% 
		select(Subject_Code, Birthyear, self.MBC, avg.elder.MBC)
# drop rows with missing observations
df <- df[!is.na(df$self.MBC),]
df <- df[!is.na(df$avg.elder.MBC),]
df <- df[df$Birthyear > 0,]

# set ordering of M, B and C 
df$self.MBC <- factor(
	df$self.MBC, 
	levels=c("M", "B", "C"), 
	labels=c("M.gen2", "B.gen2", "C.gen2"), 
	ordered=TRUE)
df$avg.elder.MBC <- factor(
	df$avg.elder.MBC, 
	levels=c("M", "B", "C"), 
	labels=c("M.gen1", "B.gen1", "C.gen1"), 
	ordered=TRUE)
# bin into age cohorts
df$cohort=as.factor(floor((df$Birthyear)/10)*10)
levels(df$cohort)

# RUN TRANSITION MATRIX ANALYSIS
xvar <- "decade"
df <- rename(df, decade = cohort)
source("tp_matrix_loop.r")
