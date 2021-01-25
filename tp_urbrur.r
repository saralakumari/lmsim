###
#  transition matrix analysis
#  predictor: urban status of childhood residence
#  6 tables for 6 subsets of data
###

rm(list=ls())
# PREPARE URBANIZATION DATA
library(dplyr)
library(tidyr)
source("inread.r")

# read in data
df <- inread("data/mbc_kid_cp.csv") # linguistic
almost_everything <- inread('data/2015_lmsim_data.csv') # geographic
# join residence data to language data
df <- left_join(df, select(
		almost_everything, Subject_Code, Urban_Status.x, Urban_Status.y), 
		by = "Subject_Code")
rm(almost_everything)

# set up rural-to-urban ordering
orderXZC <- function(myvar){
  x <- factor(myvar, 
	levels = c("XIANG_RURAL","ZHEN_TOWN","CHENG_CITY", "UNKNOWN"),
    labels=c("Rural","Town","Urban","Unknown"), 
    ordered=TRUE)
  return(x)
}
df$Urban_Status.x <- orderXZC(df$Urban_Status.x)
df$Urban_Status.y <- orderXZC(df$Urban_Status.y)

# drop rows with missing observations
df <- df[!is.na(df$self.MBC),]
df <- df[!is.na(df$avg.elder.MBC),]
df <- df[!is.na(df$Urban_Status.x),]
df <- df[!is.na(df$Urban_Status.y),]
df <- df[df$Urban_Status.x != "Unknown",]
df <- df[df$Urban_Status.y != "Unknown",]

# set the proper ordering of M, B and C 
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

# remove "Unknown" level from Rural-to-Urban factor
df$Urban_Status.x <- factor(
	df$Urban_Status.x, 
	levels=c("Rural", "Town", "Urban"), 
	ordered=TRUE)
df$Urban_Status.y <- factor(df$Urban_Status.y, 
	levels=c("Rural", "Town", "Urban"), 
	ordered=TRUE)

# RUN TRANSITION MATRIX ANALYSIS
xvar <- "urbrur"
df <- rename(df, urbrur = Urban_Status.y) # .y is childhood res
source("tp_matrix_loop.r")
