####
# calculates distribution of 
# Rural, Town, and Urban residents in sample (subjects only)
# based on current and childhood residences
# and plots as two side-by-side histograms
####

rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)

# read in data
source('inread.r')
almost_everything <- inread('data/2015_lmsim_data.csv')

df <- almost_everything %>% 
        select(Subject_Code, Urban_Status.x, Urban_Status.y)

# set up rural-to-urban ordering
df$Urban_Status.x[df$Urban_Status.x %in% "UNKNOWN"] <- NA
df$Urban_Status.y[df$Urban_Status.y %in% "UNKNOWN"] <- NA
orderXZC <- function(myvar){
  x <- factor(myvar, 
        levels = c("XIANG_RURAL","ZHEN_TOWN","CHENG_CITY"),
        labels=c("Rural","Town","Urban"), 
        ordered=TRUE)
  return(x)
}
df$Urban_Status.x <- orderXZC(df$Urban_Status.x)
df$Urban_Status.y <- orderXZC(df$Urban_Status.y)

# find and remove rows with incomplete data
no.loc.then <- length(df$Urban_Status.y[is.na(df$Urban_Status.y)])
total.then <- length(df$Subject_Code) - no.loc.then

no.loc.now <- length(df$Urban_Status.x[is.na(df$Urban_Status.x)])
total.now <- length(df$Subject_Code) - no.loc.now

df <- na.omit(df) # to get rid of all missing Location data
en <- length(df$Subject_Code)

# long format for plot
then <- as.data.frame(table(df$Urban_Status.y))
then$time <- 1
now <- as.data.frame(table(df$Urban_Status.x))
now$time <- 2
df2 <- rbind(then, now)
colnames(df2) <- c("res", "n", "time")
df2$time <- factor(df2$time, 
            labels = c("Childhood Residence", "Current Residence"))

# save plot data to text
write.csv(df2, file="analysis/urb_dist.csv", 
                fileEncoding = "UTF-8", 
                row.names=FALSE)

# plot elements
colors <- c("#57a3ad", "#dea73a") #teal, gold
  #colors <- c("gray25", "gray75")
X.lab <- "Residence Type"
Y.lab <- "Number of Interviewees"
n <- en
nphrase <- paste("N =", en, "interviewees", sep=" ")
title <- "Rural, town and urban residents in the sample"

p <- 
  ggplot(data=df2, aes(x = res, y=n, fill=time)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=colors, guide = FALSE)+
  facet_wrap( ~ time) +
  labs(title = paste(title, nphrase, sep = "\n")) +
  theme(plot.title = element_text(hjust = 0.5)) + # center
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  theme(strip.text = element_text(size=12)) +
  xlab(X.lab) + ylab(Y.lab)
  
# plot out
ggsave(plot=p, file="figures/urb_dist.pdf", 
        width=8, height=5, units="in")
ggsave(plot=p, file="figures/urb_dist.jpg", 
        width=8, height=5, units="in")
