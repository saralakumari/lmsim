##
# calculates ratio of (M, B, C) speakers
# among rural, town, and urban residents
# with side-by-side plots for childhood vs. current residence
## 

rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)

# read in data
source("inread.r")
lg <- inread("data/mbc_kid_cp.csv")
lg <- lg %>% select(Subject_Code, self.MBC)
almost_everything <- inread('data/2015_lmsim_data.csv')
df <- almost_everything %>% 
  select(Subject_Code, 
    Urban_Status.x, Urban_Status.y, Ethnicity) %>% 
  inner_join(lg, by = "Subject_Code")

# cleaning: drop rows with missing Location, Language or Ethnicity
df$Urban_Status.x[df$Urban_Status.x %in% "UNKNOWN"] <- NA
df$Urban_Status.y[df$Urban_Status.y %in% "UNKNOWN"] <- NA
df <- na.omit(df)
df$Ethnicity <- NULL

# make ordered factors
df$self.MBC <- factor(df$self.MBC, 
    levels=c("M", "B", "C"), 
    ordered=TRUE)
orderXZC <- function(myvar){
  x <- factor(myvar, 
    levels = c("XIANG_RURAL","ZHEN_TOWN","CHENG_CITY"),
    labels=c("Rural","Town","Urban"), 
    ordered=TRUE)
  return(x)
}
df$Urban_Status.x <- orderXZC(df$Urban_Status.x)
df$Urban_Status.y <- orderXZC(df$Urban_Status.y)

# subset by time point to calculate probabilities
# childhood residence data
then <- df 
then$Urban_Status.x <- NULL
then <- rename(then, Urban_Status = Urban_Status.y)
y <- as.data.frame(tally(na.omit(then) %>% 
      group_by(Urban_Status, self.MBC))) %>% 
      spread(self.MBC, n)
y[is.na(y)] <- 0
y$n <- with(y, M+B+C)
y$Mratio <- with(y, M/n)
y$Bratio <- with(y, B/n)
y$Cratio <- with(y, C/n)
then <- y %>% select(
        Urban_Status, n, Mratio, Bratio, Cratio)
rm(y)
# current residence data
now <- df
now$Urban_Status.y <- NULL
now <- rename(now, Urban_Status = Urban_Status.x)
x <- as.data.frame(tally(na.omit(now) %>% 
      group_by(Urban_Status, self.MBC))) %>% 
      spread(self.MBC, n)
x[is.na(x)] <- 0
x$n <- (x$M + x$B + x$C)
x$Mratio <- with(x, M/n)
x$Bratio <- with(x, B/n)
x$Cratio <- with(x, C/n)
now <- x %>% select(
        Urban_Status, n, Mratio, Bratio, Cratio)
rm(x)


# standard error
get.se <- function(df, col, n = "n"){
  p <- df[[col]]
  n <- df[[n]]
  x <- sqrt(p*(1-p)/n)
  return(x)
}

# reformat to long tables and calculate SE
probs <- gather(now, "outcome", "prob", 3:5)
probs$se <- get.se(probs, "prob", "n")
now <- probs
rm(probs)
probs <- gather(then, "outcome", "prob", 3:5)
probs$se <- get.se(probs, "prob", "n")
then <- probs
rm(probs)

# set ordering of M, B, C again
now$outcome <- factor(now$outcome, 
              levels=c("Mratio", "Bratio", "Cratio"), 
              ordered=TRUE)
then$outcome <- factor(then$outcome, 
              levels=c("Mratio", "Bratio", "Cratio"), 
              ordered=TRUE)
# combine THEN and NOW data
now$time <- "Current Residence"
then$time <- "Childhood Residence"
df <- rbind(then, now)
df$time <- factor(df$time, 
          levels=c("Childhood Residence", "Current Residence"), 
          ordered=TRUE)

# plotting function
lplot <- function(df){
  n <- sum(df$n)/6
  nphrase <- paste(
    "N =", n, "interviewees", sep = " ")
  pd <- position_dodge(.5)
  title <- paste(
    "Language repertoires of rural, town and urban residents", 
    nphrase, sep="\n")
  X.lab <- "Residence Type"
  Y.lab <- "Proportion of M, B and C speakers"
  Mstyle <- "solid"
  Bstyle <- "solid"
  Cstyle <- "solid"
  errorcol <- "gray50" #dark gray
  Mcol <- "#404096"
  Bcol <- "#dea73a"
  Ccol <- "#d92120"
    ggplot(df, aes(x=Urban_Status, y=prob, 
                  color=outcome, group=outcome)) +
    scale_colour_manual(
          values = c(Mcol, Bcol, Ccol), 
          name = "Languages", 
          labels = c("Mongolian only", "Bilingual", "Chinese only")) +
    geom_errorbar(aes(ymin=prob-se, ymax=prob+se),
                  width=.5, size=0.8, 
                  colour=errorcol, linetype="solid", position=pd) +
    geom_line(position=pd, size=2) +
    geom_point(position=pd, size=2.5) +
    xlab(X.lab) + ylab(Y.lab) +
    ylim(0,1) +
    labs(title = title) +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12)) +
    theme(strip.text = element_text(size=12))
}

# make plot
p <-  lplot(df) + facet_wrap(~time)
f <-  "mbc_urbrur"
ggsave(plot=p, file=paste0("figures/",f,".pdf"), 
        width=8, height=5, units="in")
ggsave(plot=p, file=paste0("figures/",f,".jpg"), 
        width=8, height=5, units="in")

# save underlying data points
write.csv(df, file=paste0("analysis/",f,".csv"), 
          fileEncoding = "UTF-8", row.names = FALSE)
