##
# calculates ratio of (M, B, C) speakers
# for all age cohorts (by decade of birth)
# across entire sample (elders + subjects)
# and plots as three lines with error bars
##

rm(list=ls())
library(dplyr)
library(tidyr)

## PREPARE DATA FOR PLOTTING ##
# read in data and remove missing obs
source("inread.r")
df <- inread("data/mbc_everybody.csv")
df <- df[!is.na(df$Profile),]
df <- df[df$Birthyear>1800,]
df <- df[!is.na(df$Birthyear),]

# bin birthyears into cohorts
df$Cohort=as.factor(floor((df$Birthyear)/10)*10)
# set the proper non-alphabetical ordering of M, B and C 
df$Profile <- factor(df$Profile, 
  levels=c("M", "B", "C"), ordered=TRUE)
# get counts of MBC profile x age cohort
tallies <- as.data.frame(tally(na.omit(df) %>% 
            group_by(Cohort, Profile))) %>% 
            spread(Profile, n)
# get ratios
tallies$n <- rowSums(tallies[,2:4], na.rm = TRUE)
tallies$Mratio <- with(tallies, M/n)
tallies$Bratio <- with(tallies, B/n)
tallies$Cratio <- with(tallies, C/n)

# transform data to a by-cohort summary
df <- tallies %>% select(
  Cohort, n, Mratio, Bratio, Cratio)
rm(tallies)

# transform data to long table
probs <- gather(df, "outcome", "prob", 3:5)
# calculate standard errors
get.se <- function(adf, col, n = "n"){
  p <- adf[[col]]
  n <- adf[[n]]
  x <- sqrt(p*(1-p)/n)
  return(x)
}
probs$se <- get.se(probs, "prob", "n")
df <- probs
rm(probs)

# set the proper ordering of M, B and C again
df$outcome <- factor(df$outcome, 
  levels=c("Mratio", "Bratio", "Cratio"), ordered=TRUE)

## MAKE PLOT ##
library(ggplot2)
n <- sum(df$n)/3
nphrase <- paste("N =", n, 
  "interviewees and elders/caregivers", sep = " ")
pd <- position_dodge(.5)
title <- paste(
  "Proportion of M, B and C language repertoires over time", 
  nphrase, sep="\n")
X.lab <- "Decade of birth"
Y.lab <- "Proportion of M, B and C speakers"
Mstyle <- "solid"
Bstyle <- "solid"
Cstyle <- "solid"
errorcol <- "gray50" #dark gray
Mcol <- "#404096"
Bcol <- "#dea73a"
Ccol <- "#d92120"

lplot <- function(adf){
  ggplot(adf, 
    aes(x=Cohort, y=prob, 
      linetype=outcome, 
      color=outcome, 
      group=outcome)) +
    scale_linetype_manual(
      values = c(Mstyle, Bstyle, Cstyle), 
      guide = FALSE, 
      labels = c("Mongolian only", "Bilingual", "Chinese")) +
    scale_colour_manual(
      values = c(Mcol, Bcol, Ccol), guide = FALSE) +
    geom_errorbar(aes(ymin=prob-se, ymax=prob+se), 
      width=.5, size=0.8, colour=errorcol, 
      linetype="solid", position=pd) +
    geom_line(position=pd, size=2) +
    geom_point(position=pd, size=2.5) +
    xlab(X.lab) + ylab(Y.lab) + 
    ylim(0,1) +
    labs(title = title) +
    theme(axis.text=element_text(size=12), 
      axis.title=element_text(size=12))
}

lineplot <- 
  lplot(df) + 
  annotate("text", x=10.5, y=.85, 
    label="B (Bilingual)", size=5) +
  annotate("text", x=9.2, y=.35, 
    label="M (Mongolian only)", size=5) +
  annotate("text", x=6, y=.05, 
    label="C (Chinese only)", size=5)

# save plot
ggsave(plot=lineplot, 
  file="figures/mbc_decade.pdf", 
  width=8, height=5, units="in")
ggsave(plot=lineplot, 
  file="figures/mbc_decade.jpg", 
  width=8, height=5, units="in")

# save underlying data
write.csv(df, 
  file = "analysis/mbc_decade.csv", 
  fileEncoding = "UTF-8", row.names = FALSE)
