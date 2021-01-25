###
# calculates the proportion of monolinguals 
# among Mongolian speakers in the sample
# across different age cohorts
# and plots as histogram with error bars
# bonus plot: the inverse proportion (bilinguals)
###

rm (list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)

# read in data and remove missing obs
source('inread.r')
df <- inread('data/mbc_everybody.csv')
df <- df[!is.na(df$Profile),]
df <- df[df$Birthyear>1800,]
df <- df[!is.na(df$Birthyear),]

# bin birthyears into cohorts
cohort <- function(x){
          ifelse(x < 1930, "1880-1930",
           ifelse(x < 1940, "1930s",
            ifelse(x < 1950, "1940s", 
             ifelse(x < 1960, "1950s", 
              ifelse(x < 1970, "1960s", 
               ifelse(x < 1980, "1970s", 
                ifelse(x < 1990, "1980s", 
                 ifelse(x < 2000, "1990s",
                   "2000-2007")))))))) }
df$Cohort <- as.factor(cohort(df$Birthyear))

# set the proper ordering of M, B and C 
df$Profile <- factor(df$Profile, 
  levels=c("M", "B", "C"), ordered=TRUE)

# get counts of language profile x age cohort
tallies <- as.data.frame(
  tally(na.omit(df) %>% 
    group_by(Cohort, Profile))) %>% 
  spread(Profile, n)
tallies$PlusM <- tallies$M + tallies$B
tallies$monoRatio <- tallies$M/tallies$PlusM
tallies$biRatio <- tallies$B/tallies$PlusM

# calculate standard error
get.se <- function(df, col, n){
  p <- df[[col]]
  n <- df[[n]]
  x <- sqrt(p*(1-p)/n)
  return(x)
}

tallies$monoSE <- get.se(tallies, "monoRatio", "PlusM")
tallies$biSE <- get.se(tallies, "biRatio", "PlusM")

# save out underlying data from plots
write.csv(tallies, 
  file = "analysis/mbc_prop.csv", 
  fileEncoding = "UTF-8", row.names = FALSE)

# define labels
N <- paste(
      "N =", 
      length(df$Profile[df$Profile %in% c("M", "B")]), 
      "(interviewees plus parents/caregivers)")
caption <- 
    "Proportion of monolinguals among Mongolian speakers"

# make first plot
prop_m <- 
  ggplot(data = tallies, aes(x = Cohort, y = monoRatio)) +
  geom_bar(stat = "identity") +
  ylim(0,1) +
  geom_errorbar(aes(ymin=monoRatio-monoSE, ymax=monoRatio+monoSE),
                width=.5, size=0.8) +
  labs(title = paste(caption, N, sep="\n"), 
        x = "Decade of Birth", 
        y = "Proportion of monolinguals") +
  theme(axis.text=element_text(size=12))

# redefine labels
N <- paste(
      "N =", 
      length(df$Profile[df$Profile %in% c("M", "B")]), 
      "(interviewees plus parents/caregivers)")
caption <- 
    "Proportion of Mongolian-Chinese bilinguals\namong Mongolian speakers"

# make second plot
prop_b <- ggplot(data = tallies, aes(x = Cohort, y = biRatio)) +
  geom_bar(stat = "identity") +
  ylim(0,1) +
  geom_errorbar(aes(ymin=biRatio-biSE, ymax=biRatio+biSE),
                width=.5, size=0.8) +
  labs(title = paste(caption, N, sep="\n"), 
        x = "Decade of Birth", 
        y = "Proportion of bilinguals") 
  
# save plots
plots <- c("prop_m","prop_b")
prefix <- "figures/mbc_"
l <- mget(plots)
invisible(mapply(ggsave, 
  file=paste0(prefix, names(l), ".pdf"), 
  width=7.5, height=5, units="in", 
  plot=l))
invisible(mapply(ggsave, 
  file=paste0(prefix, names(l), ".jpg"), 
  width=7.5, height=5, units="in", 
  plot=l))
