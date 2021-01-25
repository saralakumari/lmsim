###
# calculates the age distribution of 
# all speakers in the sample, both Subjects and Elders
# and plots as a histogram of decades of birth,
# with color coding of Subject vs. Elder
###

rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
source('inread.r')

# read in data
everybody <- inread("data/mbc_everybody.csv")
# prepare variables
everybody$role <- factor(everybody$role, 
	ordered = TRUE, levels = c("gen1", "gen2"))
everybody$Birthdecade=as.factor(floor((everybody$Birthyear)/10)*10)

# make long table for plot
tallies <- as.data.frame(
	tally(na.omit(everybody) %>% 
	group_by(Birthdecade, role)))
# save out underlying data
write.csv(tallies, 
	file = "analysis/age_dist.csv", 
	fileEncoding = "UTF-8", row.names = FALSE)

# elements for plot
gen1col <- "#dea73a" # elders - gold
gen2col <- "#404096" # interviewees - blue
gen1lab <- "Interviewee's caregiver"
gen2lab <- "Direct interviewee"
colors <- c(gen1col, gen2col) 
n2 <- length(everybody$PersonID[everybody$role %in% "gen2"])
n1 <- length(everybody$PersonID[everybody$role == "gen1"])
nn <- length(everybody$PersonID)
nphrase <- paste("N =", nn, 
	"(", n2, "interviewees and", 
	n1, "elders/caregivers)", sep=" ")
caption <- "Birthyear Distribution in Sample"

# plot
aplot <- ggplot(data = tallies, 
    aes(x = Birthdecade, y = n, fill = role)) + 
    geom_col() +
    scale_fill_manual(values = colors, name = "Role",
        labels = c(gen1lab, gen2lab), 
        guide = guide_legend()) +
    labs(title=paste(caption, nphrase, sep="\n"), 
         x = "Decade of Birth", y = "Number of People") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=12))  

# save plot
ggsave(plot=aplot, file="figures/age_dist.pdf", width=8, height=5, units="in")
ggsave(plot=aplot, file="figures/age_dist.jpg", width=8, height=5, units="in")
