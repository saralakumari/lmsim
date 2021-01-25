##
#  alternate visualization of transition matrix analysis:
#  simple barplot of "language shift" aka p(B1,C2|B1)
##

rm(list=ls())
library(ggplot2)

# read in data
source('inread.r')
B.gen1 <- inread('analysis/tp_decade_b1.csv')
# make sure every cohort will be labeled
B.gen1$cohort <- as.factor(B.gen1$cohort)

# plot setup -- could be used for any subset
baseplot <- function(thesubset){ggplot(data = thesubset, 
	aes(x=cohort, y=prob)) +
  geom_bar(stat = "identity") +
  ylim(0,1) +
  geom_errorbar(aes(ymin=prob-se, ymax=prob+se),
        width=0.5, size=0.8) +
  xlab("Child's Decade of Birth") +  
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12))
  }

# specific plot elements for p(B1,C2|B1)
shift <- baseplot(B.gen1[B.gen1$outcome %in% "C.gen2",]) +
  ylab("Proportion of Chinese-monolingual children") +
  labs(title = paste(
    "Rate of shift (loss of Mongolian) among children of bilingual households", 
    "\nN =", sum(B.gen1$n)/3, 
    "(interviewees only)"))
# other subsets could be plotted likewise

plots <- c("shift")
prefix <- "figures/plot_"
l <- mget(plots)
invisible(mapply(ggsave, 
  file=paste0(prefix, names(l), ".pdf"), 
  width=7.5, height=5, units="in", plot=l))
invisible(mapply(ggsave, 
  file=paste0(prefix, names(l), ".jpg"), 
  width=7.5, height=5, units="in", plot=l))
