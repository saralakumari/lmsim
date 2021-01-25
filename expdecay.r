#####
# plots exponential decay curves 
# for the number of new child speakers of Mongolian 
# over the next 5 generations,
# contrasting the observed rate of shift in the data
# against a hypothetical faster rate
# takes no inputs: data is coded into script
####

rm(list=ls())
library(ggplot2)

# exponential decay constant λ
# where p is the proportional decline per unit of time
# p is positive
lambda <- function(p){-log(1-p)}

# exponential decay function 
# where t is time (generations)
decay <- function(p,t){exp(-lambda(p)*t)}

# set two possible values for p:
# observed p(B1,C2|B1) = 0.165
    # this comes from the actual data analysis
# counterfactual p(B1,C2|B1) = 0.423
    # this comes from a hypothetical fake dataset
    # see Puthuval (2017) for explanation 
lowball <- function(x){decay(0.165, x)} # observed
highball <- function(x){decay(0.423, x)} # counterfactual

# plot the curves
baseplot <-
  ggplot(data.frame(x=c(0,6)), aes(x=x)) + 
  	stat_function(fun=lowball, geom="path", size=1.5) + 
  ylim(0, 1) + xlim(0,5) +
  labs(x="Generations", y="New child speakers of Mongolian") +
  ggtitle(paste0("Decline in new child speakers over five generations,",
                 "\nassuming a constant rate of shift"))
# stat_function calculates at 101 points along x range by default
  
one <- baseplot +
  annotate("text", x=3, y=.75, label="Rate of shift = 0.165", size=5)

two <- baseplot +
  stat_function(
    fun=highball, geom="path", size=1.5, linetype="dotdash") + 
  annotate("text", x=3, y=.75, 
  	label="Observed rate = 0.165", size=5) +
  annotate("text", x=2.5, y=.07, 
  	label="Counterfactual rate = 0.423", size=5)

plots <- c("one", "two")
prefix <- "figures/expdecay_"
l <- mget(plots)

invisible(mapply(ggsave, 
	file=paste0(prefix, names(l), ".pdf"), 
	width=6, height=4, units="in", plot=l))
invisible(mapply(ggsave, 
	file=paste0(prefix, names(l), ".jpg"), 
	width=6, height=4, units="in", plot=l))
