####
# sets styles for plotting the results
# of transition matrix analysis
# key vars are: "shift", "spread" and "maintenance"
####

library(ggplot2)

# for all line plots 
pd <- position_dodge(.5)
Y.lab <- "Proportion of each transition"
shiftcol <- "#d92120" # red
spreadcol <- "#dea73a" # gold
maintcol <- "#404096" # dark blue
linecol <- "gray50" # mid gray
errorcol <- "gray30" # dark gray
zerocol <- "gray70" # light gray

# for gen1 subsets
plot1 <- function(df){
  ggplot(df, aes(
    x=as.character(cohort), 
    y=prob, 
    color=outcome, 
    group=outcome)) +
    geom_errorbar(aes(
      ymin=prob-se, ymax=prob+se),
      width=.5, size=0.8, colour=errorcol, position=pd) +
    geom_line(position=pd, size=2.5) +
    geom_point(position=pd, size=5) +
    xlab(X.lab) + ylab(Y.lab) + labs(color="Transition") +
    ylim(0,1) +
    theme(axis.text=element_text(size=12))
}

# for gen2 subsets
plot2 <- function(df){
  ggplot(df, aes(
    x=as.character(cohort), 
    y=prob, 
    color=outcome, 
    group=outcome)) +
    geom_errorbar(aes(
      ymin=prob-se, ymax=prob+se),
      width=.5, size=0.8, colour=errorcol, position=pd) +
    geom_line(position=pd, size=2.5, linetype = "dashed") + 
    # dashed lines are the only difference between plot2 and plot1
    geom_point(position=pd, size=5) +
    xlab(X.lab) + ylab(Y.lab) + labs(color="Transition") +
    ylim(0,1) +
    theme(axis.text=element_text(size=12))
}
