##
# plots results of transition matrix analysis
# predictor: child's decade of birth
# 6 plots for 6 subsets
##

rm(list=ls())
source('inread.r')
# get data
M.gen1 <- inread('analysis/tp_decade_m1.csv')
B.gen1 <- inread('analysis/tp_decade_b1.csv')
C.gen1 <- inread('analysis/tp_decade_c1.csv')

M.gen2 <- inread('analysis/tp_decade_m2.csv')
B.gen2 <- inread('analysis/tp_decade_b2.csv')
C.gen2 <- inread('analysis/tp_decade_c2.csv')

# get plotting functions
source("tp_plot_styles.r")
X.lab <- "Child's decade of birth"
nphrase <- "\nN of elder-child dyads ="

# make plots of parent-child transitions
m1 <- plot1(M.gen1) + scale_colour_manual(
  values = c(spreadcol, zerocol, linecol),
  labels = c("M to B", "M to C", "M to M")) +
  labs(title = paste("Proportion of spread by decade,",
  "\ngiven Mongolian-monolingual (M) elders:", 
  nphrase, sum(M.gen1$n)/3, sep=" "))
b1 <- plot1(B.gen1) + scale_colour_manual(
  values = c(maintcol, shiftcol, linecol),
  labels = c("B to B", "B to C", "B to M")) +
  labs(title = paste(
  "Proportion of maintenance vs. shift by decade,",
  "\ngiven bilingual (B) elders:",
  nphrase, sum(B.gen1$n)/3, sep=" "))
c1 <- plot1(C.gen1) + scale_colour_manual(
  values = c(linecol, linecol, zerocol),
  labels = c("C to B", "C to C", "C to M")) +
  labs(title = paste(
  "Proportion of (M), (B) and (C) children by decade,",
  "\ngiven Chinese-monolingual (C) elders:",
  nphrase, sum(C.gen1$n)/3, sep=" "))

# make plots of child-parent transitions
m2 <- plot2(M.gen2) + scale_colour_manual(
  values = c(linecol, zerocol, linecol),
  labels = c("B to M", "C to M", "M to M")) +
  labs(title = paste(
    "Proportion of (M), (B) and (C) elders by decade,",
    "\ngiven Mongolian-monolingual (M) children:",
    nphrase, sum(M.gen2$n/3), sep=" "))
b2 <- plot2(B.gen2) + scale_colour_manual(
  values = c(maintcol, linecol, spreadcol),
  labels = c("B to B", "C to B", "M to B")) +
  labs(title = paste(
    "Proportion of maintenance vs. spread by decade,",
    "\ngiven bilingual (B) children:",
    nphrase, sum(B.gen2$n/3), sep=" "))
c2 <- plot2(C.gen2) + scale_colour_manual(
  values = c(shiftcol, linecol, zerocol),
  labels = c("B to C", "C to C", "M to C")) +
  labs(title = paste("Proportion of shift by decade,",
    "\ngiven Chinese-monolingual (C) children:",
    nphrase, sum(C.gen2$n/3), sep=" "))

# save plots
plots <- c("m1", "b1", "c1", "m2", "b2", "c2")
prefix <- "figures/plot_tp_dec_"
l <- mget(plots)
invisible(mapply(ggsave, file=paste0(prefix, names(l), ".pdf"), 
  width=8, height=5, units="in", plot=l))
invisible(mapply(ggsave, file=paste0(prefix, names(l), ".jpg"), 
  width=8, height=5, units="in", plot=l))
