####
#  carries out transition matrix analysis:
#  calculates transition probability
#  of intergenerational language transmission
#  for each level of some variable 
#  such as age cohort or urban-town-rural
#  outputs 6 data tables
####

# must be called from within other script
# that creates objects 'df' and 'xvar'
# see tp_decade.r and tp_urbrur.r

### PART 1. TRANSITION MATRICES ###

# first, make a count matrix
# aka contingency table: child-MBC x parent-MBC
# forces output to 3x3 array, preserving zeroes
transitions <- function(adf){
  x <- as.data.frame(tally(adf %>% 
    group_by(self.MBC, avg.elder.MBC))) %>% 
    spread(avg.elder.MBC, n) # table done!
# everything below here is just to preserve zeroes and var names
  rownames(x) <- as.character(x$self.MBC)
  x$self.MBC <- NULL
  outdf <- as.data.frame(matrix(nrow=3, ncol=3))
  colnames(outdf) <- c("M.gen1", "B.gen1","C.gen1")
  rownames(outdf) <- c("M.gen2", "B.gen2", "C.gen2")
    for(rr in rownames(x)){
      for(cc in colnames(x)){
      outdf[rr,cc] <- as.numeric(x[rr,cc]) }}
  outdf[is.na(outdf)] <- 0
  return(outdf)
  }

# from count matrix, calculate probabilities
  # conditional probabilities by column: p(child.mbc|parents.mbc)
  prob.bygen1 <- function(df1){
        df2 <- transitions(df1)
        x <- as.data.frame(prop.table(as.matrix(df2), 
          margin = 2)) # margin=2 means by column
        return(x)
        }
  # conditional probabilities by row: p(parents.mbc|child.mbc)
  prob.bygen2 <- function(df1){
        df2 <- transitions(df1)
        x <- as.data.frame(prop.table(as.matrix(df2), 
          margin = 1)) # margin=1 means by row
        return(x)
        }

# PART 2:  APPLY PROBABILITY MATRIX FUNCTION,
# LOOPING THROUGH ALL LEVELS OF A GROUPING VARIABLE

# loop 1: p(parents.mbc|child.mbc)
  # gen2 = the subset (defined in this script)
  # df = the full dataset (from environment)
  # xvar = the predictor variable (from enviroment)
  bygen2 <- function(gen2){
  outdf <- as.data.frame(matrix(nrow=1, ncol=6))
    colnames(outdf) <- c("cohort", levels(df$avg.elder.MBC), "n", "gen2")
  for (i in levels(df[[xvar]])){
    xgroup <- df[df[[xvar]] == i,]
    n <- length(xgroup$Subject_Code[xgroup$self.MBC == gen2])
    probs <- prob.bygen2(xgroup)
    outdf[i,] <- c(i, probs[rownames(probs) == gen2,], n, gen2)
  }
  outdf[,2] <- as.numeric(outdf[,2])
  outdf[,3] <- as.numeric(outdf[,3])
  outdf[,4] <- as.numeric(outdf[,4])
  outdf$n <- as.integer(outdf$n)
  outdf <- outdf[outdf$cohort %in% levels(df[[xvar]]),]
  outdf$cohort <- as.factor(outdf$cohort)
  rownames(outdf) <- NULL
  return(outdf)  
  }

# loop 2: p(child.mbc|parents.mbc)
  # gen1 = the subset (defined in this script)
  # df = the full dataset (from environment)
  # xvar = the predictor variable (from environment)
bygen1 <- function(gen1){
  outdf <- as.data.frame(matrix(nrow=1, ncol=6))
  colnames(outdf) <- c("cohort", levels(df$self.MBC), "n", "gen1")
  for (i in levels(df[[xvar]])){
    xgroup <- df[df[[xvar]] == i,]
    n <- length(xgroup$Subject_Code[xgroup$avg.elder.MBC == gen1])
    probs <- prob.bygen1(xgroup)
    outdf[i,] <- c(i, probs[,colnames(probs) == gen1], n, gen1)
  }
  outdf[,2] <- as.numeric(outdf[,2])
  outdf[,3] <- as.numeric(outdf[,3])
  outdf[,4] <- as.numeric(outdf[,4])
  outdf$n <- as.integer(outdf$n)
  outdf <- outdf[outdf$cohort %in% levels(df[[xvar]]),]
  outdf$cohort <- as.factor(outdf$cohort)
  rownames(outdf) <- NULL
  return(outdf)  
}

## PART 3: RUN LOOP FOR SIX SUBSETS ##
# Child-based subsets
M.gen2 <- bygen2(levels(df$self.MBC)[1])
B.gen2 <- bygen2(levels(df$self.MBC)[2])
C.gen2 <- bygen2(levels(df$self.MBC)[3])
# Elder-based subsets
M.gen1 <- bygen1(levels(df$avg.elder.MBC)[1])
B.gen1 <- bygen1(levels(df$avg.elder.MBC)[2])
C.gen1 <- bygen1(levels(df$avg.elder.MBC)[3])

## PART 4: PREPARE PLOT-READY OUTPUT ##
library(tidyr)
# standard error
get.se <- function(df, col, n = "n"){
  p <- df[[col]]
  n <- df[[n]]
  x <- sqrt(p*(1-p)/n)
  return(x)
}
# convert from wide to long format 
# then calculate SE
add.se <- function(df){
  df <- gather(df, "outcome", "prob", 2:4)
    # cols 2:4 contain the 3 transition outcomes
  df$se <- get.se(df, "prob" ,"n")
  return(df)
} # gather function is an old version of pivot_longer

M.gen1 <- add.se(M.gen1)
B.gen1 <- add.se(B.gen1)
C.gen1 <- add.se(C.gen1)

M.gen2 <- add.se(M.gen2)
B.gen2 <- add.se(B.gen2)
C.gen2 <- add.se(C.gen2)

# write out
gens <- c(
  "M.gen1", "B.gen1", "C.gen1", "M.gen2", "B.gen2", "C.gen2"
  )
l <- mget(gens)
invisible(mapply(write.csv, l, 
  file = paste0("analysis/tp_", xvar, "_", 
          tolower(gsub(".gen", "", names(l))), ".csv"), 
  row.names = FALSE, 
  fileEncoding = "UTF-8"))
