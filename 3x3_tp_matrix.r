###
# produces intergenerational transition matrices 
# of (M,B,C) language repertoires
# across various relationships: mother-child, etc.
# calculates conditional probabilities both ways
# and saves counts and probabilities
# as 3x3 tables
###

rm(list=ls())

source("inread.r")
library(dplyr)
library(tidyr)

# function to produce and write 3x3 matrices
threesquare <- function(biao, outname){
	# assign the proper order to M, B and C
	# add generation labels to lg data
	biao$lxSelf <- 
	  factor(biao$lxSelf, levels=c("M", "B", "C"),
		labels=c("M.self", "B.self", "C.self"), ordered=TRUE)
	biao$lxElder <- 
	  factor(biao$lxElder, levels=c("M", "B", "C"),
		labels=c("M.elder", "B.elder", "C.elder"), ordered=TRUE)

	# drop rows with missing observations
	biao <- biao[!is.na(biao$lxSelf),]
	biao <- biao[!is.na(biao$lxElder),]

	# count up the cases of each elder-to-kid transition
	# arrange in 3x3 matrix
	transmat1 <- 
	  spread(as.data.frame(tally(
	  biao %>% group_by(lxSelf, lxElder))), lxElder, n)
	rownames(transmat1) <- as.character(transmat1$lxSelf)
	transmat1$lxSelf <- NULL
	transmat1[is.na(transmat1)] <- 0 

	# row and column totals
	total.elder <- rowSums(transmat1)
	total.self <- c(colSums(transmat1), sum(total.elder))
	transmat1b <- 
	rbind(cbind(transmat1, total.elder), total.self)
	rownames(transmat1b) <- c(rownames(transmat1), "total.self")

	# transition probabilities for child, conditioned on elder
	transmat2 <- 
	as.data.frame(prop.table(as.matrix(transmat1),margin=2))

	# transition probabilities for elder, conditioned on child
	transmat3 <- 
	as.data.frame(prop.table(as.matrix(transmat1),margin=1))

	write.csv(transmat1b, 
		file = paste0("analysis/3x3_ct_", outname, ".csv"), 
		fileEncoding = "UTF-8")
	write.csv(transmat2, 
		file = paste0("analysis/3x3_p_", outname, ".csv"), 
		fileEncoding = "UTF-8")
	write.csv(transmat3, 
		file = paste0("analysis/3x3_revp_", outname, ".csv"), 
		fileEncoding = "UTF-8")
	}

# read in data and rename variables:
# inputs to threesquare() must have
# a column 'lxSelf' and a 'lxElder'
# containing MBC values
mothers <- inread("data/mbc_kid_mom.csv")
fathers <- inread("data/mbc_kid_dad.csv")
grandparents <- inread("data/mbc_kid_gp.csv")
centralparent <- inread("data/mbc_kid_cp.csv") %>% 
    select(Subject_Code, self.MBC, avg.elder.MBC) %>%
    rename(lxSelf = self.MBC, lxElder = avg.elder.MBC)

# apply function to each dataset
threesquare(mothers, "mom")
threesquare(fathers, "dad")
threesquare(grandparents, "gp")
threesquare(centralparent, "cp")
