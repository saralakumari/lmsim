####
# sets appropriate options for read.delim
####
inread <- function(afilename) {
  yi <- read.delim(afilename, header=TRUE, sep=",", stringsAsFactors=FALSE, encoding="UTF-8")
  return(yi)}
