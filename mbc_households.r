####
# calculates the "central parent" (CP) value, 
# a.k.a. the "household" language environment,
# a.k.a. an average of the M, B or C language abilities of 
# all (up to 4) Elders in each Subject's childhood household
####

rm(list=ls())
library(dplyr)

# read in data
source('inread.r')
almost_everything <- inread('data/2015_lmsim_data.csv')
# the 4 elders in a household are labeled A, B, C and D

# clean the data:
# remove elders less than ten years older than subject
# remove elders with missing birthyears
gap <- 10

a <- almost_everything %>% 
  select(Subject_Code, Birthyear,
    a.Birthyear, a.mong.spok, a.chin.spok)
a$a.Birthyear <- as.numeric(a$a.Birthyear)
a <- a[a$Birthyear-a$a.Birthyear>gap & a$a.Birthyear>1800 ,]

b <- almost_everything %>% 
  select(Subject_Code, Birthyear, 
    b.Birthyear, b.mong.spok, b.chin.spok)
b$b.Birthyear <- as.numeric(b$b.Birthyear)
b <- b[b$Birthyear-b$b.Birthyear>gap & b$b.Birthyear>1800,]

c <- almost_everything %>% 
  select(Subject_Code, Birthyear, 
    c.Birthyear, c.mong.spok, c.chin.spok)
c$c.Birthyear <- as.numeric(c$c.Birthyear)
c <- c[c$Birthyear-c$c.Birthyear>gap & c$c.Birthyear>1800,]

d <- almost_everything %>% 
  select(Subject_Code, Birthyear, 
    d.Birthyear, d.mong.spok, d.chin.spok)
d$d.Birthyear <- as.numeric(d$d.Birthyear)
d <- d[d$Birthyear-d$d.Birthyear>gap & d$d.Birthyear>1800,]

# almost_everything: 629 rows
# a: 615
# b: 615
# c: 576
# d: 610

# recombine the cleaned data and
# extract relevant columns
x <- almost_everything %>% 
  select(Subject_Code) %>% 
  left_join(a) %>% 
  left_join(b) %>% 
  left_join(c) %>% 
  left_join(d) %>% 
  select(
  Subject_Code, 
  a.mong.spok, a.chin.spok, 
  b.mong.spok, b.chin.spok, 
  c.mong.spok, c.chin.spok, 
  d.mong.spok, d.chin.spok)

#---- MBCnum ----
# set up special version of MBC function
# maps M, B, C to 0, 0.5, 1 (skipping MBC stage)
  # input (1, 2, 3, 4, 5) = (FLUENT, MEDIUM, LITTLE, NONE, UNKNOWN) 
  # output (1, 0.5, 0) corresponds to (M, B, C) in other analyses

MBCnum <- function(m, c){
        mbc <- ifelse(m < 3 & c < 3, 0.5, 
                ifelse(m < 3, 1,
                ifelse(c < 3, 0, NA)))
        return(mbc)
        }
#---- end label ----

#---- MBCalpha ----
# normal version of MBC function
# summarizes mong.spok and chin.spok to one value
# input (1, 2, 3, 4, 5) = (FLUENT, MEDIUM, LITTLE, NONE, UNKNOWN) 
# output (M, B, C)

MBCalpha <- function(m, c){
  mbc <- ifelse(m < 3 & c < 3, "B", 
                ifelse(m < 3, "M",
                       ifelse(c < 3, "C", NA)))
  return(mbc)
}
#---- end label ----

# get MBC values (numeric version) for each elder
x <- x %>% 
  mutate(a.MBC1 = MBCnum(a.mong.spok, a.chin.spok)) %>%
  mutate(b.MBC1 = MBCnum(b.mong.spok, b.chin.spok)) %>%
  mutate(c.MBC1 = MBCnum(c.mong.spok, d.chin.spok)) %>%
  mutate(d.MBC1 = MBCnum(d.mong.spok, d.chin.spok))

# calculate mean of Elders for each Subject
avg.elder.num <- x %>% 
    select(a.MBC1, b.MBC1, c.MBC1, d.MBC1) %>% 
    rowMeans(na.rm=TRUE)

# convert mean back to MBC scale
    # if 0 <= x < .25 then C
    # if .25 <= x <= .75 then B
    # if .75 < x <= 1 then M
avg.elder.MBC <- 
  ifelse(avg.elder.num >=0 & avg.elder.num < 0.25, "C", 
    ifelse(avg.elder.num >= 0.25 & avg.elder.num <= 0.75, "B",
      ifelse(avg.elder.num > 0.75 & avg.elder.num <=1, "M", NA)))

# attach results to main df
x <- cbind(x, avg.elder.num, avg.elder.MBC) 
rm(avg.elder.num, avg.elder.MBC)

# add MBC values (alpha version) for each elder
x <- x %>% 
  mutate(a.MBC = MBCalpha(a.mong.spok, a.chin.spok)) %>%
  mutate(b.MBC = MBCalpha(b.mong.spok, b.chin.spok)) %>%
  mutate(c.MBC = MBCalpha(c.mong.spok, d.chin.spok)) %>%
  mutate(d.MBC = MBCalpha(d.mong.spok, d.chin.spok))

# make df with MBC values (alpha version) for each subject
y <- almost_everything %>% 
  select(Subject_Code, mong.spok, chin.spok) %>%
  mutate(self.MBC = MBCalpha(mong.spok, chin.spok))
# make df with elder-to-subject relationships
z <- almost_everything %>% 
  select(Subject_Code, 
    a.Relationship, b.Relationship, c.Relationship, d.Relationship)

# combine all three dfs
yxz <- 
  left_join(y, x, by = "Subject_Code") %>% 
  left_join(z, by = "Subject_Code")
rm(x, y, z)

# drop raw language data
mbc.df <- select(yxz, 
  Subject_Code, 
  self.MBC, avg.elder.MBC, avg.elder.num, 
  a.Relationship, a.MBC, 
  b.Relationship, b.MBC, 
  c.Relationship, c.MBC, 
  d.Relationship, d.MBC)

# save to txt
csvutf8 <- function(yi, filename) {
  write.table(yi, file = filename, 
    sep = ",", row.names = FALSE, fileEncoding = "UTF-8")
}

csvutf8(mbc.df, "data/mbc_kid_cp.csv")
