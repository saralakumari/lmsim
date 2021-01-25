####
# processes raw language-ability data
# so as to classify speakers into 
# Mongolian-only, Bilingual, or Chinese-only
# (M, B, C)
# according to spoken ability
# outputs tables in dyad and individual formats
####

rm(list=ls())
library(dplyr)
library(tidyr)

#---- MBC function ----
# summarizes Spoken Mongolian Ability and
# Spoken Chinese Ability to one value
# input (1,2,3,4,5) = (FLUENT,MEDIUM,LITTLE,NONE,UNKNOWN) 
# output (M,B,C) = (Mongolian,Bilingual,Chinese)
MBCalpha <- function(m, c){
  mbc <- ifelse(m < 3 & c < 3, "B", 
                ifelse(m < 3, "M",
                       ifelse(c < 3, "C", NA)))
  return(mbc)
}
#---- end label ----

#---- table writing function ----
# file will have same name as dataframe
csvutf8 <- function(adf) {
  write.csv(adf, file = paste0("data/mbc_", 
    deparse(substitute(adf)), ".csv"), 
  row.names = FALSE, fileEncoding = "UTF-8")
}
#---- end label ----

# read in data
source('inread.r')
almost_everything <- inread('data/2015_lmsim_data.csv')

#---- arrange and process child (subject) data ----
kids <- almost_everything %>% 
  select(Subject_Code, Birthyear, mong.spok, chin.spok) %>%
  mutate(Relationship = "self") %>%
  mutate(PersonID = Subject_Code) %>%
  mutate(MBC = MBCalpha(mong.spok, chin.spok))
kids <- kids[kids$Birthyear > 0,]
kids <- kids[!is.na(kids$MBC),]
#---- end label ----

#---- arrange and process elder data ----
elders <- almost_everything %>% 
  select(Subject_Code, 
    Sibling_Group, Birth_Order,
    a.Birthyear, b.Birthyear, c.Birthyear, d.Birthyear, 
    a.Relationship, b.Relationship, c.Relationship, d.Relationship, 
    a.Elder_as_Subject, b.Elder_as_Subject, 
    c.Elder_as_Subject, d.Elder_as_Subject,
    a.mong.spok, a.chin.spok, b.mong.spok, b.chin.spok, 
    c.mong.spok, c.chin.spok, d.mong.spok, d.chin.spok) %>%
  pivot_longer( # one row per elder
    cols = a.Birthyear:d.chin.spok,
    names_to = c("index", ".value"),
    names_pattern = "(.).(.*)"
  ) %>%
  mutate(PersonID = paste(Subject_Code, index, sep="")) %>%
  mutate(MBC = MBCalpha(mong.spok, chin.spok))
elders <- elders[elders$Birthyear > 0,]
elders <- elders[!is.na(elders$MBC),]
#---- end label ----

#---- format and export MBC tables for 2-generation dyads ----
# two MBC data points per row (child and one elder)
dyadkids <- kids %>%
  select(Subject_Code, MBC) %>%
  rename(lxSelf = MBC)

dyad.tab <- function(alist, adf){
  dytab <- elders %>% 
    # use full set of elders including duplicates
    filter(Relationship %in% alist) %>%
    select(Subject_Code, MBC) %>%
    rename(lxElder = MBC)
  dytab <- merge(adf, dytab, by="Subject_Code")
  return(dytab) 
}

kid_mom <- dyad.tab("MOTHER", dyadkids)
kid_dad <- dyad.tab("FATHER", dyadkids)
kid_gp <- dyad.tab(c(
  "MAT_GRANDMA", "PAT_GRANDMA", "GRANDMA_NS",
  "MAT_GRANDPA", "PAT_GRANDPA", "GRANDPA_NS"),
  dyadkids)
csvutf8(kid_mom)
csvutf8(kid_dad)
csvutf8(kid_gp)
#---- end label ----

#---- reduce elder data to unique individuals only ----
# duplicated individuals are filtered out
# this is needed for analyses where individuals 
# are the unit (rather than dyads or families)
x <- elders %>%
  mutate(rep.id = if_else(
    Sibling_Group >=1, 
    paste(Sibling_Group, Relationship, sep="_"), 
    "error", 
    as.vector(1:length(Sibling_Group), mode="character")))

indiv.elders <- x %>%
  # Case 1: where multiple siblings were interviewed,
  # individual Elders may be referenced multiple times.
  # all but one reference should be dropped.
  arrange(Birth_Order) %>%
  filter(!duplicated(rep.id)) %>%
  # Case 2. where two generations in the same family 
  # were interviewed,
  # individuals may occur as both Elder and Subject.
  # all references in Elder table should be dropped.
  filter(is.na(Elder_as_Subject)) %>%
  arrange(PersonID) %>%
  mutate(rep.id = NULL)
# make table listing elders who were dropped
# and the reason
dup.elders <- x %>%
  add_count(rep.id, name='rep.ct') %>%
  filter(Sibling_Group >=1 | Elder_as_Subject >=1) %>%
  mutate(rep.id = if_else(
  Sibling_Group >=1, 
  paste(Sibling_Group, Relationship, sep="_"), 
  "error", 
  NULL)) %>%
  mutate(rep.ct = if_else(
    rep.ct >1, rep.ct, NULL, 99:99
  )) 

rm(x)
#---- end label ----

#---- format and export MBC table of individuals ----
# one MBC data point per row (either child or elder)
everybody <- rbind(
  indiv.elders %>% mutate(role = "gen1") %>% 
  select(PersonID, Birthyear, mong.spok, chin.spok, MBC, role),   
  kids %>% mutate(role = "gen2") %>%  
  select(PersonID, Birthyear, mong.spok, chin.spok, MBC, role))
everybody <- everybody %>% 
  arrange(everybody$PersonID) %>%
  rename(Mongolian = mong.spok, Chinese = chin.spok, Profile = MBC)
csvutf8(everybody)

# dropped elder data, saved for reference
dup_elders <- dup.elders %>%
  select(PersonID, Birthyear, mong.spok, chin.spok, MBC,
         Elder_as_Subject, Sibling_Group, rep.id, rep.ct) %>%
  rename(Mongolian = mong.spok, Chinese = chin.spok, Profile = MBC)
csvutf8(dup_elders)
#---- end label ----
