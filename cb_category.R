dim(datafiles$MSA_crunchbase)

# Author: Sifan Liu
# Date: Tue Mar 19 14:31:57 2019
# --------------
pkgs <- c('tidyverse','tidytext','wordcloud')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  } 

# read data

# crunchbase <- read.csv("source/crunchbase.csv")
cb_all <- read_all("../../Datasets/Crunchbase/data")

# remove duplicates


# tokenize
unnest_tokens(cb_all, out, Categories, token = "regex", pattern =",")%>%
  mutate(out = trimws(out))%>%
  group_by(out)%>%
  count(out, sort = TRUE)

cb_city <- unnest_tokens(cb_all, out, Categories, token = "regex", pattern =",")%>%
  mutate(out = trimws(out))%>%
  group_by(Headquarters.Location,out)%>%
  count(out, sort = TRUE)%>%
  ungroup()%>%
  mutate(total = sum(n))%>%
  group_by(out)%>%
  mutate(share_us = sum(n)/total)%>%
  ungroup()%>%
  group_by(Headquarters.Location)%>%
  mutate(share_city = n/sum(n))%>%
  mutate(lq = share_city/share_us)%>%
  filter(n>1)%>%
  # top_n(5,n)%>%
  arrange(-lq)

cb_city_g <- unnest_tokens(cb_all, out, Category.Groups, token = "regex", pattern =",")%>%
  mutate(out = trimws(out))%>%
  group_by(Headquarters.Location,out)%>%
  count(out, sort = TRUE)%>%
  ungroup()%>%
  mutate(total = sum(n))%>%
  group_by(out)%>%
  mutate(share_us = sum(n)/total)%>%
  ungroup()%>%
  group_by(Headquarters.Location)%>%
  mutate(share_city = n/sum(n))%>%
  mutate(lq = share_city/share_us)%>%
  # filter(n>1)%>%
  # top_n(5,n)%>%
  arrange(-lq)

cb_msa
