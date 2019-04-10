dim(datafiles$MSA_crunchbase)

# Author: Sifan Liu
# Date: Tue Mar 19 14:31:57 2019
# --------------
pkgs <- c('tidyverse','tidytext','wordcloud',"SifanLiu")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  } 

# read data

# crunchbase <- read.csv("source/crunchbase.csv")
cb_all <- read_all("../../Datasets/innovation/crunchbase/data")

# remove duplicates

# read AI 
cb_AI <- read.csv("../../Datasets/innovation/crunchbase/companies-4-5-2019.csv")
AI_cb <- c("artificial intelligence", "intelligent systems", "machine learning", 
"natural language processing", "predictive analytics")

# tokenize
df <- cb_AI%>%
  filter(Operating.Status=="Active")

cb_category <- unnest_tokens(df, out, Categories, token = "regex", pattern =",")%>%
  mutate(out = trimws(out))%>%
  group_by(out)%>%
  mutate(is.AI = (out%in%AI_cb))%>%
  count(out, sort = TRUE)

# by categories
cb_city <- unnest_tokens(df, out, Categories, token = "regex", pattern =",")%>%
  mutate(out = trimws(out))%>%
  group_by(Headquarters.Location,out)%>%
  count(out, sort = TRUE)%>%
  ungroup()%>%
  mutate(total = sum(n))%>%
  group_by(out)%>%
  mutate(share_us = sum(n)/total)%>%
  ungroup()%>%
  group_by(Headquarters.Location)%>%
  # mutate(share_city = n/sum(n))%>%
  # mutate(lq = share_city/share_us)%>%
  filter(n>1)%>%
  mutate(is.AI = (out%in%AI_cb))%>%
  filter(is.AI)%>%
  # top_n(5,n)%>%
  arrange(-n)

# by category groups
cb_city_g <- unnest_tokens(df, out, Category.Groups, token = "regex", pattern =",")%>%
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

