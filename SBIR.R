# Author: Sifan Liu
# Date: Fri May 17 16:13:05 2019
# -------------- SBIR/STTR
pkgs <- c('tidyverse',"skimr","SifanLiu","httr")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

# SSTR read raw --------------------------------------------

load("Temp data/SBA_loan_cleaned.Rda")
sbir <- loan_datafiles$SSTR_matched


# geocode ---------------------------------
place <- sbir %>%
  mutate(pl_label = tolower(trimws(City)))%>%
  select(pl_label, State, county14)%>%
  rename(st_ab = State)%>%
  filter(str_length(pl_label)>0)%>%
  filter(is.na(county14))%>%
  unique()%>%
  mutate(add = paste0(pl_label,",", st_ab))

KEY <- Sys.getenv("GOOGLE_MAP_KEY")

library(mapsapi)
library(xml2)

# test
add2FIPS("research triangle, NC", KEY)  
add2FIPS("south san francis, CA", KEY)

# geocode
for (i in 1:nrow(place)) {
  place$stcobk_fips[[i]] <- add2FIPS(place$add[[i]],KEY)
}

place$stco_fips <- substr(place$stcobk_fips,1,5)

# merge back
SBIR_matched <- bind_rows(
  sbir %>% filter(!is.na(county14))%>%
    mutate(stco_fips = str_pad(county14,5,"left",pad = "0")),
  sbir %>%
    filter(is.na(county14)) %>%
    select(-county14,st_ab = State)%>%
    mutate(pl_label = tolower(trimws(City)))%>% 
    left_join(place[c("pl_label","st_ab","stco_fips")], by = c("pl_label","st_ab")))

save(SBIR_matched,file = "SBIR_matched.rda")

# clean columns -----------------------
SSTR_summary <- SBIR_matched%>%
  select(stco_fips,Company, Phase, Program, Agency,
         year = Award.Year,amt = Award.Amount,
         Hubzone.Owned, Socially.and.Economically.Disadvantaged, Woman.Owned)%>%
  # standardize company names
  mutate(name = tolower(gsub("[[:punct:]]+","",Company)),
         program = as.factor(Program),
         agency = as.factor(Agency),
         phase = as.factor(Phase),
         year = as.factor(year))%>%
  mutate(Hub = ifelse(Hubzone.Owned=="Y",1,0),
         gender = ifelse(Woman.Owned=="Y",1,0),
         disadv = ifelse(Socially.and.Economically.Disadvantaged=="Y",1,0))%>%
  select(-contains("."),-Program,-Agency, -Phase,-Company)%>%
  group_by(name)%>%
  mutate(count_awards=n())%>%
  ungroup()

skim(SSTR_summary)  
save(SSTR_summary,file = "SSTR_summary.Rda")

# summary functions
SSTR_type <- function(df){
  df%>%
    group_by(phase,program,stco_fips)%>%
    summarise(count = n())%>%
    ungroup()%>%
    mutate(share = count/sum(count))
}

SSTR_gender <- function(df){
  df %>%
    select(stco_fips,name,year,count_awards,Hub:disadv)%>%
    gather(key,value, Hub:disadv)%>%
    # remove 2007 2008 inconsistency
    filter(year!=2008 & year!=2007)%>%
    group_by(name,key,stco_fips)%>%
    # assume the latest year is most accurate
    arrange(year)%>%
    slice(n())%>%
    group_by(key,stco_fips)%>%
    summarise(count.company = n(),
              count.awards = sum(count_awards),
              share.company = mean(value),
              share.award= weighted.mean(value, count_awards))
}

SSTR_all <- function(df){
  df%>%
    group_by(stco_fips)%>%
    summarise(count = n(),
              amt = sum(amt, na.rm = T))
}

SSTR_summary%>%SSTR_all()
SSTR_summary%>%SSTR_gender()
SSTR_summary%>%SSTR_type()


# summary for nation, Jefferson County, and Davidson County ---------

df.list <- purrr::map(list(SSTR_summary %>% mutate(county14=0),
                           SSTR_summary %>% filter(county14==1073),
                           SSTR_summary %>% filter(county14==47037)),
                      filter,Year > 2010)

type <- map_df(df.list,SSTR_type)%>%
  arrange(Program, Phase)

demo <- map_df(df.list,SSTR_gender)%>%
  arrange(key)

map_df(df.list, SSTR_all)

# peer distribution
temp <- SSTR_summary %>%
  # right_join(Peers[c("FIPS","county")])%>%
  # filter(FIPS%in%Peers$FIPS)%>%
  filter(year>2010)%>%
  group_by(Phase,Program,county)%>%
  summarise(count = n())%>%
  ungroup()%>%
  mutate(share = count/sum(count))

bbplot(temp%>%mutate(type = paste(Program,Phase,sep=", ")))+
  geom_bar(aes(x=reorder(county,share),y=share,fill = type), stat = "identity",position = "fill")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(name = "places")+
  coord_flip()

type%>%select(-share)%>%
  spread(county14,count)
