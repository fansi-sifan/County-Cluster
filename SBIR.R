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
         phase = as.factor(Phase))%>%
  mutate(Hub = ifelse(Hubzone.Owned=="Y",1,0),
         gender = ifelse(Woman.Owned=="Y",1,0),
         disadv = ifelse(Socially.and.Economically.Disadvantaged=="Y",1,0))%>%
  # remove duplicates
  unique()%>%
  select(-contains("."),-Program,-Agency, -Phase,-Company)%>%
  group_by(name)%>%
  mutate(count_awards=n())%>%
  ungroup()

skimr::skim(SSTR_summary)  
save(SSTR_summary,file = "SSTR_summary.Rda")

## LOAD ==============================
load("SSTR_summary.rda")


SSTR_blog <- SSTR_summary%>%
# take out transactions without addresses
    filter(!is.na(stco_fips))%>%
  # filter(year==2017)%>%
  # mutate(year_range = case_when(
  #   year<=2014 ~ "2012 ~ 2014",
  #   year>2014 ~ "2015 ~ 2017")%>%
# only recent trends, 2012
    filter(year >=2005 & year <=2017)

# To do ============================
# fix gender
# assume the latest label is accurate?
company <- SSTR_blog %>%
  group_by(stco_fips, name) %>%
  mutate(m_Hub = mean(Hub,na.rm = T),
         m_gender = mean(gender, na.rm = T),
         m_disadv = mean(disadv, na.rm = T))

company_matched <- company%>%filter(m_gender==1|m_gender ==0)%>%ungroup%>%select(stco_fips,name,gender)%>%unique()
company_unmatched <- company%>%filter(m_gender!=1&m_gender !=0)%>%ungroup%>%select(stco_fips,name,gender,year,m_gender)

# fix
company_fix <- company_unmatched %>%
  # if 2007 and 2008 are the only outliers
  filter(year!=2007 & year!=2008)%>%
  group_by(stco_fips, name) %>%
  summarise(m_gender = mean(gender, na.rm = T))%>%
  # if outliers are small enough
  mutate(m_gender = case_when(
    m_gender > 0.8 ~ 1,
    m_gender < 0.2 ~ 0,
    T ~ m_gender
  ))%>%
  filter(m_gender==1|m_gender ==0)%>%
  ungroup%>%
  mutate(gender = case_when(
    m_gender > 0.8 ~ 1,
    m_gender < 0.2 ~ 0,
    T ~ m_gender
  ))%>%
  select(stco_fips,name,gender)%>%
  unique()

# update

company_matched <- bind_rows(company_matched,company_fix)%>%unique()

company_unmatched %>%left_join(company_matched,by = c("stco_fips","name"))%>%
  filter(is.na(gender.y))%>%arrange(name)
  
SSTR_blog <- SSTR_blog %>%
  select(- gender)%>%
  left_join(company_matched,by = c("stco_fips","name"))

# companies ------------------------------
c <- SSTR_blog %>%
  group_by(stco_fips,name)%>%
  summarise(count = n(),
            amt = sum(amt,na.rm = T)) %>%
  ungroup()%>%
  mutate(count_pct = count/sum(count),
         amt_pct = amt/sum(amt))%>%
  arrange(-amt_pct)%>%
  mutate(cumamt = cumsum(amt_pct))

# match by msa -----------------------------------------------
cbsa_SSTR <- SSTR_blog %>% ungroup %>%
  left_join(county_cbsa_st, by = "stco_fips") %>%
  # rural counties as nonmetro
  mutate(cbsa_type = ifelse(is.na(cbsa_type),"nonmetro",as.character(cbsa_type)))

# Count and amount per employee, by cbsa

t <- cbsa_SSTR%>%
  # filter(cbsa_type %in% c("top100","metro"))%>%
  group_by(cbsa_code,cbsa_name,cbsa_emp,cbsa_type)%>%
  # group_by(year_range)%>%
  # group_by(agency)%>%
  summarise(cbsa_count = n(),
         cbsa_amt = sum(amt,na.rm = T))%>%
  ungroup()%>%
  mutate(cbsa_count_per_emp = cbsa_count/cbsa_emp,
         cbsa_amt_per_emp = cbsa_count/cbsa_emp,
         cbsa_count_pct =  cbsa_count/sum(cbsa_count),
         cbsa_amt_pct = cbsa_amt/sum(cbsa_amt))


# summarise by metro type =================
cbsa_type_sstr <- t %>%
  ungroup()%>%
  # filter(!is.na(cbsa_emp))%>%
  group_by(cbsa_type)%>%
  summarise(cbsa_type_count = sum(cbsa_count),
         cbsa_type_amt = sum(cbsa_amt,na.rm = T),
         cbsa_type_emp = sum(cbsa_emp,na.rm = T))%>%
  # calculate nonmetro employment, national total - all other types
  mutate(cbsa_type_emp = ifelse(cbsa_type_emp==0,
                                sum(county_cbsa_st$co_emp)-sum(cbsa_type_emp,na.rm = T),
                                cbsa_type_emp))%>%
  mutate(cbsa_type_count_per_emp = cbsa_type_count/cbsa_type_emp,
         cbsa_type_amt_per_emp = cbsa_type_amt/cbsa_type_emp,
         cbsa_type_count_pct =  cbsa_type_count/sum(cbsa_type_count),
         cbsa_type_amt_pct = cbsa_type_amt/sum(cbsa_type_amt))
    
  
write.csv(t,"cbsa_sstr.csv") 

# summary =================
# overall

y <- cbsa_SSTR %>%
  group_by(year) %>%
  summarise(year_count = n(),
            year_amt = sum(amt, na.rm = T),
            year_hub = mean(Hub,na.rm = T),
            year_gender = mean(gender, na.rm = T),
            year_disadv = mean(disadv, na.rm = T),
            year_hub_wt = weighted.mean(Hub,amt,na.rm = T),
            year_gender_wt = weighted.mean(gender, amt, na.rm = T),
            year_disadv_wt = weighted.mean(disadv, amt,na.rm = T))%>%
  ungroup()%>%
  mutate(year_pct_count = year_count/sum(year_count),
         year_pct_amt = year_amt/sum(year_amt))%>%
  arrange(-year)

# by msa
g <- cbsa_SSTR %>%
  # filter(!is.na(gender))%>%
  group_by(cbsa_code,cbsa_name,cbsa_type,gender)%>%
  summarise(cbsa_count = n(),
            cbsa_amt = sum(amt, na.rm = T))%>%
  mutate(cbsa_count_pct = cbsa_count/sum(cbsa_count),
         cbsa_amt_pct = cbsa_amt/sum(cbsa_amt))%>%
  ungroup()%>%
  mutate(cbsa_avg_amt = cbsa_amt/cbsa_count)

m <- cbsa_SSTR %>%
  # filter(!is.na(gender))%>%
  group_by(cbsa_code,cbsa_name,cbsa_type,disadv)%>%
  summarise(cbsa_count = n(),
            cbsa_amt = sum(amt, na.rm = T))%>%
  mutate(cbsa_count_pct = cbsa_count/sum(cbsa_count),
         cbsa_amt_pct = cbsa_amt/sum(cbsa_amt))%>%
  ungroup()%>%
  mutate(cbsa_avg_amt = cbsa_amt/cbsa_count)

m <- m%>%left_join(cbsa_minority,by = "cbsa_code")
  
m <- m%>%mutate(delta = cbsa_pct_minority-cbsa_amt_pct)

g %>% group_by(cbsa_type,gender)%>%
  summarise(cbsa_type_count = sum(cbsa_count),
            cbsa_type_amt = sum(cbsa_amt))%>%
  mutate(cbsa_type_count_pct = cbsa_type_count/sum(cbsa_type_count),
         cbsa_type_amt_pct = cbsa_type_amt/sum(cbsa_type_amt),
         cbsa_type_avg = cbsa_type_amt/cbsa_type_count)
