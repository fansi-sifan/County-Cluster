library(tidyverse)

univ_RD_18 <- read.csv("../../metro-datasets/source/NSF_univ_18.csv") 

univ_RD_life <- univ_RD_18 %>%
  mutate(year = as.integer(as.character(year)))%>%
  filter(year > 2012)%>%
  filter(univ_abb != "Total")%>%
  filter(field_broad %in% c("Total","Life sciences"))%>%
  mutate(field = paste(field_broad, field_detail, sep = ", "),
         value_rd = federal+non_federal)%>%  
  filter(field %in% c("Total, Total","Life sciences, Biological and biomedical sciences", 
                      "Life sciences, Other Life sciences", "Life sciences, Health sciences"))%>%
  group_by(univ_abb,st_name, field)%>%
  summarise(value_rd = sum(value_rd))%>%
  spread(field, value_rd)%>%
  rename(bio = `Life sciences, Biological and biomedical sciences`,
         health =`Life sciences, Health sciences`,
         other = `Life sciences, Other Life sciences`,
         total = `Total, Total`)%>%
  mutate(lifescience_total = sum(bio,health,other,na.rm = T),
         lifescience_share = lifescience_total/total)


write.csv(univ_RD_life,"univ_RD_lifescience_12-17.csv")


# EMSI data

EMSI <- read.csv("../../metro-datasets/Emsi_2019.2_ind_data.csv")%>%
  select(stco_code = Area, naics6_code = Industry, co_emp = Jobs)

SBE_cluster <- read.csv("../../R/xwalk/SBE clusters.csv")

county_emp_tot <- EMSI %>% 
  group_by(stco_code)%>%
  summarise(co_emp_tot = sum(co_emp, na.rm = T))

county_emp_life <- EMSI %>% 
  left_join(SBE_cluster, by = "naics6_code")%>%
  filter(cluster == "Life Sciences")%>%
  group_by(stco_code)%>%
  summarise(co_emp_life = sum(co_emp, na.rm = T))

county_emp_hospital <- EMSI %>% 
  left_join(SBE_cluster, by = "naics6_code")%>%
  filter(cluster == "Hospitals and Health Services")%>%
  group_by(stco_code)%>%
  summarise(co_emp_hospital = sum(co_emp, na.rm = T))

tmp <- county_emp_tot%>%
  left_join(county_emp_life, by = "stco_code")%>%
  left_join(county_emp_hospital, by = "stco_code")%>%
  mutate(co_pct_life = co_emp_life/co_emp_tot,
         co_pct_hospital = co_emp_hospital/co_emp_tot)%>%
  ungroup()%>%
  mutate(us_emp_life = sum(co_emp_life),
         us_emp_hospital = sum(co_emp_hospital),
         us_emp_tot = sum(co_emp_tot),
         us_pct_life = us_emp_life/us_emp_tot,
         us_pct_hospital = us_emp_hospital/us_emp_tot,
         co_lq_life = co_pct_life/us_pct_life,
         co_lq_hospital = co_pct_hospital/us_pct_hospital)

county_cbsa_st <- readr::read_csv("../../../metro_data_warehouse/crosswalks/county_cbsa_st.csv")%>%
  mutate(stco_code = stco_fips)

county_healthcluster <- tmp %>%
  mutate(stco_code = stringr::str_pad(stco_code,5,"left","0"))%>%
  left_join(county_cbsa_st[c("stco_code","co_name","cbsa_code","cbsa_name","cbsa_type")], by = "stco_code")

cbsa_healthcluster <- county_healthcluster%>%
  filter(cbsa_type == "metro"|cbsa_type =="top100")%>%
  group_by(cbsa_code,cbsa_name,us_pct_life,us_pct_hospital,us_emp_life,us_emp_hospital)%>%
  summarise(cbsa_emp_life = sum(co_emp_life),
            cbsa_emp_hospital = sum(co_emp_hospital),
            cbsa_emp_tot = sum(co_emp_tot))%>%
  ungroup()%>%
  mutate(cbsa_pct_life = cbsa_emp_life/cbsa_emp_tot,
         cbsa_pct_hospital = cbsa_emp_hospital/cbsa_emp_tot,
         cbsa_lq_life = cbsa_pct_life/us_pct_life,
         cbsa_lq_hospital = cbsa_pct_hospital/us_pct_hospital)%>%
  select(-contains("us_"))

write.csv(cbsa_healthcluster,"cbsa_healthcluster.csv")

cbsa_healthcluster%>%
  filter(cbsa_code == "13820")

county_healthcluster%>%
  filter(stco_code == "01073")
