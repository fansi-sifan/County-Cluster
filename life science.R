library(tidyverse)
source("../../SifanLiu/R/EDA tools.R")
source("../../SifanLiu/R/add2fips.R")
load("../../metro.data/data/county_cbsa_st.rda")

univ_RD_18 <- read.csv("../../metro-datasets/source/NSF_univ_18.csv")
univ_RD <- read.csv("../../metro-datasets/source/NSF_univ.csv") %>%
  select(
    univ_name = Academic.Institution..Campus.Level..survey.specific.,
    st_name = state,
    univ_code = fice, stco_code = fips, cbsa_code = cbsacode
  ) %>%
  unique() %>%
  mutate(univ_name = gsub("\\, The", "", univ_name))


univ_RD_life <- univ_RD_18 %>%
  mutate(year = as.integer(as.character(year))) %>%
  filter(year > 2012) %>%
  filter(univ_abb != "Total") %>%
  filter(field_broad %in% c("Total", "Life sciences")) %>%
  mutate(
    field = paste(field_broad, field_detail, sep = ", "),
    value_rd = federal + non_federal
  ) %>%
  filter(field %in% c(
    "Total, Total", "Life sciences, Biological and biomedical sciences",
    "Life sciences, Other Life sciences", "Life sciences, Health sciences"
  )) %>%
  group_by(univ_abb, st_name, field) %>%
  summarise(value_rd = sum(value_rd)) %>%
  spread(field, value_rd) %>%
  rename(
    bio = `Life sciences, Biological and biomedical sciences`,
    health = `Life sciences, Health sciences`,
    other = `Life sciences, Other Life sciences`,
    total = `Total, Total`
  ) %>%
  mutate(
    lifescience_total = sum(bio, health, other, na.rm = T),
    lifescience_share = lifescience_total / total
  ) %>%
  ungroup()

# uni <- univ_RD_life%>%
#   select(univ_abb, st_name)%>%
#   mutate(univ_name = gsub("U\\. ","University of ", univ_abb),
#          univ_name = gsub("C\\.", "College", univ_name),
#          univ_name = gsub("U\\.", "University", univ_name))%>%
#   unique()
#
# uni_geo <- uni %>% left_join(univ_RD, by = c("univ_name","st_name"))
#
# matched <- uni_geo%>% filter(!is.na(stco_code))%>%
#   mutate(stco_code = str_pad(stco_code, 5, "left","0"))

# geocoding ====
# unmatched <- uni_geo %>% filter(is.na(stco_code))%>%
#   mutate(address = paste(univ_name,", ", st_name))%>% ungroup()

# for (i in 1:nrow(unmatched)) {
#   unmatched$stcobk_fips[[i]] <- add2FIPS(unmatched$address[[i]], Sys.getenv("GOOGLE_MAP_KEY"))
# }

# update place list with stco_fips
uni <- bind_rows(
  matched,
  unmatched %>%
    mutate(stco_code = str_sub(unmatched$stcobk_fips, 1, 5)) %>%
    select(-stcobk_fips, -address)
) %>% select(-cbsa_code)

# merge back
univ_RD_18_geo <- univ_RD_18 %>%
  left_join(uni, by = c("univ_abb", "st_name"))

write.csv(univ_RD_18_geo, "../../metro-datasets/source/NSF_univ_18.csv")



univ_RD_life_co <- univ_RD_life %>%
  left_join(uni, by = c("univ_abb", "st_name")) %>%
  group_by(stco_code) %>%
  summarise_if(is.integer, sum, na.rm = T) %>%
  ungroup() %>%
  mutate(lifescience_share = lifescience_total / total)

univ_RD_life_cbsa <- univ_RD_life_co %>%
  left_join(county_cbsa_st[c("stco_code", "cbsa_code", "co_name", "cbsa_name")], by = "stco_code") %>%
  group_by(cbsa_code, cbsa_name) %>%
  summarise_if(is.integer, sum, na.rm = T) %>%
  ungroup() %>%
  mutate(lifescience_share = lifescience_total / total)


write.csv(univ_RD_life, "univ_RD_lifescience_12-17.csv")


# EMSI data

load("V:/metro_data_warehouse/data_raw/emsi/emsi_co_emp_18.rda")
SBE_cluster <- read.csv("../../R/xwalk/SBE clusters.csv")

county_emp_tot <- EMSI %>%
  group_by(stco_code) %>%
  summarise(co_emp_tot = sum(co_emp, na.rm = T))

county_emp_life <- EMSI %>%
  left_join(SBE_cluster, by = "naics6_code") %>%
  filter(cluster == "Life Sciences") %>%
  group_by(stco_code) %>%
  summarise(co_emp_life = sum(co_emp, na.rm = T))

county_emp_hospital <- EMSI %>%
  left_join(SBE_cluster, by = "naics6_code") %>%
  filter(cluster == "Hospitals and Health Services") %>%
  group_by(stco_code) %>%
  summarise(co_emp_hospital = sum(co_emp, na.rm = T))

tmp <- county_emp_tot %>%
  left_join(county_emp_life, by = "stco_code") %>%
  left_join(county_emp_hospital, by = "stco_code") %>%
  mutate(
    co_pct_life = co_emp_life / co_emp_tot,
    co_pct_hospital = co_emp_hospital / co_emp_tot
  ) %>%
  ungroup() %>%
  mutate(
    us_emp_life = sum(co_emp_life),
    us_emp_hospital = sum(co_emp_hospital),
    us_emp_tot = sum(co_emp_tot),
    us_pct_life = us_emp_life / us_emp_tot,
    us_pct_hospital = us_emp_hospital / us_emp_tot,
    co_lq_life = co_pct_life / us_pct_life,
    co_lq_hospital = co_pct_hospital / us_pct_hospital
  )

county_healthcluster <- tmp %>%
  mutate(stco_code = stringr::str_pad(stco_code, 5, "left", "0")) %>%
  left_join(county_cbsa_st[c("stco_code", "co_name", "cbsa_code", "cbsa_name", "cbsa_type")], by = "stco_code")

cbsa_healthcluster <- county_healthcluster %>%
  filter(cbsa_type == "metro" | cbsa_type == "top100") %>%
  group_by(cbsa_code, cbsa_name, us_pct_life, us_pct_hospital, us_emp_life, us_emp_hospital) %>%
  summarise(
    cbsa_emp_life = sum(co_emp_life),
    cbsa_emp_hospital = sum(co_emp_hospital),
    cbsa_emp_tot = sum(co_emp_tot)
  ) %>%
  ungroup() %>%
  mutate(
    cbsa_pct_life = cbsa_emp_life / cbsa_emp_tot,
    cbsa_pct_hospital = cbsa_emp_hospital / cbsa_emp_tot,
    cbsa_lq_life = cbsa_pct_life / us_pct_life,
    cbsa_lq_hospital = cbsa_pct_hospital / us_pct_hospital
  ) %>%
  select(-contains("us_"))

# write.csv(cbsa_healthcluster,"cbsa_healthcluster.csv")

cbsa_healthcluster %>%
  filter(cbsa_code == "13820")

county_healthcluster %>%
  filter(stco_code == "01073")

# Read startup data
startup_life_co <- read_csv("source/startup_life_co.csv")
startup_life_cbsa <- read_csv("source/startup_life_cbsa.csv") %>% mutate(cbsa_code = as.character(cbsa_code))


# join

co_lifesci <- univ_RD_life_co %>%
  left_join(county_healthcluster[c("stco_code", "co_emp_life", "co_emp_hospital", "co_lq_life", "co_lq_hospital")], by = "stco_code") %>%
  left_join(startup_life_co, by = "stco_code") %>%
  left_join(county_cbsa_st[c("stco_code", "co_name")], by = "stco_code") %>%
  filter(co_n_lifesci >= 1) %>%
  mutate(
    hlsup_lscrd = co_n_lifesci / lifescience_total * 1000000,
    lscemp_lscrd = co_emp_life / lifescience_total * 1000,
    hlsup_lscemp = co_n_lifesci / co_emp_life * 1000,
    hlsup_totemp = co_n_lifesci / (co_emp_life + co_emp_hospital) * 1000
  )

skimr::skim(co_lifesci)
write.csv(co_lifesci, "result/co_lifesci.csv")

cbsa_lifesci <- univ_RD_life_cbsa %>%
  left_join(cbsa_healthcluster[c("cbsa_code", "cbsa_emp_life", "cbsa_emp_hospital", "cbsa_lq_life", "cbsa_lq_hospital")], by = "cbsa_code") %>%
  left_join(startup_life_cbsa, by = "cbsa_code") %>%
  filter(cbsa_n_lifesci >= 1) %>%
  mutate(
    hlsup_lscrd = cbsa_n_lifesci / lifescience_total * 1000000,
    lscemp_lscrd = cbsa_emp_life / lifescience_total * 1000,
    hlsup_lscemp = cbsa_n_lifesci / cbsa_emp_life * 1000,
    hlsup_totemp = cbsa_n_lifesci / (cbsa_emp_life + cbsa_emp_hospital) * 1000
  )

write.csv(cbsa_lifesci, "result/cbsa_lifesci.csv")
