# Author: Sifan Liu
# Date: Thu Dec 27 10:50:31 2018
# --------------

# PREPARE ============================================
pkgs <- c('tidyverse','tigris', 'tidycensus')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

source('Func.R')

# load datasets ------------------------------
load(paste0("result/",msa_FIPS,"_Market Assessment.Rdata"))
list2env(datafiles,envir=.GlobalEnv)

# ANALYSIS ============================================
# shift share
Traded_NAICS2 <- c("FH", "21", "31", "51", "52", "54", "61", "FR")

ss <- MSA_ss %>%
  filter(cbsa2013_fips %in% msa_FIPS) %>%
  select(naics2, industryname_naics2, indicator,lsshare2006,value) %>%
  gather(type, value, lsshare2006:value) %>%
  unite(temp, indicator, type) %>%
  spread(temp, value) %>%
  mutate(Wage_2016 = `Aggregate Wage_value`/Employment_value, 
         traded_2 = ifelse(naics2 %in% Traded_NAICS2, 1, 0)) %>%
  filter(!naics2 %in% c("PH", "PNFTOT", "TOTAL"))

Wageline <- as.integer((PeerMetro_MM%>%filter(cbsa==msa_FIPS))["Average Annual Wage"]*1000)


# export
export_cty <- PeerCounty_export %>%
  select(FIPS, `County Name (BEA)`, 
         value = `Export Share of GDP (%), by All-Industries (County)`)%>%
  mutate(HL = c(FIPS == county_FIPS),
         geo = "County")
export_msa <- PeerMetro_export %>%
  select(cbsa,
         metro = `CBSA Short Name (2013)`, 
         value = `Export Share of GDP (%), by All-Industries (CBSA)`) %>%
  mutate(HL = c(cbsa == msa_FIPS),
         geo = "MSA")

export <- rbind(export_msa, setNames(export_cty, names(export_msa)))

# NSF
NSF <- bind_rows(PeerMetro_univRD %>%
                   left_join(datafiles$Peers[c('cbsa','metro',"msaemp")], by = 'cbsa') %>%
                   select(FIPS = cbsa,
                          emp = msaemp,
                          metro,
                          sum = RDtotal
                   ) %>%
                   mutate(HL = c(FIPS == msa_FIPS), 
                          value = sum/5/emp, geo = "msa"),
                 
                 PeerCounty_univRD %>%
                   left_join(datafiles$Peers[c('FIPS', "county", "ctyemp")], by = "FIPS")%>%
                   select(FIPS, 
                          emp = ctyemp, 
                          metro = county.y, 
                          sum = RDtotal) %>%
                   mutate(HL = c(FIPS == county_FIPS), 
                          value = sum/5/emp, geo = "county")
)

# UAB
RD_cat <- c("Biological and biomedical sciences", "Health sciences", "Life sciences, nec","Engineering", "All R&D fields")

UAB_RD <- datafiles$MSA_univRD$`1052` %>% 
  rename(fields  =`University of Alabama at Birmingham, The` ) %>%
  filter(fields %in% RD_cat) %>%
  select_if(is.character) %>%
  mutate_at(vars(matches("X")), as.numeric) %>%
  mutate(value = rowSums(.[2:6])/10) %>%
  select(-contains("X")) %>%
  rbind(c("All other fields",(.$value[1] - sum(.$value[2:4])))) %>%
  filter(fields != "All R&D fields") %>%
  mutate(value = as.numeric(value))

# AUTM
AUTM <- bind_rows(
  Peer_AUTM %>% 
    inner_join(Peers[c("FIPS", "county", "ctyemp")], by = "FIPS")%>%
    mutate(HL = c(FIPS == county_FIPS),
           value = inc_lic/12/ctyemp*1000,
           geo = "County")%>%
    rename(metro = county,msaemp = ctyemp),
  Peer_AUTM %>%
    group_by(cbsa) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)%>%
    inner_join(Peers[c('cbsa','metro',"msaemp")], by = 'cbsa') %>%
    mutate(HL = c(cbsa == msa_FIPS),
           value = inc_lic/12/msaemp*1000,
           geo = "MSA")
)

# university startups
uni_startup <- AUTM %>%
  select(metro, HL, msaemp,geo,tot_st,instate_st) %>%
  mutate(outstate_st = tot_st-instate_st)%>%
  gather(type, value, instate_st:outstate_st)%>%
  mutate(value_p = value/msaemp*1000)

# startcups
startup <- Peer_crunchbase%>%
  mutate(HL = c(cbsa==msa_FIPS),
         value = count/msaemp*1000)

# USPTO
patent_cty <- PeerCounty_USPTO %>%
  left_join(Peers[c("FIPS", "county", "ctyemp")], by = "FIPS")%>%
  select(FIPS,ctyemp, county, 
         value = Total
  ) %>%
  mutate(HL = c(FIPS == county_FIPS),
         value = value/ctyemp*1000, 
         geo = "county")
patent_msa <- PeerMetro_USPTO %>%
  left_join(Peers[c('cbsa','metro',"msaemp")], by = 'cbsa') %>%
  select(cbsa,msaemp,
         metro,
         value = Total
  ) %>%
  mutate(HL = c(cbsa == msa_FIPS),
         value = value/msaemp*1000, 
         geo = "MSA")

patent <- rbind(patent_msa,setNames(patent_cty, names(patent_msa)))

# PCI
patentCOMP <- PeerMetro_patentCOMP %>%
  left_join(Peers[c('cbsa','metro',"msaemp")], by = 'cbsa') %>%
  select(cbsa,msaemp,
         metro,
         value = complex
  ) %>%
  mutate(HL = c(cbsa == msa_FIPS))

# VC
VC <- PeerMetro_VC %>%
  left_join(datafiles$Peers, by = 'cbsa') %>%
  mutate(value = value*msapop/msaemp*1000,
         HL = c(cbsa == msa_FIPS))

# firm size and age
fsfa <- PeerMetro_fafs %>%
  filter(msa == msa_FIPS) %>%
  group_by(Age, Size, year2) %>%
  summarise(emp = sum(emp, na.rm = TRUE), 
            net_job_creation = sum(net_job_creation, na.rm = TRUE),
            count=n())

# young
young <- fsfa %>% 
  group_by(year2) %>% 
  mutate(emp.share = emp/sum(emp))%>%
  filter(Age == "0 ~ 5 years")%>%
  mutate(emp.stot = sum(emp.share))

# inc5000
I5HGC <- datafiles$PeerMetro_I5HGC %>%
  right_join(Peers, by = 'cbsa') %>%
  select(cbsa,msaemp,msapop,metro,I5HGC_Density) %>%
  mutate(value = I5HGC_Density*msapop/msaemp/1000) %>%
  mutate(HL = c(cbsa == msa_FIPS))

# SME loans
# function to summarise SME data
opr <- function(df){
  df %>%
    group_by(Bham, program, year_range, emp.tot, emp.traded)%>%
    summarise(amt.tot = sum(as.numeric(amt.tot), na.rm = TRUE))%>%
    filter(!is.na(year_range))%>%
    mutate(value = amt.tot/emp.tot*1000)
}

SMEloans <- bind_rows(PeerMetro_SMEloans %>%
                    right_join(Peer_traded, by = c('cbsa'='FIPS')) %>%
                    group_by(Bham = (cbsa == msa_FIPS),year_range,program) %>%
                    mutate(emp.traded = sum(Emp, na.rm = TRUE),
                           emp.tot = sum(Emptot, na.rm = TRUE))%>%
                    opr()%>%
                    mutate(geo = "MSA"),
                  
                  PeerCounty_SMEloans %>%
                    right_join(Peer_traded, by = "FIPS")%>%
                    group_by(Bham = (FIPS == county_FIPS),year_range,program) %>%
                    mutate(emp.traded = sum(Emp, na.rm = TRUE),
                           emp.tot = sum(Emptot, na.rm = TRUE))%>%
                    opr()%>%
                    mutate(geo = "County"))
# FDIC

# race
FDIC_race <- MSA_SMEloan$FDIC_matched %>%
  filter(year>=2012)%>%
  mutate(FIPS = gsub("\\.","",FIPS),
         year = as.integer(year),
         id = paste0(State, county, FIPS))%>%
  left_join(tract_emp, by = c("id" = "FIPS"))%>%
  filter(!is.na(C000))%>%
  summarise(w_tot = sum(x_tot*(CR01/C000)),
            m_tot = sum(x_tot*((C000-CR01)/C000)),
            w_pop = sum(CR01),
            m_pop = sum(C000)) %>%
  mutate(w_per = w_tot/w_pop,
         m_per = m_tot/m_pop,
         tot_w = w_tot/(w_tot+m_tot),
         pop_w = w_pop/(w_pop+m_pop))
# %>%
#   gather(var, value, w_tot:m_per) %>%
#   separate(var, into = c("race", "var"),sep="_")



# CDFI
# race
TLR_Bham_weight <- MSA_SMEloan$TLR_matched%>%
  filter(Year >=2012) %>%
  group_by(FIPS) %>%
  summarise(x_tot = sum(originalamount, na.rm = TRUE))%>%
  left_join(tract_emp, by = "FIPS")%>%
  filter(!is.na(C000))%>%
  summarise(w_tot = sum(x_tot*(CR01/C000)),
            m_tot = sum(x_tot*((C000-CR01)/C000)),
            w_pop = sum(CR01),
            m_pop = sum(C000)) %>%
  mutate(w_per = w_tot/w_pop,
         m_per = m_tot/m_pop,
         tot_w = w_tot/(w_tot+m_tot),
         pop_w = w_pop/(w_pop+m_pop))
# %>%
#   gather(var, value, w_tot:m_per) %>%
#   separate(var, into = c("race", "var"),sep="_")

# commute by car
commute_car <- PeerMetro_ACS %>%
  left_join(Peers[c('cbsa','metro',"msaemp")], by = 'cbsa') %>%
  select(cbsa,msaemp,
         metro,
         value = complex
  ) %>%
  mutate(HL = c(cbsa == msa_FIPS))

