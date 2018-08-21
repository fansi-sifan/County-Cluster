# Get Raw Data and save to 'source' folder
# Author: Sifan Liu
# Date: Fri Aug 03 14:00:12 2018
# SET UP --------------
pkgs <- c('tidyverse', 'reshape2','writexl', 'httr')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n)) 

# DEFINE GEO -------------------------------------------------
city_FIPS <- "07000"
ct_FIPS <- "073"
msa_FIPS <- "13820"
st_FIPS <- "01"
county_FIPS <- paste0(st_FIPS, ct_FIPS)
msa100_FIPS <- as.character((read.csv("V:/Sifan/R/xwalk/top100metros.csv") %>% filter(top100==1))[["GEO.id2"]])
# peers from clustering result ================================
# Peers <- read.csv("source/counties_cluster_all.csv") %>%
#   mutate(cbsa = as.character(cbsa)) %>%
#   mutate(stcofips = padz(as.character(stcofips), 5)) %>%
#   group_by(kmeans) %>%
#   filter(msa_FIPS %in% cbsa) 
# modified peers tp include Nashiville, etc. ================
Peers <- readxl::read_xlsx('result/13820_Market Assessment.xlsx', sheet = "Peers") 


# GET DATA ----------------------------------------------------
# Metro Monitor ==============================================
load('source/MetroMonitor.Rda')
Peermetro_MM <- MM  %>%
  mutate(cbsa = as.character(`CBSA`))%>%
  right_join(Peers[c('cbsa','cbsa_name')], by = 'cbsa') %>%
  unique()

# Exports ====================================================
PeerMetro_export <- readxl::read_xlsx("V:/Export Monitor/2018/Deliverables/Deliverables/Metros Data/Metros by Total, NAICS 2 3.xlsx", sheet = "Total") %>%
  filter(Year == 2017)  %>%
  mutate(cbsa = as.character(`(CBSA)`))%>%
  right_join(Peers['cbsa'], by = 'cbsa') %>%
  unique()

PeerCounty_export <- readxl::read_xlsx("V:/Export Monitor/2018/Deliverables/Deliverables/Counties Data/Counties by Total, NAICS 2.xlsx", sheet = "Total") %>%
  filter(Year == 2017) %>%
  mutate(FIPS = padz(as.character(`(County)`), 5)) %>%
  right_join(Peers[c("cbsa","FIPS")], by ="FIPS")

MSA_export <- read.csv("V:/Export Monitor/2018/Deliverables/Deliverables/Metros Data/Metros  by NAICS 4.csv") %>%
  filter(gm == msa_FIPS) %>%
  filter(year == 2017)

# Traded Sector ==============================================
load("source/EMSI_master.Rda")
PeerMetro_ind <- EMSI_master %>%
  right_join(Peers[c('cbsa','cbsa_name')], by = 'cbsa_name' )%>%
  unique()

tradable <- read.csv("V:/Sifan/R/xwalk/tradable.csv")

PeerMetro_traded <- PeerMetro_ind %>%
  left_join(tradable, by =c("NAICS6" ="NAICS")) %>%
  group_by(cbsa, cbsa_name, Traded) %>%
  summarise(emp16 = sum(Jobs2016, na.rm = TRUE),
            emp06 = sum(Jobs2006, na.rm = TRUE)) %>%
  group_by(cbsa, cbsa_name) %>%
  mutate(EMP16 = sum(emp16),
         EMP06 = sum(emp06))%>%
  filter(Traded =="Traded")%>%
  mutate(EmpTradedShare16 = emp16/EMP16,
         EmpTradedShare06 = emp06/EMP06,
         EmpTradedCAGR0616 = (emp16/emp06)^0.1-1 )

# ShiftShare =================================================
MSA_shiftshare <- read.csv('V:/Performance/Project files/Metro Monitor/2018v/Output/Shift Share/Monitor Shiftshare Cumulative (2-digit NAICS).csv') %>% 
  filter(cbsa2013_fips == msa_FIPS) %>%
  filter(year == 2016)

# Digitalization =============================================
PeerMetro_digital <- read.csv("source/metro_all_updated.csv") %>%
  mutate(cbsa = as.character(`AREA`))%>%
  right_join(Peers['cbsa'], by = 'cbsa') %>%
  unique()

# Univ R&D ===========================================
NSF_univRD <- read.csv('source/NSF_univ.csv') 

NSF_fice <- unique((NSF_univRD %>% filter(cbsacode == msa_FIPS))[["fice"]])
url <- lapply(NSF_fice,function(x)paste0("https://ncsesdata.nsf.gov/profiles/site?method=download&id=h1&fice=",x))

MSA_univRD <- lapply(url, function(x){
  GET(x, write_disk(tf <- tempfile(fileext = '.xls')))
  df <- readxl::read_xls(tf)
  return(df)
})

names(MSA_univRD) <- NSF_fice


PeerMetro_univRD <- NSF_univRD %>%
  group_by(cbsacode) %>%
  summarise(RDtotal = sum(Deflated.Total.R.D.Expenditures.in.All.Fields.Sum.),
            RDtotal_biz = sum(as.numeric(as.character(Deflated.Business.Financed.R.D.Expenditures.Sum.)))) %>%
  mutate(cbsa = as.character(cbsacode)) %>%
  right_join(Peers['cbsa'], by = 'cbsa') %>%
  unique()

# REGPAT =====================================================
PeerMetro_REGPAT <- readxl::read_xlsx("V:/Global Profiles/Data/REGPAT/Analysis Files/_g4.xlsx", sheet = "i0") %>%
  filter(`Year Range` == "2008-2012") %>%
  right_join(Peers['cbsa'], by = c("(Micro-Regions)" = "cbsa")) %>%
  rename(cbsa = '(Micro-Regions)' ) %>%
  unique()

MSA_REGPAT <- readxl::read_xlsx("V:/Global Profiles/Data/REGPAT/Analysis Files/_g4_HAN.xlsx", sheet = "i3_app") %>%
  filter(`Year Range` == "2008-YTD") %>%
  filter(`(Micro-Regions)` == msa_FIPS)  %>% 
  group_by(`Tech Family`, `Tech Subgroup`) %>%
  summarise(ApplicationSum = sum(`Number of Patent Applications by Tech Subgroup (Micro-Regions)`),
            ApplicantSum = sum(`Number of Applicants per Patent by Tech Subgroup (Micro-Regions)`)) %>%
  arrange(-ApplicantSum)

# USPTO =======================================================
PeerMetro_USPTO <- read.csv('source/USPTO_msa.csv') %>%
  mutate(cbsa = substr(as.character(ID.Code),2,6)) %>%
  right_join(Peers['cbsa'], by = 'cbsa')

PeerCounty_USPTO <- read.csv('source/USPTO_county.csv') %>%
  mutate(FIPS = padz(as.character(FIPS.Code), 5)) %>%
  right_join(Peers[c("cbsa","FIPS")], by ="FIPS")

# Patent Complexity ============================================
PeerMetro_patentCOMP <- read.csv('source/Complexity_msa.csv') %>%
  mutate(cbsa = as.character(cbsa))%>%
  right_join(Peers['cbsa'], by = 'cbsa') %>%
  unique()

# VC ================================================
PeerMetro_VC <- read.csv('source/pitchbook_msa.csv') %>%
  mutate(cbsa = as.character(cbsa13)) %>%
  right_join(Peers['cbsa'], by = 'cbsa') %>%
  unique()

# Firm dynamics ================================================

source("ACSapi.R")

name = "timeseries/bds/firms"
time = "2014"

PeerMetro_BDS <- GetACS(name, c('fage4','estabs', "emp"),'MSA', time) %>%
  rename(cbsa = metropolitan_statistical_area) %>%
  right_join(Peers['cbsa'], by = 'cbsa') 

MSA_fafs <- read.csv("source/bds_f_agesz_msa_release.csv") %>% 
  filter(msa == msa_FIPS) %>% 
  filter(year2 == 2014)

MSA100_fafs <- read.csv("source/bds_f_agesz_msa_release.csv") %>% 
  filter(msa %in% msa100_FIPS) %>% 
  filter(year2 == 2014)

# Advanced Industries =========================================
ai <- read.csv("V:/Sifan/R/xwalk/advanced industries.csv") %>%
  mutate(NAICS4 = substr(NAICS, 1,4))

PeerMetro_ind$NAICS4 <- substr(PeerMetro_ind$Industry, 1,4)

PeerMetro_ai <- PeerMetro_ind %>%
  left_join(ai, by = "NAICS4") %>%
  group_by(cbsa, AI) %>%
  summarise(emp16 = sum(Jobs2016, na.rm = TRUE),
            emp06 = sum(Jobs2006, na.rm = TRUE)) %>%
  group_by(cbsa) %>%
  mutate(EMP16 = sum(emp16),
         EMP06 = sum(emp06))%>%
  filter(AI ==1)%>%
  mutate(EmpAIShare16 = emp16/EMP16,
         EmpAIShare06 = emp06/EMP06,
         EmpAICAGR0616 = (emp16/emp06)^0.1-1 )

# Broadband ====================================================
MSA_broadband <- readxl::read_xlsx("V:/Infrastructure/2 Long Form Projects/Broadband/Final Layout/Masterfile_Final.xlsx") %>%
  filter(cbsa == msa_FIPS)

# ACS all data ==================================================
source("ACSapi.R")

MSA_race <- getCensus("acs/acs5/subject", vars = c(edu_race,unemp_race, epop_race),
                          region = paste0("metropolitan statistical area/micropolitan statistical area:",msa_FIPS),
                          vintage = 2016)

County_flows <- getCensus(name = "acs/acs5", 
                          vintage = 2016,
                          vars = migration_edu,
                          region = paste0("county:", ct_FIPS),
                          regionin = paste0("state:",st_FIPS))


MSA_trend <- bind_rows(
  getCensus("acs/acs5", vars = earning_edu,
            region = paste0("metropolitan statistical area/micropolitan statistical area:",msa_FIPS),
            vintage = 2016) %>% 
    mutate(year = 2016),
  getCensus("acs/acs5", vars = earning_edu,
            region = paste0("metropolitan statistical area/micropolitan statistical area:",msa_FIPS),
            vintage = 2010) %>% 
    mutate(year = 2010))

PeerMetro_ACS <- bind_cols(
  GetACS("acs/acs5", c("NAME", transit), 'msa', vintage = 2016),
  GetACS("acs/acs5/subject",edu, 'msa', vintage = 2016)
)
names(PeerMetro_ACS)[1]<- "cbsa"

PeerMetro_ACS <- PeerMetro_ACS %>% right_join(Peers[c("cbsa")], by = "cbsa")

# load("source/ACS_county.Rda")
# County_ACS <- cty_dt %>%
#   right_join(Peers[c('FIPS','cbsa')], by = 'FIPS')

# place_ACS <- bind_cols(
#   getCensus(name = "acs/acs5",
#             vars = c("NAME", var_acs),
#             region = paste0('place:',city_FIPS),
#             vintage = 2016,
#             regionin = paste0('state:',st_FIPS)),
#   getCensus(name = "acs/acs5/subject",
#             vars = var_acs_sub,
#             region = paste0('place:',city_FIPS),
#             vintage = 2016,
#             regionin = paste0('state:',st_FIPS))
# )
# out of work ===================================================
PeerCounty_OoW <- read.csv('source/OutOfWork_county.csv') %>%
  mutate(FIPS = padz(fips, 5)) %>%
  right_join(Peers[c('FIPS','cbsa')], by = 'FIPS')
  

# ACS by tract ==============================================
MSA_tractACS <- getCensus(name = "acs/acs5",
                           vars = c("NAME",poverty, house_cost, house_type),
                           region = "tract:*",
                           regionin = paste0("state:",st_FIPS,"+county:", ct_FIPS),
                           vintage = 2016)

MSA_tractACS$id <- paste0(MSA_tractACS$state,padz(MSA_tractACS$county,3), padz(MSA_tractACS$tract,6))

# COG ==========================================================
# https://www.census.gov/govs/local/
PeerCounty_CoG <- read.csv("source/COG_county.csv") %>%
  right_join(Peers[c('FIPS')], by = c("GC.target.geo.id2" = "FIPS"))


# SAVE OUTPUT ---------------------------------------------------
dfs <- objects()
# datafiles <- mget(dfs[grep("Peer|MSA|labels", dfs)])
new <- mget(dfs[grep("Peer|MSA|labels|County", dfs)])
datafiles <- gdata::update.list(datafiles, new)

save(datafiles, file = paste0("result/",msa_FIPS,"_Market Assessment.Rdata"))
writexl::write_xlsx(datafiles, path = paste0("result/",msa_FIPS,"_Market Assessment.xlsx"))
