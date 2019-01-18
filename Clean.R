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

source('Func.R')

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

# modified peers to include Nashiville, etc. ================
peerlist <- c("34980", "47260", "33340", "32820","40060", 
              "31140", "35380", "15380", "40380","26620",
              "28140", "17460", "26900", "12940", msa_FIPS)
Peers <- readxl::read_xlsx('result/13820_Market Assessment.xlsx', sheet = "Peers")%>%
  filter(cbsa %in% peerlist)

msa_ct_FIPS <- read.csv('V:/Sifan/R/xwalk/county2msa.csv') %>%
  mutate(fips = paste0(padz(fipsstatecode,2), padz(fipscountycode,3)),
         COUNTY = trimws(toupper(gsub("County","",countycountyequivalent))))%>%
  filter(cbsacode %in% Peers$cbsa)%>%
  mutate(cbsa = as.character(cbsacode))%>%
  select(cbsa, FIPS = fips, metro = cbsatitle, county = COUNTY)

# GET DATA ----------------------------------------------------
# Metro Monitor ==============================================
load('source/MetroMonitor.Rda')
PeerMetro_MM <- MM  %>%
  mutate(cbsa = as.character(`CBSA`))%>%
  right_join(Peers[c('cbsa','cbsa_name')], by = 'cbsa') %>%
  unique()

inclusion_change <- read_csv(paste0(paths,"Inclusion/Inclusion Change (IS 2018.12.11).csv"))%>%
  filter(cbsa == as.integer(msa_FIPS))%>%
  filter(year == "2007-2017")

MSA_inclusion <- inclusion_change%>%
  mutate(var = ifelse(race=="Total",eduatt,race))%>%
  select(-se,-eduatt,-race,-cbsa)%>%
  spread(var,value)%>%
  group_by(indicator)%>%
  summarise_if(is.numeric,sum,na.rm = TRUE)%>%
  select(indicator, Total, rank, White, `People of Color`, `BA or Above`, `High School`)
  

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

County_export <- read.csv("V:/Export Monitor/2018/Deliverables/Deliverables/Counties Data/Counties by NAICS 4.csv")%>%
  filter(gc == as.integer(county_FIPS))%>%
  filter(year == 2017)

# Traded Sector ==============================================
tradable <- read.csv("V:/Sifan/R/xwalk/tradable.csv")
Peer_ind <- read.csv("source/Emsi_2018.4_ind_data.csv")
Peer_traded <- Peer_ind %>%
  left_join(tradable, by = c("Industry" = "NAICS"))%>%
  group_by(Area, Area.Name,Area.Bucket,Traded, Year)%>%
  summarise(Emp = sum(Jobs))%>%
  group_by(Area, Year)%>%
  mutate(Emptot = sum(Emp),
         EmpShare = Emp/Emptot,
         FIPS = padz(Area, 5))%>%
  filter(Traded==1)%>%
  left_join(Peers[c("FIPS", "county")], by = "FIPS")%>%
  left_join(Peers[c("cbsa", "metro")], by = c("FIPS" = "cbsa"))%>%
  mutate(metro = ifelse(is.na(metro),county, metro))
  
  
# load("source/EMSI_master.Rda")
# PeerMetro_ind <- EMSI_master %>%
#   right_join(Peers[c('cbsa','cbsa_name')], by = 'cbsa_name' )%>%
#   unique()
# 
# 
# PeerMetro_traded <- datafiles$PeerMetro_ind %>%
#   left_join(tradable, by =c("NAICS6" ="NAICS")) %>%
#   filter(!is.na(Traded))%>%
#   group_by(cbsa, cbsa_name, Traded) %>%
#   summarise(emp16 = sum(Jobs2016, na.rm = TRUE),
#             emp06 = sum(Jobs2006, na.rm = TRUE)) %>%
#   group_by(cbsa, cbsa_name) %>%
#   mutate(EMP16 = sum(emp16),
#          EMP06 = sum(emp06))%>%
#   filter(Traded =="Traded")%>%
#   mutate(EmpTradedShare16 = emp16/EMP16,
#          EmpTradedShare06 = emp06/EMP06,
#          EmpTradedCAGR0616 = (emp16/emp06)^0.1-1 )

# oppot ========================

MSA_opp <- readxl::read_xlsx("V:/Performance/Project files/Opportunity Industries/Data/Output/Final/Metros/13820 Birmingham AL/13820 Birmingham AL BMPP Opportunity Industries - 2017 JobS.xlsx")%>%
  filter(Level == 6)%>%
  mutate(NAICS = as.integer(NAICS))%>%
  left_join(tradable, by = "NAICS")%>%
  group_by(Traded)%>%summarise_if(is.numeric, sum)%>%
  gather(type, value, `Good sub-BA jobs`:`Other jobs`)%>%
  mutate(share = value/`Total jobs`)


# Advanced Industries =========================================
ai <- read.csv("V:/Sifan/R/xwalk/advanced industries.csv") %>%
  mutate(NAICS4 = substr(NAICS, 1,4))

# PeerMetro_ind$NAICS4 <- substr(PeerMetro_ind$Industry, 1,4)
# PeerMetro_ai <- PeerMetro_ind %>%
#   left_join(ai, by = "NAICS4") %>%
#   group_by(cbsa, AI) %>%
#   summarise(emp16 = sum(Jobs2016, na.rm = TRUE),
#             emp06 = sum(Jobs2006, na.rm = TRUE)) %>%
#   group_by(cbsa) %>%
#   mutate(EMP16 = sum(emp16),
#          EMP06 = sum(emp06))%>%
#   filter(AI ==1)%>%
#   mutate(EmpAIShare16 = emp16/EMP16,
#          EmpAIShare06 = emp06/EMP06,
#          EmpAICAGR0616 = (emp16/emp06)^0.1-1 )

Peer_ind$NAICS4 <- substr(Peer_ind$Industry, 1,4)

Peer_ai <- Peer_ind %>%
  left_join(ai, by = "NAICS4") %>%
  group_by(Area, Area.Name,Area.Bucket, Year, AI) %>%
  summarise(Emp = sum(Jobs, na.rm = TRUE)) %>%
  group_by(Area, Year) %>%
  mutate(Emptot = sum(Emp),
         EmpShare = Emp/Emptot,
         FIPS = padz(Area,5))%>%
  filter(AI==1)%>%
  left_join(Peers[c("FIPS", "county")], by = "FIPS")%>%
  left_join(Peers[c("cbsa", "metro")], by = c("FIPS" = "cbsa"))%>%
  mutate(metro = ifelse(is.na(metro),county, metro))



# ShiftShare =================================================
MSA_ss <- read.csv('V:/Performance/Project files/Metro Monitor/2018v/Output/Shift Share/Monitor Shiftshare Cumulative (2-digit NAICS).csv') %>%
  filter(cbsa2013_fips == msa_FIPS) %>%
  filter(year == 2016)

t <- datafiles$PeerMetro_ind %>%
  filter(cbsa == msa_FIPS)%>%
  left_join(tradable, by =c("NAICS6" ="NAICS")) %>%
  filter(!is.na(Traded))%>%
  filter(Traded==1)%>%
  # arrange(Jobs2016)%>%
  top_n(10,Jobs2016)

ggplot(t,aes(x=reorder(Description,Jobs2016),y = Jobs2016, label = Jobs2016))+
  geom_bar(stat = "identity", fill = "#0070c0")+
  geom_text(nudge_y = 1000)+
  labs(title = "Largest tradable industries \n(6 digit NAICS) in Birmingham MSA",
       x=NULL,y="Number of Jobs, 2016")+
  pthemes%+%theme(axis.text.x = element_blank())+
  coord_flip()

MSA_shiftshare <- read.csv("source/Regional_Comparison_Report3045.csv")%>%
  gather(place, value, Birmingham.Hoover..AL:Jefferson.County..AL)%>%
  spread(Metric, value)%>%
  set_tidy_names()%>%
  mutate(Jobs_actual=`Jobs (2016)`-`Jobs (2006)`,
         Jobs_expected = Jobs_actual-`Competitive Effect`)%>%
  filter(!Sub.Category %in% c("Unclassified Industry", "Other Services (except Public Administration"))


# Digitalization =============================================
PeerMetro_digital <- read.csv("source/metro_all_updated.csv") %>%
  mutate(cbsa = as.character(`AREA`))%>%
  right_join(Peers['cbsa'], by = 'cbsa') %>%
  unique()

# Univ R&D ===========================================
NSF_univRD <- read.csv('source/NSF_univ.csv') 

PeerMetro_univRD <- NSF_univRD %>%
  group_by(cbsacode) %>%
  summarise(RDtotal = sum(Deflated.Total.R.D.Expenditures.in.All.Fields.Sum.),
            RDtotal_biz = sum(as.numeric(as.character(Deflated.Business.Financed.R.D.Expenditures.Sum.)))) %>%
  mutate(cbsa = as.character(cbsacode)) %>%
  right_join(Peers['cbsa'], by = 'cbsa') %>%
  unique()

PeerCounty_univRD <- NSF_univRD %>%
  group_by(county) %>%
  summarise(RDtotal = sum(Deflated.Total.R.D.Expenditures.in.All.Fields.Sum.),
            RDtotal_biz = sum(as.numeric(as.character(Deflated.Business.Financed.R.D.Expenditures.Sum.)))) %>%
  mutate(FIPS = padz(as.character(county), 5)) %>%
  right_join(Peers[c("cbsa","FIPS")], by ="FIPS") %>%
  unique()

# universities in the MSA
NSF_fice <- unique((NSF_univRD %>% filter(cbsacode %in% Peers$cbsa))[["fice"]])
url <- lapply(NSF_fice,function(x)paste0("https://ncsesdata.nsf.gov/profiles/site?method=download&id=h1&fice=",x))

MSA_univRD <- lapply(url, function(x){
  GET(x, write_disk(tf <- tempfile(fileext = '.xls')))
  df <- readxl::read_xls(tf)
  return(df)
})

names(MSA_univRD) <- NSF_fice


# AUTM ======================================================
AUTM <- read.csv("source/AUTM.csv") 


# geocoding ---------------------------

# add <- AUTM %>% 
#   # arrange(desc(YEAR))%>%
#   select(X.ID., INSTITUTION, STATE, INSTTYPE)%>%
#   filter(INSTTYPE=="5U")%>%
#   unique()%>%
#   group_by(X.ID.)%>%
#   slice(1)

# add2FIPS("Albert Einstein College of Med/Yeshiva University, NY", KEY)

# add <- add %>%
#   mutate(add = paste(INSTITUTION,STATE, sep = ","))
# 
# for(i in 1:nrow(add)){
#   add$FIPS[[i]] <- add2FIPS(add$add[[i]],KEY)
# }
# 
# for(i in which(is.na(add$FIPS))){
#   add$FIPS[[i]] <- add2FIPS("University of Utah, UT",KEY)
# }

# AUTM <- AUTM %>% 
#   select(-contains("INSTITUTION"), -contains("STATE"),-contains("INSTTYPE"))%>%
#   left_join(add[c("X.ID.", "FIPS")], by = "X.ID.")
# 
# AUTM <- AUTM %>% mutate(FIPS = substr(FIPS.y, 1,5))
# 
# write.csv(AUTM, "source/AUTM.csv")

# analysis --------------------------------
# convert dollar to numeric
Peer_AUTM <- AUTM %>% 
  group_by(FIPS)%>%
  # summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  summarise(tot_lic = sum(Lic.Iss,na.rm = TRUE)+sum(Opt.Iss,na.rm = TRUE),
            lg_lic = sum(Tot.Lic.Lg.Co,na.rm = TRUE),
            sm_lic = sum(Tot.Lic.Sm.Co,na.rm = TRUE),
            st_lic = sum(Tot.Lic.St.Ups,na.rm = TRUE),
            inc_lic = sum(Gross.Lic.Inc,na.rm = TRUE),
            tot_IP = sum(Inv.Dis.Rec,na.rm = TRUE),
            tot_st = sum(St.Ups.Formed,na.rm = TRUE),
            instate_st = sum(St.Ups.in.Home.St,na.rm = TRUE))%>%
  mutate(FIPS = padz(as.character(FIPS), 5)) %>%
  inner_join(msa_ct_FIPS[c("FIPS","cbsa")], by = "FIPS")

# crunchbase
crunchbase <- read.csv("source/crunchbase.csv")
Peer_crunchbase <- crunchbase%>%
  group_by(Headquarters.Location)%>%
  summarise(count = n())%>%
  separate(Headquarters.Location, c("city","state","country"), sep= ",")

# find cbsa codes
for(i in 1:nrow(Peer_crunchbase)){
  city <- Peer_crunchbase$city[[i]]
  po <- grep(city,Peers$cbsa_name)
  Peer_crunchbase$cbsa[[i]] <- Peers$cbsa[[po]]
}

Peer_crunchbase <- Peer_crunchbase%>%
  group_by(cbsa)%>%
  summarise(count = sum(count))%>%
  left_join(Peers[c("cbsa","msaemp","metro")], by = "cbsa")

MSA_crunchbase <- crunchbase%>%
  filter(grepl("Birmingham|Hoover",Headquarters.Location))


# REGPAT =====================================================
# PeerMetro_REGPAT <- readxl::read_xlsx("V:/Global Profiles/Data/REGPAT/Analysis Files/_g4.xlsx", sheet = "i0") %>%
#   filter(`Year Range` == "2008-2012") %>%
#   right_join(Peers['cbsa'], by = c("(Micro-Regions)" = "cbsa")) %>%
#   rename(cbsa = '(Micro-Regions)' ) %>%
#   unique()

MSA_REGPAT <- readxl::read_xlsx("V:/Global Profiles/Data/REGPAT/Analysis Files/_g4_HAN.xlsx", sheet = "i3_inv") %>%
  # filter(`Year Range` == "2008-YTD") %>%
  filter(`(Micro-Regions)` == msa_FIPS)
  # group_by(`Tech Family`, `Tech Subgroup`) %>%
  # mutate(ApplicationSum = sum(`Number of Patent Applications by Tech Subgroup (Micro-Regions)`),
  #           ApplicantSum = sum(`Number of Applicants per Patent by Tech Subgroup (Micro-Regions)`)) %>%
  # group_by(Company, `Tech Family`)%>%mutate(company_n = n())%>%
  # arrange(-ApplicantSum)

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
# PeerMetro_VC <- read.csv('source/pitchbook_msa.csv') %>%
#   mutate(cbsa = as.character(cbsa13)) %>%
#   right_join(Peers['cbsa'], by = 'cbsa') %>%
#   unique()

PeerMetro_VC <- read.csv('source/VC.csv') %>%
  filter(round == "Total VC" & measure == "Capital Invested ($ M) per 1M Residents") %>%
  mutate(cbsa = as.character(cbsa13))%>%
  right_join(Peers[c('cbsa', 'cbsa_name')], by = 'cbsa') %>%
  unique()
  

# Firm dynamics ================================================

# source("ACSapi.R")
# 
# name = "timeseries/bds/firms"
# time = "2016"

# PeerMetro_BDS <- GetACS(name, c('fage4','estabs', "emp"),'MSA', time) %>%
#   rename(cbsa = metropolitan_statistical_area) %>%
#   right_join(Peers['cbsa'], by = 'cbsa') 

PeerMetro_fafs <- read.csv("source/bds_f_agesz_msa_release.csv")%>% 
  # filter(year2 %in% c(2004,2009,2014)) %>% 
  filter(msa %in% Peers$cbsa) %>%
  filter(year2 >=2006)%>%
  separate(fsize, c("size_code", "size_label"), sep = "\\)") %>%
  separate(fage4, c("age_code", "age_label"), sep = "\\)") %>%
  mutate(Age = case_when(.$age_code %in% letters[1:6] ~ "0 ~ 5 years",
                         .$age_code == "g" ~ "6 ~ 10 years",
                         .$age_code %in% letters[7:12]~ "more than 11 years")) %>%
  mutate(Size = case_when(.$size_code %in% letters[1:5] ~ "Small ( < 100 employees)",
                          .$size_code %in% letters[5:6] ~ "Medium ( 100 - 500 employees)",
                          .$size_code %in% letters[6:12]~ "Large ( 500 employees and above)")) %>%
  group_by(Age, Size, year2,msa) %>%
  summarise(emp = sum(emp, na.rm = TRUE), 
            net_job_creation = sum(net_job_creation, na.rm = TRUE),
            count=n())

# t <- MSA_fafs%>%
#   filter(year2>2006)%>%
#   group_by(fsize,fage4)%>%
#   summarise(JbC = mean(net_job_creation, na.rm = TRUE))




# Broadband ====================================================
MSA_broadband <- readxl::read_xlsx("V:/Infrastructure/2 Long Form Projects/Broadband/Final Layout/Masterfile_Final.xlsx") %>%
  filter(cbsa == msa_FIPS)

# Census data ==================================================
source("ACSapi.R")

# ACS
# MSA_race <- getCensus("acs/acs5/subject", vars = c(edu_race,unemp_race, epop_race),
#                           region = paste0("metropolitan statistical area/micropolitan statistical area:",msa_FIPS),
#                           vintage = 2016)
# 
# County_flows <- getCensus(name = "acs/acs5", 
#                           vintage = 2016,
#                           vars = migration_edu,
#                           region = paste0("county:", ct_FIPS),
#                           regionin = paste0("state:",st_FIPS))
# 
# 
# MSA_trend <- bind_rows(
#   getCensus("acs/acs5", vars = earning_edu,
#             region = paste0("metropolitan statistical area/micropolitan statistical area:",msa_FIPS),
#             vintage = 2016) %>% 
#     mutate(year = 2016),
#   getCensus("acs/acs5", vars = earning_edu,
#             region = paste0("metropolitan statistical area/micropolitan statistical area:",msa_FIPS),
#             vintage = 2010) %>% 
#     mutate(year = 2010))
# 
# PeerMetro_ACS <- bind_cols(
#   GetACS("acs/acs5", c("NAME", transit), 'msa', vintage = 2016),
#   GetACS("acs/acs5/subject",edu, 'msa', vintage = 2016)
# )
# names(PeerMetro_ACS)[1]<- "cbsa"
# 
# PeerMetro_ACS <- PeerMetro_ACS %>% right_join(Peers[c("cbsa")], by = "cbsa")

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
# PeerCounty_OoW <- read.csv('source/OutOfWork_county.csv') %>%
#   mutate(FIPS = padz(fips, 5)) %>%
#   right_join(Peers[c('FIPS','cbsa')], by = 'FIPS')
  

# ACS by tract ==============================================
# MSA_tractACS_16 <- getCensus(name = "acs/acs5",
#                            vars = c("NAME",demographics, house_cost, house_type),
#                            region = "tract:*",
#                            regionin = paste0("state:",st_FIPS,"+county:", ct_FIPS),
#                            vintage = 2016) 
# 
# MSA_tractACS_10 <- getCensus(name = "acs/acs5",
#                           vars = c("NAME",demographics, house_cost, house_type),
#                           region = "tract:*",
#                           regionin = paste0("state:",st_FIPS,"+county:", ct_FIPS),
#                           vintage = 2010)
# 
# MSA_tractACS <- MSA_tractACS_16 %>%
#   left_join(MSA_tractACS_10, 
#             by = c("state","county","tract","NAME"),
#             suffix = c(".16", ".10")) %>%
#   mutate(id = paste0(state,padz(county,3), padz(tract,6)))


# COG ==========================================================
# https://www.census.gov/govs/local/
# PeerCounty_CoG <- read.csv("source/COG_county.csv") %>%
#   right_join(Peers[c('FIPS')], by = c("GC.target.geo.id2" = "FIPS"))

# SME loans peers ==========================================================

load("Temp data/SBA_loan_cleaned.Rda")
year1 <- seq(2011,2016)
year2 <- seq(2005,2010)

# EXIM
EXIM <- loan_datafiles$EXIM_matched%>%
  # only includes working capital loans, excludes insurance
  filter(Program =="Working Capital")  %>%
  mutate(
    year_range = case_when(
      Fiscal.Year %in% year2 ~ "2005 - 2010",
      Fiscal.Year %in% year1 ~ "2011 - 2016")) %>%
  group_by(county14,State, year_range)%>%
  summarise(amt.tot = sum(Approved.Declined.Amount, na.rm = TRUE),
            DISBamt.tot = sum(Disbursed.Shipped.Amount, na.rm = TRUE),
            count = n())%>%
  mutate(FIPS = padz(county14,5),
         program = "EXIM") %>%
  right_join(msa_ct_FIPS, by = "FIPS")

#SBA
SBA <- loan_datafiles$SBA_matched  %>%
  mutate(
    year_range = case_when(
      ApprovalFiscalYear %in% year2 ~ "2005 - 2010",
      ApprovalFiscalYear %in% year1 ~ "2011 - 2016")) %>%
  group_by(State, county14, year_range) %>%
  summarise(amt.tot = sum(GrossApproval, na.rm = TRUE))%>%
  mutate(FIPS = padz(as.integer(county14),5),
         program = "SBA") %>%
  right_join(msa_ct_FIPS, by = "FIPS")

#SSTR
SSTR <- loan_datafiles$SSTR_matched %>%
  mutate(
    year_range = case_when(
      Award.Year %in% year2 ~ "2005 - 2010",
      Award.Year %in% year1 ~ "2011 - 2016")) %>%
  group_by(State,county14, year_range) %>%
  summarise(amt.tot = sum(Award.Amount, na.rm = TRUE), 
            count = n())%>%
  mutate(FIPS = padz(as.integer(county14),5),
         program = "SSTR")%>%
  right_join(msa_ct_FIPS, by = "FIPS")


#TLR
TLR <- loan_datafiles$TLR_matched %>%
  mutate(
    year_range = case_when(
      Year %in% year2 ~ "2005 - 2010",
      Year %in% year1 ~ "2011 - 2016")) %>%
  # only includes investees that are businesses or CDFIs, excludes individuals
  filter(investeetype != "IND") %>%
  group_by(county14, year_range) %>%
  summarise(amt.tot = sum(originalamount, na.rm = TRUE),
            count = n()) %>%
  mutate(FIPS = padz(as.integer(county14),5),
         program = "CDFI") %>%
  inner_join(msa_ct_FIPS, by = "FIPS")

#FDIC
FDIC <- loan_datafiles$FDIC_matched %>%
  mutate(
    year_range = case_when(
      year %in% year2 ~ "2005 - 2010",
      year %in% year1 ~ "2011 - 2016")) %>%
  group_by(county14, year_range) %>%
  summarise_if(is.numeric,sum, na.rm = TRUE)%>%
  mutate(FIPS = padz(as.integer(county14),5),
         program = "FDIC",
         amt.tot = x_tot*1000) %>%
  inner_join(msa_ct_FIPS, by = "FIPS")

# COMBINED
PeerCounty_SMEloans <- bind_rows(
  sapply(list(SBA, EXIM,SSTR, TLR, FDIC), filter,FIPS%in%Peers$FIPS))

PeerMetro_SMEloans <- bind_rows(
  lapply(list(SBA, EXIM,SSTR, TLR, FDIC), function(df){
    df %>% group_by(cbsa, metro, year_range, program)%>%
      summarise(amt.tot = sum(amt.tot, na.rm = TRUE))}))

# PeerCounty_SMEloans <- bind_rows(datafiles$PeerCounty_SMEloans %>% filter(program != "SSTR"), PeerCounty_SSTR)

# SME loans Bham ==========================================================
MSA_SMEloan <- purrr::map(loan_datafiles,filter,county14 == as.numeric(county_FIPS))

# SBA geocoding -----------------------------------------------
# address <- MSA_SMEloan$SBA_matched %>% select(BorrStreet, BorrCity, BorrState) %>% distinct()
# names(address) <- c("street","city","state")
# address$street <- gsub("\\#", "Apt", address$street)
# address <- censusr::append_geoid(address, geoid_type = "tract")
# 
# names(address) <- c("BorrStreet", "BorrCity","BorrState", "FIPS")
# 
# MSA_SMEloan$SBA_matched <- MSA_SMEloan$SBA_matched %>%
#   left_join(address, by = c("BorrStreet", "BorrCity","BorrState"))
# 
# MSA_SMEloan$SBA_matched <- MSA_SMEloan$SBA_matched %>%
#   mutate(FIPS = ifelse(is.na(FIPS.x), FIPS.y, FIPS.x)) %>%
#   select(-FIPS.x, -FIPS.y)
# na.share(address,"geoid")
# # SSTR geocoding -----------------------------------------------
# address <- MSA_SMEloan$SSTR_matched %>% select(Address1, City, State) %>% distinct()
# names(address) <- c("street","city","state")
# address <- censusr::append_geoid(address, geoid_type = "tract")
# 
# names(address) <- c("Address1", "City", "State", "FIPS")
# 
# MSA_SMEloan$SSTR_matched  <- MSA_SMEloan$SSTR_matched %>%
#   left_join(address, by = c("Address1", "City", "State"))


# source("V:/Sifan/R/code/add2FIPS.R")

# add2FIPS("Albert Einstein College of Med/Yeshiva University, NY", KEY)

# address_unmatched <- address_unmatched %>%
#   mutate(add = paste(street,city,state, sep = ","))
# for(i in 1:nrow(address)){
#   address$FIPS[[i]] <- add2FIPS(address$add[[i]],key)
# }
# for(i in which(is.na(address_unmatched$FIPS))){
#   address_unmatched$FIPS[[i]] <- add2FIPS(address_unmatched$add[[i]],key)
# }

# EXIM geocoding -----------------------------------------------
# address <- datafiles$MSA_SMEloan$EXIM_matched %>% select(Primary.Exporter, Primary.Exporter.City, Primary.Exporter.State.Code) %>% distinct()
# address <- address %>%
#   mutate(add = paste(Primary.Exporter, Primary.Exporter.City, Primary.Exporter.State.Code, sep = ","))
# 
# for(i in 1:nrow(address)){
#   address$FIPS[[i]] <- add2FIPS(address$add[[i]],key)
# }
# 
# MSA_SMEloan$EXIM_matched <- MSA_SMEloan$EXIM_matched %>%
#   left_join(address, by = c("Primary.Exporter", "Primary.Exporter.City", "Primary.Exporter.State.Code"))
# 
# 
# writexl::write_xlsx(datafiles$MSA_SMEloan, path = paste0("result/",county_FIPS,"_SMEloans.xlsx"))


# CBP ============================================================
# readLines("source/cbp16msa/cbp16msa.txt",5)
CBP <- read.delim("source/cbp16msa/cbp16msa.txt", sep = ",")
CBP_Bham <- CBP %>%
  filter(msa == msa_FIPS)

# SAVE OUTPUT ---------------------------------------------------
dfs <- objects()
# datafiles <- mget(dfs[grep("Peer|MSA|labels", dfs)])
new <- mget(dfs[grep("Peer_ai|Peer_traded", dfs)])
datafiles <- gdata::update.list(datafiles, new)

save(datafiles, file = paste0("result/",msa_FIPS,"_Market Assessment.Rdata"))
# writexl::write_xlsx(new, path = paste0("result/",msa_FIPS,"_Market Assessment_new.xlsx"))
