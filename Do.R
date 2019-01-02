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

# assign geocodes for analysis ------------------------
city_FIPS <- "07000"
ct_FIPS <- "073"
msa_FIPS <- "13820"
st_FIPS <- "01"
county_FIPS <- paste0(st_FIPS, ct_FIPS)

# assign names for text display ------------------------
placename <- "Birmingham"
countyname <- "Jefferson County, AL"
metroname <- "Birmingham MSA"

# load datasets ------------------------------
load(paste0("result/",msa_FIPS,"_Market Assessment.Rdata"))
list2env(datafiles,envir=.GlobalEnv)

# maps data ----------------------------------
usa <- map_data("state")
cbsa = core_based_statistical_areas()
# map.cty <- tidy(tracts(state = st_FIPS, county = ct_FIPS), region = "GEOID")
map.Bham <- tidy(places(state = st_FIPS) %>% filter_place(placename), region = "GEOID")

# LODES by census block group -----------------------
downl_LODES <- function(year, st){
  temp <- tempfile()
  download.file(paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",st,"/wac/",st,"_wac_S000_JT02_",year,".csv.gz"), temp)
  data <- read.csv(gzfile(temp))
  unlink(temp)
  return(data)
}
tract_emp <- downl_LODES(2015,"al") %>% 
  mutate(FIPS = substr(padz(as.character(w_geocode),15),1,11),
         county = substr(FIPS, 1,5)) %>%
  filter(county == county_FIPS) %>%
  select(-w_geocode, - createdate)%>%
  group_by(FIPS,county) %>%
  summarise_if(is.numeric, sum)
# census tracts ---------------------------------------
var1990 <- c(sapply(seq(1,5),function(x)paste0("P006000",x)),"P0010001")
var2000 <-  sapply(seq(1,6),function(x)paste0("P00300",x)) 
var2010 <-  sapply(seq(1,5),function(x)paste0("P00300",x)) 

Bham_tract_data <- function(YEAR){get_decennial(geography = "tract", year = YEAR,
                                                state = st_FIPS, county = ct_FIPS,output = "wide",geometry = TRUE,
                                                variables = eval(parse(text = paste0("var",YEAR))))}

years <- lst(1990, 2000, 2010)
census <- purrr::map(seq(1990,2010,10),Bham_tract_data) %>%map2(years, ~ mutate(.x, id = .y))


# ANALYSIS ============================================
# Peer map
map_data <- cbsa@data %>% filter(CBSAFP %in% Peers$cbsa)
map_data$long = -as.numeric(substring(map_data$INTPTLON,2))
map_data$lat = as.numeric(map_data$INTPTLAT)

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
         geo = "county")
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
  mutate(value = rowSums(.[2:6])/8) %>%
  select(-contains("X")) %>%
  rbind(c("All other fields",(.$value[1] - sum(.$value[2:4])))) %>%
  filter(fields != "All R&D fields") %>%
  mutate(value = as.numeric(value))

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
  mutate(value = value*msapop/msaemp/1000,
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
SMEloans <- bind_rows(PeerMetro_SMEloans %>%
                    right_join(datafiles$Peers[c('cbsa','metro',"msaemp")], by = 'cbsa') %>%
                    group_by(Bham = (cbsa == msa_FIPS),year_range,program) %>%
                    mutate(emp.tot = sum(msaemp, na.rm = TRUE))%>%
                    opr()%>%
                    mutate(geo = "MSA"),
                  
                  PeerCounty_SMEloans %>%
                    right_join(datafiles$Peers[c("FIPS", "county", "ctyemp")], by = "FIPS")%>%
                    group_by(Bham = (FIPS == county_FIPS),year_range,program) %>%
                    mutate(emp.tot = sum(ctyemp, na.rm = TRUE))%>%
                    opr()%>%
                    mutate(geo = "county"))
# FDIC
# map
FDIC_Bham_alt <- MSA_SMEloan$FDIC_matched %>%
  mutate(FIPS = gsub("\\.","",FIPS),
         year = as.integer(year),
         id = paste0(State, county, FIPS))%>%
  left_join(tract_emp, by = c("id" = "FIPS"))%>%
  group_by(id, C000) %>%
  summarise(value = sum(x_tot, na.rm = TRUE)) %>%
  mutate(value = value/C000,
         level = cut(value, breaks = c(0,10,50,100,Inf), include.lowest = TRUE))%>%
  left_join(census[[3]][c("GEOID","geometry")], by = c("id" = "GEOID"))

# race
FDIC_race <- MSA_SMEloan$FDIC_matched %>%
  filter(year>=2012)%>%
  mutate(FIPS = gsub("\\.","",FIPS),
         year = as.integer(year),
         id = paste0(State, county, FIPS))%>%
  left_join(tract_emp, by = c("id" = "FIPS"))%>%
  filter(!is.na(C000))%>%
  summarise(w_tot = sum(x_tot)*sum(CR01/C000),
            m_tot = sum(x_tot)*(sum((C000-CR01)/C000)),
            w_pop = sum(CR01),
            m_pop = sum(C000)) %>%
  mutate(w_per = w_tot/w_pop,
         m_per = m_tot/m_pop)%>%
  gather(var, value, w_tot:m_per) %>%
  separate(var, into = c("race", "var"),sep="_")

FDIC_Bham_tract <- MSA_SMEloan$FDIC_matched %>%
  # filter(year>=2012)%>%
  mutate(FIPS = gsub("\\.","",FIPS),
         year = as.integer(year),
         id = paste0(State, county, FIPS))%>%
  group_by(id, year)%>%
  summarise(x_tot = sum(x_tot, na.rm = TRUE))%>%
  left_join(tract_emp, by = c("id" = "FIPS"))%>%
  filter(!is.na(C000))%>%
  mutate(is.white = (CR01/C000>0.5))%>%
  group_by(is.white, year)%>%
  summarise(emp = sum(C000, na.rm = TRUE),
            x_tot = sum(x_tot, na.rm = TRUE))%>%
  mutate(value = x_tot/emp)

# CDFI
# MAP
CDFI_Bham <- MSA_SMEloan$TLR_matched%>%
  filter(Year>=2006)%>%
  select(Year, FIPS, gender, race,investeetype, purpose,originalamount) %>%
  left_join(tract_emp, by = "FIPS")%>%
  group_by(FIPS, C000) %>%
  summarise(sum = sum(originalamount, na.rm = TRUE)) %>%
  mutate(value = sum/C000,
         level = cut(value, breaks = c(0,10,50,100,Inf), include.lowest = TRUE))%>%
  right_join(census[[3]][c("GEOID","geometry")], by = c("FIPS" = "GEOID"))


# race
TLR_Bham_weight <- MSA_SMEloan$TLR_matched%>%
  filter(Year >=2012) %>%
  group_by(FIPS) %>%
  summarise(x_tot = sum(originalamount, na.rm = TRUE))%>%
  left_join(tract_emp, by = "FIPS")%>%
  filter(!is.na(C000))%>%
  summarise(w_tot = sum(x_tot)*sum(CR01/C000),
            m_tot = sum(x_tot)*(sum((C000-CR01)/C000)),
            w_pop = sum(CR01),
            m_pop = sum(C000)) %>%
  mutate(w_per = w_tot/w_pop,
         m_per = m_tot/m_pop)%>%
  gather(var, value, w_tot:m_per) %>%
  separate(var, into = c("race", "var"),sep="_")

TLR_Bham_tract <- MSA_SMEloan$TLR_matched%>%
  # filter(Year >=2012) %>%
  group_by(FIPS) %>%
  summarise(x_tot = sum(originalamount, na.rm = TRUE))%>%
  right_join(tract_emp[c("FIPS","C000","CR01")], by = "FIPS")%>%
  filter(!is.na(C000))%>%
  mutate(is.white = (CR01/C000>0.5))%>%
  group_by(is.white)%>%
  summarise(emp = sum(C000),
            x_tot = sum(x_tot, na.rm = TRUE))%>%
  mutate(value = x_tot/emp)
