# Author: Sifan Liu
# Date: Wed Feb 27 12:12:30 2019
# --------------
pkgs <- c('tidyverse',"tigris",'tidycensus')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 
source("Func.R")

# GET MAPS =========================================
# maps data ----------------------------------
usa <- map_data("state")
cbsa = core_based_statistical_areas()
map.Bham <- places(state = st_FIPS, class = "sf") %>% filter_place(placename)
# map.cty <- tracts(state = st_FIPS, county = substr(msa_ct_fips,3,5), class = "sf")
map.cty <- get_acs(geography = "tract", year = 2017,
        state = st_FIPS, county = substr(msa_ct_fips,3,5),
        output = "wide",geometry = TRUE,
        variables = "B01003_001E")

# Peer map ----------------------------------
map_data <- cbsa@data %>% filter(CBSAFP %in% Peers$cbsa)
map_data$long = -as.numeric(substring(map_data$INTPTLON,2))
map_data$lat = as.numeric(map_data$INTPTLAT)
# Bham neighborhoods ----------------------------------
downtown <- sapply(c("002400","002700", "001500"), function(x)paste0(county_FIPS,x))
five_points_south <- sapply(c("004500","004902","004901"),function(x)paste0(county_FIPS,x))
crest_wood <- sapply(c("002306","005600","004901"),function(x)paste0(county_FIPS,x))

nb_Bham <- c(downtown, five_points_south, crest_wood)

# CREATE MAPS ==================================================================================

map_Bham <- function(df,var){
  ggplot()+
    geom_sf(data = map.cty %>% filter(COUNTYFP==ct_FIPS), color = "#bdbdbd")+
    geom_sf(data = df, aes_string(fill = var))+
    geom_sf(data = map.Bham, color = "#ffd966", size = 1, fill = NA)+
    theme(axis.title = element_blank(), 
          axis.text = element_blank())+
    coord_sf(datum = NA)
}

# LODES by census block group -----------------------
# all private jobs, by workplace block
downl_LODES_wp <- function(year, st){
  temp <- tempfile()
  download.file(paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",st,"/wac/",st,"_wac_S000_JT02_",year,".csv.gz"), temp)
  data <- read.csv(gzfile(temp))
  unlink(temp)
  return(data)
}

downl_LODES_od <- function(year, st){
  temp <- tempfile()
  download.file(paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",st,"/od/",st,"_od_main_JT02_",year,".csv.gz"), temp)
  data <- read.csv(gzfile(temp))
  unlink(temp)
  return(data)
}

# GET DATA =========================================
# GET decennial employment ---------------------------------------
var1990 <- c(sapply(seq(1,5),function(x)paste0("P006000",x)),"P0010001")
var2000 <-  sapply(seq(1,6),function(x)paste0("P00300",x)) 
var2010 <-  sapply(seq(1,5),function(x)paste0("P00300",x)) 

# time series
# Bham_tract_time <- function(YEAR){get_decennial(geography = "tract", year = YEAR,
#                                                 state = st_FIPS, county = ct_FIPS,output = "wide",geometry = TRUE,
#                                                 variables = eval(parse(text = paste0("var",YEAR))))}
# 
# years <- lst(1990, 2000, 2010)
# census <- purrr::map(seq(1990,2010,10),Bham_tract_time) %>%map2(years, ~ mutate(.x, id = .y))

# ANALYSIS =============================================================
# Job access -----------------------------------------------------------

# origin-destination data in Jefferson County
tract_od <- downl_LODES_od(2015, "al") %>% 
  mutate(FIPS_w = substr(padz(as.character(w_geocode),15),1,11),
         FIPS_h = substr(padz(as.character(h_geocode),15),1,11),
         county_w = substr(FIPS_w, 1,5),
         county_h = substr(FIPS_h, 1,5)) %>%
  filter(county_w == county_FIPS) %>%
  select(-w_geocode, -h_geocode, - createdate) %>%
  group_by(FIPS_w, FIPS_h, county_w, county_h) %>%
  summarise_if(is.numeric, sum)

# categorize Bham tracts
tract_od_Bham <- tract_od %>% 
  filter(FIPS_w%in%nb_Bham)%>%
  mutate(htype = case_when(
    county_h %in%county_FIPS ~ "within Jefferson County",
    # substr(FIPS_h,6,11) %in% city_tracts ~ "within City of Birmingham",
    TRUE ~ "outside Jefferson County"
  ))%>%
  mutate(nb = case_when(
    FIPS_w%in%downtown ~ "Central City, Fountain Heights",
    FIPS_w%in%five_points_south ~ "Five Points South",
    FIPS_w%in%crest_wood ~ "Crestline, Crestwood South",
    TRUE ~ "Others"
  ))

# summary stats
tb <- bind_rows(
  tract_od_Bham %>% group_by(nb, htype)%>%
    summarise_if(is.numeric, sum)%>%
    mutate_if(is.numeric,function(x)(x/sum(x))),
  tract_od_Bham %>% group_by(htype)%>%
    summarise_if(is.numeric, sum)%>%
    mutate_if(is.numeric,function(x)(x/sum(x)))%>%
    mutate(nb="All neighboorhoods")
)


# top residential tracts for selected neighborhoods
tract_od_map <- tract_od_Bham%>%
  group_by(nb, FIPS_h, county_h)%>%
  summarise_if(is.numeric, sum)%>%
  group_by(nb)%>%
  mutate_if(is.numeric,function(x)(x/sum(x)))%>%
  top_n(20, S000)

# create map
Bham_access <- map.cty %>%
  full_join(tract_od_map, by = c("GEOID"="FIPS_h")) %>%
  filter(!is.na(nb))
centroids <- tract_od_Bham%>%
  ungroup()%>%group_by(nb)%>%
  summarise(GEOID = first(FIPS_w))%>%
  left_join(map.cty)
glabel <- Bham_access%>%group_by(nb)%>%
  summarise(share = sum(S000))

map_Bham(Bham_access, "S000")+
  geom_sf(data = centroids,color = "red", size =2)+
  coord_sf(datum = NA)+
  scale_fill_gradient(low = "#bdd7e7", high = "#08519c",
                      label = scales::percent_format(accuracy =0.1), 
                      name = "Share of workers by household tract")+
  facet_wrap(~nb,nrow = 2)+
  geom_text(data = glabel, aes(x=-87,y=33.1,
                               label = paste0("% total = ",scales::percent(share)), 
                               fill = NULL))+
  ggtitle("Top 20 household tracts with the largest number of workers communting to the job hubs")

 
# Capital Access -----------------------------------------------------------
# employment by workplace tracts 
tract_emp <- downl_LODES_wp(2015, "al")

# FDIC
FDIC_Bham_alt <- MSA_SMEloan$FDIC_matched %>%
  mutate(FIPS = gsub("\\.","",FIPS),
         year = as.integer(year),
         id = paste0(State, county, FIPS))%>%
  left_join(tract_emp, by = c("id" = "FIPS"))%>%
  group_by(id, C000) %>%
  summarise(value = sum(x_tot, na.rm = TRUE)) %>%
  # annual average, 1996 - 2017
  mutate(value = value/C000/22,
         level = cut(value, breaks = c(0,1,5,10,Inf), include.lowest = TRUE))%>%
  left_join(census[[3]][c("GEOID","geometry")], by = c("id" = "GEOID"))

FDIC_Bham_tract <- MSA_SMEloan$FDIC_matched %>%
  # filter(year>=2012)%>%
  mutate(FIPS = gsub("\\.","",FIPS),
         year = as.integer(year),
         id = paste0(State, county, FIPS))%>%
  group_by(id)%>%
  summarise(x_tot = sum(x_tot, na.rm = TRUE))%>%
  left_join(tract_emp, by = c("id" = "FIPS"))%>%
  filter(!is.na(C000))%>%
  mutate(is.white = (CR01/C000>0.5))%>%
  group_by(is.white)%>%
  summarise(emp = sum(C000, na.rm = TRUE),
            x_tot = sum(x_tot, na.rm = TRUE))%>%
  mutate(value = x_tot/emp/22)

# CDFI
CDFI_Bham <- MSA_SMEloan$TLR_matched%>%
  filter(Year>=2006)%>%
  select(Year, FIPS, gender, race,investeetype, purpose,originalamount) %>%
  left_join(tract_emp, by = "FIPS")%>%
  group_by(FIPS, C000) %>%
  summarise(sum = sum(originalamount, na.rm = TRUE)) %>%
  #annual average, 2006 - 2017
  mutate(value = sum/C000/12,
         level = cut(value, breaks = c(0,10,50,100,Inf), include.lowest = TRUE))%>%
  right_join(census[[3]][c("GEOID","geometry")], by = c("FIPS" = "GEOID"))

TLR_Bham_tract <- MSA_SMEloan$TLR_matched%>%
  # filter(Year >=2012) %>%
  group_by(FIPS) %>%
  summarise(x_tot = sum(originalamount, na.rm = TRUE))%>%
  left_join(tract_emp[c("FIPS","C000","CR01")], by = "FIPS")%>%
  filter(!is.na(C000))%>%
  mutate(is.white = (CR01/C000>0.5))%>%
  group_by(is.white)%>%
  summarise(emp = sum(C000),
            x_tot = sum(x_tot, na.rm = TRUE))%>%
  mutate(value = x_tot/emp/12)

# Health outcomes ----------------------------------------------------------
library(RSocrata)
token <- "4T1vhrRM49HffDDXPFQJfiVhM"

cities <- paste0(gsub("\\,.+","",Peers$metro), collapse = "','")

cityhealth <- read.socrata(paste0("https://chronicdata.cdc.gov/resource/csmm-fdhi.csv?category=Health Outcomes&$where=cityname in",
                                  "('",cities,"')"),token)

summary(factor(cityhealth$geographiclevel))
# summary(health_tract$data_value)

health_tract <- cityhealth %>%
  filter(measureid %in% c("MHLTH", "PHLTH")) %>%
  filter(geographiclevel == "Census Tract") %>%
  filter(cityfips == paste0(substr(st_FIPS,2,2),city_FIPS))%>%
  mutate(level = cut(data_value,breaks = c(5,10,15,20,Inf)),
         GEOID = padz(as.character(tractfips),11)) %>%
  left_join(map.cty, by = "GEOID")

map_Bham(health_tract,"level")+
  facet_wrap(~measureid)+
  scale_fill_manual(values = c("#bdd7e7","#0070c0","#08519c", "#003249"),
                    label = c("5 - 10%", "10 - 15%", "15 - 20%", "> 20%"),
                    name = "Share of adults reported not good for >=14 days")+
  ggtitle("Mental health and physical health outcomes")


# peer comparison
health_chart <- cityhealth %>% 
  filter(measureid %in% c("MHLTH", "PHLTH")) %>%
  filter(geographiclevel == "City") %>%
  filter(datavaluetypeid == "AgeAdjPrv") %>%
  select(cityname, measure, data_value, year) %>%
  group_by(cityname, measure)%>%
  summarise(data_value = mean(data_value))

ggplot(health_chart, 
       aes(x = reorder(cityname, data_value), y = data_value, fill = measure, label = data_value))+
  geom_bar(stat = "identity", position = "dodge")+
  # geom_text(position = position_dodge(width = 1))+
  scale_y_continuous(name = NULL)+
  scale_x_discrete(name = NULL)+
  scale_fill_manual(values = c("#ffd966", "#0070c0"), 
                    labels = c("Mental Health", "Physical Health"), name = NULL)+
  coord_flip()+
  guides(fill = guide_legend(reverse = T))+
  pthemes

# Crimes -----------------------------
folder <- "V:/Building Inclusive Cities/Birmingham/Market Assessment/Data/Crime"
allfiles <- list.files(folder, full.names = TRUE)

Bham_crime <- lapply(allfiles, read_csv,
                     col_types=(list(col_double(),col_character(),col_double(),col_character(),col_character())))%>%
  bind_rows()%>%
  filter(!is.na(`Street Name`))%>%
  mutate(date_time = lubridate::mdy_hms(`Case Occurred From Date`,truncated = 3))

# geocoding using tigris geocoder
# address <- Bham_crime%>%
#   select(Block, `Street Name`)%>%
#   mutate(Block = ifelse(Block==0,"",Block))%>%
#   mutate(street = paste(Block,`Street Name`," "))%>%
#   unique()

# add <- address %>%
#   filter(!is.na(Block))%>%
#   filter(Block!="")%>%
#   mutate(FIPS = NA)
# 
# for (i in i:nrow(add)) {
#   tryCatch({
#     add$FIPS[[i]] <- call_geolocator(add$street[[i]],"Birmingham", "AL")},error=function(e){})
# }
# 
# save(add,file = "add.Rda")

# na.share(add,"FIPS")

load("add.Rda")
Bham_crime <- Bham_crime %>%
  left_join(add%>%mutate(Block=as.numeric(Block)), by = c("Block","Street Name"))

# na.share(Bham_crime,"FIPS")
sfactor(Bham_crime$`Case Offense Statute Description`)

Bham_crime <- Bham_crime%>%
  mutate(Type = case_when(grepl("Aggravated|Assault|Murder|Rape|Robbery|Shooting|Sodomy|Menacing|Suffocation|Spray",
                                `Case Offense Statute Description`, ignore.case = TRUE)~"Violent",
                          grepl("Burglary|Theft|Shoplifting|Breaking|Snatching|Burglars|Vehicle",
                                `Case Offense Statute Description`, ignore.case = TRUE)~"Property",
                          TRUE ~ "Others"))

# sfactor(Bham_crime$Type)
# sfactor((Bham_crime%>%filter(Type=="Others"))$`Case Offense Statute Description`)

na.share(Bham_crime%>%filter(Type=="Property"),"FIPS")

Bham_crime_summary <- Bham_crime%>%
  filter(!is.na(FIPS))%>%
  filter(Type=="Violent")%>%
  mutate(GEOID = substr(FIPS,1,11))%>%
  group_by(GEOID,Type)%>%
  summarise(count=n())%>%
  left_join(temp, by = "GEOID")%>%
  mutate(value = count/B01003_001E*1000,
         level = cut(value, c(0,20,40,60,Inf)))

map_Bham(Bham_crime_summary,"level")+
  scale_fill_manual(values = c("#bdd7e7","#0070c0","#08519c", "#003249"),
                    label = c(" < 20", "20 - 40", "40 - 60", "> 60"),
                    name = "Number of crime reports per 1000 residents")+
  # facet_wrap(~Type,nrow = 2)+
  ggtitle("Total number of violent crime reports, 2017 - 2018")
  
# chetty ------------------------------------------
# Bham_chetty <- read.csv("source/tract_outcomes/tract_outcomes_early.csv")%>%
#   filter(State==st_FIPS&County==county_FIPS)
