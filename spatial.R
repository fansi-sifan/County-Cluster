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
load(paste0("result/",msa_FIPS,"_Market Assessment.Rdata"))

# GET MAPS =========================================
# maps data ----------------------------------
usa <- map_data("state")
cbsa = core_based_statistical_areas()
map.Bham <- places(state = st_FIPS, class = "sf") %>% filter_place(placename)

novehicle <- c("B08141_001E","B08141_002E")
unemp <- c("B23025_003E","B23025_005E")
poverty <- c("B17001_001E","B17001_002E")
pop <- c("B01003_001E")
tract_acs_vars <- c(novehicle,unemp,poverty,pop)

map.cty <- get_acs(geography = "tract", 
                   year = 2017,
                   state = st_FIPS, 
                   county = substr(msa_ct_FIPS,3,5),
                   output = "wide",geometry = TRUE,
                   variables = tract_acs_vars,
                   key = Sys.getenv("CENSUS_API_KEY"))

# mapview(map.cty%>%filter(GEOID%in%nb_Bham),zcol = pop)

# Peer map ----------------------------------
map_data <- cbsa@data %>% filter(CBSAFP %in% Peers$cbsa)
map_data$long = -as.numeric(substring(map_data$INTPTLON,2))
map_data$lat = as.numeric(map_data$INTPTLAT)
# Bham neighborhoods ----------------------------------
nb_Bham <- list(downtown = sapply(c("002700", "002400"), function(x)paste0(county_FIPS,x)), 
                homewood = sapply(c("010702","010703","010704", "010706","005800"), function(x)paste0(county_FIPS,x)),
                UAB = sapply(c("004500","004800","004701","004901","004902"),function(x)paste0(county_FIPS,x)),
                hoover = c("01117030314","01073014408","01073012913"),
                avondale = sapply(c("002306","005600"),function(x)paste0(county_FIPS,x)),
                ensley = sapply(c("003400","003300"),function(x)paste0(county_FIPS,x)))

nb_Bham <- plyr::ldply(nb_Bham,cbind)
names(nb_Bham)<- c("nb","GEOID")

# CREATE MAPS ==================================================================================

map_Bham <- function(df,var){
  ggplot()+
    geom_sf(data = map.cty %>% filter(substr(GEOID,3,5)==ct_FIPS), color = "#bdbdbd")+
    geom_sf(data = df, aes_string(fill = var))+
    geom_sf(data = map.Bham, color = "#ffc000", size = 1, fill = NA)+
    theme(axis.title = element_blank(), 
          axis.text = element_blank(),
          rect = element_rect(fill = "#D9D9D9", colour=NA),
          panel.background = element_rect(fill = "#D9D9D9",colour = NA),
          plot.background = element_rect(fill = "#D9D9D9", colour = NA),
          panel.grid = element_blank())+
    coord_sf(datum = NA)
}

map_Bham_tp <- function(df,var){
  ggplot()+
    geom_sf(data = map.cty %>% filter(substr(GEOID,3,5)==ct_FIPS), color = "white")+
    geom_sf(data = df, aes_string(fill = var))+
    geom_sf(data = map.Bham, color = "#ffc000", size = 1, fill = NA)+
    theme(axis.title = element_blank(), 
          axis.text = element_blank(),
          rect = element_rect(fill = "transparent", colour=NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid = element_blank(),
          legend.text = element_text(color = "white"))+
    coord_sf(datum = NA)
}


# LODES by census block group -----------------------
# all private jobs, by workplace block
downl_LODES_wp <- function(seg,year, st){
  temp <- tempfile()
  download.file(paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",st,"/wac/",st,"_wac_",seg,"_JT02_",year,".csv.gz"), temp)
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
# var1990 <- c(sapply(seq(1,5),function(x)paste0("P006000",x)),"P0010001")
# var2000 <-  sapply(seq(1,6),function(x)paste0("P00300",x)) 
# var2010 <-  sapply(seq(1,5),function(x)paste0("P00300",x)) 

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
    FIPS_w%in%UAB ~ "UAB",
    FIPS_w%in%homewood ~ "Homewood",
    FIPS_w%in%hoover ~ "Hoover",
    FIPS_w%in%avondale ~ "Avondale, Crestline",
    FIPS_w%in%ensley ~ "Ensley",
    
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
Bham_jobhubs <- tract_od_Bham%>%
  # filter(!is.na(nb))%>%
  group_by(nb, FIPS_h, county_h)%>%
  summarise_if(is.numeric, sum)%>%
  group_by(nb)%>%
  mutate_if(is.numeric,function(x)(x/sum(x)))%>%
  # S000: all jobs
  # SE01: earnings $1250/month or less  
  top_n(20, SE01)

# create map
Bham_jobhubs <- map.cty %>%
  full_join(Bham_jobhubs, by = c("GEOID"="FIPS_h")) %>%
  filter(!is.na(nb))%>%
  mutate(access_level = cut(S000,c(0,0.01,0.015,0.025,Inf)))

jobhub_centroids <- tract_od_Bham%>%
  ungroup()%>%group_by(nb)%>%
  summarise(GEOID = first(FIPS_w))%>%
  left_join(map.cty, by = "GEOID")

jobhub_glabel <- Bham_jobhubs%>%
  group_by(nb)%>%
  summarise(share = sum(S000))

# map access to jobhubs
map_Bham(Bham_jobhubs, "access_level")+
  geom_sf(data = jobhub_centroids,color = "red", size =1)+
  coord_sf(datum = NA)+
  scale_fill_manual(values = c("#bdd7e7","#0070c0","#08519c", "#003249"),
                    labels = c("0 - 1%","1 - 1.5%", "1.5 - 2.5%", "> 2.5%"),
                    name = "Share of workers\nby household tract")+
  facet_wrap(~nb,nrow = 2)+
  geom_text(data = jobhub_glabel, aes(x=-87,y=33.1,
                               label = paste0("% total = ",scales::percent(share)),
                               fill = NULL))


# low income density
tract_wp_lowinc <- downl_LODES_wp("SE01",2015,"al")

tract_wp_lowinc <- tract_wp_lowinc%>%
  mutate(GEOID = substr(padz(as.character(w_geocode),15),1,12))%>%
  filter(substr(GEOID,1,5)==county_FIPS)%>%
  group_by(GEOID)%>%
  summarise_if(is.numeric, sum)
  
Bham_density_lowinc <- density %>%
  filter(year==2015)%>%
  filter(naics == "00")%>%
  filter(cntyfips == county_FIPS)%>%
  left_join(tract_wp_lowinc,by = "GEOID")%>%
  mutate(GEOID = substr(GEOID,1,11))%>%
  group_by(GEOID, year)%>%
  # filter(GEOID %in% nb_Bham)%>%
  summarise(job_tot = sum(job_tot, na.rm = T),
            landarea = sum(landarea, na.rm = T),
            density = job_tot/landarea)%>%
  select(-job_tot)%>%
  spread(year,density)

# Capital Access -----------------------------------------------------------
# employment by workplace tracts 
tract_emp <- downl_LODES_wp("S000",2015,"al")

tract_wp_emp <- tract_emp%>%
  mutate(GEOID = substr(padz(as.character(w_geocode),15),1,11))%>%
  group_by(GEOID)%>%
  summarise_if(is.numeric, sum)%>%
  rename(tot.emp = C000)%>%
  mutate(lowinc.emp = CE01+CE02,
         lowedu.emp = CD01+CD02,
         tradable.emp = CNS01+CNS02+CNS05+CNS09+CNS10+CNS12+CNS13)%>%
  right_join(Bham_density[c("GEOID","landarea")], by = "GEOID")

tract_wp_map <- tract_wp_emp%>%
  select(GEOID, tot.emp, lowinc.emp, lowedu.emp, tradable.emp, landarea)%>%
  mutate(tot.dst = tot.emp/landarea,
         lowinc.dst = lowinc.emp/landarea,
         lowedu.dst = lowedu.emp/landarea,
         tradable.dst = tradable.emp/landarea,
         lowinc.share = lowinc.emp/tot.emp,
         lowedu.share = lowedu.emp/tot.emp,
         tradable.share = tradable.emp/tot.emp)%>%
  filter(substr(GEOID,1,5)==county_FIPS)%>%
  mutate(tot.dst_level = cut(tot.dst,c(0,15,100,500,3000,8500,Inf)),
         lowinc.dst_level = cut(lowinc.dst, c(0,15,100,500,3000,8500,Inf)),
         lowedu.dst_level = cut(lowedu.dst, c(0,15,100,500,3000,8500,Inf)),
         tradable.dst_level = cut(tradable.dst,c(0,15,100,500,3000,8500,Inf)))%>%
  left_join(map.cty, by = "GEOID")

# # explore interactive map -------
# tract_wp_map <- st_as_sf(tract_wp_map)
# m1 <- mapview(tract_wp_map, zcol = "tradable.dst_level")
# m2 <- mapview(tract_wp_map, zcol = "lowedu.dst_level")
# 
# # and sync
# sync(m1, m2)

# tract maps -------
map_Bham(tract_wp_map,"lowedu.dst_level")+
  scale_fill_brewer(palette = "YlOrRd",
                    name = "Job density (jobs per sq mi)",
                    label = c("Very Low: < 15","Low: 15 - 100", "Moderate: 100 - 500","High: 500 - 3000", "Very High:> 3000"))+
  ggtitle("Density of jobs for workers with education attainment: high school or below")

map_Bham(tract_wp_map,"lowinc.dst_level")+
  scale_fill_brewer(palette = "YlOrRd",
                    name = "Job density (jobs per sq mi)",
                    label = c("Very Low: < 15","Low: 15 - 100", "Moderate: 100 - 500","High: 500 - 3000", "Very High:> 3000"))+
  ggtitle("Density of jobs with earnings $3333/month or less")

map_Bham(tract_wp_map,"tradable.dst_level")+
  scale_fill_brewer(palette = "YlOrRd",
                    label = c("Very Low: < 15","Low: 15 - 100", "Moderate: 100 - 500","High: 500 - 3000", "Very High:> 3000"),
                    name = "Job density (jobs per sq mi)")+
  ggtitle("Density of jobs in tradable industries")

map_Bham(tract_wp_map,"tot.dst_level")+
  scale_fill_brewer(palette = "YlOrRd",
                    # label = c("Very Low: < 15","Low: 15 - 100", "Moderate: 100 - 500","High: 500 - 3000", "Very High:> 3000"),
                    name = "Job density (jobs per sq mi)")+
  ggtitle("Density of all jobs")

map_Bham(tract_wp_map%>%
           filter(tradable.dst >500)%>%
           filter(lowinc.dst>500)%>%
           filter(lowedu.dst>500),
         "tot.dst_level")+
  scale_fill_brewer(palette = "YlOrRd",
                    label = c("High: 500 - 3000", "Very High: 3000 - 8000", "Extremely High: > 8000"),
                    name = "Total job density (jobs per sq mi)")+
  ggtitle("Tracts with density of jobs  higher than 500 in tradable industries, for low income, and for low education workers")

library(mapview)

mapview()

# FDIC
Bham_FDIC <- datafiles$MSA_SMEloan$FDIC_matched %>%
  mutate(FIPS = gsub("\\.","",FIPS),
         year = as.integer(year),
         GEOID = paste0(State, county, FIPS))%>%
  left_join(tract_emp, by = "GEOID")%>%
  group_by(GEOID, tot.emp) %>%
  summarise(value = sum(x_tot, na.rm = TRUE)) %>%
  # annual average, 1996 - 2017
  mutate(FDIC = value/tot.emp/22,
         FDIC_level = cut(FDIC, breaks = c(0,0.1,0.5,1,5,Inf), include.lowest = TRUE))

# CDFI
Bham_CDFI <- datafiles$MSA_SMEloan$TLR_matched%>%
  filter(Year >= 2006)%>%
  select(Year, GEOID = FIPS, gender, race,investeetype, purpose,originalamount) %>%
  left_join(tract_emp, by = "GEOID")%>%
  group_by(GEOID, tot.emp) %>%
  summarise(value = sum(originalamount, na.rm = TRUE)) %>%
  #annual average, 2006 - 2017
  mutate(CDFI = value/tot.emp/12,
         level = cut(CDFI, breaks = c(0,1,10,100,Inf), include.lowest = TRUE))

# Health outcomes ----------------------------------------------------------
library(RSocrata)
token <- "4T1vhrRM49HffDDXPFQJfiVhM"

cities <- paste0(gsub("\\,.+","",Peers$metro), collapse = "','")

cityhealth <- read.socrata(paste0("https://chronicdata.cdc.gov/resource/csmm-fdhi.csv?category=Health Outcomes&$where=cityname in",
                                  "('",cities,"')"),token)

summary(factor(cityhealth$geographiclevel))
# summary(health_tract$data_value)

Bham_health <- cityhealth %>%
  filter(measureid %in% c("MHLTH", "PHLTH")) %>%
  filter(geographiclevel == "Census Tract") %>%
  filter(cityfips == paste0(substr(st_FIPS,2,2),city_FIPS))%>%
  mutate(GEOID = padz(as.character(tractfips),11)) %>%
  select(GEOID,measureid,data_value)%>%
  spread(measureid,data_value)

# health_crime <- inner_join(health_tract[c("GEOID","measureid","data_value")],
#                            Bham_crime_summary[c("GEOID","value")],
#                            by = "GEOID")
# 
# ggplot(health_crime,aes(x=value,y=data_value, color = measureid))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   labs(x = "Number of violent crime reports per 1000 residents",
#        y = "Share of adults reported health not good for >=14 days")+pthemes
# 
# fit <- lm(data_value ~ value, data = health_crime%>%filter(measureid=="PHLTH"))
# summary(fit)$r.squared



# peer comparison
health_chart <- cityhealth %>% 
  filter(measureid %in% c("MHLTH", "PHLTH")) %>%
  filter(geographiclevel == "City") %>%
  filter(datavaluetypeid == "AgeAdjPrv") %>%
  select(cityname, measure, data_value, year) %>%
  group_by(cityname, measure)%>%
  summarise(data_value = mean(data_value))

# ggplot(health_chart, 
#        aes(x = reorder(cityname, data_value), y = data_value, fill = measure, label = data_value))+
#   geom_bar(stat = "identity", position = "dodge")+
#   # geom_text(position = position_dodge(width = 1))+
#   scale_y_continuous(name = NULL)+
#   scale_x_discrete(name = NULL)+
#   scale_fill_manual(values = c("#ffd966", "#0070c0"), 
#                     labels = c("Mental Health", "Physical Health"), name = NULL)+
#   coord_flip()+
#   guides(fill = guide_legend(reverse = T))+
#   pthemes

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

Bham_crime <- Bham_crime%>%
  filter(!is.na(FIPS))%>%
  filter(Type=="Violent")%>%
  mutate(GEOID = substr(FIPS,1,11))%>%
  group_by(GEOID,Type)%>%
  summarise(count=n())
  
# chetty ------------------------------------------
# Chetty <- read.csv("source/tract_outcomes/tract_outcomes_early.csv")
# save(Chetty,file="Chetty.Rda")

# Bham_chetty <- Chetty%>%
#   filter(state==1&county==73)
# Bham_chetty <- Bham_chetty[,colSums(is.na(Bham_chetty))<nrow(Bham_chetty)]
# save(Bham_chetty,file = "Bham_Chetty.Rda")

load("Bham_Chetty.Rda")

Bham_Chetty <- Bham_chetty%>%
  mutate(GEOID = paste0(padz(state,2),padz(county,3),padz(tract,6)))%>%
  select(GEOID,
         chetty_black = kfr_top20_black_pooled_mean,
         chetty_white = kfr_top20_white_pooled_mean)
  

# map_Bham(Bham_Chetty_map,"kir_black_female_p50")+
#   scale_fill_gradient(low = "#bdd7e7", high = "#08519c")+
#   ggtitle("Mean percentile rank in the national distribution of household income, black, female, 50th pctile")
# 
# map_Bham(Bham_Chetty_map,"kir_white_male_p50")
# map_Bham(Bham_Chetty_map,"kir_top20_white_pooled_mean")



# read shapefiles ----------------------------------------------------
# library('sf')
# library('mapview')
# library('dplyr')
# 
# paths <- "V:/Building Inclusive Cities/Birmingham/Market Assessment/Data/Job hubs/13820_tr_2017_0700-0859/13820_tr_2017_0700-0859.shp"
# file.exists(paths)
# job_access <- sf::st_read(paths)
# 
# job_access <- job_access %>%
#   mutate(GEOID = as.character(geoid),
#          tract = substr(GEOID,1,11))%>%
#   filter(substr(tract,1,5)=="01073")%>%
#   filter(threshold==30)
# 
# job_access_tract <- aggregate(job_access["tot_jobs"], 
#                               by = list(job_access$tract),
#                               FUN = sum,na.rm = TRUE)
# 
# mapview(job_access_tract,zcol = "tot_jobs")
# 
# plot(job_access_tract['tot_jobs'])

# school ------------------
paths <- "V:/Building Inclusive Cities/Birmingham/Market Assessment/Data/Andre data/bhm_school_proficiency.csv"
file.exists(paths)
school <- read.csv(paths)

Bham_school <- school %>%
  mutate(GEOID = padz(TRACTFP,11))%>%
  filter(substr(GEOID,1,5)=="01073")

# broadband ------------------
MSA_broadband <- datafiles$MSA_broadband
Bham_broadband <- MSA_broadband %>%
  mutate(GEOID=padz(as.integer(tract),11),
         Broadband_level = as.factor(pcat_10x1))%>%
  filter(substr(GEOID,1,5)=="01073")%>%
  select(GEOID, Broadband_level)

# transit ----------------------------------------------------------
drive <- c("B08141_001E","B08141_006E")

PeerCounty_drive <- get_acs(geography = "county", 
                            year = 2017,output = "wide",
                            state = unique(Peers$st_name),variables = drive) %>%
  filter(GEOID %in% Peers$FIPS)

PeerCounty_drive <- PeerCounty_drive %>%
  mutate(FIPS = GEOID)%>%
  left_join(Peers[c('FIPS', "county", "ctyemp")], by = "FIPS")%>%
  mutate(value = B08141_006E/B08141_001E,
         metro = county,
         HL = c(FIPS == county_FIPS))

# bar_plot(PeerCounty_drive,"")+
#   scale_y_continuous(labels = NULL, limits = c(0,1))+
#   geom_text(data = PeerCounty_drive, aes(label = scales::percent(value,accuracy = 1),hjust = -0.1))+
#   pthemes
  
# Density data ----------------------------------------------------------
paths <- "V:/Building Inclusive Cities/Birmingham/Market Assessment/Data/Job hubs/bhm_jobdensity.xlsx"
file.exists(paths)
density <- readxl::read_xlsx(paths)

Bham_density <- density %>%
  filter(year==2015|year==2004)%>%
  filter(naics == "00")%>%
  # filter(cntyfips == county_FIPS)%>%
  mutate(GEOID = substr(GEOID,1,11))%>%
  group_by(GEOID, year)%>%
  # filter(GEOID %in% nb_Bham)%>%
  summarise(job_tot = sum(job_tot, na.rm = T),
            landarea = sum(landarea, na.rm = T),
            density = job_tot/landarea)%>%
  select(-job_tot)%>%
  spread(year,density)

density %>%
  filter(year==2015|year==2004)%>%
  filter(naics != "00")%>%
  # filter(cntyfips == county_FIPS)%>%
  mutate(GEOID = substr(GEOID,1,11))%>%
  right_join(nb_Bham, by = "GEOID")%>%
  group_by(nb, year, naics)%>%
  summarise(job_tot = sum(job_tot, na.rm = T),
            landarea = sum(landarea, na.rm = T),
            density = job_tot/landarea)%>%
  select(-job_tot)%>%
  spread(year,density)%>%
  mutate(delta = abs(`2015`-`2004`),r_delta = `2015`-`2004`)%>%
  top_n(3,delta)

# rds <- tigris::primary_roads(class = "sf")
# Bham_rds <- sf::st_join(map.cty,rds)


# MERGE TRACT DATA  ==========================================================
Bham_tract_all <- map.cty%>%
  # population, unemployment, poverty
  mutate(county = substr(GEOID,1,5),
         pop = B01003_001E,
         une = B23025_005E/B23025_003E,
         poverty = B17001_002E/B17001_001E,
         no_vehicle = B08141_002E/B08141_001E,
         une_level = cut(une, c(0,0.05,0.1,0.15,0.2,Inf)),
         poverty_level = cut(poverty, c(0,0.2,0.4,0.6,0.8,1)),
         novehicle_level = cut(no_vehicle, c(0,0.1,0.2,0.25,Inf)))%>%
  # health outcome
  left_join(Bham_health,by = "GEOID")%>%
  mutate(mhealth_level = cut(MHLTH,breaks = c(5,10,15,20,Inf)),
         phealth_level = cut(PHLTH,breaks = c(5,10,15,20,Inf)))%>%
  # crime per 1000 residents
  left_join(Bham_crime, by = "GEOID")%>%
  mutate(value = count/pop*1000,
         crime_level = cut(value, c(0,20,40,60,Inf)))%>%
  # neighborhoods outcome
  left_join(Bham_Chetty, by = "GEOID")%>%
  mutate(chettyblack_level = cut(chetty_black,
                                 c(0,0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.5,Inf)),
         chettywhite_level = cut(chetty_white,
                                 c(0,0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.5,Inf)))%>%
  # school quality
  left_join(Bham_school, by = "GEOID")%>%
  mutate(school_level = cut(TRACTSCORE_H,c(0,0.1,0.2,0.6,0.8,Inf)))%>%
  # broadband rates
  left_join(Bham_broadband, by = "GEOID")%>%
  # job density
  left_join(Bham_density,by = "GEOID")%>%  
  mutate(densitydelta = `2015`-`2004`,
         densitydelta_level = cut(densitydelta, c(-4000,-1000,0,500,1000,4000)),
         density_level = cut(`2015`,c(500,3000,8500,Inf)))%>%
  # capital access
  left_join(Bham_CDFI, by = "GEOID")%>%
  left_join(Bham_FDIC, by = "GEOID")

save(Bham_tract_all, file = "Bham_tract_all.Rda")


# PLOT MAPS ==========================================================
load("Bham_tract_all.Rda")

map_Bham(Bham_tract_all%>%filter(county==county_FIPS),
         "densitydelta_level")+
  scale_fill_manual(values = c("#a50f15","#ef3b2c","#9ecae1","#6baed6","#084594"),
                    label = c("-4000 ~ -1000", "-1000 ~ 0", "0 ~ 500", "500 ~ 1000", "1000 ~ 4000"),
                    name = "")

map_Bham(Bham_tract_all%>%filter(GEOID %in% nb_Bham),"density_level")+
           scale_fill_manual(values = c("#fd8d3c","#f03b20","#bd0026"))
         

map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "poverty_level")+
  scale_fill_manual(
                    # values = c("#a50f15","#ef3b2c","#9ecae1","#6baed6","#084594"),
                    values = c("#084594","#6baed6","#ef3b2c","#a50f15"),
                    label = c("< 20%", "20 - 40%", "40 - 60%", "60 - 80%", "> 80%"),
                    name = "")+
  ggtitle("Poverty rate")


map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "novehicle_level")+
  scale_fill_manual(values = c("#084594","#6baed6","#ef3b2c","#a50f15"),
                    label = c(" < 10%","10 - 20%", "20 - 25%","> 25%"),
                    name = "")+
  ggtitle("Share of residents with no vehicles")


map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "une_level")+
  scale_fill_manual(values = c("#084594","#6baed6","#9ecae1","#ef3b2c","#a50f15"),
                    label = c(" < 5%","5 - 10%", "10 - 15%", "15 - 20%", "> 20%"),
                    name = "")+
  ggtitle("Unemployment rate for population 16 and over")


map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "Broadband_level") + 
  scale_fill_manual(values = c("#a50f15","#ef3b2c","#9ecae1","#6baed6","#084594"),
                    labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                    name = "") +  
  ggtitle("Broadband subscription rates by census tract")

map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "school_level")+
  scale_fill_manual(values = c("#a50f15","#ef3b2c","#9ecae1","#6baed6","#084594"),
                    label = c(" < 10%","10 - 20%", "20 - 60%", "60 - 80%", "> 80%"),
                    name = "")+
  ggtitle("Share of public school students 10 -14 achieved a passing grade in AL state proficiency tests")

t <- map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
              "chettyblack_level")+
  scale_fill_brewer(palette = "RdBu",
                    label = c("0 - 1%",
                              "1 - 3%",
                              "3 - 5%",
                              "5 - 10%",
                              "10 - 15%",
                              "15 - 20%",
                              "20 - 25%",
                              "25 - 30%"
                              ),
                    name = "Probability")+
  geom_sf(data = map.Bham, color = "black", size = 1, fill = NA)+
  coord_sf(datum = NA)+
  ggtitle("Probability of reaching the top quintile of the national individual income distribution")

w <- map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
              "chettywhite_level")+
  scale_fill_brewer(palette = "RdBu",
                    label = c("0 - 1%",
                              "1 - 3%",
                              "3 - 5%",
                              "5 - 10%",
                              "10 - 15%",
                              "15 - 20%",
                              "20 - 25%",
                              "25 - 30%",
                              "30 - 50%",
                              "> 50%"),
                    name = "Probability")+
  geom_sf(data = map.Bham, color = "black", size = 1, fill = NA)+
  coord_sf(datum = NA)+
  ggtitle("Probability of reaching the top quintile of the national individual income distribution")

t
w

t+coord_sf(xlim = c(-86.85,-86.7),ylim =c(33.45,33.6))+ggtitle("")
w+coord_sf(xlim = c(-86.85,-86.7),ylim =c(33.45,33.6))+ggtitle("")


map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "phealth_level")+
  scale_fill_manual(values = c("#084594","#6baed6","#ef3b2c","#a50f15"),
                    label = c("5 - 10%", "10 - 15%", "15 - 20%", "> 20%"),
                    name = "")+
  ggtitle("Share of adults reported physical health not good for >=14 days")

map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "mhealth_level")+
  scale_fill_manual(values = c("#084594","#6baed6","#ef3b2c","#a50f15"),
                    label = c("5 - 10%", "10 - 15%", "15 - 20%", "> 20%"),
                    name = "")+
  ggtitle("Share of adults reported mental health not good for >=14 days")

map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "crime_level")+
  scale_fill_manual(values = c("#084594","#6baed6","#ef3b2c","#a50f15"),
                    label = c(" < 20", "20 - 40", "40 - 60", "> 60"),
                    name = "")+
  # facet_wrap(~Type,nrow = 2)+
  ggtitle("Total number of violent crime reports per 1000 residents, 2017 - 2018")

map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "FDIC_level")+
  scale_fill_manual(values = c("#a50f15","#ef3b2c","#6baed6","#084594"),
                    label = c(" 0.1 - 0.5M", "0.5 - 1M", "1 - 5M", "> 5M"),
                    name = "")+
  theme(legend.position = "bottom")


map_Bham(Bham_tract_all%>%filter(county == county_FIPS),
         "level")+
  scale_fill_manual(values = c("#a50f15","#ef3b2c","#6baed6","#084594"),
                    label = c(" <1K", "1 - 10K", "10 - 100K", "> 100K"),
                    name = "")+
  theme(legend.position = "bottom")



# hist(Bham_tract_all$FDIC)

# benchmark ----------------------------------
tract_share <- function(col, benchmark, dir=T){
  share_b <- sum(Bham_tract_all[[col]] > benchmark, na.rm = T)
  share_s <- sum(Bham_tract_all[[col]] <= benchmark, na.rm = T)
  
  if (dir) {
    return(share_b/(share_b+share_s))
  }else{return(share_s/(share_b+share_s))}
  
}

pop_share <- function(col, benchmark, pop,dir=T){
  share_b <- sum((Bham_tract_all[[col]] > benchmark)*(Bham_tract_all[[pop]]), na.rm = T)
  share_s <- sum((Bham_tract_all[[col]] <= benchmark)*(Bham_tract_all[[pop]]), na.rm = T)
  
  if (dir) {
    return(share_b/(share_b+share_s))
  }else{return(share_s/(share_b+share_s))}
  
}


acs_us <- get_acs(geography = "us" , year = 2017,output = "wide",variables = tract_acs_vars)
b_novehicle <- acs_us$B08141_002E/acs_us$B08141_001E
b_poverty <- acs_us$B17001_002E/acs_us$B17001_001E
b_une <- acs_us$B23025_005E/acs_us$B23025_003E

# https://chronicdata.cdc.gov/500-Cities/500-Cities-Local-Data-for-Better-Health-2018-relea/6vp6-wxuq/data
b_phlth <- 12.1
b_mhlth <- 11.7
b_chetty <- 0.2000008005
# AL state average
b_school <- 0.4429794

tract_share("PHLTH", b_phlth)
tract_share("MHLTH", b_mhlth)
tract_share("poverty", b_poverty)
tract_share("une", b_une)
tract_share("no_vehicle", b_novehicle)
tract_share("kfr_top20_pooled_pooled_mean", b_chetty)

pop_share("PHLTH", b_phlth, "pop")
pop_share("MHLTH", b_mhlth, "pop")
pop_share("poverty", b_poverty, "pop")
pop_share("une", b_une, "pop")
pop_share("no_vehicle", b_novehicle, "pop")
pop_share("kfr_top20_pooled_pooled_mean", b_chetty, "pop")
pop_share("TRACTSCORE_H",b_school,"n_students")

Bham_tract_all<- Bham_tract_all%>%
  mutate(broadband = as.numeric(as.character(Broadband_level)))
pop_share("broadband",2,F)


# correlation matrix -------------------
install.packages('corrplot')
library(corrplot)

m <- Bham_tract_all%>%select(poverty_rate = poverty, 
                             unemployment_rate = une, 
                             p_no_vehicle = no_vehicle, 
                             p_mental_health = MHLTH, 
                             p_physical_health = PHLTH, 
                             p_crime = value.x, 
                             chetty_outcome = kfr_top20_pooled_pooled_mean,
                             school_score = TRACTSCORE_L, 
                             # amt_FDIC = FDIC,
                             # job_density = `2015`,
                             job_density_change = densitydelta)
st_geometry(m) <- NULL

M <- cor(m,use = "pairwise.complete.obs")
head(round(M,2))

plot.new()
corrplot(M, method = "color", type ="upper",add = T,
         addCoef.col = "black", tl.col = "black",tl.srt=45)

usr <- par("usr")
par(usr=usr)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "#D9D9D9")


plot_access <- tibble::tribble(
                ~type, ~Birmingham.MSA, ~City.of.Birmingham,
                "All",       "-14.50%",           "-13.40%",
       "High-poverty",       "-36.40%",           "-16.10%",
  "Majority-minority",       "-24.80%",           "-17.30%"
  )

ggplot(data = plot_access%>%gather(geo, percentage,-type),
       aes(x=geo, y=-as.numeric(gsub("-|%","",percentage)), 
           fill = type, label = percentage))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(position = position_dodge(width = 1),vjust = 1.5)+
  scale_fill_brewer(palette = "Blues", name = "Type of neighborhoods")+
  pthemes
