# Author: Sifan Liu
# Date: Fri Mar 08 17:16:50 2019
# --------------

source("Func.R")
pkgs <- c('tidycensus','data.table')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  } 

# QWI data

traded_2 <- c("31-33","51","54","52","52(652)")

# API --------------
# devtools::install_github("medewitt/tidyqwi")
# library(tidyqwi)
# key = Sys.getenv("CENSUS_API_KEY")
# qwi_var_names <- qwi_var_names
# 
# test_qwi <- get_qwi(years = "2018",
#                     states = "AL",
#                     geography = "cbsa",
#                     apikey = key,
#                     # endpoint = "rh",
#                     owner_code = "05",
#                     variables = c("sEmp", "Emp","firmage"), 
#                     all_groups = FALSE,
#                     industry_level = "2", processing = "sequential")
# 
# test_qwi%>%
#   filter(county==ct_FIPS)%>%
#   filter(industry%in%traded_2)


# direct download -------------
# qwi_al <- read.csv("V:/Building Inclusive Cities/Birmingham/Market Assessment/Data/business owner/qwi/qwi_al_rh_fa_gc_ns_op_u.csv")
# qwi_Bham <- qwi_al%>%filter(geography==1073|geography ==1)
# 
# save(qwi_Bham,file="qwi_Bham.Rda")
# labels: https://lehd.ces.census.gov/data/schema/latest/lehd_public_use_schema.html#_firmage

load("qwi_ai.Rda")

# average size of employment by firm size in tradable sectors
qwi_Bham%>%
  filter(industry%in% traded_2)%>%
  group_by(firmage, industry)%>%
  summarise(EmpTotal = mean(EmpTotal, na.rm = T))%>%
  spread(firmage, EmpTotal)

# minority representaton in traded industry
qwi_Bham%>%
  filter(industry%in% traded_2)%>%
  group_by(race, industry)%>%
  summarise(EmpTotal = mean(EmpTotal, na.rm = T))%>%
  spread(race, EmpTotal)%>%
  mutate(white = A1/A0, black = A2/A0)

# comparison

file <- "V:/Sifan/Birmingham/County Cluster/source/qwi"
all_files <- list.files(file, full.names = T)

qwi <- bind_rows(lapply(all_files,read.csv))%>%
  select(geography, industry, race, firmage, year, Emp, EmpTotal)

qwi_race_gap <- qwi%>%
  filter(industry%in% traded_2)%>%
  group_by(geography, race, industry)%>%
  summarise(EmpTotal = mean(EmpTotal, na.rm = T))%>%
  spread(race, EmpTotal)%>%
  mutate(white = A1/A0, black = A2/A0)

# edu by race from ACS subject tables
library(tidycensus)

edu_race <- c("B15003_001E","C15002A_001E", "C15002A_006E", "C15002A_011E","C15002B_001E", "C15002B_006E", "C15002B_011E")
edu_major_race <- c("C15010","C15010A","C15010B")

edu_race <- bind_rows(map_df(edu_major_race, function(x)get_acs(geography = "us",
                                 key =  Sys.getenv("CENSUS_API_KEY"),cache_table = T,
                                 table = x,
                                 output = "wide",
                                 year = 2017)),
                      map_df(edu_major_race, function(x)get_acs(geography = "county",
                                                                key =  Sys.getenv("CENSUS_API_KEY"),cache_table = T,
                                                                table = x,state = st_FIPS,county = ct_FIPS,output = "wide",
                                                                year = 2017)),
                      map_df(edu_major_race, function(x)get_acs(geography = "county",
                                                                key =  Sys.getenv("CENSUS_API_KEY"),cache_table = T,
                                                                table = x,state = "47",county = "037",output = "wide",
                                                                year = 2017)))

get_peer_acs <- function(var)bind_rows(get_acs(geography = "county", state = st_FIPS,county = ct_FIPS,
                  key = Sys.getenv("CENSUS_API_KEY"),
                  cache_table = T,variables = var, output = "wide", year = 2017),
          get_acs(geography = "county",  state = "47",county = "037",
                  key = Sys.getenv("CENSUS_API_KEY"),
                  cache_table = T,variables = var,output = "wide", year = 2017),
          get_acs(geography = "us", 
                  key = Sys.getenv("CENSUS_API_KEY"),
                  cache_table = T,variables = var, output = "wide", year = 2017))

# edu
get_peer_acs(edu_race)
edu_race <- edu_race %>%
  # group_by(GEOID, NAME)%>%
  # summarise_all(sum, na.rm = T)%>%
  select(-contains("M"))

# emp
emp_race_m <- map_chr(LETTERS[1:9],function(x)paste0("C23002",x,"_007E"))
emp_race_f <- map_chr(LETTERS[1:9],function(x)paste0("C23002",x,"_020E"))

t <- get_peer_acs(c(emp_race_m,emp_race_f))

t%>%select(-contains("M"), -contains("H"))%>%
  mutate(black = (C23002B_007E+C23002B_020E)/rowSums(.[2:17]))%>%
  select(GEOID, black)

# BDS ======================

BDS_avg_emp <- datafiles$PeerMetro_BDS%>%
  mutate(age = case_when(
    fage4 %in% c("a","b") ~ "firm age [0:1]",
    fage4 %in% c("c","d","e","f") ~ "firm age [2:5]",
    fage4 == "g" ~ "firm age [6:10]",
    T ~ "firm age [11:Inf]"
  ))%>%
  group_by(age, cbsa)%>%
  mutate(emp = as.numeric(emp),
         estabs = as.numeric(estabs))%>%
  mutate(avg_emp = emp/estabs,
         g_avg_emp = sum(emp)/sum(estabs))%>%
  right_join(Peers[c('cbsa',"metro")], by = 'cbsa')

ggplot(BDS_avg_emp%>%filter(age != "firm age [11:Inf]"), 
       aes(x=fage4,y=reorder(metro,g_avg_emp), label = avg_emp))+
  geom_tile(aes(fill = avg_emp),color = "white")+
  geom_text(aes(label = scales::comma(avg_emp,accuracy = 0.1)))+
  scale_x_discrete(labels = c("a"="0","b"="1","c"="2","d"="3","e"="4","f"="5", "g" = "6-10"),
                   name = "Firm age")+
  scale_fill_gradient(low = "white", high = "#0070c0")+
  labs(y="",title = "Average size of employment by firm age, 2014")

# firm death rates ------------------------------------------------------
PeerMetro_firmdeaths <- read.csv("source/bds_f_agesz_msa_release.csv")%>%
  # filter(msa %in% Peers$cbsa) %>%
  # filter(year2 >2009)%>%
  filter(msa %in% c("13820","34980"))

firmdeaths <- PeerMetro_firmdeaths%>%
  # filter(msa=="13820")%>%
  filter(year2 > 2000)%>%
  mutate(fa = substr(fage4,1,1),
         fs = substr(fsize,1,1))%>%
  mutate(firmage = case_when(
    fa%in%c("a","b") ~ " 0~1",
    fa%in%c("c","d","e","f") ~ "2~5",
    fa%in%c("g","h") ~ "6~15",
    fa%in%c("i","j","k","l") ~ ">15"
  ))%>%
  mutate(firmsize = case_when(
    fs%in%c("a") ~ "1~4",
    fs%in%c("b","c") ~ "5~19",
    fs%in%c("d","e") ~ "20~100",
    fs%in%c("f","g") ~ "100~500",
    fs%in%c("h","i","j","k","l") ~ ">500"
  ))%>%
  group_by(year2,msa,firmage,firmsize)%>%
  summarise(estabs = sum(estabs,na.rm = T),
            estabs_entry = sum(estabs_entry,na.rm = T),
            estabs_exit = sum(estabs_exit, na.rm = T))%>%
  # filter(!is.na(firmage))%>%
  mutate(estabs_entry_rate = estabs_entry/estabs,
         estabs_exit_rate = estabs_exit/estabs,
         metro = case_when(
           msa==13820 ~ "Birmingham",
           msa==34980 ~ "Nashville"
         ),
         firmtype = paste0("Age:",firmage," ,Size:",firmsize))%>%
  group_by(msa,firmtype)%>%
  mutate(exit_rate_m = mean(estabs_exit_rate),
         entry_rate_m = mean(estabs_entry_rate))%>%
  # filter(firmage == " 0~1")%>%
  filter(exit_rate_m>0.05)%>%
  filter(metro=="Birmingham")

# firm deaths over the year
ggplot(firmdeaths, 
       aes(x=year2, y = estabs_exit_rate, color = firmtype, group = firmtype))+
  geom_point()+
  geom_smooth(method = 'loess',se=F)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(limits = c(1998,2014))+
  ggrepel::geom_text_repel(data = firmdeaths%>%
                             filter(year2==2001),color = "black",
                           aes(x=year2-2, y = estabs_exit_rate, group = firmtype,label = firmtype))+
  ggtitle("Establishment exit rate in Birmingham MSA by firm age and size")+
  
  
  firm_BDS <- PeerMetro_firmdeaths%>%
  filter(msa %in% c("13820","34980"))%>%
  select(msa, fage4,fsize, contains("rate"), firms,estabs, emp)%>%
  mutate(msa = as.character(msa))%>%
  left_join(Peers[c("cbsa", "metro")], by = c("msa"="cbsa"))%>%
  group_by(metro, fage4, fsize)%>%
  summarise_if(is.numeric, mean, na.rm = T)%>%
  mutate(exit_level = cut(estabs_exit_rate, c(0,5,10,20,30,100)),
         emp_avg = emp/firms)%>%
  group_by(metro)%>%
  mutate(estabs_share = estabs/sum(estabs,na.rm = T),
         emp_share = emp/sum(emp, na.rm = T))%>%
  mutate(emp_level = cut(emp_share, c(0,0.001,0.01,0.1,0.5)),
         estabs_level = cut(estabs_share, c(0,0.0001,0.001,0.01,0.1)))

ggplot(firm_BDS, 
       aes(x=fage4, y=fsize,fill = emp_avg))+
  geom_tile()+
  # ggtitle("Average number of exits per 100 establishments, 2010 - 2014")+
  facet_wrap(~metro)

# SBIR loans ===============
# load(paste0("result/",msa_FIPS,"_Market Assessment.Rdata"))
load("Temp data/SBA_loan_cleaned.Rda")

SSTR_summary <- loan_datafiles$SSTR_matched%>%
  select(county14,Company, Phase, Program, Year = Award.Year,amt = Award.Amount,
         Hubzone.Owned, Socially.and.Economically.Disadvantaged, Woman.Owned)%>%
  mutate(name = tolower(gsub("[[:punct:]]+","",Company)))%>%
  mutate(Hub = ifelse(Hubzone.Owned=="Y",1,0),
         gender = ifelse(Woman.Owned=="Y",1,0),
         disadv = ifelse(Socially.and.Economically.Disadvantaged=="Y",1,0))%>%
  select(-contains("."))%>%
  group_by(name)%>%mutate(count_awards=n())

# summary functions
SSTR_type <- function(df){
  df%>%
    group_by(Phase,Program,county14)%>%
    summarise(count = n())%>%
    ungroup()%>%
    mutate(share = count/sum(count))
}

SSTR_gender <- function(df){
  df %>%
    select(county14,name,Year,count_awards,Hub:disadv)%>%
    gather(key,value, Hub:disadv)%>%
    # remove 2007 2008 inconsistency
    filter(Year!=2008 & Year!=2007)%>%
    group_by(name,key,county14)%>%
    # assume the latest year is most accurate
    arrange(Year)%>%
    slice(n())%>%
    group_by(key,county14)%>%
    summarise(count.company = n(),
              count.awards = sum(count_awards),
              share.company = mean(value),
              share.award= weighted.mean(value, count_awards))
}

SSTR_all <- function(df){
  df%>%
    group_by(county14)%>%
    summarise(count = n(),
              amt = sum(amt, na.rm = T))
  }
# hist(t$value)
# sfactor(t$Year)

# summary for nation, Jefferson County, and Davidson County


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
  mutate(FIPS = padz(county14,5))%>%
  right_join(Peers[c("FIPS","county")])%>%
  # filter(FIPS%in%Peers$FIPS)%>%
  filter(Year>2010)%>%
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

# SBA ===========================================
sba_msa <- read_delim("source/msa_3digitnaics_2012.txt",delim = ",")
sba_bham <- sba_msa %>%filter(`MSA  Code`==msa_FIPS)%>%filter(MSA==13820)

# ASE ------------------------

load("ASE_xwalk.Rda")

ASE_firmage <- bind_rows(read.csv("source/ASE_2016_Bham.csv"),
                         read.csv("source/ASE_2016_US.csv"),
                         read.csv("source/ASE_2016_Nashville.csv"))
ASE_firmage%>%
  filter(NAICS.id == "00")%>%
  filter(SEX.id == "096")%>%
  filter(YIBSZFI.id =="001")%>%select(GEO.display.label, FIRMPDEMP, EMP)

ASE_traded <- ASE_firmage%>%
  filter(NAICS.id%in%c(traded_2,"00"))%>%
  # filter(SEX.id == "096")%>%
  filter(SEX.id == "001")%>%
  filter(ETH_GROUP.id == "001")%>%
  filter(RACE_GROUP.id == "00")%>%
  filter(VET_GROUP.id == "001")%>%
  mutate(firmage = case_when(
    YIBSZFI.id == "001" ~ "All firms",
    YIBSZFI.id %in% c("311","318","319")~ " < 5 years",
    YIBSZFI.id %in% c("321","322")~ "6 - 16 years",
    YIBSZFI.id %in% c("323") ~ " > 16 years"
  ))%>%left_join(ASE_labels,by = c("EMP" = "label"))%>%
  mutate(emp_s = is.na(as.numeric(EMP)),
         emp_low = ifelse(!emp_s,as.numeric(EMP),low),
         emp_mean = ifelse(!emp_s,as.numeric(EMP),Mean.value),
         emp_high = ifelse(!emp_s,as.numeric(EMP),high),
         firm = as.numeric(FIRMPDEMP))%>%
  group_by(firmage, GEO.id, NAICS.id, YEAR.id)%>%
  summarise(emp_mean = sum(emp_mean),
            emp_low = sum(emp_low),
            emp_high = sum(emp_high),
            emp_s = sum(emp_s),
            firm = sum(firm))%>%
  mutate(firm_size_m = emp_mean/firm,
         firm_size_l = emp_low/firm,
         firm_size_h = emp_high/firm)


ASE_gap <- ASE_traded %>%
  setDT()%>%
  dcast(firmage+NAICS.id ~ GEO.id, value.var = c("emp_mean","emp_s","firm","firm_size_m"))%>%
  # reshape2::dcast(firmage+NAICS.id ~ GEO.id, value.var ="firm_size")%>%
  mutate(Bham_US = `firm_size_m_310M200US13820`-`firm_size_m_0100000US`,
         Bham_NV = `firm_size_m_310M200US13820`-`firm_size_m_310M200US34980`)

output <- ASE_gap%>%
  bind_rows(ASE_gap%>%
              filter(!NAICS.id=="00")%>%
              group_by(firmage)%>%
              summarise_if(is.numeric,sum))
  
sfactor(ASE_traded$YIBSZFI.id)
sfactor(ASE_firmage$SEX.id)
sfactor(ASE_traded$YIBSZFI.display.label)


# low wage ==============================================

lw_transform <- function(GEO){
  readxl::read_xlsx('source/low wage.xlsx',sheet = paste0(GEO," Age"),n_max = 200,skip = 2)%>%
    select(-...1, -...2, -...8)%>%
    mutate(young_mh = `18-24...4`,
           old_mh = rowSums(select(.,3:5)),
           young_low = `18-24...9`,
           old_low = rowSums(select(.,7:9)))%>%
    select(-contains("..."))%>%
    filter(!is.na(agecats2))%>%
    mutate(geo = GEO)
}

lw_df <- bind_rows(lw_transform("National"),lw_transform("Jefferson"),lw_transform("Davidson"))

lw_summary <- lw_df %>%
  mutate(
    edu = case_when(
      agecats2 %in% c("lths","hs") ~ "HS or below",
      agecats2 == "sc" ~ "Some college",
      agecats2 %in% c("aa", "baplus") ~ "AA/BA or higher"
    ))%>%
  mutate(
    race = case_when(
      agecats2 =="whiteNH" ~ "White",
      agecats2 %in% c("blackNH","latino", "asianNH","otherNH") ~ "Non-white"
    ))%>%
  filter(!(is.na(edu)&is.na(race)))%>%
  group_by(edu, race,geo)%>%
  summarise_if(is.numeric, sum)

# gap charts ====================================================================
# paste data from spreadsheet datapasta:: -----------
# positive changes
temp_p <- tibble::tribble(
                             ~var, ~current, ~to.match.the.U.S., ~to.match.Nashville,
            "tradable_young_firm",     1100,                340,                  90,
            "tradable_young_jobs",     4700,               4500,                1600,
                       "startups",       56,                  0,                  50,
                           "SBIR",       42,                 65,                   0,
                     "dense_jobs",   366000,              38000,               50000,
                    "access_jobs",   231000,              26000,               21000
            )
# negative changes
temp_n <- tibble::tribble(
                             ~var, ~current, ~to.match.the.U.S., ~to.match.Nashville,
            "lowwage_nocol_young",     9800,                140,                 340,
              "lowwage_min_young",    14000,                100,                 400,
              "oow_somecol_young",     3700,                940,                1160,
                "oow_somecol_old",    10250,                  0,                1400,
                  "oow_min_young",     7900,               1200,                1100
            )


# reshape data -------------------
library("scales")

t_p <- temp_p %>%
  mutate(type = "top")%>%
  bind_rows(temp_p%>%mutate(to.match.the.U.S.= current,
                            to.match.Nashville = current,
                            current = NA,
                            type = "bottom"))%>%
  gather(gaps, value, current:to.match.Nashville)%>%
  mutate(type = ifelse(gaps=="current","current",type))%>%
  mutate(lab = ifelse(type=="top",paste0("+ ",comma(value)),comma(value)))

t_n <- temp_n %>%
  mutate(type = "top")%>%
  bind_rows(temp_n%>%mutate(to.match.the.U.S.= current - to.match.the.U.S.,
                          to.match.Nashville = current - to.match.Nashville,
                          current = NA,
                          type = "bottom"))%>%
  gather(gaps, value, current:to.match.Nashville)%>%
  mutate(type = ifelse(gaps=="current","current",type))%>%
  mutate(lab = ifelse(type=="top",paste0("- ",comma(value)),scales::comma(value)))

# bind together
t <-  bind_rows(t_p,t_n)
temp <- bind_rows(temp_p,temp_n)


# function to create bar plots and save
gap_charts <- function(var.p){

t <- t%>%filter(var==var.p)  

bbplot(t[order(-t$value),],
       aes(x=gaps, y = value,label = lab,
           fill = factor(type,levels = c("current","top","bottom"))))+
  geom_bar(stat = "identity")+
  geom_text(aes(color = factor(type,levels = c("current","top","bottom"))),
            size = 5, position = position_stack(vjust=0.5))+
  scale_color_manual(values = c("current"="white",
                                "top"="black",
                                "bottom"="#D9D9D9"),guide = F)+
  scale_fill_manual(values = c("top"="#ffc000",
                               "current"="#003249",
                               "bottom"="#D9D9D9"),guide = F)+
  scale_x_discrete(limits = c("current","to.match.the.U.S.","to.match.Nashville"),
                   labels = c("current"="Current BHM", 
                              "to.match.the.U.S."="to match the U.S.", 
                              "to.match.Nashville"="... Nashville"))+
  scale_y_continuous(labels = NULL)+
   # scale_fill_metro("cool", guide = F)+
  labs(x = NULL, y = NULL)+
  theme(axis.line.x.bottom = element_line(colour = "black"),
        plot.margin = unit(c(0.5,0,0,0),"cm"))
}

# test
gap_charts("lowwage_min_young")
gap_charts("access_jobs")

# save charts
save_charts <- function(var.p){
  ggsave(filename = paste0(var.p,".emf"),path = "plots",
         width = 5,height = 1,dpi=500)
}

save_charts("access_jobs")

# save all plots
purrr::map(temp$var,function(x){
  gap_charts(x)
  save_charts(x)
})

