# Author: Sifan Liu
# Date: Wed Feb 27 12:16:17 2019
# --------------
pkgs <- c('tigris', "rgdal",'maptools', 'tidyverse', "ggmap", "maps", "grid", "gridExtra",'rgeos','broom', 'ggrepel')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

# FUNCTIONS =========================================================

# add leading zeros
padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n)) 

# NA values
na.share <- function(df,col)(sum(is.na(df[[col]]))/length(df[[col]]))

# check distribution
range <- function(df,col)(summary(as.factor(df[[col]])))

# quickly summarize factor levels
sfactor <- function(x)summary(as.factor(as.character(x)))

# plot themes
pthemes <- theme(rect = element_rect(fill = "#D9D9D9", colour=NA),
                 panel.background = element_rect(fill = "#D9D9D9",colour = NA),
                 plot.background = element_rect(fill = "#D9D9D9", colour = NA),
                 panel.grid = element_blank(),
                 legend.background = element_rect(fill = "transparent"),
                 legend.key = element_rect(fill = "transparent", color = NA),
                 legend.box.background = element_rect(fill = "transparent", colour = NA),
                 text = element_text(size = 15,family ="sans" ), 
                 axis.text = element_text(size = 12, family = "sans"),
                 plot.title = element_text(hjust = 0.5),
                 axis.ticks = element_blank()
)

# bar plot with title and highlights
bar_plot <- function(df,title, HL){
  ggplot(df %>% filter(!is.na(value)), aes(x = reorder(metro,value), y = value, fill = HL))+
    geom_bar(stat = "identity")+
    coord_flip()+
    labs(title = title, x = NULL,y = NULL)+
    scale_fill_manual(values = c("#0070c0", "#ffc000"), guide = FALSE)
}

# SMEloans plot, peer vs Bham
opr <- function(df){
  df %>%
    group_by(Bham, program, year_range, emp.tot, emp.traded)%>%
    summarise(amt.tot = sum(as.numeric(amt.tot), na.rm = TRUE))%>%
    filter(!is.na(year_range))%>%
    mutate(value = amt.tot/emp.tot*1000)
}

p_SME <- function(df,pgm){ggplot(data = df%>%filter(program==pgm), 
                                 aes(x = year_range, y = value/5, fill = Bham))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_manual(name = element_blank(), 
                      values = c("#0070c0", "#ffc000"), 
                      labels = c( "Peer average", paste(placename,countyname, sep = " /")))+
    labs(x = NULL, y = NULL)+
    facet_wrap(~geo, nrow = 1)+
    pthemes%+%theme(legend.position = "bottom")}

# SETUP =============================================================
# assign geocodes for analysis ------------------------
city_FIPS <- "07000"
ct_FIPS <- "073"
msa_FIPS <- "13820"
st_FIPS <- "01"
county_FIPS <- paste0(st_FIPS, ct_FIPS)

# tracts within city boundary ------------------------
xwalk.Bham <- read.csv("source/Bham_city_tracts.csv", colClasses = "character") %>%
  separate(tract, c("a","b"))%>%
  mutate(b = replace(b, is.na(b), 0))
city_tracts <- paste0(padz(xwalk.Bham$a,4), padz(xwalk.Bham$b,2))

# assign names for text display ------------------------
placename <- "Birmingham"
countyname <- "Jefferson County, AL"
metroname <- "Birmingham MSA"

# peers ------------------------
# peers from clustering result 
# Peers <- read.csv("source/counties_cluster_all.csv") %>%
#   mutate(cbsa = as.character(cbsa)) %>%
#   mutate(stcofips = padz(as.character(stcofips), 5)) %>%
#   group_by(kmeans) %>%
#   filter(msa_FIPS %in% cbsa)

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
msa_ct_fips <- (msa_ct_FIPS%>%filter(cbsa==msa_FIPS))$FIPS

msa100_FIPS <- as.character((read.csv("V:/Sifan/R/xwalk/top100metros.csv") %>% filter(top100==1))[["GEO.id2"]])



