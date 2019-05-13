# Author: Sifan Liu
# Date: Wed Feb 27 12:16:17 2019
# --------------
# pkgs <- c('SifanLiu','tidyverse','tigris', "rgdal",'maptools',  "ggmap", "maps", "grid", "gridExtra",'rgeos','broom', 'ggrepel')

pkgs <- c('SifanLiu','tidyverse')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

# SETUP =============================================================
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

# get tracts within city boundary ------------------------
# xwalk.Bham <- read.csv("source/Bham_city_tracts.csv", colClasses = "character") %>%
#   separate(tract, c("a","b"))%>%
#   mutate(b = replace(b, is.na(b), 0))
# city_tracts <- paste0(padz(xwalk.Bham$a,4), padz(xwalk.Bham$b,2))

# [Depreciated] peers from clustering result 
# Peers <- read.csv("source/counties_cluster_all.csv") %>%
#   mutate(cbsa = as.character(cbsa)) %>%
#   mutate(stcofips = padz(as.character(stcofips), 5)) %>%
#   group_by(kmeans) %>%
#   filter(msa_FIPS %in% cbsa)

peers.county <- c("01073","18097","22033","22071","29095",
                  "36029","36055","39035","47157","51710",
                  "51760","55079","21111","47037","01089", county_FIPS)

# Peers <- readxl::read_xlsx('result/13820_Market Assessment.xlsx', sheet = "Peers")%>%
#   filter(cbsa %in% peerlist)


Peers <- county_cbsa_st %>% filter(code.county %in% peers.county)
msa_ct_FIPS <- (county_cbsa_st %>% filter(code.cbsa == msa_FIPS))$code.county
msa100_FIPS <- (metro100 %>% filter(istop100.cbsa))$code.cbsa


# function to split df by na value, merge by a different xwalk, bind
split_join_merge <- function(df.source, df.target,col.filter,col.join){
  
  df.matched <- df.source %>% 
    filter(!is.na(!!col.filter))
  
  df.tomatch <- df.source %>% 
    filter(is.na(!!col.filter))%>%
    select(-!!col.filter)%>%
    left_join(df.target, by = col.join)
  
  return(bind_rows(df.matched, df.tomatch))
}

# Visualizations ===================================================
# bar plot with title and highlights
bar_plot <- function(df,title, HL){
  bbplot(df %>% filter(!is.na(value)), aes(x = reorder(metro,value), y = value, fill = HL))+
    geom_bar(stat = "identity")+
    coord_flip()+
    labs(title = title, x = NULL,y = NULL)+
    scale_fill_manual(values = c("#0070c0", "#ffc000"), guide = FALSE)
}
