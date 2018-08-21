# Author: Sifan Liu
# Date: Fri Aug 10 14:29:26 2018
# --------------
pkgs <- c('dplyr')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n)) 
# XWALK -------------------------------------------------------------------

county2msa <- read.csv('V:/Sifan/R/xwalk/county2msa.csv') %>%
  mutate(fips = paste0(padz(fipsstatecode,2), padz(fipscountycode,3)))
zip2county <- read.csv("V:/Sifan/R/xwalk/zip2county.csv") %>%
  mutate(ZIP = padz(zip, 5))
msa2msa <- read.csv("V:/Sifan/R/xwalk/msa2msa.csv") %>%
  mutate(MSA = gsub(" Micropolitan Statistical Area| Metropolitan Statistical Area","",cbsaname10)) %>%
  group_by(cbsa10) %>%
  arrange(-afact) %>%
  top_n(1,afact)

state <- read.csv('V:/Sifan/R/xwalk/state2abb.csv', colClasses = "character")
metro100 <- read.csv("V:/Sifan/R/xwalk/top100metros.csv") %>%
  mutate(MSA = gsub(" Metro Area","",cbsa_name))

county2msa <- left_join(county2msa, state, by = c('st_name'= "Name"))
county2msa$st_cty_name <- paste0(county2msa$cty_name, ", ", county2msa$State)
county2msa$st_cty <- paste0(county2msa$cty_name, ", ", county2msa$st_name)

# VC ==========================================================================
VC <- read.csv('source/metro_VC.csv') %>%
  left_join(msa2msa, by = "MSA") %>%
  select(MSA, contains("X"), cbsa13)
  
write.csv(VC, "source/metro_VC_code.csv")

# Out of work =====================================================================
OoW <- read.csv('source/OutOfWork_county.csv')
OoW <- OoW %>% left_join(county2msa[c('fips', 'st_cty_name')], 
                         by = c("Jurisdiction" = "st_cty_name" ))

OoW_unmatched <- filter(OoW, is.na(fips))

# GEOcoding
source("V:/Sifan/R/code/add2FIPS.R")
add2FIPS("New York-Northern New Jersey-Long Island, NY-NJ-PA")

OoW_unmatched$fips <- sapply(OoW_unmatched[["Jurisdiction"]],add2FIPS)
OoW_matched <- OoW %>% filter(!is.na(fips)) %>% mutate(fips = as.character(fips))
OoW <- bind_rows(OoW_matched, OoW_unmatched)

OoW <- left_join(OoW, county2msa[c("cbsacode", 'fips')], by = "fips")
write.csv(OoW, 'source/OutOfWork_county.csv')

# NSf R&D =================================================================
NSF_RD <- read.csv('source/NSF_univ.csv')

NSF_RD_unique <- NSF_RD %>%
  select(Zip, CITY, state) %>% 
  unique() %>%
  mutate(ZIP = padz(as.character(Zip), 5)) %>%
  left_join(zip2county, by = "ZIP") %>%
  mutate(st_cty_name = paste0(CITY,", ",state))

NSF_RD_unmatched <- filter(NSF_RD_unique, is.na(county))
# Geocoding
NSF_RD_unmatched$county <- sapply(NSF_RD_unmatched[['st_cty_name']], add2FIPS)
NSF_RD_unmatched <- bind_rows(NSF_RD_unmatched, NSF_RD_unmatched2, NSF_RD_unmatched3, NSF_RD_unmatched4) %>% filter(!is.na(county))

# rbind
NSF_RD_matched <- NSF_RD_unique %>% 
  filter(!is.na(county)) %>%
  mutate(county = as.character(county))

NSF_RD_unique <- bind_rows(NSF_RD_matched,NSF_RD_unmatched)
NSF_RD <- NSF_RD %>% 
  left_join(NSF_RD_unique[c("Zip","county")], by = "Zip")%>%
  mutate(fips = padz(county,5)) %>%
  left_join(county2msa[c("fips", "cbsacode")], by = "fips")

write.csv(NSF_RD, 'source/NSF_univ.csv')
