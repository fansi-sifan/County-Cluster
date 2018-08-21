# Author: Sifan Liu
# Date: Fri Aug 03 12:59:33 2018
# --------------

# https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
# https://hrecht.github.io/censusapi/articles/example-masterlist.html

# SETUP ===================
pkgs <- c('dplyr','censusapi')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 


Sys.setenv(CENSUS_KEY = "81ac4021863bd89e0405a89a21f461c35bc9304c")
# readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_KEY")

padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n)) 

# Contents ===================
# apis <- listCensusApis()
# geo <- listCensusMetadata(name = "acs/acs5",vintage = 2016, type = "geography")
# var_sub <- listCensusMetadata(name = "acs/acs5/subject",vintage = 2016, type = "variables")
# var <- listCensusMetadata(name = "acs/acs5", vintage = 2016, type = "variables")
# acs_var <- bind_rows(var, var_sub)
# save(acs_var, file = "ACSvar.RData")
load('V:/Sifan/Birmingham/County Cluster/ACSvar.RData')

# Function ------

GetACS <- function(name,varlist,geotype, time = NULL, vintage = NULL){
  if(geotype == "msa"){
    region = "metropolitan statistical area/micropolitan statistical area:*"
  } else if (geotype == "MSAs"){
    region = "metropolitan statistical areas:*"
  } else if (geotype == "MSA"){
    region = "metropolitan statistical area:*"
  } else if (geotype == "county"){
    region = "county:*"
  } else {
    region = "state:*"
  }
  
  data <- getCensus(name = name, 
                    vintage = vintage,
                    vars = varlist,
                    region = region,
                    time = time)
  
  return(data)
}

# ACS varlist ----------------------
# acs/acs5
poverty <- c("B01003_001E","B17001_001E", "B17001_002E")
earning_edu <- sapply(seq(1,6),function(x){paste0("B20004_00",x,"E")})
# transit_race <- sapply(LETTERS[1:7],function(x,y){paste0("B08105",x,"_00",y,"E")},seq(1,7))
transit <- sapply(seq(1,6), function(x){paste0("B08105A","_00",x,"E")})
migration_edu <- sapply(padz(c(seq(1,6),seq(19,36)),2), function(x){paste0("B07009_0",x,"E")})
house_type <- sapply(seq(1,9), function(x){paste0("B25024_00",x,"E")})
house_cost <- c("B25001_001E", "B06011_001E")

# acs/acs5/subject
edu_race <- sapply(c(seq(28,42), seq(52,54), "14"),function(x){paste0("S1501_C01_0",x,"E")})
edu <- c("S1501_C02_014E", "S1501_C02_015E")
epop_race <- sapply(seq(12,20), function(x){paste0("S2301_C03_0",x,"E")})
unemp_race <- sapply(seq(12,20), function(x){paste0("S2301_C04_0",x,"E")})

# get var label
varlist <- c(poverty, earning_edu, edu, transit,
             migration_edu,house_type, house_cost,edu_race, epop_race, unemp_race)
var_labels <- acs_var %>% 
  select(name, label,concept) %>%
  filter(name %in% varlist) 

rm(acs_var)