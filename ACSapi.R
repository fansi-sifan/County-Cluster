# Author: Sifan Liu
# Date: Fri Aug 03 12:59:33 2018
# --------------

# https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
# https://hrecht.github.io/censusapi/articles/example-masterlist.html

# SETUP ===================
pkgs <- c('tidyverse','censusapi', 'httr')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

# Sys.setenv(CENSUS_KEY = KEY)
# census_api_key(KEY, overwrite = FALSE, install = TRUE)
# readRenviron("~/.Renviron")
# key = Sys.getenv("CENSUS_API_KEY")
# apis <- listCensusApis()
# geo <- listCensusMetadata(name = "acs/acs5",vintage = 2016, type = "geography")



# MAIN ===================

# Function ------
# customize the function to simplify geography calls
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
                    time = time, 
                    key = Sys.getenv("CENSUS_API_KEY"))
  
  return(data)
}

# Make ACS varlist ===================
# acs/acs5 --------
demographics <- c("B01003_001E","B17001_001E", "B17001_002E","B02001_001E", "B02001_002E", "B02001_003E")
earning_edu <- sapply(seq(1,6),function(x){paste0("B20004_00",x,"E")})
poverty_race <- c(sapply(LETTERS[1:9],function(x){paste0("B17001",x,"_001E")}),
                  sapply(LETTERS[1:9],function(x){paste0("B17001",x,"_002E")}))

# transit_race <- sapply(LETTERS[1:7],function(x,y){paste0("B08105",x,"_00",y,"E")},seq(1,7))
transit <- sapply(seq(1,6), function(x){paste0("B08105A","_00",x,"E")})
drive <- c("B08141_001E","B08141_006E")
migration_edu <- sapply(padz(c(seq(1,6),seq(19,36)),2), function(x){paste0("B07009_0",x,"E")})
house_type <- sapply(seq(1,9), function(x){paste0("B25024_00",x,"E")})
house_cost <- c("B25001_001E", "B06011_001E")

# acs/acs5/subject -----
edu_race <- sapply(c(seq(28,42), seq(52,54), "14"),function(x){paste0("S1501_C01_0",x,"E")})
edu <- c("S1501_C02_014E", "S1501_C02_015E")
epop_race <- sapply(seq(12,20), function(x){paste0("S2301_C03_0",x,"E")})
unemp_race <- sapply(seq(12,20), function(x){paste0("S2301_C04_0",x,"E")})

# get var label ===============
# Get var labels --------
# var_sub <- listCensusMetadata(name = "acs/acs5/subject",vintage = 2016, type = "variables")
# var <- listCensusMetadata(name = "acs/acs5", vintage = 2016, type = "variables")
# acs_var <- bind_rows(var, var_sub)
# save(acs_var, file = "ACSvar.RData")
# load('V:/Sifan/Birmingham/County Cluster/ACSvar.RData')

varlist <- c(demographics, earning_edu, edu, transit,poverty_race,
             migration_edu,house_type, house_cost,edu_race, epop_race, unemp_race)
var_labels <- acs_var %>% 
  select(name, label,concept) %>%
  filter(name %in% varlist) 

rm(acs_var)

# RUN =======

# Metro_ACS <- bind_cols(
#   GetACS("acs/acs5", c("NAME", transit), 'msa', vintage = 2016),
#   GetACS("acs/acs5/subject",edu, 'msa', vintage = 2016)
# )

# QWI modification ======


library("httr", "tidyverse")
apiParse <- function (req) {
  if (jsonlite::validate(httr::content(req, as="text"))[1] == FALSE) {
    error_message <- (gsub("<[^>]*>", "", httr::content(req, as="text")))
    stop(paste("The Census Bureau returned the following error message:\n", error_message))
  } else {
    raw <- jsonlite::fromJSON(httr::content(req, as = "text"))
  }
}

cleanColnames <- function(dt) {
  # No trailing punct
  colnames(dt) <- gsub("\\.[[:punct:]]*$", "", colnames(dt))
  # All punctuation becomes underscore
  colnames(dt) <- gsub("[[:punct:]]", "_", colnames(dt))
  # Get rid of repeat underscores
  colnames(dt) <- gsub("(_)\\1+", "\\1", colnames(dt))
  return(dt)
}

responseFormat <- function(raw) {
  # Make first row the header
  colnames(raw) <- raw[1, ]
  df <- data.frame(raw)
  df <- df[-1,]
  df <- cleanColnames(df)
  # Make all columns character
  df[] <- lapply(df, as.character)
  # Make columns numeric if they have numbers in the column name - note some APIs use string var names
  # For ACS data, do not make columns numeric if they are ACS annotation variables - ending in MA or EA or SS
  # Do not make label variables (ending in _TTL) numeric
  value_cols <- grep("[0-9]", names(df), value=TRUE)
  error_cols <- grep("MA|EA|SS|_TTL|_NAME|NAICS2012|NAICS2012_TTL|fage4|FAGE4", value_cols, value=TRUE, ignore.case = T)
  for(col in setdiff(value_cols, error_cols)) df[,col] <- as.numeric(df[,col])
  
  row.names(df) <- NULL
  return(df)
}


qwi_api_url <- "https://api.census.gov/data/timeseries/qwi/sa"
getQWI <- function(x)responseFormat(apiParse(req(key, get, region, regionin, time)))httr::GET(qwi_api_url,
                                                                                              query = list(key = CENSUS_KEY, get = get, 
                                                                                                           "for" = region,"in" = regionin, 
                                                                                                           time=time,
                                                                                                           ownercode = "A05",seasonadj="U",
                                                                                                           # industry="23",industry ="31-33",
                                                                                                           firmage=1,firmage=2,firmage=3,firmage=4,firmage=5))


# 
# ase_api_url <- "https://api.census.gov/data/2014/ase/csa?get=RACE_GROUP,RACE_GROUP_TTL,SEX,SEX_TTL,NAICS2012,NAICS2012_TTL,GEO_TTL,FIRMPDEMP&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:13820"
# 
# ase_api_url <- "https://api.census.gov/data/2014/ase/csa?get=RACE_GROUP,RACE_GROUP_TTL,SEX,SEX_TTL,NAICS2012,NAICS2012_TTL,GEO_TTL,YEAR,FIRMPDEMP&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:13820"
# 
# test <- GET(ase_api_url)
# temp <- content(test)
