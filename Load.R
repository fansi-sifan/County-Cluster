# Clean geographic names and match to geocodes
# Author: Sifan Liu
# Date: Fri Aug 10 14:29:26 2018

source("Func.R")

# VC ==========================================================================
# Pitchbook NCVA downloads
# VC <- read.csv('source/metro_VC.csv') %>%
## MSA definition change
#   left_join(msa2msa, by = "MSA") %>%
#   select(MSA, contains("X"), cbsa13)
#
# write.csv(VC, "source/metro_VC_code.csv")

# VC_xwalk_msa <- msa2msa %>% separate(MSA, c("name", "State"), sep = ", ") %>%
#   mutate(name_short = gsub("-.+","",name),
#          name_state = paste0(name_short, " (",State,")"))
#
# VC_xwalk <- VC %>%
#   select(MSA, Country) %>% unique() %>%
#   left_join(VC_xwalk_msa[c("name_state","cbsa13")], by = c("MSA"="name_state")) %>%
#   left_join(VC_xwalk_msa[c("name_short","cbsa13")], by = c("MSA" = "name_short")) %>%
#   mutate(cbsa13 = ifelse(is.na(cbsa13.y),cbsa13.x,cbsa13.y))

# write.csv(VC_xwalk, "source/VC_xwalk.csv")

VC_xwalk <- read.csv("source/VC_xwalk.csv")
VC <- read.csv("source/gsr_map_final.csv") %>%
  separate(name, c("MSA", "Country"), sep = ", ") %>%
  filter(Country == "United States") %>%
  left_join(VC_xwalk, by = "MSA")

write.csv(VC, "source/VC.csv")

# Out of work =====================================================================
OoW <- read.csv("source/OutOfWork_county.csv")
OoW <- OoW %>% left_join(county2msa[c("fips", "st_cty_name")],
  by = c("Jurisdiction" = "st_cty_name")
)

OoW_unmatched <- filter(OoW, is.na(fips))

# GEOcoding
source("V:/Sifan/R/code/add2FIPS.R")
add2FIPS("New York-Northern New Jersey-Long Island, NY-NJ-PA")

OoW_unmatched$fips <- sapply(OoW_unmatched[["Jurisdiction"]], add2FIPS)
OoW_matched <- OoW %>% filter(!is.na(fips)) %>% mutate(fips = as.character(fips))
OoW <- bind_rows(OoW_matched, OoW_unmatched)

OoW <- left_join(OoW, county2msa[c("cbsacode", "fips")], by = "fips")
write.csv(OoW, "source/OutOfWork_county.csv")

# NSf R&D =================================================================
NSF_RD <- read.csv("source/NSF_univ.csv")

NSF_RD_unique <- NSF_RD %>%
  select(Zip, CITY, state) %>%
  unique() %>%
  mutate(ZIP = padz(as.character(Zip), 5)) %>%
  left_join(zip2county, by = "ZIP") %>%
  mutate(st_cty_name = paste0(CITY, ", ", state))

NSF_RD_unmatched <- filter(NSF_RD_unique, is.na(county))
# Geocoding
NSF_RD_unmatched$county <- sapply(NSF_RD_unmatched[["st_cty_name"]], add2FIPS)
NSF_RD_unmatched <- bind_rows(NSF_RD_unmatched, NSF_RD_unmatched2, NSF_RD_unmatched3, NSF_RD_unmatched4) %>% filter(!is.na(county))

# rbind
NSF_RD_matched <- NSF_RD_unique %>%
  filter(!is.na(county)) %>%
  mutate(county = as.character(county))

NSF_RD_unique <- bind_rows(NSF_RD_matched, NSF_RD_unmatched)
NSF_RD <- NSF_RD %>%
  left_join(NSF_RD_unique[c("Zip", "county")], by = "Zip") %>%
  mutate(fips = padz(county, 5)) %>%
  left_join(county2msa[c("fips", "cbsacode")], by = "fips")

write.csv(NSF_RD, "source/NSF_univ.csv")


# SSTR grant =================================================
# SSTR read raw --------------------------------------------
# SSTR_archive <- read.csv("https://query.data.world/s/n7cv53ycwjwcfaukfnbplz4n7ndb73", header=TRUE, stringsAsFactors=FALSE)
SSTR_archive <- read.csv("../../SBA finance/SSTR/SBIR-STTR 1983-2017.csv", header = TRUE, stringsAsFactors = FALSE)
### 2016, 2017
SSTR_files <- grep(".xlsx", list.files(path = "../../SBA finance/SSTR", full.names = TRUE), value = TRUE)
SSTR <- bind_rows(purrr::map(SSTR_files, readxl::read_xlsx))
names(SSTR) <- make.names(names(SSTR))
SSTR_all <- SSTR %>%
  mutate_at(c("Solicitation.Year", "Award.Year", "Award.Amount", "Number.Employees"), as.numeric) %>%
  bind_rows(SSTR_archive)
# save(SSTR_all, file = "SSTR.Rda")

# SSTR_clean -----------------------------------------------
# load('SSTR.Rda')

# first match by city

SSTR_all <- SSTR_all %>%
  mutate(PLACE = trimws(toupper(City))) %>%
  mutate(ZIP = padz(substr(Zip, 0, 5), 5))

SSTR_city <- SSTR_all %>%
  select(PLACE, State) %>%
  distinct() %>%
  left_join(place2county, by = c("PLACE", "State"))

SSTR_city_matched <- SSTR_all %>%
  left_join(SSTR_city[c("PLACE", "State", "county14")], by = c("PLACE", "State"))

SSTR_city_nomatch <- SSTR_city_matched %>%
  filter(is.na(county14))

SSTR_zip <- SSTR_city_nomatch %>%
  mutate(ZIP = padz(substr(Zip, 0, 5), 5)) %>%
  select(ZIP, State) %>%
  distinct() %>%
  left_join(zip2county[c("ZIP", "county")], by = "ZIP")

SSTR_zip_matched <- SSTR_city_nomatch %>%
  left_join(SSTR_zip, by = c("ZIP", "State")) %>%
  select(-county14) %>%
  rename(county14 = county)

SSTR_matched <- SSTR_city_matched %>%
  filter(!is.na(county14)) %>%
  bind_rows(SSTR_zip_matched)

# save(SSTR_matched, file = "SSTR_matched.Rda")
# SSTR_summary -----------------------------------------------
SSTR_summary_cty <- SSTR_matched %>%
  group_by(county14, State, Award.Year) %>%
  summarise(
    amt.tot = sum(Award.Amount, na.rm = TRUE),
    count = n()
  )


# SBA loans ==================================================
# SBA read raw -----------------------------------------------
# SBA_7a <- readxl::read_excel("../../SBA finance/SBA/FOIA - 7(a)(FY2010-Present).xlsx", col_names = TRUE, skip = 1)
# SBA_504<- readxl::read_excel("../../SBA finance/SBA/FOIA - 504 (FY1991-Present).xlsx", col_names = TRUE, skip = 1)
# SBA <- bind_rows(SBA_7a, SBA_504)
# save(SBA, file = "SBA.Rda")

# SBA clean --------------------------------------------------
# load("SBA.Rda")
#
# SBA_tomatch <- SBA %>%
#   mutate(PLACE = trimws(toupper(BorrCity)),
#          COUNTY = trimws(toupper(ProjectCounty)),
#          State = BorrState,
#          ZIP = BorrZip)
#
# SBA_county <- SBA_tomatch %>%
#   select(COUNTY, State) %>%
#   distinct() %>%
#   left_join(counties, c("COUNTY", "State"))
#
# SBA_county_matched <- SBA_tomatch %>%
#   left_join(SBA_county, by = c("COUNTY", "State"))
#
# SBA_county_nomatch <- SBA_county_matched %>%
#   filter(is.na(FIPS))
#
# SBA_city <- SBA_county_nomatch %>%
#   select(PLACE, State) %>%
#   distinct() %>%
#   left_join(place2county[c("PLACE","State","county14")], c("PLACE", "State"))
#
# SBA_city_matched <- SBA_county_nomatch %>%
#   left_join(SBA_city,by = c("PLACE", "State"))
#
# SBA_city_nomatch <- SBA_city_matched %>%
#   filter(is.na(county14))
#
# SBA_zip <- SBA_city_nomatch %>%
#   select(ZIP, State) %>%
#   distinct() %>%
#   left_join(zip2county[c("ZIP", "county")], by = "ZIP")
#
# SBA_zip_matched <- SBA_city_nomatch %>%
#   left_join(SBA_zip, by = c("ZIP", "State")) %>%
#   select(- county14) %>%
#   rename(county14 = county)
#
# SBA_matched <- SBA_county_matched %>%
#   filter(!is.na(FIPS)) %>%
#   rename(county14 = FIPS) %>%
#   bind_rows(SBA_city_matched) %>%
#   filter(!is.na(county14)) %>%
#   bind_rows(SBA_zip_matched)
#
# save(SBA_matched, file = "SBA_matched.Rda")

# SBA_summary -----------------------------------------------

SBA_summary_cty <- SBA_matched %>%
  group_by(county14, State, ApprovalFiscalYear) %>%
  summarise(
    SBAamt.tot = sum(SBAGuaranteedApproval, na.rm = TRUE),
    GROSSamt.tot = sum(GrossApproval, na.rm = TRUE),
    count = n()
  )


# EXIM loan guarantees =======================================
EXIM <- read.csv("../../SBA finance/EXIM/Authorizations_From_10_01_2006_Thru_03_31_2018.csv", header = TRUE, stringsAsFactors = FALSE)

EXIM_tomatch <- EXIM %>%
  mutate(
    PLACE = trimws(toupper(Primary.Exporter.City)),
    State = Primary.Exporter.State.Code
  )

EXIM_city <- EXIM_tomatch %>%
  select(PLACE, State) %>%
  distinct() %>%
  left_join(place2county, c("PLACE", "State"))

EXIM_city_matched <- EXIM_tomatch %>%
  left_join(EXIM_city, by = c("PLACE", "State"))

EXIM_city_tomatch <- EXIM_city_matched %>%
  select(Primary.Exporter.City, Primary.Exporter.State.Name, county14) %>%
  filter(is.na(county14)) %>%
  filter(Primary.Exporter.State.Name != "N/A") %>%
  distinct() %>%
  mutate(add = paste(Primary.Exporter.City, Primary.Exporter.State.Name, sep = ", "))

# EXIM geocoding --------------------------------------------
source("V:/Sifan/R/code/add2FIPS.R")
# test
add2FIPS("Thompson, CT", KEY)

EXIM_city_tomatch$county14 <- map(EXIM_city_tomatch$add, add2FIPS, KEY)

for (i in 3:nrow(EXIM_city_tomatch)) {
  EXIM_city_tomatch$county14[[i]] <- substr(add2FIPS(EXIM_city_tomatch$add[[i]], KEY), 1, 5)
}

EXIM_city_tomatch$county14 <- as.integer(EXIM_city_tomatch$county14)

EXIM_city_nomatch <- EXIM_city_matched %>%
  filter(is.na(county14))

EXIM_matched <- EXIM_city_nomatch %>%
  select(-county14) %>%
  left_join(EXIM_city_tomatch, by = c("Primary.Exporter.City", "Primary.Exporter.State.Name")) %>%
  bind_rows(EXIM_city_matched %>% filter(!is.na(county14))) %>%
  filter(Decision == "Approved" & Deal.Cancelled == "No")

save(EXIM_matched, file = "EXIM_matched.Rda")

# EXIM summary data -------------------------------------------

EXIM_summary_cty <- EXIM_city_matched %>%
  group_by(county14, State, Fiscal.Year) %>%
  summarise(
    APPamt.tot = sum(Approved.Declined.Amount, na.rm = TRUE),
    DISBamt.tot = sum(Disbursed.Shipped.Amount, na.rm = TRUE),
    count = n()
  )

# CDFI =======================================================
# CDFI <- read.csv("../../SBA finance/CDFI/CDFI.csv")
# CDFI %>% reshape2::dcast(State ~ Program, value.var = "Amount")

# [depreciated] CDFI summary data -------------------------------------------
#
# CDFI_tomatch <- CDFI %>%
#   mutate(PLACE = toupper(trimws(City)))
#
# CDFI_city <- CDFI_tomatch %>%
#   select(State, PLACE)%>%
#   distinct()%>%
#   left_join(place2county, c("PLACE", "State"))
#
# CDFI_city_matched <- CDFI_tomatch %>%
#   left_join(CDFI_city,by = c("PLACE", "State"))%>%
#   filter(State != "PR")
#
# CDFI_city_tomatch <- CDFI_city_matched %>%
#   filter(is.na(county14))%>%
#   select(City, State)%>%
#   distinct()%>%
#   mutate(add = paste(City, State,sep = ","))
#
# CDFI_city_tomatch$county14 <- sapply(CDFI_city_tomatch$add, add2FIPS,KEY)
# CDFI_city_tomatch$county14 <- as.integer(CDFI_city_tomatch$county14)
# #
# CDFI_matched <- CDFI_city_matched %>%
#   filter(is.na(county14))%>%
#   select(-county14)%>%
#   left_join(CDFI_city_tomatch, by = c("City","State"))%>%
#   bind_rows(CDFI_city_matched%>%filter(!is.na(county14)))
#
# save(CDFI_matched, file = "CDFI_matched.Rda")


# [depreciated] CDFI summary ----------------------------------------------------

# CDFI_summary_cty <- CDFI_matched %>%
#   mutate(Amount = as.numeric(gsub("\\$|,","", Amount)))%>%
#   group_by(county14, State, Year) %>%
#   summarise(amt.tot = sum(Amount, na.rm = TRUE),
#             count = n())


# CDFI transaction level -------------------------------------------
allfiles <- list.files(path = "../../SBA finance/CDFI/FY 2016 Data, Documentation, Instructions/", full.names = TRUE)
temp <- purrr::map(grep(".csv", allfiles, value = TRUE), read.csv)
ILR <- temp[[1]]
TLR <- bind_rows(temp[-1])

TLR_matched <- TLR %>%
  mutate(
    FIPS = padz(projectfipscode_2000, 11),
    county14 = as.numeric(substr(FIPS, 1, 5))
  )

# save(TLR,file = "TLR.Rda")
TLR_matched$Year <- format(as.Date(TLR_matched$dateclosed, "%d-%B-%y"), "%Y")


# FDIC ========================================================

# FDIC download, unzip and load all year data from FFIEC -----

download_FDIC <- function(year) {
  temp <- tempfile()
  download.file(paste0("https://www.ffiec.gov/cra/xls/", year, "exp_aggr.zip"), temp)
  # handle changes in file names
  con1 <- unz(temp, "exp_aggr.dat")
  con2 <- unz(temp, paste0(year, "exp_aggr.dat"))
  con3 <- unz(temp, paste0("cra20", year, "_Aggr_A11.dat"))
  data <- tryCatch(read_delim(con1, col_names = FALSE, delim = " "),
    error = function(e) {
      tryCatch(read_delim(con2, col_names = FALSE, delim = " "),
        error = function(c) {
          read_delim(con3, col_names = FALSE, delim = " ")
        }
      )
    }
  )
  # close connection, return data
  unlink(temp)
  return(select(data, X1, X2, X5))
}

# correct for '96 format
# FDIC_96 <- download_FDIC("96")
# FDIC_96 <- FDIC_96 %>%
#   separate(X1, c("X1", "X2"),sep = "(?=1996)") %>%
#   rename(X5 = X4)

# FDIC_year <- c(seq(97,99),padz(seq(0,4,1),2))
FDIC_year <- c(padz(seq(5, 17), 2))
FDIC_data <- purrr::map(FDIC_year, download_FDIC)
FDIC_df <- bind_rows(FDIC_data)


# FDIC clean ------------------------------------------------

X2_colnames <- c("year", "type", "State", "county", "CBSA", "FIPS", "pop", "income")
X5_colnames <- c("n_100k", "x_100k", "n_250k", "x_250k", "n_1m", "x_1m", "n_stot", "x_stot")

FDIC_detectformat <- FDIC_df %>%
  filter(X1 == "A1-1") %>%
  mutate(format = case_when(
    nchar(X2) == 27 ~ TRUE,
    nchar(X2) == 28 ~ FALSE
  ))

FDIC_interpret <- FDIC_detectformat %>%
  filter(format == TRUE) %>%
  separate(X2, X2_colnames, sep = c(4, 6, 8, 11, 15, 22, 24)) %>%
  separate(X5, X5_colnames, sep = c(6, 14, 20, 28, 34, 42, 48)) %>%
  bind_rows(FDIC_detectformat %>%
    filter(format == FALSE) %>%
    separate(X2, X2_colnames, sep = c(4, 6, 8, 11, 16, 23, 25)) %>%
    separate(X5, X5_colnames, sep = c(10, 20, 30, 40, 50, 60, 70))) %>%
  mutate_at(vars(matches("x_|n_")), as.numeric)

FDIC_matched <- FDIC_interpret %>%
  # filter(CBSA!="NA")%>%
  # select(-X2,-X5) %>%
  mutate(
    n_tot = n_100k + n_250k + n_1m,
    x_tot = x_100k + x_250k + x_1m,
    county14 = as.numeric(paste0(State, county))
  ) %>%
  filter(x_tot > 0)

loan_datafiles$FDIC_matched <- bind_rows(loan_datafiles$FDIC_matched, FDIC_matched)

# FDIC_matched_early <- FDIC_matched
# load("FDIC_matched.Rda")
# FDIC_matched <- bind_rows(FDIC_matched_early, FDIC_matched)

# save(FDIC_matched, file = "FDIC_matched.Rda")

# NMTC ==================================================
NMTC <- readxl::read_xlsx("../../SBA finance/NMTC/FY 2017 NMTC Public Data Release_v2.xlsx", sheet = "Projects 2 - Data Set PUBLISH.P")
NMTC_matched <- NMTC %>%
  mutate(
    FIPS = padz(as.character(`2010 Census Tract`), 11),
    county14 = as.numeric(substr(FIPS, 1, 5))
  )

NMTC_summary_cty <- NMTC_matched %>%
  group_by(`Origination Year`, State, county14) %>%
  summarise(amt.tot = sum(`Project QLICI Amount`, na.rm = TRUE))

# save to files

range(loan_datafiles$CDFI_matched, "Year")
na.share(loan_datafiles$CDFI_matched, "Year")

# save all Bham extract to csv
dfs <- objects()
# loan_datafiles <- mget(dfs[grep("matched", dfs)])
new <- mget(dfs[grep("NMTC_matched", dfs)])
loan_datafiles <- gdata::update.list(loan_datafiles, new)

save(loan_datafiles, file = "Temp data/SBA_loan_cleaned.Rda")

# MetroMonitor -------------------------------------------------------------
# paths <- "V:/Performance/Project files/Metro Monitor/2018v/Output/"
paths <- "V:/Performance/Project files/Metro Monitor/2019v/Output/"


# Growth, Prosperity, Inclusion




# change
growth_rank <- read.csv(paste0(paths, "Growth/Growth Ranks (IS 2017.11.15).csv"))
prosperity_rank <- read.csv(paste0(paths, "Prosperity/Prosperity Ranks (IS 2017.11.14).csv"))
inclusion_rank <- read.csv(paste0(paths, "Inclusion/Inclusion Ranks (IS 2017.11.17).csv"))

# value
growth_value <- read.csv("Growth/Growth Values (IS 2017.11.15).csv") %>%
  filter(year == 2016) %>%
  dcast(year + cbsa ~ indicator, var.value = "value")
prosperity_value <- read.csv("Prosperity/Prosperity Values (IS 2017.11.14).csv") %>%
  filter(year == 2016) %>%
  dcast(year + CBSA ~ indicator, var.value = "value")
inclusion_value <- read.csv("Inclusion/Inclusion Values (IS 2017.11.17).csv") %>%
  filter(year == 2016) %>%
  filter(race == "Total") %>%
  filter(eduatt == "Total") %>%
  dcast(year + cbsa ~ indicator, var.value = "value")

# change
names(inclusion_change) <- names(growth_change)

MM_change <- prosperity_change %>%
  filter(year == "2006-2016") %>%
  left_join(growth_change, by = c("year", "CBSA")) %>%
  left_join(inclusion_change, by = c("year", "CBSA")) %>%
  # right_join(peercities, by = c("CBSA" = "FIPS" )) %>%
  select(-contains("score"), -contains("name"))

# value
names(prosperity_value)[[2]] <- "cbsa"

MM_value <- prosperity_value %>%
  filter(year == "2016") %>%
  left_join(growth_value, by = c("year", "cbsa")) %>%
  left_join(inclusion_value, by = c("year", "cbsa"))

MM <- MM_change %>%
  left_join(MM_value, by = c("CBSA" = "cbsa")) %>%
  rename(
    rank_inclusion = rank,
    rank_prosperity = rank.x,
    rank_growth = rank.y
  )

save(MM, file = "V:/Sifan/Birmingham/County Cluster/source/MetroMonitor.Rda")

# EMSI NAICS 6 100 metros -------------------------------------------------
NAICS6_US <- read.csv("V:/Sacramento/R script/source/Trade/shiftshare/NAICS6_US_0616.csv")
NAICS6_master <- NAICS6_US[c("NAICS", "Description", "X2016.Jobs", "X2006.Jobs")] %>%
  mutate(
    SUM_JOBS_2006 = sum(X2006.Jobs, na.rm = TRUE),
    SUM_JOBS_2016 = sum(X2016.Jobs, na.rm = TRUE)
  )
library("readxl")

allfiles <- list.files(path = "V:/Sacramento/R script/source/Trade/shiftshare/EMSI", full.names = TRUE)
Jobs2006 <- lapply(allfiles, read_xlsx, sheet = "Industry Breakdown - 2006 Jobs", skip = 2, col_types = "numeric", n_max = 993)
Jobs2016 <- lapply(allfiles, read_xlsx, sheet = "Industry Breakdown - 2016 Jobs", skip = 2, col_types = "numeric", n_max = 993)
LQ2006 <- lapply(allfiles, read_xlsx, sheet = "Location Quotient Breakdown ...", skip = 2, col_types = "numeric", n_max = 993)
LQ2016 <- lapply(allfiles, read_xlsx, sheet = "Location Quotient Breakd... (2)", skip = 2, col_types = "numeric", n_max = 993)
ComEffect <- lapply(allfiles, read_xlsx, sheet = "Shift Share Breakdown - Comp...", skip = 2, col_types = "numeric", n_max = 993)

df_transform <- function(df) {
  bind_rows(lapply(df, function(x) x %>% select(-Description) %>% gather(MetroNames, value, names(x)[-c(1, 2)])))
}


EMSI <- plyr::join_all(lapply(
  list(Jobs2006, Jobs2016, LQ2006, LQ2016, ComEffect),
  df_transform
),
by = c("Industry", "MetroNames")
)

names(EMSI) <- c("NAICS6", "cbsa_name", "Jobs2006", "Jobs2016", "LQ2006", "LQ2016", "ComEffect")

EMSI_master <- EMSI %>%
  unique() %>%
  filter(!is.na(NAICS6)) %>%
  group_by(cbsa_name) %>%
  mutate(
    sum_jobs_2006 = sum(Jobs2006, na.rm = TRUE),
    sum_jobs_2016 = sum(Jobs2016, na.rm = TRUE)
  ) %>%
  left_join(NAICS6_master, by = c("NAICS6" = "NAICS"))

save(EMSI_master, file = "EMSI_master.Rda")



# CoG ==========================================================
# "https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk"
