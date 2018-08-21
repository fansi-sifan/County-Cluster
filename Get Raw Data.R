# Get Raw Data and save to 'source' folder
# Author: Sifan Liu
# Date: Fri Aug 03 14:00:12 2018
# --------------
pkgs <- c('tidyverse', 'reshape2')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n)) 




# MetroMonitor -------------------------------------------------------------
setwd("V:/Performance/Project files/Metro Monitor/2018v/Output")
# Growth, Prosperity, Inclusion

# change
growth_change <- read.csv("Growth/Growth Ranks (IS 2017.11.15).csv") 
prosperity_change <- read.csv("Prosperity/Prosperity Ranks (IS 2017.11.14).csv") 
inclusion_change <- read.csv("Inclusion/Inclusion Ranks (IS 2017.11.17).csv") 

# value
growth_value <- read.csv("Growth/Growth Values (IS 2017.11.15).csv") %>%
  filter(year == 2016) %>%
  dcast(year + cbsa ~ indicator, var.value = "value")
prosperity_value <- read.csv("Prosperity/Prosperity Values (IS 2017.11.14).csv")  %>%
  filter(year == 2016) %>%
  dcast(year + CBSA ~ indicator, var.value = "value")
inclusion_value <- read.csv("Inclusion/Inclusion Values (IS 2017.11.17).csv")  %>%
  filter(year == 2016) %>%
  filter(race == "Total") %>%
  filter(eduatt =="Total") %>%
  dcast(year + cbsa ~ indicator, var.value = "value")

# change
names(inclusion_change) <- names(growth_change)

MM_change <- prosperity_change %>% 
  filter(year == "2006-2016") %>%
  left_join(growth_change, by = c("year","CBSA")) %>%
  left_join(inclusion_change, by = c("year","CBSA")) %>%
  # right_join(peercities, by = c("CBSA" = "FIPS" )) %>%
  select(-contains("score"), -contains("name"))

# value
names(prosperity_value)[[2]] <- 'cbsa'

MM_value <- prosperity_value %>% 
  filter(year == "2016") %>%
  left_join(growth_value, by = c("year","cbsa")) %>%
  left_join(inclusion_value, by = c("year","cbsa")) 

MM <- MM_change %>% 
  left_join(MM_value, by = c("CBSA" = "cbsa")) %>%
  rename(rank_inclusion = rank,
         rank_prosperity = rank.x,
         rank_growth = rank.y)

save(MM, file = "V:/Sifan/Birmingham/County Cluster/source/MetroMonitor.Rda")

# EMSI NAICS 6 100 metros -------------------------------------------------
NAICS6_US <- read.csv("V:/Sacramento/R script/source/Trade/shiftshare/NAICS6_US_0616.csv")
NAICS6_master <- NAICS6_US[c("NAICS","Description", "X2016.Jobs", "X2006.Jobs")] %>%
  mutate(SUM_JOBS_2006 = sum(X2006.Jobs, na.rm = TRUE),
         SUM_JOBS_2016 = sum(X2016.Jobs, na.rm = TRUE)) 
library('readxl')

allfiles <- list.files(path = "V:/Sacramento/R script/source/Trade/shiftshare/EMSI", full.names = TRUE)
Jobs2006 <- lapply(allfiles, read_xlsx,sheet = "Industry Breakdown - 2006 Jobs", skip = 2, col_types = "numeric", n_max = 993)
Jobs2016 <- lapply(allfiles, read_xlsx,sheet = "Industry Breakdown - 2016 Jobs", skip = 2, col_types = "numeric", n_max = 993)
LQ2006 <- lapply(allfiles, read_xlsx, sheet = "Location Quotient Breakdown ...", skip = 2, col_types = "numeric", n_max = 993)
LQ2016 <- lapply(allfiles, read_xlsx, sheet = "Location Quotient Breakd... (2)", skip = 2, col_types = "numeric", n_max = 993)
ComEffect <- lapply(allfiles, read_xlsx, sheet = 'Shift Share Breakdown - Comp...', skip = 2, col_types = "numeric", n_max = 993)

df_transform <-function(df){bind_rows(lapply(df,function(x) x %>% select(-Description) %>% gather(MetroNames,value,names(x)[-c(1,2)])))}


EMSI <- plyr::join_all(lapply(list(Jobs2006, Jobs2016, LQ2006, LQ2016, ComEffect), 
                              df_transform),  
                       by = c("Industry", "MetroNames")) 

names(EMSI) <- c("NAICS6", "cbsa_name", "Jobs2006", "Jobs2016", "LQ2006", "LQ2016", "ComEffect")

EMSI_master <- EMSI %>% 
  unique() %>%
  filter(!is.na(NAICS6))%>%
  group_by(cbsa_name) %>%
  mutate(sum_jobs_2006 = sum(Jobs2006, na.rm = TRUE),
         sum_jobs_2016 = sum(Jobs2016, na.rm = TRUE))%>%
  left_join(NAICS6_master, by = c("NAICS6" = "NAICS"))

save(EMSI_master, file = "EMSI_master.Rda")

# NSF

# CoG ==========================================================
# "https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk"