# Author: Sifan Liu
# Date: Fri Dec 28 13:40:50 2018
# --------------
pkgs <- c("SifanLiu","httr", "tidyverse", "devtools",'tidycensus')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  } 

# GEOCODING with street address ==================================

tigris::call_geolocator("5300 COURT Q", "Birmingham", "AL")

# PUMA ===========================================================
install.packages('ipumsr')
library(ipumsr)
ddi <- read_ipums_ddi("usa_00012.xml")
data <- read_ipums_micro(ddi)

# tidycensus =================================================
# group, Employment rates, labor force participation rates by sex, race, education
emp_school <- sapply(seq(1,7),function(x)paste0("B14005_00",x))
emp_edu <- sapply(seq(1,29),function(x)paste0("B23006_",pad(x,3)))
emp_sex_age_race <- c(sapply(seq(1,27),function(x)paste0("C23002A_",padz(x,3))),
                      sapply(seq(1,27),function(x)paste0("C23002B_",padz(x,3))))
var <- load_variables(2017, "acs5")

# MSA
t <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
        variables = emp_edu,
        year = 2017)%>%
  filter(GEOID%in%Peers$cbsa)

# county
t <- get_acs(geography = "county",variables = emp_sex_age_race,year = 2017)%>%
  filter(GEOID%in%Peers$FIPS)

# Bham
t <- get_acs(geography = "place", variables = emp_sex_age_race, year = 2017,
        state = "AL", place = "Birmingham") %>% filter(grepl("Birmingham", NAME))

temp <- t %>% 
  left_join(var, by = c("variable"="name"))

write.csv(temp, "Bham_emp_sex_age_race.csv")
# opportunity index HUD =============================================
# source: https://www.cbpp.org/research/housing/interactive-map-where-voucher-households-live-in-the-50-largest-metropolitan-areas

# read json data---------------------------------------------------
URL <- "https://apps.cbpp.org/4-16-18hous/data/13820.json" 
raw <- jsonlite::fromJSON(URL, simplifyVector = TRUE) 
# clean---------------------------------------------------
t <- raw %>%
  bind_rows()%>%
  t()%>%
  as_tibble()%>%
  mutate(FIPS = names(raw)) %>%
  rename(CBSA = V1, pov_rate = V2, pov_bkt = V3, opp_index = V5, color_share = V6)

t%>%filter(FIPS=="1073010602")


# modify censusr package for QWI queries ===========================================
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


apiurl <- "https://api.census.gov/data/timeseries/qwi/sa"
key = Sys.getenv("CENSUS_KEY")


req <- function(key, get, region, regionin, time)httr::GET(apiurl,
                                                           query = list(key = key, get = get, 
                                                                        "for" = region,"in" = regionin, 
                                                                        time=time,
                                                                        ownercode = "A05",seasonadj="U",
                                                                        # industry="23",industry ="31-33",
                                                                        firmage=1,firmage=2,firmage=3,firmage=4,firmage=5))


# USPTO patent database ====================================================================

pkgs <- c('tidyverse','patentsview')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

View(fieldsdf)

# UAB -------------------------------
# query <- with_qfuns(
#   and(eq(assignee_id = "29e18557907c764249b4a340158fe219"),
#       gt(patent_date = "2016-01-01"))
# )

# Bham MSA 7 counties -------------------------------
query <- with_qfuns(
  and(eq(assignee_county_fips = c("73","7","9","21","115","117","112")),
      eq(assignee_state_fips = "01"),
      gt(patent_date = "2006-01-01"))
)

# fields -------------------------------
fields <- c(
  "patent_number", "assignee_organization", "patent_year","assignee_county_fips",
  "assignee_total_num_patents", "wipo_field_title","wipo_sector_title",
  "cpc_subsection_title","cpc_group_title", "cpc_subgroup_title",
  "nber_subcategory_title")

#test
# search_pv(
#   query = query,endpoint = "patents",
#   fields = fields)

output <- search_pv(
  query = query,endpoint = "patents",
  fields = fields, all_pages = TRUE)

summary(as.factor(output$data$patents$patent_year))

# results -------------------------------
# top patent category
output$data$patents%>%unnest(wipos)%>%
  group_by(wipo_sector_title, wipo_field_title)%>%
  summarise(count = n())%>%
  arrange(desc(count))

output$data$patents%>%unnest(cpcs)%>%
  group_by(cpc_subgroup_title)%>%
  summarise(count = n())%>%
  arrange(desc(count))

output$data$patents%>%unnest(nbers)%>%
  group_by(nber_subcategory_title)%>%
  summarise(count = n())%>%
  arrange(desc(count))

# output$data$patents%>%unnest(uspcs)%>%
#   group_by(uspc_subclass_title)%>%
#   summarise(count = n())%>%
#   arrange(desc(count))

# top patent assignees
s <- output$data$patents%>%unnest(assignees)
s%>%group_by(assignee_organization, assignee_total_num_patents)%>%summarise(count = n())%>%arrange(desc(count))

t <- output$data$patents%>%unnest(wipos)
pat_top <- t%>%left_join(s, by = "patent_number")%>%
  group_by(wipo_field_title,assignee_organization)%>%
  summarise(count = n())%>% 
  arrange(desc(count))


# Federal Reporter API ======================
library(devtools)
install_github("muschellij2/fedreporter")
library(fedreporter)
res = fedreporter::fe_projects_search(
  text = "precision medicine")

length(res$content$items)

install.packages("tidytext")
library(tidyverse)

term <- read.csv("source/UAB_funding terms.csv") %>% unique()

data("stop_words")


term %>% group_by(Project.Title)%>%
  summarise(cost = sum(FY.Total.Cost, na.rm = TRUE),
            terms = first(Project.Terms))%>%
  mutate(text = as.character(terms))%>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  separate(bigram, c("word1", "word2", sep = " "))%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(word1!="research")%>%
  filter(word2 != "research")%>%
  filter(!word2 %in% stop_words$word) %>%
  unite(word, word1, word2, sep = " ") %>%
  count(word, sort = TRUE)%>%
  filter(n>200)%>%
  mutate(word = reorder(word, n))%>%
  ggplot(aes(word,n))+
  geom_col()+
  labs(x = NULL,title = "Frequency of key terms in all federal fundings to UAB")+
  coord_flip()
  

