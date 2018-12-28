
library("httr", "tidyverse")
# modify censusr package for QWI
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


# USPTO patent database

View(fieldsdf)

pkgs <- c('tidyverse','patentsview')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 


query <- with_qfuns(
  and(eq(assignee_county_fips = "73"),
      eq(assignee_state_fips = "1"),
      gt(patent_date = "2006-01-01"))
)

fields <- c(
  "patent_number", "assignee_organization", "app_date", "patent_date",
  "assignee_total_num_patents", "wipo_field_title","cpc_subsection_title"
)

output <- search_pv(
  query = query,endpoint = "patents",
  fields = fields, all_pages = TRUE)

# top patent category
t <- output$data$patents%>%unnest(wipos)
t%>%group_by(wipo_field_title)%>%summarise(count = n())%>%arrange(desc(count))

t <- output$data$patents%>%unnest(cpcs)
t%>%group_by(cpc_subsection_title)%>%summarise(count = n())%>%arrange(desc(count))

# top patent assignees
t <- output$data$patents%>%unnest(assignees)
t%>%group_by(assignee_organization, assignee_total_num_patents)%>%summarise(count = n())%>%arrange(desc(count))

# UAB categories
query <- with_qfuns(
  and(eq(assignee_id = "29e18557907c764249b4a340158fe219"),
      gt(patent_date = "2016-01-01"))
)

