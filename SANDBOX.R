
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


pkgs <- c('tidyverse','patentsview')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 


query <- with_qfuns(
  and(eq(inventor_lastknown_city = "Birmingham"),
      eq(inventor_lastknown_state = "AL"))
)

fields <- c(
  "patent_number", "assignee_organization",
  "patent_num_cited_by_us_patents", "app_date", "patent_date",
  "assignee_total_num_patents"
)

output <- search_pv(
  query = '{"_and":[{"inventor_lastknown_city":"Birmingham"},{"inventor_lastknown_state":"AL"}]}',
  fields = fields)

unnest_pv_data(output$data, "patent_number")


             