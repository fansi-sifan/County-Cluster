# USPTO patent database ====================================================================

pkgs <- c('tidyverse','patentsview')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

View(fieldsdf)

# Bham peers -------------------------------

allpeers <- msa_ct_FIPS %>% group_by(fipsstatecode)%>%
  summarise(county = list(as.character(fipscountycode)))%>%
  mutate(state = padz(fipsstatecode,2))

# fields -------------------------------
fields <- c(
  "patent_number", "assignee_organization", "patent_year","assignee_county_fips",
  "assignee_state_fips","assignee_total_num_patents", "wipo_field_title","wipo_sector_title",
  # "cpc_subsection_title","cpc_group_title", "cpc_subgroup_title",
  "nber_subcategory_title")

# Get query result

query <- with_qfuns(
  and(eq(assignee_county_fips = eval(allpeers$county[[i]])),
      eq(assignee_state_fips = eval(allpeers$state[[i]])),
      gt(patent_date = "2008-01-01"))
)

query <- with_qfuns(
  and(or(and(eq(assignee_county_fips = eval(allpeers$county[[1]])),
             eq(assignee_state_fips = eval(allpeers$state[[1]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[2]])),
             eq(assignee_state_fips = eval(allpeers$state[[2]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[3]])),
             eq(assignee_state_fips = eval(allpeers$state[[3]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[4]])),
             eq(assignee_state_fips = eval(allpeers$state[[4]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[5]])),
             eq(assignee_state_fips = eval(allpeers$state[[5]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[6]])),
             eq(assignee_state_fips = eval(allpeers$state[[6]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[7]])),
             eq(assignee_state_fips = eval(allpeers$state[[7]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[8]])),
             eq(assignee_state_fips = eval(allpeers$state[[8]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[9]])),
             eq(assignee_state_fips = eval(allpeers$state[[9]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[10]])),
             eq(assignee_state_fips = eval(allpeers$state[[10]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[11]])),
             eq(assignee_state_fips = eval(allpeers$state[[11]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[12]])),
             eq(assignee_state_fips = eval(allpeers$state[[12]])))),
      gt(patent_date = "2008-01-01"))
)

query_2 <- with_qfuns(
  and(or(and(eq(assignee_county_fips = eval(allpeers$county[[13]])),
             eq(assignee_state_fips = eval(allpeers$state[[13]]))),
         and(eq(assignee_county_fips = eval(allpeers$county[[14]])),
             eq(assignee_state_fips = eval(allpeers$state[[14]])))),
      gt(patent_date = "2008-01-01"))
)

output_2 <- search_pv(
  query = query_2,endpoint = "patents",
  fields = fields, all_pages = TRUE)

t <- output$data$patents%>%unnest(wipos)
s <- output$data$patents%>%unnest(assignees)

all_2 <- full_join(output_2$data$patents%>%unnest(wipos), output_2$data$patents%>%unnest(assignees), by = "patent_number")
all <- full_join(t,s,by = "patent_number")

Peermetro_patents <- bind_rows(all,all_2)
save(Peermetro_patents,file = "patents.Rda")

# analysis
load("patents.Rda")
year1 <- seq(2013,2017)
year2 <- seq(2008,2012)

Peermetro_patents <- Peermetro_patents %>%
  mutate(fips = paste0(padz(assignee_state_fips,2),padz(assignee_county_fips,3)),
         patent_year = as.numeric(patent_year.x)) %>%
  left_join(msa_ct_FIPS[c("cbsa","fips")], by = "fips")%>%
  mutate(
    year_range = case_when(
      patent_year %in% year2 ~ "2008 - 2012",
      patent_year %in% year1 ~ "2013 - 2017"))%>%
  left_join(Peers[c("cbsa","metro","msapop","msaemp")], by = "cbsa")

# by year_range
patents_count <- Peermetro_patents %>%
  filter(!is.na(year_range))%>%
  group_by(cbsa,msaemp,year_range,metro)%>%
  summarise(count = n_distinct(patent_number))%>%
  mutate(value = count/msaemp*1000)

ggplot(patents_count,aes(x=reorder(metro,value), y=value,fill=year_range))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("#0070c0", "#ffc000"))+
  coord_flip()+
  labs(title = "Total number of USPTO patents per 1000 workers by year range",y=NULL, x=NULL)+
  pthemes

patents_cluster <- Peermetro_patents %>%
  filter(!is.na(wipo_sector_title))%>%
  group_by(cbsa, metro, wipo_field_title, wipo_sector_title)%>%
  summarise(count = n_distinct(patent_number))%>%
  group_by(cbsa)%>%
  arrange(desc(count))%>%
  top_n(5)

patents_cluster%>%group_by(cbsa)%>%mutate(rank=rank(-count,ties.method = "first"))%>%
  select(-count,-wipo_sector_title)%>%
  spread(rank, wipo_field_title)

patents_summary <- left_join(
  patents_count%>%
    select(-msaemp, -count)%>%
    spread(year_range, value),
  patents_cluster%>%group_by(cbsa)%>%mutate(rank=rank(-count,ties.method = "first"))%>%
    select(-count,-wipo_sector_title)%>%
    spread(rank, wipo_field_title),
  by = "cbsa")

write.csv(patents_summary,"result/patents_summary.csv")
