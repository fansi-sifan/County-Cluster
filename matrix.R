# Author: Sifan Liu
# Date: Thu Feb 21 14:17:15 2019
# --------------
pkgs <- c('tidyverse',"SifanLiu")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

# load loan data files
load("Temp data/SBA_loan_cleaned.Rda")

# SBIR_MSA <- loan_datafiles$SSTR_matched%>%
#   filter(Award.Year>=2011 & Award.Year<2017)%>%
#   group_by(county14)%>%
#    summarise(amt.tot = sum(Award.Amount, na.rm = T),
#             count = n())%>%
#   mutate(FIPS = padz(county14,5))%>%
#   left_join(msa_ct_FIPS[c("cbsa","FIPS")],by = "FIPS")%>%
#   group_by(cbsa)%>%
#   summarise(amt.tot = sum(amt.tot),
#             count = sum(count))
cbsa_sbir <- t

cbsa_VC <- read.csv('source/VC.csv') %>%
  filter(round == "Total VC" & measure == "Capital Invested ($ M) per 1M Residents") %>%
  mutate(cbsa_code = as.character(cbsa13))

cbsa_rd <- read.csv("source/NSF_univ.csv")%>%
  mutate(cbsa_code = str_pad(cbsacode,5,"left","0"))%>%
  group_by(cbsa_code)%>%
  summarise(RD_amt = sum(`Deflated.Total.R.D.Expenditures.in.All.Fields.Sum.`, na.rm = T))

temp <- plyr::join_all(list(cbsa_rd,cbsa_sbir,cbsa_VC),
                       by = "cbsa_code",
                       type = "inner")

# relationship between patents and startup activities ------------------------------
temp <- plyr::join_all(list(
  # read.csv('source/USPTO_msa.csv') %>%
  #   filter(GeoType == "Metropolitan Statistical Area")%>%
  #   select(ID.Code, Total)%>%
  read.csv('source/USPTO_msa.csv') %>%
    filter(GeoType == "Metropolitan Statistical Area")%>%
    select(ID.Code, Total)%>%
    select(cbsa_code,complex),
  read.csv('source/young_firms.csv')%>%
    spread(indicator, value)%>%
    mutate(young_share = `Employment at firms 0-5 years old`/Jobs)%>%
    select(cbsa_code = cbsa,young_share),
  UnivRD_MSA,
  read.csv('source/VC.csv') %>%
    filter(round == "Total VC" & measure == "Capital Invested ($ M) per 1M Residents") %>%
    mutate(cbsa_code = as.character(cbsa13)),
  
  SBIR_MSA,
  # read.csv("source/I5HGC_density.csv")%>%
  #   mutate(cbsa_code = as.character(CBSA)),
    
  by = "cbsa_code",
  type = "inner"
))
# education attainment
library(censusapi)
edu <- c("S1501_C02_012E", "S1501_C02_009E", "S1501_C02_010E","S1501_C02_011E")
t_edu <- GetACS("acs/acs5/subject",edu, 'msa', vintage = 2016)

t_edu <- t_edu%>%
  mutate(college_share = S1501_C02_012E+S1501_C02_012E,
         HSabove_share = college_share + S1501_C02_009E+S1501_C02_010E,
         cbsa = metropolitan_statistical_area_micropolitan_statistical_area)%>%
  select(-contains("S1501"),-contains("metro"))

# combine
cbsa_innovation <- temp %>% 
  left_join(county_cbsa_st[c("cbsa_code","cbsa_pop")]%>%unique(), by = "cbsa_code") %>%
  mutate(SBIR_pc = cbsa_amt/cbsa_pop,
         # patent_pc = Total/cbsa_pop,
         UnivRD_pc = RD_amt/cbsa_pop,
         SBIR_VC = SBIR_pc/value) %>%
  select(cbsa_code, metro_name = MSA,
         # young_firm_share = young_share, 
         cbsa_pop,
         # patent_complex = complex,
         # patent_pc,
         SBIR_pc,cbsa_count,cbsa_amt,
         UnivRD_pc,
         VC_pc = value,
         # Inc_pc = I5HGC_Density,
         SBIR_VC) 
# %>%
#   # inner_join(MM %>% mutate(cbsa = as.character(`CBSA`)), by = "cbsa") %>%
#   left_join(t_edu, by = "cbsa")

# write.csv(t, "result/patent_startup.csv")

# plot ============================================
t_alt <- t %>% filter(patent_pc<0.02)

# plot one
fit <- lm(BA ~ `Output per Person`, data = t_alt)

ggplot(t_alt, aes(x = BA, y = `Output per Person`))+
  geom_point(aes(color = cbsa %in%HL),size = 4)+
  geom_text(x = 40, y = 40, label = paste0("R-square = ", Rsquare))+
  ggrepel::geom_label_repel(data= t_alt %>% filter(cbsa %in% HL), aes(label=metro_name), nudge_y = 2)+
  # scale_x_continuous(limits = c(0,20))+
  scale_color_manual(values = c("#0070c0", "#ffc000"), guide = F)+
  # stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, formula = ) +
  geom_smooth(method="lm",se=FALSE, fill = "#000fff", linetype = 3)+
  labs(x="Share of adults 25 or older with Bachelor's degree or higher")+
  theme(panel.background = element_rect(fill = "#D9D9D9",colour = NA),
        plot.background = element_rect(fill = "#D9D9D9", colour = NA),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        axis.line.x = element_line(color="grey", size = 1),
        axis.line.y = element_line(color="grey", size = 1))
# plot two

fit <- lm(patent_pc ~ `Output per Person`, data = t_alt)
Rsquare <- format(summary(fit)$r.squared, digits = 2)

HL <- c("28140", "41740", "34980", "33460", "17460", "13820", "16980", "31080", "45060", "19820", "19740", "26900")
  

# library(devtools)
# devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")

ggplot(t_alt, aes(x = patent_pc*1000, y = `Output per Person`))+
  geom_point(aes(color = cbsa %in%HL),size = 4)+
  geom_text(x = 15, y = 40, label = paste0("R-square = ", Rsquare))+
  ggrepel::geom_label_repel(data= t_alt %>% filter(cbsa %in% HL), aes(label=metro_name), nudge_y = 2)+
  scale_x_continuous(limits = c(0,20))+
  scale_color_manual(values = c("#0070c0", "#ffc000"), guide = F)+
  # stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, formula = ) +
  geom_smooth(method="lm",se=FALSE, fill = "#000fff", linetype = 3)+
  labs(x="Number of patents per 1000 residents")+
  theme(panel.background = element_rect(fill = "#D9D9D9",colour = NA),
        plot.background = element_rect(fill = "#D9D9D9", colour = NA),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        axis.line.x = element_line(color="grey", size = 1),
        axis.line.y = element_line(color="grey", size = 1))

  
  # correlation matrix -------------------
# install.packages('corrplot')
library(corrplot)

M <- cor(t[3:11],use = "pairwise.complete.obs")
head(round(M,2))

corrplot(M, method = "color", type ="upper",
         addCoef.col = "black", tl.col = "black",tl.srt=45)

# fit

fit <- lm(young_firm_share ~ pop2016+patent_complex+patent_pc+SBIR_pc+UnivRD_pc+VC_pc+Inc_pc+college_share, data = t)

summary(fit)

