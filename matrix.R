# Author: Sifan Liu
# Date: Thu Feb 21 14:17:15 2019
# --------------
pkgs <- c('tidyverse')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

# relationship between patents and startup activities ------------------------------
temp <- plyr::join_all(list(
  read.csv('source/USPTO_msa.csv') %>%
    filter(GeoType == "Metropolitan Statistical Area")%>%
    select(ID.Code, Total)%>%
    mutate(cbsa = substr(as.character(ID.Code),2,6)),
  read.csv('source/Complexity_msa.csv') %>%
    mutate(cbsa = as.character(cbsa)),
  read.csv('source/VC.csv') %>%
    filter(round == "Total VC" & measure == "Capital Invested ($ M) per 1M Residents") %>%
    mutate(cbsa = as.character(cbsa13)),
  read.csv("source/I5HGC_density.csv")%>%
    mutate(cbsa = as.character(CBSA))),
  by = "cbsa",
  type = "inner"
)

# education attainment
edu <- c("S1501_C02_012E", "S1501_C02_015E")
t_edu <- GetACS("acs/acs5/subject",edu, 'msa', vintage = 2016)
names(t_edu)<-c("cbsa","BA_25", "BA")

#combine
t <- temp %>% 
  left_join(read.csv("../../R/xwalk/msa.pop.csv")%>%
                     mutate(cbsa = as.character(cbsa)), by = "cbsa") %>%
  mutate(patent_pc = Total/pop2016) %>%
  select(cbsa, metro_name = MSA, pop2016,
         patent_complex = complex,
         patent_pc,
         VC_pc = value,
         Inc_pc = I5HGC_Density) %>%
  inner_join(MM %>% mutate(cbsa = as.character(`CBSA`)), by = "cbsa") %>%
  left_join(t_edu, by = "cbsa")

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
# cormat <- round(cor(t[4:7]),2)
# melted_cormat <- reshape2::melt(cormat)
# ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()