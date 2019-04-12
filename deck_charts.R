# Author: Sifan Liu
# Date: Thu Dec 27 10:47:45 2018
# --------------
pkgs <- c("tidyverse", "ggubr")

check <- sapply(pkgs, require, warn.conflicts = TRUE, character.only = TRUE)
if (any(!check)) {
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing, require, warn.conflicts = TRUE, character.only = TRUE)
}

source("Do.R")

# Peer map
p_peermap <- ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "white", color = "grey") +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  geom_point(data = map_data, aes(x = long, y = lat, fill = (CBSAFP == msa_FIPS)), size = 6, shape = 21) +
  scale_fill_manual(values = c("#0070c0", "#ffc000"), guide = FALSE) +
  pthemes %+%
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())


# shift share
MSA_shiftshare$industry <- MSA_shiftshare$Sub.Category

# lolipop chart
p_shiftshare <- bbplot(data = MSA_shiftshare) +
  geom_segment(aes(
    x = reorder(industry, Jobs_actual), xend = industry,
    y = Jobs_actual, yend = Jobs_expected, color = `Competitive Effect` > 0
  ), size = 2) +
  scale_color_manual(labels = c("Negative shifts", "Positive shifts"), name = NULL, values = c("#003249", "#ffc000")) +
  geom_point(aes(x = industry, y = Jobs_actual), color = "#0070c0", size = 3) +
  geom_point(aes(x = industry, y = Jobs_expected), color = "#ffd966", size = 3) +
  coord_flip() +
  labs(
    title = "Actual and expected number of jobs in 2016",
    x = "Number of Jobs",
    y = "Industry"
  ) +
  facet_wrap(~place)

# bar chart
bbplot(data = MSA_shiftshare %>% filter(place == "Birmingham.Hoover..AL")) +
  geom_bar(aes(
    x = reorder(industry, `Competitive Effect`),
    y = `Competitive Effect`, fill =
    ),
  stat = "identity"
  ) +
  scale_fill_manual(
    labels = c("Slower than nation", "Faster than nation"),
    name = NULL, values = c("#0070c0", "#ffc000")
  ) +
  scale_x_discrete(labels = c(
    "Finance and Insurance" = "Finance",
    "Transportation and Warehousing" = "Logistics",
    "Utilities" = "Utilities",
    "Real Estate and Rental and Leasing" = "Real Estate, etc.",
    "Arts, Entertainment, and Recreation" = "Arts",
    "Educational Services" = "Education",
    "Agriculture, Forestry, Fishing and Hunting" = "Agriculture",
    "Other Services (except Public Administration)" = "Local Services",
    "Mining, Quarrying, and Oil and Gas Extraction" = "Mining",
    "Government" = "Government",
    "Manufacturing" = "Manufacturing",
    "Management of Companies and Enterprises" = "Headerquaters",
    "Accommodation and Food Services" = "Hospitality",
    "Wholesale Trade" = "Wholesale",
    "Information" = "Information",
    "Waste Management and Remediation Services" = "Administrative",
    "Construction" = "Construction",
    "Retail Trade" = "Retail",
    "Professional, Scientific, and Technical Services" = "Professional",
    "Health Care and Social Assistance" = "Healthcare"
  )) +
  labs(
    title = "Employment differential between Birmingham MSA and nation, 2006 - 2016",
    y = "Number of Jobs",
    x = "Industry"
  ) +
  coord_flip() +
  theme(axis.ticks = element_blank())

# 2*2
Wageline <- MSA_MM$`Average Annual Wage` * 1000

p_2by2 <- bbplot() +
  geom_hline(yintercept = Wageline, color = "#0070c0", size = 1) +
  geom_vline(xintercept = 0, color = "#0070c0", size = 1) +
  geom_point(data = ss, aes(Employment_lsshare2006, Wage_2016,
    size = Employment_value, color = as.factor(traded_2)
  ), alpha = 0.4) +
  geom_text_repel(data = ss, aes(Employment_lsshare2006, Wage_2016,
    label = industryname_naics2
  ), color = "#636363") +
  xlab(paste0("Industry employment growth rate differential between ", metroname, " and nation, 2006-2016")) +
  # annotate("label", x=-0.22, y = Wageline-100,
  #          label = paste0(scales::dollar(Wageline), ", Avg. annual wage"), color = "#0070c0")+
  # annotate("label", x=-0.22, y = 90000, label = "Higher wage\nslower growth than U.S.")+
  # annotate("label", x=-0.22, y = 20000, label = "Lower wage\nslower growth than U.S.")+
  # annotate("label", x=0.1, y = 90000, label = "Higher wage\nfaster growth than U.S.")+
  # annotate("label", x=0.1, y = 20000, label = "Lower wage\nfaster growth than U.S.")+
  ylab("Average wage, 2016") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(
    range = c(1, 15),
    labels = scales::comma, name = "Total employment, 2016"
  ) +
  scale_color_manual(labels = c("Non-tradable", "Tradable"), name = NULL, values = c("#0070c0", "#ffc000")) +
  ggtitle("Average wage and employment shift share by industry, 2006 - 2016")

p_2by2


# export emp share
# slop chart
p_expemp <- bbplot(data = Peer_traded, aes(x = factor(Year), y = EmpShare, color = c(Area == msa_FIPS | Area == "1073"), group = Area)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  # geom_text(data = Peer_traded%>%filter(Year == 2016),
  #           aes(label = Area.Name,y = EmpShare),
  #           color = "black", check_overlap = TRUE, nudge_x = 0.5)+
  scale_color_manual(values = c("#0070c0", "#ffc000"), guide = FALSE) +
  labs(
    title = "Change in employment share of traded industries, 2006 - 2016",
    x = element_blank(), y = "Share of employment"
  ) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Area.Bucket)

# lolipop chart
bbplot(data = Peer_traded %>%
  filter(!is.na(metro)) %>%
  select(-Emp, -Emptot) %>%
  spread(Year, EmpShare)) +
  geom_segment(aes(
    x = reorder(metro, `2016`), xend = metro,
    y = `2006`, yend = `2016`
  )) +
  geom_point(aes(x = metro, y = `2006`), color = "#0070c0", size = 3) +
  geom_point(aes(x = metro, y = `2016`), color = "#ffd966", size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Change in employment share of traded industries, 2006 - 2016",
    y = "Share of employment",
    x = NULL
  ) +
  facet_wrap(~Area.Bucket, scales = "free_y")

# export share
p_expshare <- bar_plot(export, "Export share of GDP, 2017", HL) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~geo, scales = "free_y")

# export industries
bbplot(export_ind, aes(
  x = reorder(industry, export_value), y = export_value,
  label = scales::comma_format(accuracy = 0.1, suffix = "M")(export_value)
)) +
  geom_bar(stat = "identity", fill = "#0070c0") +
  geom_text(nudge_y = 80) +
  labs(
    title = "Largest tradable industries (4 digit NAICS) in the region",
    x = NULL, y = "Export volume, 2017"
  ) +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~geo, scales = "free_y") +
  coord_flip()


# opp
# reorder factor level for charts
MSA_opp$type <- factor(MSA_opp$type, levels = c(
  "Other jobs", "Good high-skill jobs", "Good sub-BA jobs",
  "Promising high-skill jobs", "Promising sub-BA jobs"
))

p_opp <- bbplot(MSA_opp, aes(x = factor(Traded), y = share, fill = type)) +
  scale_x_discrete(labels = c("Non-traded", "Traded"), name = element_blank()) +
  scale_fill_manual(values = c("#797171", "#ffd966", "#ffc000", "#0070c0", "#003249")) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Share of total employment", title = "Share of opportunity jobs in Birmingham MSA, 2016") +
  geom_bar(stat = "identity")

# EXIM
p_EXIM <- p_SME(SMEloans %>% mutate(value = value * emp.tot / emp.traded), "EXIM") +
  ggtitle("Average annual loan volume from Export-Import Bank\nper 1000 traded industries workers") +
  scale_y_continuous(labels = scales::dollar_format())

# university R&D
p_RDuni <- bar_plot(NSF, "Average annual R&D expenditure at higher education institutions\nper 1000 workers, 2012 - 2016", HL) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1000, suffix = "K")) +
  # labs(x = "($ per capita)",y = element_blank())+
  facet_wrap(~geo, scales = "free_y") +
  theme(strip.text = element_blank())

# UAB funding
myorder <- c("All other fields", "Engineering", "Life sciences, nec", "Biological and biomedical sciences", "Health sciences")
UAB_RD$fields <- factor(UAB_RD$fields, levels = myorder)
UAB_RD$value_adj <- scales::dollar(UAB_RD$value, scale = 0.001, accuracy = 0.1, suffix = "M")

p_UAB <- bbplot(UAB_RD, aes(x = "", y = value / 1000, fill = fields, label = fields)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = NULL, y = NULL, title = paste0("Average annual R&D expenditure (million) at UAB, 2009 - 2016")) +
  # scale_x_discrete(limits = myorder)+
  scale_fill_manual(
    limits = myorder,
    values = c(
      "Biological and biomedical sciences" = "#2166ac",
      "Health sciences" = "#67a9cf",
      "Life sciences, nec" = "#d1e5f0",
      "Engineering" = "#fddbc7",
      "All other fields" = "#ef8a62"
    ),
    guide = FALSE
  ) +
  coord_polar("y")

# require ggpar package
p_UAB_donut <- ggpar(ggdonutchart(UAB_RD, "value",
  label = "value_adj",
  fill = "fields", color = "#D9D9D9"
),
palette = c("#797171", "#ffd966", "#d1e5f0", "#0070c0", "#003249"),
legend = "right", ticks = FALSE,
title = "Average annual R&D expenditure at UAB, 2007 - 2016"
) +
  theme(legend.title = element_blank())

# AUTM

p_AUTM <- bar_plot(AUTM %>% filter(!is.na(value)), "Average annual licensing income per 1000 workers, 2006 - 2017", HL) +
  scale_y_continuous(labels = scales::dollar) +
  facet_wrap(~geo, scales = "free_y") +
  theme(strip.text = element_blank())

p_uni_startup <- bbplot(
  uni_startup %>% filter(!is.na(tot_st)),
  aes(x = reorder(metro, value), y = value, fill = HL)
) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    name = NULL, guide = FALSE,
    values = c("#0070c0", "#ffc000")
  ) +
  # scale_alpha_manual(name = NULL,
  #                    values = c(0.5,1),
  #                    label = c("Out of state","In state"))+
  coord_flip() +
  labs(title = "Number of start-ups launched at universities, 2006 - 2017", x = NULL, y = NULL) +
  facet_wrap(~geo, scales = "free_y") +
  theme(strip.text = element_blank())

p_startup <- bar_plot(startup, "Number of start-ups per 1M workers, by MSA") +
  scale_y_continuous(labels = scales::number_format(scale = 1000))

# USPTO
p_USPTO <- bar_plot(patent, "Total number of USPTO patents per 1000 workers, 2000 - 2015", HL) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~geo, scales = "free_y") +
  theme(strip.text = element_blank())

# patent complexity
p_pci <- bar_plot(patentCOMP, "Metro knowledge complexity index, 2000 - 2004", HL)

# Venture capital
p_VC <- bar_plot(VC, "VC investment per 1000 workers, 2015 - 2017", HL) +
  scale_y_continuous(labels = scales::dollar)

# advanced industries
# slope chart
bbplot(
  data = Peer_ai %>% filter(EmpShare < 0.2),
  aes(
    x = factor(Year), y = EmpShare, color = c(Area == msa_FIPS | Area == "1073"),
    group = Area, label = Area.Name
  )
) +
  geom_text() +
  geom_point(size = 5) +
  geom_line(size = 2) +
  scale_color_manual(values = c("#0070c0", "#ffc000"), guide = FALSE) +
  # geom_text(nudge_x = 0.5)+
  labs(
    x = NULL, y = "Employment Share",
    title = "Change in employment share of advanced industries, 2006 - 2016"
  ) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Area.Bucket)

# lollipop chart
bbplot(data = Peer_ai %>%
  filter(!is.na(metro)) %>%
  select(-Emp, -Emptot) %>%
  spread(Year, EmpShare)) +
  geom_segment(aes(
    x = reorder(metro, `2016`), xend = metro,
    y = `2006`, yend = `2016`
  )) +
  geom_point(aes(x = metro, y = `2006`), color = "#0070c0", size = 3) +
  geom_point(aes(x = metro, y = `2016`), color = "#ffd966", size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Change in employment share of traded industries, 2006 - 2016",
    y = "Share of employment",
    x = NULL
  ) +
  facet_wrap(~Area.Bucket, scales = "free_y")

# SBIR
p_SBIR <- p_SME(SMEloans, "SSTR") +
  ggtitle("Average annual SBIR/SSTR grants per 1000 workers") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(legend.position = "bottom")

# BDS
p_BDS <- bbplot(
  data = fsfa %>% group_by(year2, Age) %>% summarise(JbC = sum(net_job_creation)),
  aes(x = factor(year2), y = JbC, fill = Age)
) +
  scale_y_continuous(labels = scales::comma, name = "Net job creation") +
  scale_fill_manual(values = c("#0070c0", "#ffc000", "#ffd966")) +
  labs(x = NULL, title = "Net job creation by firm age, Birmingham MSA") +
  geom_bar(stat = "identity")

# young firm
p_young <- bbplot(
  data = young,
  aes(x = factor(year2), y = emp.share, fill = Size)
) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent, name = "Share of employment") +
  geom_text(
    data = young %>% filter(Size == "Large ( 500 employees and above)"),
    aes(label = scales::percent(emp.stot), y = emp.stot),
    position = position_stack(vjust = 1.1)
  ) +
  scale_fill_manual(values = c("#0070c0", "#ffc000", "#ffd966")) +
  labs(x = NULL, title = "Share of employment at young firms (0  ~ 10 years) by firm size in Birmingham MSA")

# inc 5000
p_inc <- bar_plot(I5HGC, "Number of Inc. 5000 high growth companies per 1M workers, 2011 - 2017", HL) +
  scale_y_continuous(labels = scales::number_format(scale = 1000))

# FDIC
p_FDIC <- p_SME(SMEloans, "FDIC") +
  ggtitle("Average annual loan volume from FDIC-insured banks per 1000 workers") +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.000001, suffix = "M"))

p_FDICmap <- bbplot() +
  geom_sf(data = FDIC_Bham_alt, aes(fill = level)) +
  geom_sf(data = map.Bham, fill = NA, color = "#ffd966", size = 1) +
  scale_fill_manual(
    values = c("#bdd7e7", "#6baed6", "#3182bd", "#08519c"),
    labels = c("0 - 1", "1 - 5", "5 - 10", "> 10"),
    name = "amount"
  ) +
  ggtitle("Annual average FDIC loans (million) per 1000 workers, 1996 - 2017") +
  coord_sf(datum = NA) +
  theme(axis.title = element_blank(), axis.text = element_blank(), legend.position = "bottom")

# [depreciated] ------
# p_FDIC_r <- ggplot(FDIC_race %>% filter(var == "per"),
#                    aes(x = race, y = value, fill = race, label = scales::dollar(value)))+
#   geom_bar(stat="identity")+
#   geom_text()+
#   labs(title = "Annual average per employee FDIC loans, weighted by tract level racial composition",
#        x = element_blank(),y="Per employee amount")+
#   scale_fill_manual(values = c("#0070c0", "#ffc000"),
#                     label = c("minority", "white"))+pthemes

p_FDIC_r <- bbplot(
  FDIC_Bham_tract,
  aes(
    x = is.white, y = value, fill = is.white,
    label = scales::dollar_format(accuracy = 0.1, suffix = "M")(value)
  )
) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 1.05)) +
  ggtitle("Average annual loan volume from FDIC-insured banks\nby census tract type per 1000 workers , 1996 - 2017") +
  scale_fill_manual(
    name = NULL,
    values = c("#0070c0", "#ffc000"),
    label = c(
      "share of white workers < 50%",
      "share of white workers >= 50%"
    )
  ) +
  labs(x = NULL, y = NULL) +
  pthemes %+%
  theme(axis.text = element_blank())

# SBA
p_SBA <- p_SME(SMEloans, "SBA") +
  ggtitle("Average annual loan volume from SBA per 1000 workers") +
  scale_y_continuous(labels = scales::dollar_format())


# CDFI
p_CDFI <- p_SME(SMEloans, "CDFI") +
  ggtitle("Average annual loan volume from CDFIs per 1000 workers") +
  scale_y_continuous(labels = scales::dollar_format())

# per capita map
p_CDFImap <- bbplot() +
  geom_sf(data = CDFI_Bham, aes(fill = level)) +
  geom_sf(data = map.Bham, fill = NA, color = "#ffd966", size = 1) +
  scale_fill_manual(
    values = c("#bdd7e7", "#6baed6", "#3182bd", "#08519c"),
    labels = c("0 - 10", "10 - 50", "50 - 100", "> 100"),
    name = "amount"
  ) +
  ggtitle("Annual average CDFI loans in Jefferson County by workplace tract, 2006 - 2017") +
  coord_sf(datum = NA) +
  theme(axis.title = element_blank(), axis.text = element_blank(), legend.position = "bottom")

# total amount
bbplot() +
  geom_sf(data = CDFI_Bham, aes(fill = sum / 1000000)) +
  geom_sf(data = map.Bham, color = "#ffd966", size = 1, fill = NA) +
  scale_fill_gradient(name = "Amount (million)", low = "#bdd7e7", high = "#08519c") +
  # scale_fill_manual(values = c("#bdd7e7","#6baed6","#3182bd","#08519c"),
  #                   labels = c("0 - 10", "10 - 50", "50 - 100", "> 100"),
  #                   name = "per capita CDFI amount") +
  # ggtitle("Per employee CDFI lendings in Jefferson County by workplace tract, 2006 - 2017") +
  coord_sf(datum = NA) +
  theme(axis.title = element_blank(), axis.text = element_blank(), legend.position = "bottom")

# p_CDFI_r <- ggplot(TLR_Bham_weight  %>% filter(var == "per"),
#                    aes(x = race, y = value, fill = race, label = scales::dollar(value)))+
#   geom_bar(stat="identity")+
#   geom_text()+
#   ggtitle("Annual average per employee CDFI loans, weighted by tract level racial composition")+
#   scale_fill_manual(values = c("#0070c0", "#ffc000"),label = c("minority", "white"))+
#   pthemes

p_CDFI_r <- bbplot(
  TLR_Bham_tract,
  aes(
    x = is.white, y = value, fill = is.white,
    label = scales::dollar_format(accuracy = 0.1)(value)
  )
) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 1.05)) +
  ggtitle("Average annual loan volume from CDFIs\nby census tract type per 1000 workers, 2006 - 2017") +
  scale_fill_manual(
    name = NULL,
    values = c("#0070c0", "#ffc000"),
    label = c(
      "share of white workers < 50%",
      "share of white workers >= 50%"
    )
  ) +
  # scale_y_continuous(labels = scales::dollar)+
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank())
