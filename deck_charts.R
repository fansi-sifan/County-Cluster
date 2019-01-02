# Author: Sifan Liu
# Date: Thu Dec 27 10:47:45 2018
# --------------
pkgs <- c('tidyverse')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

source("Do.R")

# Peer map
p_peermap <- ggplot()+
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "white", color = "grey") +
  coord_map("albers", lat0=39, lat1=45) +
  geom_point(data = map_data, aes(x = long, y = lat, fill = (CBSAFP == msa_FIPS)), size = 6, shape = 21)+
  scale_fill_manual(values = c("#0070c0", "#ffc000"), guide = FALSE)+
  pthemes%+%theme(axis.text = element_blank(),axis.ticks = element_blank(), axis.title = element_blank())


# shift share
MSA_shiftshare$industry <- MSA_shiftshare$Sub.Category

p_shiftshare <- ggplot(data = MSA_shiftshare)+
  geom_segment(aes(x = reorder(industry,Jobs_actual), xend = industry,
                   y = Jobs_actual, yend = Jobs_expected, color = `Competitive Effect`>0),size = 2)+
  scale_color_manual(labels = c("Negative shifts", "Positive shifts"), name = NULL, values = c("#003249", "#ffc000"))+
  geom_point(aes(x=industry,y = Jobs_actual), color = "#0070c0", size = 3)+
  geom_point(aes(x=industry,y = Jobs_expected), color = "#ffd966", size = 3)+
  coord_flip()+
  labs(title = "Actual and expected number of jobs in 2016",
       x = "Number of Jobs",
       y = "Industry")+
   facet_wrap(~place)+
  pthemes

ggplot(data = MSA_shiftshare %>% filter(place == "Birmingham.Hoover..AL"))+
  geom_bar(aes(x=reorder(industry, `Competitive Effect`),
               y = `Competitive Effect`,fill = `Competitive Effect`>0), 
           stat = "identity")+
  scale_fill_manual(labels = c("Slower than nation", "Faster than nation"), 
                    name = NULL, values = c("#0070c0", "#ffc000"))+
  labs(title = "Employment differential between Birmingham MSA and nation, 2006 - 2016",
       y = "Number of Jobs",
       x = "Industry")+
  coord_flip()+
  pthemes%+%theme(axis.ticks = element_blank())

# 2*2
Wageline <- MSA_MM$`Average Annual Wage`*1000

p_2by2 <- ggplot()+
  geom_hline(yintercept = Wageline, color = "#0070c0", size=1) +
  geom_vline(xintercept = 0, color = "#0070c0", size = 1)+
  geom_point(data = ss, aes(Employment_lsshare2006, Wage_2016, 
                            size = Employment_value, color = as.factor(traded_2)), alpha = 0.4) +
  geom_text_repel(data = ss, aes(Employment_lsshare2006, Wage_2016,
                                 label = industryname_naics2),color = "#636363")+
  xlab(paste0("Industry employment growth rate differential between ",metroname," and nation, 2006-2016")) +
  # annotate("label", x=-0.22, y = Wageline-100, 
  #          label = paste0(scales::dollar(Wageline), ", Avg. annual wage"), color = "#0070c0")+
  # annotate("label", x=-0.22, y = 90000, label = "Higher wage\nslower growth than U.S.")+
  # annotate("label", x=-0.22, y = 20000, label = "Lower wage\nslower growth than U.S.")+
  # annotate("label", x=0.1, y = 90000, label = "Higher wage\nfaster growth than U.S.")+
  # annotate("label", x=0.1, y = 20000, label = "Lower wage\nfaster growth than U.S.")+
  ylab("Average wage, 2016")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::comma)+
  scale_size_continuous(range = c(1,15),
                        labels = scales::comma, name = "Total employment, 2016")+
  scale_color_manual(labels = c("Non-tradable", "Tradable"), name = NULL, values = c("#0070c0", "#ffc000"))+
  ggtitle("Average wage and employment shift share by industry, 2006 - 2016")+
  pthemes

# export emp share
p_expemp <- ggplot(data = Peer_traded,aes(x=factor(Year), y = EmpShare, color = c(Area==msa_FIPS | Area == "1073"), group = Area))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  # geom_text(data = Peer_traded%>%filter(Year == 2016), 
  #           aes(label = Area.Name,y = EmpShare), 
  #           color = "black", check_overlap = TRUE, nudge_x = 0.5)+
  scale_color_manual(values = c("#0070c0", "#ffc000"), guide = FALSE)+
  labs(title = "Change in employment share of traded industries, 2006 - 2016",
      x = element_blank(),y = "Share of employment")+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~Area.Bucket)+
  pthemes

# export share
p_expshare <- bar_plot(export, "Export share of GDP, 2017", HL)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~geo, scales = "free_y")+pthemes

# opp
p_opp <- ggplot(MSA_opp, aes(x = factor(Traded), y = share, fill = type))+
  scale_x_discrete(labels = c("Non-traded", "Traded"), name = element_blank())+
  scale_fill_manual(values = c("#797171","#ffd966", "#ffc000", "#0070c0", "#003249"))+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  labs(y="Share of total employment",title = "Share of opportunity jobs in Birmingham MSA, 2016")+
  geom_bar(stat = "identity")+pthemes

# EXIM
p_EXIM <- p_SME(SMEloans, "EXIM")+
  ggtitle("Annual average loans from Export-Import Bank per 1000 workers")+
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001))


# university R&D
p_RDuni <- bar_plot(NSF, "Average annual R&D expenditure (thousand) at higher education institution per 1000 workers, 2012 - 2016", HL)+
  scale_y_continuous(labels = scales::dollar_format(scale = 1000)) +
  # labs(x = "($ per capita)",y = element_blank())+
  facet_wrap(~geo, scales = "free_y")+pthemes

# UAB funding
myorder <- c("All other fields", "Engineering", "Life sciences, nec","Biological and biomedical sciences", "Health sciences")

p_UAB <- ggplot(UAB_RD, aes(x = fields, y = value/1000, fill = fields, label = fields)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::dollar)+
  labs(x = NULL, y = NULL,title = paste0("Average annual R&D expenditure (million) at UAB, FY2012 - 2016"))+
  scale_x_discrete(limits = myorder)+
  scale_fill_manual(values = c("Biological and biomedical sciences" = "#2166ac", 
                               "Health sciences" = "#67a9cf", 
                               "Life sciences, nec" = "#d1e5f0",
                               "Engineering" = "#fddbc7", 
                               "All other fields" = "#ef8a62"),
                    labels = c("Life sciences, nec" = "All other life sciences"),
                    guide = FALSE)+
  coord_flip()+pthemes


# USPTO

p_USPTO <- bar_plot(patent, "Total number of USPTO patents per 1000 workers, 2000 - 2015", HL)+
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(~geo, scales = "free_y")+pthemes

# patent complexity
p_pci<- bar_plot(patentCOMP, "Metro knowledge complexity scores, 2000 - 2004", HL)+pthemes

# Venture capital
p_VC <- bar_plot(VC, "VC investment per 1000 workers, 2015 - 2017", HL)+
  scale_y_continuous(labels = scales::dollar)+
  pthemes

# BDS
p_BDS <- ggplot(data = fsfa %>%group_by(year2, Age)%>%summarise(JbC = sum(net_job_creation)),
                aes(x = factor(year2), y=JbC, fill = Age))+
  scale_y_continuous(labels = scales::comma, name = "Net job creation")+
  scale_fill_manual(values = c("#0070c0", "#ffc000", "#ffd966"))+
  labs(x = NULL, title = "Net job creation by firm age, Birmingham MSA")+
  geom_bar(stat = "identity")+pthemes

# young firm
p_young <- ggplot(data = young,
                  aes(x = factor(year2), y=emp.share, fill = Size))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent, name = "Share of employment")+
  geom_text(data = young %>%filter(Size=="Large ( 500 employees and above)"), 
            aes(label= scales::percent(emp.stot), y = emp.stot), 
            position = position_stack(vjust = 1.1))+
  scale_fill_manual(values = c("#0070c0", "#ffc000", "#ffd966"))+
  labs(x = NULL, title = "Share of employment at young firms (0  ~ 10 years) by firm size in Birmingham MSA")+pthemes

# inc 5000
p_inc<- bar_plot(I5HGC, "Number of Inc5000 high growth companies per 1000 worker, 2011 - 2017", HL)+
  # scale_y_continuous(labels = scales::comma)+
  pthemes

# FDIC
p_FDIC <- p_SME(SMEloans, "FDIC")+
  ggtitle("Annual average loans from private FDIC-insured banks (million) per 1000 workers")+
  scale_y_continuous(labels = scales::dollar_format(scale = 0.000001))


# p_FDIC_r <- ggplot(FDIC_race %>% filter(var == "per"), 
#                    aes(x = race, y = value, fill = race, label = scales::dollar(value)))+
#   geom_bar(stat="identity")+
#   geom_text()+
#   labs(title = "Annual average per employee FDIC loans, weighted by tract level racial composition",
#        x = element_blank(),y="Per employee amount")+
#   scale_fill_manual(values = c("#0070c0", "#ffc000"),
#                     label = c("minority", "white"))+pthemes

p_FDIC_r <- ggplot(FDIC_Bham_tract,
                   aes(x = is.white, y = value, fill = is.white, label = scales::dollar(value)))+
  geom_bar(stat="identity")+
  # geom_text(position = position_nudge(y=1))+
  ggtitle("Per 1000 workers FDIC loans (million), 1996 - 2017")+
  scale_fill_manual(name = NULL,
                    values = c("#0070c0", "#ffc000"),
                    label = c("share of white workers < 50%", 
                              "share of white workers >= 50%"))+
  scale_y_continuous(labels = scales::dollar)+
  labs(x=NULL, y = NULL)+
  pthemes%+%
  theme(axis.text.x = element_blank())


# CDFI 
p_CDFI <- p_SME(SMEloans, "CDFI")+
  ggtitle("Annual average CDFI loans per 1000 workers")+
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001))

p_CDFImap <- ggplot(CDFI_Bham, aes(fill = level))+
  geom_sf()+
  geom_polygon(data = map.Bham, aes(x = long, y = lat, group = group), fill = NA, color = "#ffd966", size = 1)+
  scale_fill_manual(values = c("#bdd7e7","#6baed6","#3182bd","#08519c"),
                    labels = c("0 - 10", "10 - 50", "50 - 100", "> 100"),
                    name = "per capita CDFI amount") +
  ggtitle("Per employee CDFI loans in Jefferson County by workplace tract, 2006 - 2017") +
  coord_sf(datum=NA)+pthemes%+%
  theme(axis.title = element_blank(), axis.text = element_blank(),legend.position = "bottom")

ggplot(CDFI_Bham, aes(fill = sum/1000000))+
  geom_sf()+
  geom_polygon(data = map.Bham, aes(x = long, y = lat, group = group), fill = NA, color = "#ffd966", size = 1)+
  scale_fill_gradient(name = "Amount (million)",low = "#bdd7e7", high = "#08519c")+
  # scale_fill_manual(values = c("#bdd7e7","#6baed6","#3182bd","#08519c"),
  #                   labels = c("0 - 10", "10 - 50", "50 - 100", "> 100"),
  #                   name = "per capita CDFI amount") +
  # ggtitle("Per employee CDFI lendings in Jefferson County by workplace tract, 2006 - 2017") +
  coord_sf(datum=NA)+
  pthemes%+%
  theme(axis.title = element_blank(), axis.text = element_blank(),legend.position = "bottom")

# p_CDFI_r <- ggplot(TLR_Bham_weight  %>% filter(var == "per"), 
#                    aes(x = race, y = value, fill = race, label = scales::dollar(value)))+
#   geom_bar(stat="identity")+
#   geom_text()+
#   ggtitle("Annual average per employee CDFI loans, weighted by tract level racial composition")+
#   scale_fill_manual(values = c("#0070c0", "#ffc000"),label = c("minority", "white"))+
#   pthemes

p_CDFI_r <- ggplot(TLR_Bham_tract,
                   aes(x = is.white, y = value, fill = is.white, label = scales::dollar(value)))+
  geom_bar(stat="identity")+
  # geom_text(position = position_nudge(y=1))+
  ggtitle("Per 1000 workders CDFI loans, 2006 - 2017")+
  scale_fill_manual(name = NULL,
                    values = c("#0070c0", "#ffc000"),
                    label = c("share of white workers < 50%", 
                              "share of white workers >= 50%"))+
  scale_y_continuous(labels = scales::dollar)+
  labs(x=NULL, y = NULL)+
  pthemes%+%
  theme(axis.text.x = element_blank())
