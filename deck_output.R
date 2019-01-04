# Author: Sifan Liu
# Date: Thu Dec 27 10:46:17 2018
# --------------

pkgs <- c('tidyverse','officer')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 


# Read slide template and charts ======================
pptx <- read_pptx("V:/Building Inclusive Cities/Birmingham/Market Assessment/test.pptx")
source('deck_charts.R')

# function to add new slide with graph ----------------
new_slides <- function(title, plot, width = 8, height = 5.5){
  pptx <- pptx %>%
    add_slide(layout = "Title and Content", master =  "Office Theme") %>%
    ph_with_text(type = "title",str = title)
  
  if(!missing(plot)){
    if(capabilities(what = "png"))pptx <- ph_with_gg(pptx, value = plot, index = 2, width = width, height = height)}
}

# add slides -----------------------------------------
# Metromonitor
new_slides("Birmingham lags its peers in economic performance", p_peermap, 10)


#tradables
new_slides("Birmingham would have had 38k more jobs in 2016 if it had grown at national rates",p_shiftshare, 11)
new_slides("Industrial decline touches both non-tradable and tradable sectors", p_2by2,11)
new_slides("But tradable industries acount for a lower share of jobs than peers", p_expemp)
new_slides("Tradable indusries disproportionately house 'good jobs'", p_opp)
new_slides("Birmingham relies less on exports than peer metro areas", p_expshare)
new_slides("Business receive less export financing in Birmingham and Jefferson County", p_EXIM)

#innovation
new_slides("Local universities are an incredible research asset", p_RDuni)
new_slides("R&D dominated by health sciences and biomedical sciences", p_UAB_donut)
new_slides("But research spend is not translating into commercialization", p_USPTO)
new_slides("Top patenting activities in Birmingham concentrate in life sciences and advanced manufacturing")
new_slides("A second challenge is that Birmingham has low levels of technological complexity", p_pci)
new_slides("Licensing and start-ups at UAB")
new_slides("Venture capital investment", p_VC)

#dynamism
new_slides("Dynamic firm growth is critical to advancing innovation and job creation",p_BDS)
new_slides("But the share of employment at young firms has been in decline", p_young)
new_slides("And Birmingham’s share of high growth companies has lagged", p_inc)

#capital access
new_slides("Is this a capital access issue?", p_FDIC)
new_slides("But there may be capital gaps by place and by race", p_FDICmap,5)
new_slides("But there may be capital gaps by place and by race", p_FDIC_r,5)
new_slides("Birmingham has not had significant CDFI funding", p_CDFImap,5)
new_slides("Birmingham has not had significant CDFI funding", p_CDFI_r,5)

# FINAL OUTPUT ================================================

print(pptx, target = "V:/Building Inclusive Cities/Birmingham/Market Assessment/test.pptx") 
