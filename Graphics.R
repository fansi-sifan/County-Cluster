pkgs <- c('tigris', "rgdal",'maptools', 'tidyverse', "ggmap", "maps", "grid", "gridExtra",'rgeos','broom', 'ggrepel')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

# map themes
pthemes <- theme(axis.ticks = element_blank(),
                 axis.text = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank(),
                 panel.grid = element_blank(),
                 axis.title = element_blank())

bar_plot <- function(df,title, HL){
  ggplot(df, aes(x = reorder(metro,value), y = value, fill = HL))+
    geom_bar(stat = "identity")+
    coord_flip()+
    ggtitle(title)+
    scale_fill_discrete(guide = FALSE)
  
}
