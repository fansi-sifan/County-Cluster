
pkgs <- c('tigris', "rgdal",'maptools', 'tidyverse', "ggmap", "maps", "grid", "gridExtra",'rgeos','broom', 'ggrepel')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 


# add leading zeros
padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n)) 

# NA values
na.share <- function(df,col)(sum(is.na(df[[col]]))/length(df[[col]]))

# check distribution
range <- function(df,col)(summary(as.factor(df[[col]])))


# plot themes
pthemes <- theme(rect = element_rect(fill = "D9D9D9", colour=NA),
                 panel.background = element_rect(fill = "#D9D9D9",colour = NA),
                 plot.background = element_rect(fill = "#D9D9D9", colour = NA),
                 panel.grid = element_blank(),
                 legend.background = element_rect(fill = "transparent"),
                 legend.key = element_rect(fill = "transparent", color = NA),
                 legend.box.background = element_rect(fill = "transparent", colour = NA),
                 text = element_text(size = 15), axis.text = element_text(size = 12),
                 axis.ticks = element_blank(),
                 strip.text = element_blank()
)

# bar plot with title and highlights
bar_plot <- function(df,title, HL){
  ggplot(df, aes(x = reorder(metro,value), y = value, fill = HL))+
    geom_bar(stat = "identity")+
    coord_flip()+
    labs(title = title, x = NULL,y = NULL)+
    scale_fill_manual(values = c("#0070c0", "#ffc000"), guide = FALSE)
}

# SMEloans plot, peer vs Bham
opr <- function(df){
  df %>%
    group_by(Bham, program, year_range, emp.tot)%>%
    summarise(amt.tot = sum(as.numeric(amt.tot), na.rm = TRUE))%>%
    filter(!is.na(year_range))%>%
    mutate(value = amt.tot/emp.tot*1000)
}

p_SME <- function(df,pgm){ggplot(data = df%>%filter(program==pgm), 
                                 aes(x = year_range, y = value, fill = Bham))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_manual(name = element_blank(), 
                      values = c("#0070c0", "#ffc000"), 
                      labels = c( "Peer average", paste(placename,countyname, sep = " /\n")))+
    labs(x = NULL, y = NULL)+
    facet_wrap(~geo, nrow = 1)+pthemes}

