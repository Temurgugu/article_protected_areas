rm(list=ls())    # clear the workspace


#=== Temur Gugushvii === 
# avout author https://temurgugushvili.ge/
# data source https://apa.gov.ge/ge/statistika/vizitorta-statistika

#libraries

library(ggplot2)
library(tidyverse)

# import data

tg_pa_visitors <- readr::read_csv("data/pa_visitors.csv",
                                   col_types = cols(year = "n",
                                                    visits = "n",
                                                    pa = "c"))

#Prepare data for visualization (filter data)

tg_pa_visitors <- tg_pa_visitors %>% 
                  dplyr::filter(pa == "pa_total")


#Build the visualization 

tg_pa_visitors_diagram <- ggplot2::ggplot(tg_pa_visitors, aes(year, visits)) +
                          geom_col(fill = "#408000") +
                          geom_label(label = tg_pa_visitors$visits, size = 3)+
                          theme_minimal(base_family="Sylfaen")+
                          theme(axis.title.x = element_text(colour="black", size=10, hjust=0.5),
                                axis.title.y = element_text(colour="black", size=10, hjust=0.5),
                                axis.text.x=element_text(angle = 90, hjust=0.5, size=10, colour="black"),
                                axis.text.y=element_text(angle = 0, hjust=0.5, size=10, colour="black"),
                                plot.caption = element_text(size=10, colour="black", hjust=0),
                                plot.title=element_text(colour="black", size=12))+
                          labs(title = "Number of visitors in protected areas (Georgia)\nდაცულ ტერიტორიებზე ვიზიტორთა რაოდენობა (საქართველო)",
                               subtitle ="",
                               caption = "Source: Agency of Protected Areas \nწყარო: დაცული ტერიტორიების სააგენტო",
                               x = "Year\nწელი",
                               y = "Visits\nვიზიტები")+
                          scale_y_continuous(breaks=seq(0, 120000000, 200000), labels = scales::comma)+
                          scale_x_continuous(breaks=seq(2007, 2020, 1))

#Save the ggplot
ggsave("visualization/tg_pa_visitors_diagram.png", 
       plot = tg_pa_visitors_diagram,
       width = 10,
       height = 6) 
                 

