rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 
# Author https://temurgugushvili.ge/
# Data source https://apa.gov.ge/ge/statistika/vizitorta-statistika

#libraries

library(ggplot2)
library(tidyverse)
library(lubridate)
library(patchwork)


# import data

tg_pa_visits_2014 <- readr::read_csv("data/gpa_2014_2020.csv",
                                  col_types = cols(Year_Month = "c",
                                                   citizen_ge = "c",
                                                   visitor_type_en = "c",
                                                   pa_ge = "c",
                                                   pa_en = "c",
                                                   visits = "n"))


tg_pa_visits_2014 <- tg_pa_visits_2014 %>%
                     dplyr::mutate(Date = as.Date(paste(Year_Month,"-01",sep="")))  %>%
                     dplyr::mutate(Year_Month = lubridate::format_ISO8601(Date, precision = "ym"))  %>%
                     dplyr::mutate(year = lubridate::year(Date)) %>%
                     dplyr::mutate(month = lubridate::month(Date, label = TRUE))
  
tg_pa_visits_2014$Year_Month <- zoo::as.Date(zoo::as.yearmon(tg_pa_visits_2014$Year_Month))

tg_pa_visits_2014_visitors <- tg_pa_visits_2014  %>% 
                              group_by(year, month, visitor_type_en)  %>% 
                              summarise(visits_group = sum(visits))

tg_pa_visitors_visitor_type_en <- tg_pa_visits_2014  %>% 
                                  dplyr::group_by(year, visitor_type_en)  %>% 
                                  dplyr::summarise(visits_group = sum(visits))
                                  
                              
  
tg_pa_visitors_visitor_type_en_Kazbegi <- tg_pa_visits_2014  %>% 
  dplyr::filter(pa_en == "Kazbegi National Park")%>%
  dplyr::group_by(year, visitor_type_en)  %>% 
  dplyr::summarise(visits_group = sum(visits))



tg_pa_visits_2014_domestic <- tg_pa_visits_2014 %>% filter(visitor_type_en == "Domestic")

tg_pa_visits_2014_International <- tg_pa_visits_2014 %>% filter(visitor_type_en == "International")



pa_visitors  <- ggplot2::ggplot(tg_pa_visits_2014_visitors, aes(y= month, x= year, fill =visits_group))+
                             geom_tile() +
                             scale_fill_distiller(name = "Number of Visits", direction = 1, palette = "Oranges", limits=c(0, 140000), breaks=seq(0, 140000, 20000))+
                             scale_x_continuous(breaks=seq(2014, 2020, 1))+
                             labs(title = "Number of visits in protected areas (Georgia)",
                                  subtitle ="",
                                  caption = "Source: Agency of Protected Areas",
                                  x = "Year",
                                  y = "Visits")+
                             facet_wrap(vars(visitor_type_en), labeller = label_wrap_gen())

ggsave("visualization/pa_visitors.JPEG", 
       plot = pa_visitors)

domestic_pa_visitors  <- ggplot2::ggplot(tg_pa_visits_2014_domestic, aes(y= month, x= year, fill =visits))+
                            geom_tile() +
                            theme(axis.text.x=element_text(angle = 90, hjust=0.5, size=9, colour="black"),
                                  axis.text.y=element_text(angle = 0, hjust=0.5, size=9, colour="black"))+
                            labs(title = "Number of domestic visits in protected areas (Georgia)",
                                 subtitle ="",
                                 caption = "Source: Agency of Protected Areas",
                                 x = "Year",
                                 y = "Visits")+
                            scale_fill_distiller(name = "Number of Visits", palette = "Spectral", limits=c(0, 33000), direction = -1, breaks=seq(0, 33000, 3000))+
                            scale_x_continuous(breaks=seq(2014, 2020, 1))+
                            facet_wrap(vars(pa_en), labeller = label_wrap_gen())

ggsave("visualization/domestic_pa_visitors.JPEG", 
       plot = domestic_pa_visitors,
       width = 290,
       height = 290,
       units = "mm")

international_pa_visitors <- ggplot2::ggplot(tg_pa_visits_2014_International, aes(y= month, x= year, fill =visits))+
                                geom_tile() +
                                theme(axis.text.x=element_text(angle = 90, hjust=0.5, size=9, colour="black"),
                                      axis.text.y=element_text(angle = 0, hjust=0.5, size=9, colour="black"))+
                                labs(title = "Number of international visits in protected areas (Georgia)",
                                     caption = "Source: Agency of Protected Areas",
                                     x = "Year",
                                     y = "Visits")+
                                scale_fill_distiller(name = "Number of Visits",
                                                     palette = "Spectral", 
                                                     limits=c(0, 33000), 
                                                     breaks=seq(0, 33000, 3000))+
                                scale_x_continuous(breaks=seq(2014, 2020, 1))+
                                facet_wrap(vars(pa_en), labeller = label_wrap_gen())  


international_pa_visitors_1 <- ggplot2::ggplot(tg_pa_visits_2014_International, aes(y= month, x= year, fill =visits))+
  geom_tile() +
  theme(axis.text.x=element_text(angle = 90, hjust=0.5, size=9, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=0.5, size=9, colour="black"))+
  labs(title = "Number of international visits in protected areas (Georgia)",
       caption = "",
       x = "Year",
       y = "")+
  scale_fill_distiller(name = "Number of Visits",
                       palette = "Spectral", 
                       limits=c(0, 33000), 
                       breaks=seq(0, 33000, 3000),
                       guide = FALSE)+
  scale_x_continuous(breaks=seq(2014, 2020, 1))+
  facet_wrap(vars(pa_en), labeller = label_wrap_gen())  



ggsave("visualization/international_pa_visitors.JPEG", 
       plot = international_pa_visitors,
       width = 290,
       height = 220,
       units = "mm")

international_domestic_pa_visitors <- international_pa_visitors_1 + domestic_pa_visitors

ggsave("visualization/international_domestic_pa_visitors.JPEG", 
       plot = international_domestic_pa_visitors,
       width = 510,
       height = 320,
       units = "mm")



tg_pa_visitors_type_en <- ggplot2::ggplot(tg_pa_visitors_visitor_type_en, aes(year, visits_group)) +
                          geom_col(aes(fill = visitor_type_en), position = "dodge") +
                          theme_minimal(base_family="Sylfaen")+
                          theme(axis.title.x = element_text(colour="black", size=9, hjust=0.5),
                                axis.title.y = element_text(colour="black", size=9, hjust=0.5),
                                axis.text.x=element_text(angle = 90, hjust=0.5, size=9, colour="black"),
                                axis.text.y=element_text(angle = 0, hjust=0.5, size=9, colour="black"),
                                plot.caption = element_text(size=9, colour="black", hjust=0),
                                plot.title=element_text(colour="black", size=13),
                                panel.grid.major = element_line(size = 0.05))+
                          labs(title = "Number of visits in protected areas (Georgia)",
                               subtitle ="",
                               caption = "Source: Agency of Protected Areas",
                               x = "Year",
                               y = "Visits")+
                          scale_fill_manual(name = "Type of Visit",
                                            values = c("#0057b7", "#ffcc00"))+
                          scale_y_continuous(breaks=seq(0, 6000000, 100000), labels = scales::comma)+
                          scale_x_continuous(breaks=seq(2007, 2020, 1))
ggsave("visualization/tg_pa_visitors_type_en.JPEG", 
       plot = tg_pa_visitors_type_en,
       width = 290,
       height = 220,
       units = "mm")




tg_pa_visitors_type_en_Kazbegi <- ggplot2::ggplot(tg_pa_visitors_visitor_type_en_Kazbegi, aes(year, visits_group)) +
  geom_col(aes(fill = visitor_type_en), position = "dodge") +
  theme_minimal(base_family="Sylfaen")+
  theme(axis.title.x = element_text(colour="black", size=9, hjust=0.5),
        axis.title.y = element_text(colour="black", size=9, hjust=0.5),
        axis.text.x=element_text(angle = 90, hjust=0.5, size=9, colour="black"),
        axis.text.y=element_text(angle = 0, hjust=0.5, size=9, colour="black"),
        plot.caption = element_text(size=9, colour="black", hjust=0),
        plot.title=element_text(colour="black", size=13),
        panel.grid.major = element_line(size = 0.05))+
  labs(title = "Number of visits in protected areas (Georgia):Kazbegi National Park",
       subtitle ="",
       caption = "Source: Agency of Protected Areas",
       x = "Year",
       y = "Visits")+
  scale_fill_manual(name = "Type of Visit",
                    values = c("#0057b7", "#ffcc00"))+
  #scale_y_continuous(breaks=seq(0, 6000000, 100000), labels = scales::comma)+
  scale_x_continuous(breaks=seq(2007, 2020, 1))
ggsave("visualization/tg_pa_visitors_type_en_Kazbegi.JPEG", 
       plot = tg_pa_visitors_type_en_Kazbegi,
       width = 290,
       height = 220,
       units = "mm")