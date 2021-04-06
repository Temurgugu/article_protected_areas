rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 
# Author https://temurgugushvili.ge/
# Data source:

library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(hrbrthemes)
library(viridis)
library(plotly)
library(haven)


# import data

surveydata <- readxl::read_xlsx("data/pa_attitude.xlsx")

attitude_qestions <- surveydata %>% 
                      dplyr::select(ID:PA_1.12) %>% 
                      replace_na(list(Scale = "NA")) %>% 
  mutate(PA_1.3 = recode(PA_1.3, "1"= "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1", .default = NA_character_), 
         PA_1.7 = recode(PA_1.7, "1"= "5", "2" = "4", "3" = "3",  "4" = "2", "5" = "1", .default = NA_character_))


attitude_qestions$PA_1.3 <- as.numeric(as.character(attitude_qestions$PA_1.3))
attitude_qestions$PA_1.7 <- as.numeric(as.character(attitude_qestions$PA_1.7))


attitude_qestions <- attitude_qestions %>% 
                      pivot_longer(cols = starts_with("PA") , names_to = "Questions", values_to = "Scale") %>% 
                      mutate(Questions_1 = recode(Questions, 
                       "PA_1.1" = "I am happy about the presence of the PA in my community",
                       "PA_1.2" = "The presence of the PA has improved my living conditions",	
                       "PA_1.3" = "The presence of the PA has worsened my situation",
                       "PA_1.4" = "The presence of the PA has brought development to my village",	
                       "PA_1.5" =	"Management of the PA is effective",
                       "PA_1.6" = "The PA is necessary for protecting the remaining resources",
                       "PA_1.7" = "People should be allowed to hunt in the PA",
                       "PA_1.8" = "Relationships between the community & park officials are cordial",
                       "PA_1.9" = "Park officials understand & are concerned about our needs",
                       "PA_1.10" = "The community is involved in decision making & management of the PA",
                       "PA_1.11" = "We are encouraged to participate in conservation programmes",
                       "PA_1.12" = "My personal relationship with the PA is good"))

# Prepare data for visualization (filter data)

attitude_qestions <-  dplyr::filter(attitude_qestions, Scale != "NA")


attitude_avg <- attitude_qestions  %>% 
                 group_by(Questions_1) %>%  
                 summarise(avg = mean(Scale))

count(surveydata, PAA_2)

mean(attitude_avg$avg)

attitude_avg_chart <- ggplot2::ggplot(attitude_avg, aes(avg, Questions_1))+
  geom_col(fill = "#9ab078")+
  geom_vline(xintercept = 3.327101)+
  theme_bw()+
  scale_x_continuous(breaks=seq(1, 5, 1), limits = c(0, 5))


