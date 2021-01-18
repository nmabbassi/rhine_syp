install.packages("tidyverse")
install.packages("readxl")
library(ggplot2)
library(readxl)

#load data
rh_data <- read_excel("~/Desktop/Rhine/Rhine_Chloride_Total.xlsx",
                      na="NA") 

#fig. 1 - average chloride pollution in Rhine
ggplot(rh_data, aes(x=`Year`, 
                    y=`Total Averages`))+
  geom_line()+
  labs(x=element_blank(), 
       y="Average Chloride Ion Pollution in Rhine River (kg/s)", 
       title=element_blank(),
       fill=element_blank())+
  theme_gray()+      
  #add vert lines for treaty introduction
  geom_vline(aes(xintercept=1976,
                 color="Treaty Introduced"),
             linetype="longdash",
             size=1)+
  geom_vline(aes(xintercept=1986,
                 color="Treaty Ratified"),
             linetype="dashed",
             size=1)+
  scale_color_manual(name="Legend", 
                     values = c(`Treaty Introduced` = "steelblue3", 
                                `Treaty Ratified` = "tomato4"))


