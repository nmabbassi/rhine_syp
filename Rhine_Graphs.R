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
                 color="Treaty Signed"),
             linetype="longdash",
             size=1)+
  geom_vline(aes(xintercept=1986,
                 color="Treaty Ratified"),
             linetype="dashed",
             size=1)+
  scale_color_manual(name=element_blank(), 
                     values = c(`Treaty Signed` = "gray7", 
                                `Treaty Ratified` = "gray27"))+
      theme(legend.position="bottom", legend.background=element_rect
           (fill="gray95", linetype=1, size=0.15, color=1))

#fig.2 - avg. chloride pollution in rhine per state
  #create combined country data frame
labels <- c(`Switzerland` = "red4", 
           `Netherlands` = "dodgerblue4", 
           `Germany` = "slateblue",
           `France` = "darkgreen")

ggplot(rh_data, aes(x=`Year`))+
          geom_line(aes(y=`Switzerland`, color="Switzerland"))+
          geom_line(aes(y=`Netherlands`, color="Netherlands"))+
          geom_line(aes(y=`Germany`, color="Germany"))+
          geom_line(aes(y=`France`, color="France"))+
    labs(x=element_blank(), 
         y="Average Chloride Ion Pollution in Rhine River (kg/s) per State", 
         title=element_blank(),
         fill=element_blank(),
         color=element_blank())+
      scale_color_manual(values=labels)+
  theme_gray()+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=0.15, color=1))+
 
   geom_vline(xintercept = 1976, 
             linetype="longdash",
             color="gray7",
             size=1)+
   geom_vline(xintercept = 1988, 
             linetype="dashed",
             color="gray27",
             size=1)
