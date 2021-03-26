#DON'T RUN FOLLOWING - JUST FOR NOTES#
######################################
install.packages("tidyverse")
install.packages("readxl")
install.packages("ca")
install.packages("forecast")
install.packages("MLmetrics")
######################################


library(ggplot2)
library(readxl)
library(carData)

#below used for forecasting counterfactual
library(forecast)
library(MLmetrics)


#load data
rh_data <- read_excel("~/Desktop/Rhine/Rhine_Chloride_Total.xlsx",
                      na="NA") 

rh_avg <- read_excel("~/Desktop/Rhine/Rhine_Chloride_Avg.xlsx",
                     na="NA") 
ad <- read_excel("argumentdata.xlsx")

rh_forecast <- read_excel("~/Desktop/Rhine/rhine_forecast.xlsx",
                          na="NA") 

rh_avgflow <- read_excel("~/Desktop/Rhine/rhine_avgflow.xlsx",
                         na="NA")


#ORGANIZE DATA

###create lines and labels for each country
####data frame for countries w  colors
labels <- c(
            `Switzerland` = "red4", 
            `Netherlands` = "dodgerblue4", 
            `Germany` = "slateblue",
            `France` = "darkgreen")

####treaty lines and data frame
treatyrat <- c(xintercept = 1983)
treatyimp <- c(xintercept = 1986)
protocolrat <- c(xintercept = 1991)
protocolimp <- c(xintercept = 1994)
dfone <- c(treatyrat,
           treatyimp,
           protocolimp,
           protocolrat)

dftest <- c(treatyrat = "gray1",
            treatysig = "gray2",
            protocolrat = "gray29",
            protocolimp = "gray30")

#fig. 1: total average flow rate
##code ggplot
ggplot(rh_avgflow, aes(x=`Year`))+
  geom_line(aes(y=`Switzerland`, 
                color="Switzerland"))+
  geom_line(aes(y=`Netherlands`, 
                color="Netherlands"))+
  geom_line(aes(y=`Germany`, 
                color="Germany"))+
  geom_line(aes(y=`France`, 
                color="France"))+
  labs(x=element_blank(), 
       y="Average annual flow rate in Rhine River (m3/s)", 
       title=element_blank(),
       fill=element_blank())+
  theme_gray()+ 
  theme(axis.title=element_text(size=12))+
  scale_color_manual(name=element_blank(),
                     values=c(labels))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))


#fig. 2(?) cooperation conflict simple scatterplot
plot(ad, 
     xlab=expression("greater treaty cooperation" * symbol('\256')),
     ylab=expression("more costly for downstream states" * symbol('\256')))


#fig.3 - avg. chloride pollution in Rhine
#using both chloride ion pollution and chloride concentration
#concentration variable
rh_data$totalconc <- 100*(rh_data$`Total Averages`/rh_data$ConcentrationAvg)

#code ggplot
ggplot(rh_data, aes(x=`Year`, 
                    y=`Total Averages`,
                    color="     Chloride Ion Averages (kg/s)"))+
  geom_line(aes(y=totalconc, 
                color="      Chloride Concentration (kg/m3)"))+
  
  geom_line()+
  labs(x=element_blank(), 
       y="Chloride Pollution in Rhine River", 
       title=element_blank(),
       fill=element_blank())+
  theme_gray()+ 
  theme(axis.title=element_text(size=12))+
  
  #add vert lines for treaty introduction
  geom_vline(aes(xintercept=treatyrat,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept = treatyimp,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept = protocolrat,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept = protocolimp,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  #create legend
  scale_color_manual(name=element_blank(), 
                     values = c("blue3", "steelblue4", "gray1", "gray2", "gray29", "gray30"))+
              
  
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=0.15, color=1))


#####fig. 3 alternate#####
ggplot(rh_data, aes(x=`Year`, 
                    y=`Total Averages`,
                    color="    Chloride Ion Averages (kg/s)"))+
  geom_line()+
  labs(x=element_blank(), 
       y= "Chloride Pollution in Rhine River", 
       title=element_blank(),
       fill=element_blank())+
  theme_gray()+ 
  theme(axis.title=element_text(size=12))+
  #add vert lines for treaty introduction
  geom_vline(aes(xintercept = treatyrat,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept = treatyimp,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept = protocolrat,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept = protocolimp,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  #create legend
  scale_color_manual(name=element_blank(), 
                     values = c("steelblue4", "gray1", "gray2", "gray29", "gray30"))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=0.15, color=1))



#fig. 4 - avg. chloride pollution in rhine per state
#code ggplot
ggplot(rh_data, aes(x=`Year`))+
  geom_line(aes(y=`Switzerland`, 
                color="      Switzerland"))+
  geom_line(aes(y=`Netherlands`, 
                color="        Netherlands"))+
  geom_line(aes(y=`Germany`, 
                color="         Germany"))+
  geom_line(aes(y=`France`, 
                color="          France"))+
  
  labs(x=element_blank(), 
       y="Average Chloride Ion Pollution 
in Rhine River (kg/s) per State", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  
  #add horizontal lines for treaty info
  geom_vline(aes(xintercept=1983,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept=1986,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept=1991,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept=1994,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  
  
  #create legend
  scale_color_manual(name=element_blank(),
                     values=c(`          France` = "darkgreen",
                              `         Germany` = "slateblue",
                              `        Netherlands` = "dodgerblue4",
                              `      Switzerland` = "red4",
                              `   Treaty Ratified` = "gray1",
                              `  Treaty Implemented` = "gray1",
                              ` Protocol Ratified` = "gray30",
                              `Protocol Implemented` = "gray30"))+
                             
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))


#fig.5 - chloride concentration in Rhine
#note flow rate data is limited - both in years and from monitoring stations
#calculating concentrations (kg/s) divided by (m3/s) = kg/m3
rh_data$swiss_conc <- 100*(rh_data$Switzerland/rh_data$FR_Switzerland)
rh_data$neth_conc <- 100*(rh_data$Netherlands/rh_data$FR_Netherlands)
rh_data$germ_conc <- 100*(rh_data$Germany/rh_data$FR_Germany)
rh_data$france_conc <- 100*(rh_data$France/rh_data$FR_France)

#code ggplot
ggplot(rh_data, aes(x=`Year`))+
  geom_line(aes(y=swiss_conc, 
                color="      Switzerland"))+
  geom_line(aes(y=neth_conc, 
                color="        Netherlands"))+
  geom_line(aes(y=germ_conc, 
                color="         Germany"))+
  geom_line(aes(y=france_conc, 
                color="          France"))+
  
  labs(x=element_blank(), 
       y="Chloride concentrations in Rhine River per state (kg/m3)", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  
  #add horizontal lines for treaty info
  geom_vline(aes(xintercept=1983,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept=1986,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept=1991,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept=1994,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  #code legend
  scale_color_manual(name=element_blank(),
                     values=c(`          France` = "darkgreen",
                              `         Germany` = "slateblue",
                              `        Netherlands` = "dodgerblue4",
                              `      Switzerland` = "red4",
                              `   Treaty Ratified` = "gray1",
                              `  Treaty Implemented` = "gray1",
                              ` Protocol Ratified` = "gray30",
                              `Protocol Implemented` = "gray30"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))

#fig. 6 - avg. chloride pollution at selected monitoring stations
ggplot(rh_data, aes(x=`Year`))+
  geom_line(aes(y=`Before AM`, 
                color="        Before AM (CH)"))+
  geom_line(aes(y=`After AM 1`, 
                color="       After AM 1 (FR)"))+
  geom_line(aes(y=`After AM 2`, 
                color="      After AM 2 (DE)"))+
  geom_line(aes(y=`After AM 3`, 
                color="     After AM 3 (NL)"))+
  geom_line(aes(y=`After AM 4`, 
                color="    After AM 4 (NL)"))+
  
  labs(x=element_blank(), 
       y="Chloride  pollution in Rhine at select monitoring stations (kg/s)", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  
  #add horizontal lines for treaty info
  geom_vline(aes(xintercept=1983,
                 color="   Treaty Ratified"),
             linetype = "solid",
             size=.5)+
  geom_vline(aes(xintercept=1986,
                 color="  Treaty Implemented"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept=1991,
                 color=" Protocol Ratified"),
             linetype = "dotdash",
             size=.5)+
  geom_vline(aes(xintercept=1994,
                 color="Protocol Implemented"),
             linetype = "twodash",
             size=.5)+
  
  #code legend
  scale_color_manual(name=element_blank(),
                     values=c(`        Before AM (CH)` = "red4",
                              `       After AM 1 (FR)` = "darkgreen",
                              `      After AM 2 (DE)` = "slateblue",
                              `     After AM 3 (NL)` = "dodgerblue4",
                              `    After AM 4 (NL)` = "dodgerblue1",
                              `   Treaty Ratified` = "gray1",
                              `  Treaty Implemented` = "gray1",
                              ` Protocol Ratified` = "gray30",
                              `Protocol Implemented` = "gray30"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))

#fig. 7 orignal forecasting test exp model

