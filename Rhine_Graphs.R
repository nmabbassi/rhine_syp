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

ad <- read_excel("argumentdata.xlsx")

rh_forecast <- read_excel("~/Desktop/Rhine/rhine_forecast.xlsx",
                          na="NA") 

rh_avgflow <- read_excel("~/Desktop/Rhine/rhine_avgflow.xlsx",
                         na="NA")




#fig. 1: total average flow rate
#create lines and labels for each country
labels <- c(`Switzerland` = "red4", 
            `Netherlands` = "dodgerblue4", 
            `Germany` = "slateblue",
            `France` = "darkgreen")
#code ggplot
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
       y="Average Annual Flow Rate 
in Rhine River (m3/s)", 
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
                    color="Chloride Ion Averages (kg/s)"))+
  geom_line(aes(y=totalconc, 
                color="Chloride Concentration (kg/m3)"))+
  
  geom_line()+
  labs(x=element_blank(), 
       y="Chloride Pollution 
in Rhine River", 
       title=element_blank(),
       fill=element_blank())+
  theme_gray()+ 
  theme(axis.title=element_text(size=12))+
  
  #add vert lines for treaty introduction
  geom_vline(aes(xintercept=1976,
                 color="Treaty Signed"),
             linetype="longdash",
             size=.5)+
  geom_vline(aes(xintercept=1986,
                 color="Treaty Ratified"),
             linetype="dashed",
             size=.5)+
  
  #create legend
  scale_color_manual(name=element_blank(), 
                     values = c(`Treaty Signed` = "gray7", 
                                `Treaty Ratified` = "gray27",
                                `Chloride Ion Averages (kg/s)`= "steelblue4",
                                `Chloride Concentration (kg/m3)`= "blue3"))+
  
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=0.15, color=1))

#fig. 4 - avg. chloride pollution in rhine per state
#create data frame for country info
labels <- c(`Switzerland` = "red4", 
            `Netherlands` = "dodgerblue4", 
            `Germany` = "slateblue",
            `France` = "darkgreen")
#create data frame for horizontal lines to later add to legend
treatysig <- c(xintercent= 1976)
treatyrat <- c(xintercept=1986)

dftest <- c(treatysig = "gray7",
            treatyrat = "gray27")

#code ggplot
ggplot(rh_data, aes(x=`Year`))+
  geom_line(aes(y=`Switzerland`, 
                color="Switzerland"))+
  geom_line(aes(y=`Netherlands`, 
                color="Netherlands"))+
  geom_line(aes(y=`Germany`, 
                color="Germany"))+
  geom_line(aes(y=`France`, 
                color="France"))+
  
  labs(x=element_blank(), 
       y="Average Chloride Ion Pollution 
in Rhine River (kg/s) per State", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  
  #add horizontal lines for treaty info
  geom_vline(aes(xintercept=treatysig,
                 color="Treaty Signed"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept = treatyrat,
                 color="Treaty Ratified"),
             linetype = "dashed",
             size=.5)+
  
  #create legend
  scale_color_manual(name=element_blank(),
                     values=c(labels, 
                              `Treaty Signed` = "gray7",
                              `Treaty Ratified` = "gray27"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))


#fig.5 - avg. chloride concentration in Rhine
#note flow rate data is limited - both in years and from monitoring stations
#calculating concentrations (kg/s) divided by (m3/s) = kg/m3
rh_data$swiss_conc <- 100*(rh_data$Switzerland/rh_data$FR_Switzerland)
rh_data$neth_conc <- 100*(rh_data$Netherlands/rh_data$FR_Netherlands)
rh_data$germ_conc <- 100*(rh_data$Germany/rh_data$FR_Germany)
rh_data$france_conc <- 100*(rh_data$France/rh_data$FR_France)

#code ggplot
ggplot(rh_data, aes(x=`Year`))+
  geom_line(aes(y=swiss_conc, 
                color="Switzerland"))+
  geom_line(aes(y=neth_conc, 
                color="Netherlands"))+
  geom_line(aes(y=germ_conc, 
                color="Germany"))+
  geom_line(aes(y=france_conc, 
                color="France"))+
  
  labs(x=element_blank(), 
       y="Average Chloride Concentrations 
in Rhine River (kg/m3) per State", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  
  #add horizontal lines for treaty info
  geom_vline(aes(xintercept=treatysig,
                 color="Treaty Signed"),
             linetype = "longdash",
             size=.5)+
  geom_vline(aes(xintercept = treatyrat,
                 color="Treaty Ratified"),
             linetype = "dashed",
             size=.5)+
  #code legend
  scale_color_manual(name=element_blank(),
                     values=c(labels, 
                              `Treaty Signed` = "gray7",
                              `Treaty Ratified` = "gray27"))+
  theme_gray()+
  theme(axis.title=element_text(size=12))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))

#fig. 6 - avg. chloride pollution at selected monitoring stations


#fig. 7 orignal forecasting test exp model

