install.packages("tidyverse")
install.packages("readxl")
library(ggplot2)
library(readxl)

#load data
rh_data <- read_excel("~/Desktop/Rhine/Rhine_Chloride_Total.xlsx",
                      na="NA") 




#fig.1 - avg. chloride pollution in Rhine
  #code ggplot
ggplot(rh_data, aes(x=`Year`, 
                    y=`Total Averages`,
                    color="Total Averages"))+
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
             size=.5)+
  geom_vline(aes(xintercept=1986,
                 color="Treaty Ratified"),
             linetype="dashed",
             size=.5)+
  #create legend
   scale_color_manual(name=element_blank(), 
                     values = c(`Treaty Signed` = "gray7", 
                                `Treaty Ratified` = "gray27",
                                `Total Averages`= "steelblue4"))+
  
      theme(legend.position="bottom", legend.background=element_rect
           (fill="gray95", linetype=1, size=0.15, color=1))



#fig.2 - avg. chloride pollution in rhine per state
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
       y="Average Chloride Ion Pollution in Rhine River (kg/s) per State", 
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
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))


#fig.3 - avg. chloride concentration in Rhine
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
       y="Average Chloride Concentrations in Rhine River (kg/m3) per State", 
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
    theme(legend.position="bottom", legend.background=element_rect
         (fill="gray95", linetype=1, size=.15, color=1))

#fig. 4 - avg. chloride pollution at selected monitoring stations

#fig. 5 - avg. chloride concentrations
