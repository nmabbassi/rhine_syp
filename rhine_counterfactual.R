library(ggplot2)
library(readxl)
library(carData)


#Counterfactual plots
##Potash Demand
##flow rate
##growing environmentalism in region


##### Potash Consumption #####
###fertilizer consumption measures quantity of plant nutrients
###and is calculated as production plus imports minus exports.
fert_consump <- read_excel("~/Desktop/Rhine/FR_fert_consumption.xlsx",
           na="NA") 

#code ggplot
ggplot(fert_consump, aes(x=`Year`, 
                         y=`Fertilizer`),
                    color="Fertilizer Consumption")+
  geom_line()+
  labs(x=element_blank(), 
       y="Fertilizer Consumption", 
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
             size=.5)
  #create legend
######
  #scale_color_manual(name=element_blank(), 
                #     values = c(`Treaty Signed` = "gray7", 
                 #               `Treaty Ratified` = "gray27",
                 #               `Chloride Ion Averages (kg/s)`= "steelblue4",
                  #              `Chloride Concentration (kg/m3)`= "blue3"))+
  
#  theme(legend.position="bottom", legend.background=element_rect
    #    (fill="gray95", linetype=1, size=0.15, color=1))
