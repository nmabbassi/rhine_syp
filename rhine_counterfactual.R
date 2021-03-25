library(ggplot2)
library(readxl)
library(carData)


#Counterfactual plots
##Potash Demand
##flow rate
##growing environmentalism in region
treatyrat <- c(xintercept = 1983)
treatyimp <- c(xintercept = 1986)
protocolrat <- c(xintercept = 1991)
protocolimp <- c(xintercept = 1994)

dftest <- c(treatysig = "gray7",
            treatyrat = "gray27")

##### Potash Consumption #####
###fertilizer consumption measures quantity of plant nutrients
###and is calculated as production plus imports minus exports.
###data from The World Bank Group, World Development Indicators
###www.databank.worldbank.org 
fert_consump <- read_excel("~/Desktop/Rhine/FR_fert_consumption.xlsx",
           na="NA") 

#code ggplot
ggplot(fert_consump, aes(x=`Year`))+
  geom_line(aes(y=`Fertilizer`,
                color="        Fertilizer Consumption"))+
  geom_line(aes(y=`Imports`, 
                color="       Agricultural Imports"))+
  geom_line(aes(y=`Exports`, 
                color="     Agricultural Exports"))+
  
  labs(x=element_blank(), 
       y="Fertilizer use in France", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
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
                     values=c( `   Treaty Ratified` = "gray1",
                               `  Treaty Implemented` = "gray1",
                               ` Protocol Ratified` = "gray30",
                               `Protocol Implemented` = "gray30",
                              `        Fertilizer Consumption` = "blue4",
                              `       Agricultural Imports` = "steelblue",
                              `     Agricultural Exports` = "blue1"))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))

#agricultural production
###data from Food and Agricultural Organization of the United Nations
###http://www.fao.org/faostat
ag_production <- read_excel("~/Desktop/Rhine/fr_ag_production.xls",
                           na="NA") 
ggplot(ag_production, aes(x=`Year`))+
  geom_line(aes(y=scale(`beans_dry`),
                color="    Beans, dry"))+
  geom_line(aes(y=scale(`sugar_beet`), 
                color="    Sugar Beets"))+
  geom_line(aes(y=scale(`veggie`), 
                color="    Vegetables, all"))+
  geom_line(aes(y=scale(`roots_tubers`), 
                color="    Roots and Tubers"))+
  geom_line(aes(y=scale(`citrus`), 
                color="    Citrus, fresh"))+
  geom_line(aes(y=scale(`peas_dry`), 
                color="    Peas, dry"))+
  labs(x=element_blank(), 
       y="Scaled Tonnes of Agricultural Production in France", 
       title=element_blank(),
       fill=element_blank(),
       color=element_blank())+
  theme_gray()+ 
  theme(axis.title=element_text(size=12))+
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
                     values=c( `   Treaty Ratified` = "gray1",
                               `  Treaty Implemented` = "gray1",
                               ` Protocol Ratified` = "gray30",
                               `Protocol Implemented` = "gray30",
                               `    Beans, dry` = "coral4",
                               `    Sugar Beets` = "indianred",
                               `    Vegetables, all` = "forestgreen",
                               `    Roots and Tubers` = "burlywood4",
                               `    Citrus, fresh` = "tan2",
                               `    Peas, dry` = "slateblue4"))+
  theme(legend.position="bottom", legend.background=element_rect
        (fill="gray95", linetype=1, size=.15, color=1))

##flow rate
###flow rate in Rhine_graphs.r


##growing environmentalism in region



