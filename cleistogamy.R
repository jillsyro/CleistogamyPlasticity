#These are packages used to analyze data. You will probably have to install these using the install.packages("package name") command
install.packages("tidyverse") 
install.packages("dplyr")
install.packages("ggplot2")
library("tidyverse") 
library("dplyr")
library("ggplot2")

#this is the location (folder) your data file is in. You will need to change it to wherever you saved your data file
setwd("~/Dropbox/Tulane/Plasticity/High:low light plasticity/Data analysis") 

#this is the shortcut way to access the data file and load it into R
finaldata <- read.csv("./Cleistogamy/finaldata.csv")
finaldata <- finaldata[ !(finaldata$Population %in% c("QAL", "SNB2", "TIGR", "WUV")), ] #removes pops w/ < 3 lines

#change vectors to factor and numeric classes as appropriate
finaldata$Population <- as.factor(finaldata$Population)
finaldata$Line <- as.factor(finaldata$Line)
finaldata$Pot <- as.factor(finaldata$Pot)
finaldata$Photoperiod <- as.factor(finaldata$Photoperiod)
finaldata$Days.to.flowering <- as.numeric(finaldata$Days.to.flowering)

#this is an easy way to change a column name, which removes that pesky period after "Cleistogamy" that the dataset was imported with!
finaldata$Cleistogamy <- finaldata$Cleistogamy. ; finaldata$Cleistogamy. <- NULL

#Bring in the elevation data file (just in case)
elevdata<-read.csv("./Cleistogamy/elevdata.csv")
elevdata <- elevdata[ !(elevdata$Population %in% c("ST", "QAL", "SNB2", "TIGR", "WUV")), ] #removes ST pop, pops w/ < 3 lines
elevdata$`Elevation (m)` <- as.numeric(elevdata$`Elevation (m)`) #force it to be a number
elevdata$Population <- factor(elevdata$Population, levels = elevdata$Population) #make pops a factor

#grouping the data
finaldata_sum <- finaldata %>% group_by(Population, Photoperiod) %>% count(Cleistogamy)

#bar plot for overall trends per photoperiod
barplot <- ggplot(data = finaldata_sum)+
  aes(x = Cleistogamy)+
  aes(y = `n`)+
  ylab("Number of individuals")+
  geom_bar(stat = "identity")+
  facet_wrap(~Photoperiod)+
  aes(fill = Cleistogamy)+
  theme_bw()+
  theme(legend.position = "none")
plot(barplot)

#reaction norms grouped by cleistogamy phenotype
rxnnorm_cleist <- ggplot(data=finaldata_sum)+   
  aes(x=Photoperiod)+
  aes(y=n)+
  aes(group=Population)+
  aes(color=Population)+
  aes(fill=Population)+
  geom_point()+
  geom_line()+
  facet_wrap(~Cleistogamy)+
  ylab("Number of individuals")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(rxnnorm_cleist)

#reaction norms grouped by population
rxnnorm_pop <- ggplot(data=finaldata_sum)+   
  aes(x=Photoperiod)+
  aes(y=n)+
  aes(group=Cleistogamy)+
  aes(color=Cleistogamy)+
  geom_point()+
  geom_line()+
  facet_wrap(~Population)+
  ylab("Number of individuals")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(rxnnorm_pop)


