getwd()
setwd("C:/Users/shiva/Desktop/R working directory/EXTRA")

data<- read.csv("C:/Users/shiva/Downloads/vehicle (2).csv", header = T)

View(data)
str(data)
summary(data)
data$State<- as.factor(data$State)

library(dplyr)
#FILTER

car<- data %>% filter(State=="TX" | State=="CA")
dim(car)
View(car)


#ARRANGE
car1<- data %>% filter(State=="TX" | State=="CA") %>% arrange(Mileage)

#SUMMARISE
car3<- data %>% filter(State=="TX" | State=="CA") %>% group_by(State) %>%  summarise(Average=mean(Mileage))
 

data %>% group_by(State) %>% summarise(AVERAGE=mean(Mileage))

data %>% group_by(State) %>% summarise(AVERAGE=mean(Mileage),
                                       SD= sd(Mileage),
                                       MAX= max(Mileage),
                                       MIN= min(Mileage),
                                       Count= n())


data %>% group_by(State) %>% summarise(AVERAGE=mean(lc),
                                       SD= sd(lc),
                                       MAX= max(lc),
                                       MIN= min(lc),
                                       Count= n()) %>% arrange(desc(AVERAGE))

 

#MUTATE
data %>% group_by(State)  %>% mutate(CPH= lc/lh) %>% summarise(Avg= mean(CPH)) %>%arrange(desc(Avg))
 


 
#VISUALIZATIONS
library(ggplot2)

#HISTOGRAMS
data %>% filter(State=="TX" | State=="CA") %>% ggplot(aes(x=lc,fill=State))+ geom_histogram()+ ggtitle("Labour cost in CA and Tx")+facet_wrap(~State)


#INTERACTIVE PLOTS
library(highcharter)

#BOXPLOT
hcboxplot(data$Mileage,data$State)
hcboxplot(data$Mileage,data$State, color="darkred")


# GEO MAP
data1<-data %>% group_by(State) %>% summarise(count=n())
data1

#highcharter package doesnt accept States names
#as CA,TX it always accepts full names, so we use OPENINTRO PACKAGE

library(openintro)

data1$State<- abbr2state(data1$State)

highchart() %>% 
  hc_add_series_map(usgeojson,data1,name="State",
                    value = "count",
                    joinBy = c("woename","State")) %>% 
  hc_mapNavigation(enable=T)
  


#NETWORK DIAGRAM
library(igraph)

g<-graph(c(1,2,2,3,3,4,4,1))
plot(g)
plot(g,vertex.color="green",vertex.size=30,edge.color="red")


g1<-graph(c(1,2,2,3,3,4,4,1),directed = F,n=8)
plot(g1)
