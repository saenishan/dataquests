setwd("C:/Users/pathw/Documents/R/Projects/forest fire")
library(readr)
library(dplyr)
library(ggplot2)
forestfires<-read.csv("forestfires.csv")
forestfires <- forestfires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")),day = factor(day, levels = c("sun","mon","tue","wed","thu","fri","sat")))
#n - Use with group_by(). Count the number of rows
firemonth<-forestfires%>%group_by(month)%>%summarize(area=n())
ggplot(data=firemonth) +
  aes(x=month,y=area) +
  geom_bar(stat = "identity")
fireday<-forestfires%>%group_by(day)%>%summarize(area=n())
ggplot(data=fireday) +
  aes(x=day,y=area) +
  geom_bar(stat = "identity")

####Creating box plots

library(purrr)
boxfires<-function(x,y){
ggplot(data=forestfires) +
  aes_string(x=x,y=y) +
  geom_boxplot()+
    theme(panel.background = element_rect(fill = "white"))}

## Assign x and y variable names 
x_var_month <- names(forest_fires)[3] ## month
x_var_day <- names(forest_fires)[4] ## day
y_var <- names(forest_fires)[5:12]

## use the map() function to apply the function to the variables of interest
##I use part of solution from dataquestio due to i think their code on ending is much more feasible than mine
#https://github.com/dataquestio/solutions/blob/master/Mission277Solutions.Rmd
month_box <- map2(x_var_month, y_var, create_boxfires) ## visualize variables by month
day_box <- map2(x_var_day, y_var, create_boxfires) ## visualize variables by day



##scatter plots

scatterfires<-function(x,y){
ggplot(data=forestfires) +
  aes_string(x=x,y=y) +
  geom_point(alpha=0.3)+
    theme(panel.background = element_rect(fill = "white"))}

x_varscat<-names(forestfires)[5:12]
y_varscat<-names(forestfires)[13]

map2(x_varscat,y_varscat,scatterfires)

##histogram

  ggplot(data=forestfires) +
    aes(x=area,fill=month) +
    geom_histogram(binwidth=20)+
    theme(panel.background = element_rect(fill = "pink"))




 