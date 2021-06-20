---
title: "Women, Business And The Law"
subtitle: "Bee Aware"
author: "Burcu Cebecioğlu - Ebru Çelik"
date: "04 06 2021"
output: slidy_presentation
---



## Project goal & Social Benefits

_Our goal in this project is to show the differences between men and women in different indicators such that mobility, workplace, pay, marriage, parenthood, entrepreneurship, assets, and pension.This issue is a considerable social problem and we prefer to choose this topic and our goal is to raise awareness._

![](https://openknowledge.worldbank.org/bitstream/handle/10986/35094/9781464816529.pdf.jpg?sequence=3&isAllowed=y)

## Introduction of data & problem statement
_Our data contains Women, Business and the Law (WBL) data for 190 economies in 2020. Data is provided for 35 different questions across 8 scored indicators. These indicators are mobility, workplace, pay, marriage, parenthood, entrepreneurship, assets, and pension. The WBL index scores are based on the average of each economy’s scores for the 8 topics included in this year’s aggregate score. A higher score indicates more gender equal laws._Our data's dimension is 190x61._
To access the data please click
[DATA](https://datacatalog.worldbank.org/dataset/women-business-and-law)
![](http://blogs.worldbank.org/sites/default/files/blogs-images/2020-01/indicators_graphic.png)

```{r,echo = FALSE,message=FALSE,warning=FALSE}

library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(purrr)
library(tidyr)
library(magrittr)
library(gganimate)

wbl2021<-read_xlsx("C:/Users/BURCUCBC/Desktop/Mat381Proje/data/WBL2021.xlsx")
names(wbl2021)[2]<-"Country"
names(wbl2021)[3]<-"Country_Code"
names(wbl2021)[6]<-"Year"

#View(wbl2021)


```
```{r,echo = FALSE,message=FALSE,warning=FALSE}
#dim(wbl2021)
```
```{r,echo = FALSE,message=FALSE,warning=FALSE}
#str(wbl2021)
```
```{r,echo = FALSE,message=FALSE,warning=FALSE,include = FALSE}
wbl2021 %>%
  group_by(Region)%>%
  summarise(total_country=n())
```

## Number of Countries per Region

```{r}
library(ggplot2)
wbl2021%>%
mutate(Region = factor(Region, levels=c("East Asia & Pacific", "Europe & Central Asia", "High income: OECD", "Latin America & Caribbean","Middle East & North Africa","South Asia","Sub-Saharan Africa"))) %>%
  count(Region)%>%
ggplot(aes(x="",y=n,fill =`Region`))+
   geom_bar(stat = "identity", color = "black") +
  labs(title = "Number of Countries per Region") +
  coord_polar("y") +
  geom_text(aes(label = paste0(n)), position = position_stack(vjust = 0.5))+
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, color = "black"))
```


## Number of Income Levels

```{r}
wbl2021 %>%
  group_by(`Income group`)%>%
  summarise(total_number=n())

```

```{r}
wbl2021%>%
mutate(`Income group` = factor(`Income group`, levels=c("Low income", "Lower middle income", "Upper middle income","High income")))%>%
  count(`Income group`)%>%
ggplot(aes(x = "", y =n, fill = `Income group`)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Number of Income Levels") +
  coord_polar("y") +
  geom_text(aes(label = paste0(n)), position = position_stack(vjust = 0.5))+
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, color = "black"))
```

## Mobility Scores by Region
```{r}
  
mytheme <- theme_set(theme_minimal()) 
mytheme <- theme_update(panel.grid = element_line(linetype="solid", size = 0.4),
           panel.grid.major.x = element_blank(), 
           panel.grid.minor= element_blank(), 
           plot.title = element_text(face="bold"), 
           plot.caption = element_text(hjust = 0))
  
```

```{r}
wbl2021%>%
  mutate(MOBILITY = factor(MOBILITY, levels=c(0,25,50,75,100))) %>% 
  group_by(Region)%>%
  count(MOBILITY)%>%
ggplot(aes(x=Region, y=n, fill=MOBILITY)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,40)) +
  labs(title="Mobility Scores by Region", 
      y="", x="",fill="Mobility Score")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=15),
        legend.title=element_text(color="black",size=9))
```

```{r}
names(wbl2021)[9]<-"M1"
names(wbl2021)[10]<-"M2"
names(wbl2021)[11]<-"M3"
names(wbl2021)[12]<-"M4"
#View(wbl2021)
```
## Mobility Questions

```{r}
wbl2021%>%
  mutate(M1 = factor(M1, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(M1)%>%
ggplot(aes(x=Region, y=n, fill=M1)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,40)) +
  labs(title="Can a woman apply for a passport in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="red4", "No"="red3"))
```
```{r}
wbl2021%>%
  mutate(M2 = factor(M2, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(M2)%>%
ggplot(aes(x=Region, y=n, fill=M2)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title=" Can a woman travel outside the country in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="red4", "No"="red3"))
```


```{r}
wbl2021%>%
  mutate(M3 = factor(M3, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(M3)%>%
ggplot(aes(x=Region, y=n, fill=M3)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title=" Can a woman travel outside her home in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="red4", "No"="red3"))
```
```{r}
wbl2021%>%
  mutate(M4 = factor(M4, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(M4)%>%
ggplot(aes(x=Region, y=n, fill=M4)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman choose where to live in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="red4", "No"="red3"))
```

## Workplace Scores by Region

```{r}
wbl2021%>%
  mutate(WORKPLACE = factor(WORKPLACE, levels=c(0,25,50,75,100))) %>% 
  group_by(Region)%>%
  count(WORKPLACE)%>%
ggplot(aes(x=Region, y=n, fill=WORKPLACE)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,40)) +
  labs(title="Workplace Scores by Region", 
      y="", x="",fill="Workplace Score")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=15),
        legend.title=element_text(color="black",size=9))
```
```{r}
names(wbl2021)[14]<-"W1"
names(wbl2021)[15]<-"W2"
names(wbl2021)[16]<-"W3"
names(wbl2021)[17]<-"W4"
#View(wbl2021)
```
## Workplace Questions

```{r}
wbl2021%>%
  mutate(W1 = factor(W1, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(W1)%>%
ggplot(aes(x=Region, y=n, fill=W1)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title=" Can a woman get a job in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="gray49", "No"="gray79"))
```

```{r}
wbl2021%>%
  mutate(W2 = factor(W2, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(W2)%>%
ggplot(aes(x=Region, y=n, fill=W2)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Does the law prohibit discrimination in employment based on gender?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="gray49", "No"="gray79"))
```
```{r}
wbl2021%>%
  mutate(W3 = factor(W3, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(W3)%>%
ggplot(aes(x=Region, y=n, fill=W3)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is there legislation on sexual harassment in employment?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="gray49", "No"="gray79"))
```
```{r}
wbl2021%>%
  mutate(W4 = factor(W4, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(W4)%>%
ggplot(aes(x=Region, y=n, fill=W4)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Are there criminal penalties or civil remedies for sexual harassment \n in employment?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="gray49", "No"="gray79"))

```

## Pay Scores by Region

```{r}
wbl2021%>%
  mutate(PAY = factor(PAY, levels=c(0,25,50,75,100))) %>% 
  group_by(Region)%>%
  count(PAY)%>%
ggplot(aes(x=Region, y=n, fill=PAY)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,30)) +
  labs(title="Pay Scores by Region", 
      y="", x="",fill="Pay Score")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=15),
        legend.title=element_text(color="black",size=9))
```
```{r}
names(wbl2021)[19]<-"P1"
names(wbl2021)[20]<-"P2"
names(wbl2021)[21]<-"P3"
names(wbl2021)[22]<-"P4"
#View(wbl2021)
```
## Pay Questions

```{r}
wbl2021%>%
  mutate(P1 = factor(P1, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(P1)%>%
ggplot(aes(x=Region, y=n, fill=P1)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Does the law mandate equal remuneration for work of equal value?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="seagreen1", "No"="seagreen"))
```
```{r}
wbl2021%>%
  mutate(P2 = factor(P2, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(P2)%>%
ggplot(aes(x=Region, y=n, fill=P2)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman work at night in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="seagreen1", "No"="seagreen"))
```
```{r}
wbl2021%>%
  mutate(P3 = factor(P3, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(P3)%>%
ggplot(aes(x=Region, y=n, fill=P3)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman work in a job deemed dangerous in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="seagreen1", "No"="seagreen"))
```
```{r}
wbl2021%>%
  mutate(P4 = factor(P4, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(P4)%>%
ggplot(aes(x=Region, y=n, fill=P4)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman work in an industrial job in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="seagreen1", "No"="seagreen"))

```

## Marriage Scores by Region

```{r}
wbl2021%>%
  mutate(MARRIAGE = factor(MARRIAGE, levels=c(0,20,40,60,80,100))) %>% 
  group_by(Region)%>%
  count(MARRIAGE)%>%
ggplot(aes(x=Region, y=n, fill=MARRIAGE)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,30)) +
  labs(title="Marriage Scores by Region", 
      y="", x="",fill="Marriage Score")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=15),
        legend.title=element_text(color="black",size=9))
```
```{r}
names(wbl2021)[24]<-"MR1"
names(wbl2021)[25]<-"MR2"
names(wbl2021)[26]<-"MR3"
names(wbl2021)[27]<-"MR4"
names(wbl2021)[28]<-"MR5"
#View(wbl2021)
```
## Marriage Questions

```{r}
wbl2021%>%
  mutate(MR1 = factor(MR1, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(MR1)%>%
ggplot(aes(x=Region, y=n, fill=MR1)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is there no legal provision that requires a married woman to obey her husband?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="mediumorchid1", "No"="mediumorchid4"))
```
```{r}
wbl2021%>%
  mutate(MR2 = factor(MR2, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(MR2)%>%
ggplot(aes(x=Region, y=n, fill=MR2)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman be head of household in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="mediumorchid1", "No"="mediumorchid4"))
```
```{r}
wbl2021%>%
  mutate(MR3 = factor(MR3, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(MR3)%>%
ggplot(aes(x=Region, y=n, fill=MR3)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is there legislation specifically addressing domestic violence?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="mediumorchid1", "No"="mediumorchid4"))
```
```{r}
wbl2021%>%
  mutate(MR4 = factor(MR4, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(MR4)%>%
ggplot(aes(x=Region, y=n, fill=MR4)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman obtain a judgment of divorce in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="mediumorchid1","No"="mediumorchid4"))

```
```{r}
wbl2021%>%
  mutate(MR5 = factor(MR5, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(MR5)%>%
ggplot(aes(x=Region, y=n, fill=MR5)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Does a woman have the same rights to remarry as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="mediumorchid1","No"="mediumorchid4"))
```

## Parenthood Scores by Region

```{r}
wbl2021%>%
  mutate(PARENTHOOD = factor(PARENTHOOD, levels=c(0,20,40,60,80,100))) %>% 
  group_by(Region)%>%
  count(PARENTHOOD)%>%
ggplot(aes(x=Region, y=n, fill=PARENTHOOD)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,30)) +
  labs(title="Parenthood Scores by Region", 
      y="", x="",fill="Parenthood Score")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=15),
        legend.title=element_text(color="black",size=9))
```
```{r}
names(wbl2021)[30]<-"PR1"
names(wbl2021)[31]<-"PR2"
names(wbl2021)[32]<-"PR3"
names(wbl2021)[33]<-"PR4"
names(wbl2021)[34]<-"PR5"
names(wbl2021)[35]<-"PR6"
names(wbl2021)[36]<-"PR7"
names(wbl2021)[37]<-"PR8"
names(wbl2021)[38]<-"PR9"
names(wbl2021)[39]<-"PR10"
#View(wbl2021)
```
## Parenthood Questions

```{r}
wbl2021%>%
  mutate(PR1 = factor(PR1, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PR1)%>%
ggplot(aes(x=Region, y=n, fill=PR1)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is paid leave of at least 14 weeks available to mothers?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="orangered","No"="orange"))
```
```{r}
wbl2021%>%
  mutate(PR3 = factor(PR3, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PR3)%>%
ggplot(aes(x=Region, y=n, fill=PR3)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Does the government administer 100% of maternity leave benefits?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="orangered","No"="orange"))
```
```{r}
wbl2021%>%
  mutate(PR4 = factor(PR4, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PR4)%>%
ggplot(aes(x=Region, y=n, fill=PR4)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is there paid leave available to fathers?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="orangered","No"="orange"))
```
```{r}
wbl2021%>%
  mutate(PR6 = factor(PR6, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PR6)%>%
ggplot(aes(x=Region, y=n, fill=PR6)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is there paid parental leave?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="orangered","No"="orange"))
```
```{r}
wbl2021%>%
  mutate(PR10 = factor(PR10, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PR10)%>%
ggplot(aes(x=Region, y=n, fill=PR10)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is dismissal of pregnant workers prohibited?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="orangered","No"="orange"))
```

## Entrepreneurship Scores by Region

```{r}
wbl2021%>%
  mutate(ENTREPRENEURSHIP = factor(ENTREPRENEURSHIP, levels=c(0,25,50,75,100))) %>% 
  group_by(Region)%>%
  count(ENTREPRENEURSHIP)%>%
ggplot(aes(x=Region, y=n, fill=ENTREPRENEURSHIP)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,40)) +
  labs(title="Entrepreneurship Scores by Region", 
      y="", x="",fill="Entrepreneurship Score")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=15),
        legend.title=element_text(color="black",size=9))

```
```{r}
names(wbl2021)[41]<-"E1"
names(wbl2021)[42]<-"E2"
names(wbl2021)[43]<-"E3"
names(wbl2021)[44]<-"E4"
#View(wbl2021)
```

## Entrepreneurship Questions 

```{r}
wbl2021%>%
  mutate(E1 = factor(E1, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(E1)%>%
ggplot(aes(x=Region, y=n, fill=E1)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman sign a contract in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="darkslategrey", "No"="darkslategray4"))
```
```{r}
wbl2021%>%
  mutate(E2 = factor(E2, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(E2)%>%
ggplot(aes(x=Region, y=n, fill=E2)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman register a business in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="darkslategrey", "No"="darkslategray4"))
```
```{r}
wbl2021%>%
  mutate(E3 = factor(E3, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(E3)%>%
ggplot(aes(x=Region, y=n, fill=E3)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Can a woman open a bank account in the same way as a man?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="darkslategrey", "No"="darkslategray4"))
```
```{r}
wbl2021%>%
  mutate(E4 = factor(E4, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(E4)%>%
ggplot(aes(x=Region, y=n, fill=E4)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Does the law prohibit discrimination in access to credit based on gender?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="darkslategrey", "No"="darkslategray4"))
```

## Assets Scores by Region


```{r}
wbl2021%>%
  mutate(ASSETS = factor(ASSETS, levels=c(0,20,40,60,80,100))) %>% 
  group_by(Region)%>%
  count(ASSETS)%>%
ggplot(aes(x=Region, y=n, fill=ASSETS)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,40)) +
  labs(title="Assets Scores by Region", 
      y="", x="",fill="Assets Score")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=15),
        legend.title=element_text(color="black",size=9))
```
```{r}
names(wbl2021)[46]<-"A1"
names(wbl2021)[47]<-"A2"
names(wbl2021)[48]<-"A3"
names(wbl2021)[49]<-"A4"
names(wbl2021)[50]<-"A5"
#View(wbl2021)
```

## Assets Questions

```{r}
wbl2021%>%
  mutate(A1 = factor(A1, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(A1)%>%
ggplot(aes(x=Region, y=n, fill=A1)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Do men and women have equal ownership rights to immovable property?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="#355e3b", "No"="#ffd700"))
```
```{r}
wbl2021%>%
  mutate(A2 = factor(A2, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(A2)%>%
ggplot(aes(x=Region, y=n, fill=A2)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Do sons and daughters have equal rights to inherit assets from their parents?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="#355e3b", "No"="#ffd700"))
```
```{r}
wbl2021%>%
  mutate(A3 = factor(A3, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(A3)%>%
ggplot(aes(x=Region, y=n, fill=A3)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Do male and female surviving spouses have equal rights to inherit assets?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="#355e3b", "No"="#ffd700"))
```
```{r}
wbl2021%>%
  mutate(A4 = factor(A4, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(A4)%>%
ggplot(aes(x=Region, y=n, fill=A4)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Does the law grant spouses equal administrative authority over assets during marriage?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="#355e3b", "No"="#ffd700"))
```
```{r}
wbl2021%>%
  mutate(A5 = factor(A5, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(A5)%>%
ggplot(aes(x=Region, y=n, fill=A5)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Does the law provide for the valuation of nonmonetary contributions?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="#355e3b", "No"="#ffd700"))
```

## Pension Scores by Region

```{r}
wbl2021%>%
  mutate(PENSION = factor(PENSION, levels=c(0,25,50,75,100))) %>% 
  group_by(Region)%>%
  count(PENSION)%>%
ggplot(aes(x=Region, y=n, fill=PENSION)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,30)) +
  labs(title="Pension Scores by Region", 
      y="", x="",fill="Pension Score")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=15),
        legend.title=element_text(color="black",size=9))
```

```{r}
names(wbl2021)[52]<-"PS1"
names(wbl2021)[55]<-"PS2"
names(wbl2021)[58]<-"PS3"
names(wbl2021)[61]<-"PS4"

#View(wbl2021)
```

## Pension Questions

```{r}
wbl2021%>%
  mutate(PS1 = factor(PS1, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PS1)%>%
ggplot(aes(x=Region, y=n, fill=PS1)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is the age at which men and women can retire with full pension benefits the same?",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="cornsilk4", "No"="cornsilk3"))
```
```{r}
wbl2021%>%
  mutate(PS2 = factor(PS2, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PS2)%>%
ggplot(aes(x=Region, y=n, fill=PS2)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is the age at which men and women can retire with partial pension benefits the same?
",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="cornsilk4", "No"="cornsilk3"))
```
```{r}
wbl2021%>%
  mutate(PS3 = factor(PS3, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PS3)%>%
ggplot(aes(x=Region, y=n, fill=PS3)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Is the mandatory retirement age for men and women the same?
",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="cornsilk4", "No"="cornsilk3"))
```
```{r}
wbl2021%>%
  mutate(PS4 = factor(PS4, levels=c("Yes","No"))) %>% 
  group_by(Region)%>%
  count(PS4)%>%
ggplot(aes(x=Region, y=n, fill=PS4)) +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(limits = c(0,50)) +
  labs(title="Are periods of absence due to childcare accounted for in pension benefits?
",y="", x="",fill="")+
  theme(axis.text.x= element_text(angle=90,size=10,color="black"),
        plot.title=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=9))+
  scale_fill_manual(values=c("Yes"="cornsilk4", "No"="cornsilk3"))
```

## East Asia & Pacific

```{r}

library(ggplot2)
wbl2021%>%
  filter(Region=="East Asia & Pacific")%>%
  select(Region,Country,`WBL INDEX`)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="East Asia & Pacific")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="skyblue") +
  geom_point( color="blue", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12)
  )
```

## Europe & Central Asia

```{r}
library(ggplot2)
wbl2021%>%
  filter(Region=="Europe & Central Asia")%>%
  select(Region,Country,`WBL INDEX`)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="Europe & Central Asia ")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="chartreuse3") +
  geom_point( color="chartreuse", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12)
  )

```

## High income: OECD 

```{r}
library(ggplot2)
wbl2021%>%
  filter(Region=="High income: OECD")%>%
  select(Region,Country,`WBL INDEX`)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="High income: OECD")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="red3") +
  geom_point( color="red", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12)
  )
  

```

## Latin America & Caribbean

```{r}
wbl2021%>%
  filter(Region=="Latin America & Caribbean")%>%
  select(Region,Country,`WBL INDEX`)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="Latin America & Caribbean")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="yellow3") +
  geom_point( color="yellow", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12)
  )

```

## Middle East & North Africa

```{r}
wbl2021%>%
  filter(Region=="Middle East & North Africa")%>%
  select(Region,Country,`WBL INDEX`)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="Middle East & North Africa")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="sienna3") +
  geom_point( color="sienna1", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12)
  )

```

## South Asia

```{r}
library(ggplot2)
wbl2021%>%
  filter(Region=="South Asia")%>%
  select(Region,Country,`WBL INDEX`)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="South Asia")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="gold") +
  geom_point( color="darkorange", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12)
  )

```

## Sub-Saharan Africa

```{r}
library(ggplot2)
wbl2021%>%
  filter(Region=="Sub-Saharan Africa")%>%
  select(Region,Country,`WBL INDEX`)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="Sub-Saharan Africa")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="chartreuse4") +
  geom_point( color="chartreuse2", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=5),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12)
  )
```

## Conclusion

_We analyzed our data for 2021 and tried to explain it with various visualizations for 8 indicators. According to these results, although we are in 2021, we unfortunately see that the inequality between men and women in many different fields and countries still continues.
For this, we wanted to show the wbl index change from 1971 to 2021, which is on a different page of our data. We believe that awareness of this issue will increase over time and that we can solve this problem with education._

![](https://wbl.worldbank.org/content/dam/photos/780x439/2020/mar/WBL_Banner_022820_OP1.jpg)

```{r,echo = FALSE,message=FALSE,warning=FALSE}
wbl2021<-read_xlsx("C:/Users/BURCUCBC/Desktop/Mat381Proje/data/WBL_1971_2021.xlsx")
names(wbl2021)[2]<-"Country"
names(wbl2021)[3]<-"Country_Code"
names(wbl2021)[6]<-"Year"
```

## WBL INDEX Change Between 1971-2021

```{r}
data_1<-wbl2021%>%
  filter(Region=="East Asia & Pacific")%>%
  select(Region,Country,`WBL INDEX`,Year)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="East Asia & Pacific \n Year: {as.integer(frame_time)}")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="skyblue") +
  geom_point( color="blue", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12))+
  transition_time(Year)
animate(data_1,fps=50,nframes=500,width=1200)
data_2<-animate(data_1, ,nframes=500, fps=50,renderer=gifski_renderer())
anim_save("East Asia & Pacific.gif")

      
```

## WBL INDEX Change Between 1971-2021

```{r}
library(ggplot2)
data_3<-wbl2021%>%
  filter(Region=="Europe & Central Asia")%>%
  select(Region,Country,`WBL INDEX`,Year)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="Europe & Central Asia \n Year: {as.integer(frame_time)}")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="chartreuse3") +
  geom_point( color="chartreuse", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
   plot.title=element_text(color="black",hjust=0.5,size=12))+
  transition_time(Year)
animate(data_3,fps=50,nframes=500,width=1200)
data_4<-animate(data_3, ,nframes=500, fps=50,renderer=gifski_renderer())
anim_save("Europe & Central Asia.gif")
```

## WBL INDEX Change Between 1971-2021

```{r}
library(ggplot2)
data_5<-wbl2021%>%
  filter(Region=="High income: OECD")%>%
  select(Region,Country,`WBL INDEX`,Year)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="High income: OECD \n Year: {as.integer(frame_time)}")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="red3") +
  geom_point( color="red", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12))+
  transition_time(Year)
animate(data_5,fps=50,nframes=500,width=1200)
data_6<-animate(data_5, ,nframes=500, fps=50,renderer=gifski_renderer())
anim_save("High income: OECD.gif")
```

## WBL INDEX Change Between 1971-2021

```{r}
library(ggplot2)
data_7<-wbl2021%>%
  filter(Region=="Latin America & Caribbean")%>%
  select(Region,Country,`WBL INDEX`,Year)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="Latin America & Caribbean \n Year: {as.integer(frame_time)}")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="yellow3") +
  geom_point( color="yellow", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12))+
  transition_time(Year)
animate(data_7,fps=50,nframes=500,width=1200)
data_8<-animate(data_1, ,nframes=500, fps=50,renderer=gifski_renderer())
anim_save("Latin America & Caribbean.gif")
   
```

## WBL INDEX Change Between 1971-2021

```{r}
library(ggplot2)
data_9<-wbl2021%>%
  filter(Region=="Middle East & North Africa")%>%
  select(Region,Country,`WBL INDEX`,Year)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="Middle East & North Africa \n Year: {as.integer(frame_time)}")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="sienna3") +
  geom_point( color="sienna1", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12))+
  transition_time(Year)
animate(data_9,fps=50,nframes=500,width=1200)
data_10<-animate(data_9, ,nframes=500, fps=50,renderer=gifski_renderer())
anim_save("Middle East & North Africa.gif")
    

```

## WBL INDEX Change Between 1971-2021

```{r}
library(ggplot2)
data_11<-wbl2021%>%
  filter(Region=="South Asia")%>%
  select(Region,Country,`WBL INDEX`,Year)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="South Asia \n Year: {as.integer(frame_time)}")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="gold") +
  geom_point( color="darkorange", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12))+
  transition_time(Year)
animate(data_11,fps=50,nframes=500,width=1200)
data_12<-animate(data_11, ,nframes=500, fps=50,renderer=gifski_renderer())
anim_save("South Asia.gif")
    

```

## WBL INDEX Change Between 1971-2021

```{r}
library(ggplot2)
data_13<-wbl2021%>%
  filter(Region=="Sub-Saharan Africa")%>%
  select(Region,Country,`WBL INDEX`,Year)%>%
ggplot(aes(x=Country, y=`WBL INDEX`)) +
  labs(title="Sub-Saharan Africa \n Year: {as.integer(frame_time)}")+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=`WBL INDEX`), color="chartreuse4") +
  geom_point( color="chartreuse2", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=5),
    axis.title.x=element_text(size=8,hjust=1),
    plot.title=element_text(color="black",hjust=0.5,size=12))+
  transition_time(Year)
animate(data_13,fps=50,nframes=500,width=1200)
data_14<-animate(data_13, ,nframes=500, fps=50,renderer=gifski_renderer())
anim_save("Sub-Saharan Africa.gif")
  
```

## References

[DATA](https://datacatalog.worldbank.org/dataset/women-business-and-law)

[GRAPH](https://www.data-to-viz.com/)

[COLOR](http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)

Lecture Notes

