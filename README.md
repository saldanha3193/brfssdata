---
title: "Exploring the BRFSS data"
output: 
  html_document: 
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages

```{r load-packages, message = FALSE}


library(dplyr)
library(ggplot2)
library(statsr)
library(devtools)
library(rmarkdown)

```

### Load data


```{r load-data}
load("brfss2013.RData")

```

* * *

## Part 1: Data

**Data Generalisability:**

Since data is collected from residents that are 18+ from all the 50 states of the US including Puerto Rico, Gaum and DC, this data is a stratified sample, thus analysis of this large data set can be generalised to the entire US population.
The method of data collection are oral based methods via telephone and cellular connectivity. This method of collection can lead to misinterpreted data, as participants who where contacted may be unwilling/uncomfortable to share personal details, also they might have been contacted at a time or place where they are unable to share personal information, such as place of work. Participants may refuse to answer questions on ones health status. These conditions may result in a bias in the results of the observations.

**Data Causality:**

As this is not an experimental study, and participants are self reporting observations causality cannot be considered. Since there was no experiment conducted and thus no random assignment of participants to conditions, causal relationships cannot be assumed, however correlations can be analyzed from the data.


* * *

## Part 2: Research questions

**Research question 1:**

RQ1.1 Is there a relation between the general health vs the amount of sleep recorded for the adults surveyed ?

National Sleep Foundation guidelines(2015) advise that healthy adults (ages 18-64, being the major demographic of this data set) need between 7 and 9 hours of sleep per night.

As we are aware of the health benefits of good sleeping habits, namely in repair and restoration of the body leading to good health, it would be interesting to see the correlation between average sleep times and the reported heath status of the population.


**Research question 2:**

RQ2.2 Is there a relationship between life satisfaction and income levels?

Across various studies there is no sure correlation between income levels and life satisfaction as life satisfaction is different for different individuals and changes across genders as well, this is also corroborated in the research submitted by Felix Chung and Richard E.Lucas(2015).

Hence it would be interesting to find out the correlation between income levels and life satisfaction for the following brfss data set.

**Research question 3:**
RQ3.3 How does life satisfaction vary between genders based on income level?

The common notion is that women are happier than men even though they are worse off in many measurable ways, like less educated, lower incomes, worse self-reported health. This notion has been found to be misinterpreted as per the findings by Mallory Montgomery in her article dated 2016, where it was found that when the response scales for men and women was normalized, it was found that women are actually less happier than men on average. 

Hence, evaluation of the above pattern in the brfss data set would be interesting as well. It would be interesting to evaluate if on average women are less satisfied compared to men, using income levels as the scale for comparison.

* * *

**References:**

Max Hirshkowitz et al.(2015). National Sleep Foundation's sleep time duration recommendations:methodology and results summary.2015 Mar;1(1):40-43. 

Cheung F, Lucas RE. When does money matter most? Examining the association between income and life satisfaction over the life course. Psychol Aging. 2015 Mar;30(1):120-35.

Montgomery, Mallory. “Reversing the Gender Gap in Happiness : Validating the Use of Life Satisfaction Self-Reports Worldwide ∗ Job market paper.” (2016).

## Part 3: Exploratory data analysis


**Research quesion 1:**

**Data preparation for RQ1:**

-The values to be considered are in columns sleptim1 & genhlth.

-Clean the dataframe(brfss2013) by filtering out null values from the required columns.

-Additionally also remove value 7 and 8 from genhlth as these correspond to response as Don’t know/Not Sure Or Refused/Missing as indicated in the codebook.

-In the sleptim1 column, as per codebook there are some participants who have reported where high sleeptimes like 15hrs, however the number of responses are too low to cause any discrepency in the analysis.

-Select the filtered columns and export into new dataframe RQ1.1 to be further worked with for analysis.

**Data Analysis Steps:**

-Select only columns genhlth and sleptim1 for the dataset RQ1.1.

-Calculate the average sleep time sleptim1_mean from the values in sleptim1 column.

-Plot a column graph with the x="Average sleep time", y="count of the population" and legend is the quality of the general health reported.

-Column graph is chosen as we are comparing data across two categories of variables.

```{r}
RQ1.1 <- brfss2013 %>%
  filter(!is.na(genhlth))%>%
  filter(genhlth!="7")%>%
  filter(genhlth!="8")%>%
  filter(!is.na(sleptim1))
RQ1.1<-RQ1.1%>%
  select(genhlth,sleptim1)%>%
  group_by(genhlth)%>%
  mutate(sleptim1_mean = mean(sleptim1))%>%
  summarise(sleptim1_mean = mean(sleptim1), n=n(), .groups='drop')

ggplot(RQ1.1, aes(genhlth, sleptim1_mean))+
  geom_bar(stat="identity", width=0.5)+
  geom_text(aes(label=round(sleptim1_mean,2)),
            vjust =2, color ="white", size=3.5)+
  labs(title = "Health Status by average amount of sleep time", 
       x="Health Status", y="Average Sleep time")
  
```

**Summary Statistics:**

From the column plots above it can be inferred that individuals who recorded lower level of well being have subsequently lower sleep times. This trend is significant for those who recording well being as Poor recorded average sleep time of 6.74hrs.

**Conclusion:**

The observations reported by the National Sleep Foundation(2015), hold true from the analysis of responses of the participants. It can be inferred that a large fraction of the population surveyed have average sleep time of 7.04hrs or more and recorded well being as good, very good and excellent, hence validating the recommendations for minimum sleep times.


**Research question 2:**

**Data Preparation for RQ2:**

-Filter data to remove null values

-For the purpose of analysis, convert income2 to numerical value as per below: (same as indicated in codebook)

Less than $10,000, 1

Less than $10,000,2

Less than $10,000,3

Less than $10,000,4

Less than $10,000,5

Less than $10,000,6

Less than $50,000,7

&75000 or more, 8

Do the same for the variable lsatisfy--satisfaction with life

Very satisfied,4

Satisfied, 3

Dissatisfied, 2

very dissatisfied,1

The above categorical variable are convert to numerical variables to calculate the average life satisfaction across different income levels. The column plots are plotted as per the frequency of the responses received for each of the categories by the participants.

**Data Analysis:**

column plots are generated to compared the average response of life satisfaction across various levels to analyze and compare the responses from the participants.


```{r}

RQ2.1 <- brfss2013%>%
  filter(!is.na(income2))%>%
  filter(!is.na(lsatisfy))

RQ2.1<-RQ2.1%>%
  select(income2,lsatisfy)%>%
  mutate(income_num = ifelse(income2=="Less than $10,000",1,
                             ifelse(income2=="Less than $15,000",2,
                                  ifelse(income2 =="Less than $20,000",3,
                                         ifelse(income2 =="Less than $25,000",4,
                                                ifelse(income2 =="Less than $35,000",5,
                                                       ifelse(income2 =="Less than $50,000",6,
                                                              ifelse(income2 =="Less than $75,000",7,
                                                                     ifelse(income2 =="$75,000 or more",8,NA))))))))) %>%
  
  mutate(lsatisfy_number = ifelse(lsatisfy == "Very satisfied", 4,
                                ifelse(lsatisfy == "Satisfied", 3, 
                                       ifelse(lsatisfy == "Dissatisfied", 2, 
                                             ifelse(lsatisfy == "Very dissatisfied", 1, NA))))) %>%
  
  group_by(income2)%>%
  summarise(lsatisfy_mean_inc = mean(lsatisfy_number), n=n(), .groups='drop')

ggplot(RQ2.1, aes(x=income2, y=lsatisfy_mean_inc))+
  geom_bar(stat='identity', width=0.5)+
  geom_text(aes(label = round(lsatisfy_mean_inc, 2)), 
            vjust = 1.5, color = "white", size = 3.5) + 
  labs(title = "Average life satisfaction by household income levels", 
       x = "household income levels", y = "Average life satisfaction") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




```

**Summary Statistics:**

From the above plot we can see that there is a direct positive correlation with the reported average life satisfaction and an increase in household income levels.
I.e, with higher income levels, more respondents reported that they are satisfied with life.

**Conclusion:**

It can be noted that as the household income levels move from lower to higher there is steady increase in the reported average life satisfactions. However this does not imply that lower income levels lead to lower life satisfaction, but there seems to be a trend for this particular data set.
Hence there seems to be a positive correlation, and this finding for the survey is in agreement with the finds in the article cited. 

**Research question 3:**

**Data Preparation for RQ3.1:**

-select the brfss2013 data set again.

-we will use personal income levels (X_incomg) of females and males for this research question.

-The lsatisfy - satisfaction in life, variable will be converted to numerical form as done in question RQ2.

-X_incomg will be converted to numeric variable form as per below:
Less than $15,000,1

$15,000 to less than $25,000,2

$25,000 to less than $35,000,3

$35,000 to less than $50,000,4

$50,000 or more,5

**Data Analysis Step:**

Using a column plot the various of average life satisfaction across income levels among the genders. The legend for the plot will include the averages for the female and male respectively.
Here again we will use column charts, as we are comparing the data across different income levels.

```{r}

RQ3.1<-brfss2013%>%
  filter(!is.na(X_incomg))%>%
  filter(!is.na(lsatisfy))%>%
  filter(!is.na(sex))

RQ3.1<-RQ3.1%>%
  select(X_incomg,lsatisfy,sex)%>%
  mutate(income_num = ifelse(X_incomg=="Less than $15,000",1,
                             ifelse(X_incomg=="%15,000 to less than %25000",2,
                                  ifelse(X_incomg=="$25,000 to less than &35000",3,
                                         ifelse(X_incomg=="%35,000 to less than %50,000",4,
                                                ifelse(X_incomg =="$50,000 or more",5,NA)))))) %>%
  
  mutate(lsatisfy_number = ifelse(lsatisfy == "Very satisfied", 4,
                                ifelse(lsatisfy == "Satisfied", 3, 
                                       ifelse(lsatisfy == "Dissatisfied", 2, 
                                             ifelse(lsatisfy == "Very dissatisfied", 1, NA))))) %>%
  group_by(X_incomg,sex)%>%
  summarise(lsatisfy_mean_inc_sex = mean(lsatisfy_number), n=n(), .groups='drop')

ggplot(RQ3.1, aes(x=X_incomg, y=lsatisfy_mean_inc_sex, fill=sex))+
  geom_bar(stat='identity', width=0.5, position=position_dodge())+
  geom_text(aes(label = round(lsatisfy_mean_inc_sex, 2)), 
            vjust = 2, color = "black", position = position_dodge(0.8), size = 3)+
  labs(title = "Average life satisfaction by personal income level and gender",
       x = "Personal Income Level", y = "Average life satisfaction", fill =
         "Gender")+ theme_minimal()+theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

**Summary Statistics:**
From the column plots, it can be inferred that there is no conclusive evidence that women reported lower live satisfaction as compared to men in the same personal income bracket, with the exception for the income levels between %35,000 to less than $50,000 where more females than men reported average life satisfaction.

**Conclusion:**

From the above column plots we can conclude that for a particular income level, both men and women have reported almost equal levels of life satisfaction and dissatisfaction respectively. There is no clear trend observed. 
 

