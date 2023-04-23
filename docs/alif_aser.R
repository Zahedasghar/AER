## Lesson Plan

## Reading Data
## Using Required package
## data frame
## glimpse (to have an overview of data, rows, variables, variables nature)
## structure of data with `str` command , similar as glimpse from base R
## head
## tail
## View

## glimpse()  or str()

## select()

## filter()


## arrange()

## mutate()

## summarise  (mean, min, max, median, sd,q1, q3)









library(tidyverse)
library(readr) ## To read csv file

library(janitor)## For cleaning data

## Now read data of ASER and Alifailan 2013-2016

edf <- read_csv("docs/data/edu_data_kaggle.csv")   ## Read csv file

edf |> glimpse()## To have overview of data

## 580 row and 51 columns

## variable names are in messy format

View(edf)

## To change these variables to nice format

edf |> clean_names()

edf |> clean_names() |> View()

## Assign this data name as edf_c

edf |> clean_names() -> edf_c


## Lets take year, province and city as first three and
## rename city as district

edf_c |> select(year, province, city, everything())

## Lets assign this data as 
 edf_c |> select(year, province, city, everything()) |> rename(district=city) -> edu_df

## Use select to have variables containing percent 
 
 edu_df |> select(contains("percent"))

edu_df |> select(ends_with("score")) 

## Lets assing this data as scores

edu_sc <- edu_df |> select(year, province, district, everything())



## Distinct years

edu_sc |> distinct(year)

edu_sc |> distinct(district) |> count()

edu_sc |> group_by(province) |> distinct(district) |> count()

edu_sc |> group_by(province) |> distinct(district) |> count() |> 
  arrange(-n)

## Count year and district province wise

edf_c |> count(year,province)


edf_c |> count(year,province) |> 
  summarise(n=n())


## Filter only for year 2014 
edu_sc$

edu_sc |> filter(year==2014) |> head(4)

edu_sc |> filter(year==2014) |> slice(103:108)

## Minimum rention score district wise
edu_sc |> select(year, province, district, retention_score, everything()) |>  filter(year==2013) |> filter(retention_score==min(retention_score), .by=province)


edu_sc |> select(year, province, district, education_score, everything()) |>  filter(year==2013, province %in% c("Punjab",
                                                                                                                 "KP","Balochistan", "Sindh")) |> filter(education_score==min(education_score), .by=province)



edf_short <- edu_sc |> select(year, province, district, contains("score"))

edf_short$enrolment_score
## mutate

edf_short |> rowwise() |> mutate(score_index=mean(c_across(c('education_score','retention_score',
                                           'gender_parity_score',
                                          'learning_score' , 'enrolment_score',
                                         'school_infrastructure_score' )))) ->edf_short

edf_short |> glimpse()

## summarise

summary(edf_short)


edf_short |> filter(province %in% c("Punjab","Balochistan","Sindh", "KP")) |> 
  summarise(mean_learn=mean(learning_score), 
                      q1=quantile(learning_score,probs = 0.25),
                       med_learn=median(learning_score),
                       q3=quantile(learning_score,probs = 0.75),
                       min_learn=min(learning_score),
                       max_learn=max(learning_score),
                  
                       sd=sd(learning_score),
                       .by=province)



edf_short |> filter(province %in% c("Punjab","Balochistan","Sindh", "KP")) |> 
  summarise(
            mean_inf=mean(school_infrastructure_score),
            med_inf=median(school_infrastructure_score),
           q3=quantile(school_infrastructure_score,probs = 0.75),
            min_inf=min(school_infrastructure_score),
            max_inf=max(school_infrastructure_score),
           
            sd=sd(school_infrastructure_score),.by=province)


## Quantitles

edf_short |> filter(province %in% c("Punjab","Balochistan","Sindh", "KP")) |> 
summarise(q1=quantile(learning_score,probs = 0.25),
          med_learn=median(learning_score),
          min_learn=min(learning_score),
          max_learn=max(learning_score),
          sd=sd(learning_score),
          .by=province)



edf_short |> filter(province %in% c("Punjab","Balochistan","Sindh", "KP")) |> 
  summarise(
    q1=quantile(school_infrastructure_score,probs = 0.25),
    med_inf=median(school_infrastructure_score),
    q3=quantile(school_infrastructure_score,probs = 0.75),
    min_inf=min(school_infrastructure_score),
    max_inf=max(school_infrastructure_score),
   .by=province)


## Summarise by year, and by province and year 

edf_short |> filter(province %in% c("Punjab","Balochistan","Sindh", "KP")) |> 
  summarise(mean_learn=mean(learning_score), 
            q1=quantile(learning_score,probs = 0.25),
            med_learn=median(learning_score),
            q3=quantile(learning_score,probs = 0.75),
            min_learn=min(learning_score),
            max_learn=max(learning_score),
            
            sd=sd(learning_score),
            .by=year)

edf_short |> filter(province %in% c("Punjab","Balochistan","Sindh", "KP")) |> 
  summarise(mean_learn=mean(learning_score), 
            q1=quantile(learning_score,probs = 0.25),
            med_learn=median(learning_score),
            q3=quantile(learning_score,probs = 0.75),
            min_learn=min(learning_score),
            max_learn=max(learning_score),
            
            sd=sd(learning_score),
            .by=c(province,year))



edf_short |> filter(province =="Sindh") |> 
  summarise(mean_learn=mean(learning_score), 
            q1=quantile(learning_score,probs = 0.25),
            med_learn=median(learning_score),
            q3=quantile(learning_score,probs = 0.75),
            min_learn=min(learning_score),
            max_learn=max(learning_score),
            
            sd=sd(learning_score),
            .by=c(province,year))

edf_c |> glimpse()


