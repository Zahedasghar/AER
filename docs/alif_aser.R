


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

## You can download  data from https://github.com/Zahedasghar/AER/blob/main/data/alif_aser.csv

edf <- read_csv("data/edu_data_kaggle.csv")   ## Read csv file

edf |> glimpse()## To have overview of data    

## 580 row and 51 columns

## variable names are in messy format

View(edf)

## To change these variables to nice format

edf |> clean_names()



edf |> clean_names() |> View()

## Assign this data name as edf_c

edf |> clean_names() -> edf_c

edf_c |> glimpse()
## Lets take year, province and city as first three and
## rename city as district

edf_c |> select(year, province, city, everything())

## Lets assign this data as 
 edf_c |> select(year, province, city, everything()) |> 
   rename(district=city) -> edu_df

## Use select to have variables containing percent 
 
 View(edu_df)
 
 edu_df |> select(contains("percent")) |> 
   View()

edu_df |> select(ends_with("score")) 

## Lets assing this data as scores

edu_sc <- edu_df |> select(year, province, district, everything())



## Distinct years

edu_sc |> distinct(year)

edu_sc |>filter(year==2013) |>  distinct(district) |> count()

edu_sc |> group_by(province) |> distinct(district) |> count()

edu_sc |> group_by(province) |> distinct(district) |> count() |> 
  arrange(-n)

## Count year and district province wise

edf_c |> count(year,province)


edf_c |> count(year,province) |> 
  summarise(n=n())


## Filter only for year 2014 
edu_sc |> filter(year==2014) |> head(4)

edu_sc |> filter(year==2014) |> slice(103:108)

## Minimum rention score district wise
edu_sc |> select(year, province, district,
                 retention_score, everything()) |> 
  filter(year==2013) |> 
  filter(retention_score==min(retention_score), 
         .by=province)


edu_sc |> select(year, province, district,
                 education_score, everything()) |>
  filter(year==2013, province %in% c("Punjab", 
                                     "Sindh"))
                                                                                                                 "KP","Balochistan", "Sindh")) |> filter(education_score==min(education_score), .by=province)



edf_short <- edu_sc |> select(year, province, district, contains("score"))

edf_short |> glimpse() |> View()

## summarise

summary(edf_short)
library(skimr)
edf_short |> skim()

edf_short |> filter(year==2014) |>  select(province,  learning_score) |> 
  group_by(province) |> 
  skim()


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
            .by=c(year,province))

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



## Which district in each province has minimum education score



edf_short |> select(year, province, district, education_score, everything()) |>  filter(year==2013, province %in% c("Punjab",
                                                                                                                 "KP","Balochistan", "Sindh")) |> 
  group_by(province)|> filter(education_score==min(education_score))


## mutate to calculate score index based on all score variables

edf_short |> rowwise() |> mutate(score_index=mean(c_across(c('education_score','retention_score',
                                                             'gender_parity_score',
                                                             'learning_score' , 'enrolment_score',
                                                             'school_infrastructure_score' )))) ->edf_short

## Which district has lowest score index 
edf_short|> select(year, province, district, score_index, everything()) |>  filter(year==2013, province %in% c("Punjab",
                                                                                                                   "KP","Balochistan", "Sindh")) |> 
  group_by(province)|> filter(score_index==min(score_index))


edf_short|> select(year, province, district, score_index, everything()) |>  filter(year==2016, province %in% c("Punjab",
                                                                                                               "KP","Balochistan", "Sindh")) |> 
  group_by(province)|> filter(score_index==max(score_index))


edf_short|> select(year, province, district, score_index, everything()) |>  filter(province %in% c("Punjab",
                                                                                                               "KP","Balochistan", "Sindh")) |> 
  group_by(province,year)|> filter(score_index==max(score_index)) |> arrange(province)

edf_short|> select(year, province, district, score_index, everything()) |>  filter(province %in% c("Punjab",
                                                                                                   "KP","Balochistan", "Sindh")) |> 
  group_by(province,year)|> filter(score_index==min(score_index)) |> arrange(province)


## To make nice tables and quick summary

library(gt)
library(gtsummary)
library(gtExtras)


edf_short |> select(is.numeric, -year) |> gt_plt_summary()

edf_short|> select(year, province, district, score_index, everything()) |>  filter(year==2016, province %in% c("Punjab",
                                                                                                               "KP","Balochistan", "Sindh")) |> 
  group_by(province)|> filter(score_index==max(score_index)) -> table_scr 
table_scr

table_scr |> select(-year) |> gt() |> fmt_number(decimals = 2) |> gt_theme_nytimes()|> 
  tab_header("Minimum education score (index based on score variable) in 2016") |> 
  tab_footnote("source:alifailan, by Zahid Asghar")


countrypops |>
  dplyr::select(country_code_3, year, population) |>
  dplyr::filter(country_code_3 %in% c("CHN", "IND", "USA", "PAK", "IDN")) |>
  dplyr::filter(year > 1975 & year %% 5 == 0) |>
  tidyr::spread(year, population) |>
  dplyr::arrange(desc(`2015`)) |>
  gt(rowname_col = "country_code_3") |>
  fmt_number(suffixing = TRUE)


countrypops |>
  dplyr::select(country_code_3, year, population) |>
  dplyr::filter(country_code_3 %in% c("CHN", "IND", "USA", "PAK", "IDN", "BGD")) |>
  dplyr::filter(year > 1975 & year %% 5 == 0) |>
  tidyr::spread(year, population) |>
  dplyr::arrange(desc(`2015`)) |>
  gt(rowname_col = "country_code_3") |>
  fmt_number(suffixing = TRUE, n_sigfig = 3) |> 
  gt_highlight_rows(rows = 5, fill="red", font_weight = "normal")|> 
  tab_header(title="Pakistan's population: the ticking bomb",
             subtitle ="Had Pakistan observed Bangladesh population growth rate pattern, it would 
             not have been left behind" ) |> 
  tab_footnote("Source:https://data.worldbank.org/indicator/SP.POP.TOTL by: Zahid Asghar")


  
