library(tidyverse)

##https://ggplot2tor.com/aesthetics/

gss_cat |> 
  ggplot(aes(x=marital))+
  geom_bar()

gss_cat |> 
  summarise(mean_tv=mean(tvhours,na.rm = TRUE),
            sd_tv=sd(tvhours,na.rm=TRUE),.by=marital) |> 
  ggplot(aes(x=mean_tv,
             ymin=mean_tv-sd_tv,
             ymax=mean_tv+sd_tv))+
  geom_errorbar()

gss_cat |> group_by(marital) |> 
  summarise(mean_tv=mean(tvhours,na.rm = TRUE),
            sd_tv=sd(tvhours,na.rm=TRUE)) |> 
  ungroup() |> 
  ggplot(aes(x=mean_tv,
             ymin=mean_tv-sd_tv,
             ymax=mean_tv+sd_tv))+
  geom_errorbar()
