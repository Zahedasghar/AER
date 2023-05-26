library(tidyverse)
library(janitor)
#install.packages("ggtext")
library(ggtext)

gapminder_2007_1952 <- gapminder::gapminder |> janitor::clean_names() |> 
  filter(year %in% c(1952,2007)) |> 
  mutate(year=factor(year))

selected_countries <- gapminder_2007_1952 |> filter(country %in% c("Pakistan","India",
                                                        "Bangladesh","Sri Lanka","Nepal","Bhutan"))

color_palette <- c("#E69F00","#009E73")
names(color_palette) <- c(1952,2007)

selected_countries |> 
  ggplot(aes(x=life_exp,y=country,col=year))+
  geom_point(size=4)+
  scale_color_manual(values = color_palette)+
  theme_minimal(base_size = 16,base_family = "Merriweather")+
  theme(legend.position = "none")

title_text <- glue::glue(
  "Comparison of life expectancies between <span style=
  'color:{color_palette['1952']}'>2007</span>"
)

selected_countries |> 
  ggplot(aes(x=life_exp,y=country,col=year))+
  geom_point(size=4)+
  scale_color_manual(values = color_palette)+
  theme_minimal(
    base_size = 16, base_family = 'Merriweather'
  )+
  theme(legend.position = 'none')


segment_helper <- selected_countries |>
  select(country, year, life_exp) |>
  pivot_wider(names_from = year,
              values_from = life_exp,
              names_prefix = 'year_') |>
  mutate(change = year_2007 - year_1952,
         country = fct_reorder(country,
                               year_2007 * if_else(change < 0, -1, 1)))

selected_countries |> 
  ggplot(aes(x=life_exp,y=country,col=year))+
  geom_segment(data=segment_helper,
               aes(x=year_1952, xend=year_2007,
                   y=country, yend=country),
               col='grey60',
               linewidth=1.25)+
  geom_point(size=4)+
  
  scale_color_manual(values = color_palette)+
  labs(x='Life Expectancy',y=element_blank(),
       title = title_text)+
  theme_minimal(
    base_size = 16, base_family = 'Merriweather'
  )+
  theme(legend.position = 'none',
        plot.title = ggtext::element_markdown(),
        plot.title.position = 'plot')


df %>% 
  ggplot(aes(x= lifeExp, y= reorder(country,lifeExp), fill=year)) +
  geom_col(position="dodge")+
  labs(y="Country")

library(gapminder)

df <- gapminder %>%
  filter(year %in% c(1952,2007)) %>%
  filter(continent=="Asia")

df <- df %>%
  mutate(paired = rep(1:(n()/2),each=2),
         year=factor(year))



df %>% 
  ggplot(aes(x= lifeExp, y= reorder(country,lifeExp))) +
  geom_line(aes(group = paired),color="grey")+
  geom_point(aes(color=year), size=6) +
  labs(y="country")+
  theme_classic(24)+
  theme(legend.position="top") +
  scale_color_brewer(palette="Accent", direction=-1)

df %>% 
  ggplot(aes(x= lifeExp, y= reorder(country,lifeExp))) +
  geom_line(aes(group = paired))+
  geom_point(aes(color=year), size=4) +
  labs(y="country")






df %>% 
  ggplot(aes(x= lifeExp, y= country)) +
  geom_line(aes(group = paired))+
  geom_point(aes(color=year), size=4) +
  theme(legend.position="top")
