library(readr)
library(readxl)

library(tidyverse)

basic_survey <- read_excel("D:/RepTemplates/PIDE/basic_survey.xlsx", 
                               sheet = "Sheet6")
long <- basic_survey |> pivot_longer(cols = c(3:8),
                                     names_to = 'category',values_to = 'values')
#View(long)

ggplot(long)+aes(x=category,y=values, fill=Age)+
  geom_bar(stat = "identity")

# ggplot(long)+aes(y=, color=Age)+
#   geom_bar(position = 'dodge')


ggplot(long)+aes(x=category,y=values, fill=Age)+
  geom_bar(stat = "identity")+facet_wrap(~Gender)


ggplot(long)+aes(x=category,y=values, fill=Gender)+
  geom_bar(stat = "identity")+facet_wrap(~Age)+coord_flip()+
  scale_y_continuous(breaks=seq(0,100,20),
                     limits=c(0,100),
                     expand=c(0,0))
  

p1 <- ggplot(long)+aes(x=category,y=values)+
  geom_bar(stat = "identity", aes(fill=Gender))+facet_grid(Gender~ Age)+coord_flip()+
  #scale_y_continuous(breaks=seq(0,100,20),
         #            limits=c(0,70),
   #                  expand=c(0,0))+
  labs(y="percentage response", title="Having access to playground by age and sex",
       caption = "PIDE-RASTA Basic notes")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank() )+
  geom_text(aes(label = values), hjust = 0.5)+theme_economist()

p1 +  theme(legend.position = "none")




ggplot(long)+aes(x=category,y=values)+
  geom_bar(stat = "identity", aes(fill=Gender))+theme(axis.text.x = element_blank(),
                                                      axis.ticks.x = element_blank(),
                                                      axis.text.y = element_blank(),
                                                      axis.ticks.y = element_blank() )+
  geom_text(aes(label = values), hjust = 0.5)+theme_economist()






library(tidyverse)
library(viridis)

# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

data |> head()

long |> head()

empty_bar <- 2
nObsType <- nlevels(as.factor(long$Age))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$category)*nObsType, ncol(long)) )
colnames(to_add) <- colnames(long)
to_add$group <- rep(levels(long$category), each=empty_bar*nObsType )
data <- rbind(long, to_add)
data
data <- data %>% arrange(category, Gender)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, Gender) %>% summarize(tot=sum(values))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(category) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=values, fill=Age), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE )+
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1)+
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar()+
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=Gender, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )
+
  geom_text(data=base_data, aes(x = title, y = -18, label=category), hjust=c(1,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p
# Save at png
ggsave(p1, file="output.png", width=10, height=10)
RELATED CHART TYPES

View(long)

B
