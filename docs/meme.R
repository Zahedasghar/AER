
library(tidyverse)
data(ames, package = 'modeldata')
dat <- ames %>% 
  janitor::clean_names() %>% 
  select(lot_area, sale_price, neighborhood)

ggplot(dat,aes(x=lot_area,y=sale_price))+
  geom_point(col='gray80')+
  geom_point(data = slice(dat,1,5,7,10),col='red',size=2)+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal()


x <- dat$sale_price
med <- median(x)

ggplot() +
  geom_jitter(aes(x = x, y = 1), alpha = 0.25) +
  geom_point(aes(x = med, y = 1), col = 'red', size = 5) +
  scale_x_log10() +
  labs(x = 'Sale price') +
  theme_minimal()


summaries <- dat |> 
  group_by(neighborhood) |> 
  summarise(med=median(sale_price))

ggplot(dat)+aes(x=sale_price,y=neighborhood)+
  geom_point(alpha=0.2)+
  geom_point(data=summaries,aes(x=med),col='red',size=3)+
  scale_x_log10()+
  theme_minimal()


#install.packages("meme")
library(meme)
#u <- "https://www.shutterstock.com/shutterstock/photos/157245086/display_1500/stock-photo-portrait-of-young-angry-man-157245086.jpg"

u <- system.file("angry8.jpg", package = "meme")
meme(u, "code", "all the things!")
mmplot(u) + mm_caption("calm down", "and RTFM", color="purple")

mmplot(u) + mm_caption("calm down", "and RTFM",
                       color="purple")

u2 <- system.file("success.jpg", package="meme")
x <- meme(u2, "please", "tell me more")
outfile <- tempfile(fileext=".png")
meme_save(x, file=outfile)

plot(x, size = 2, "happy friday!", "wait, sorry, it's monday", color = "firebrick", font = "Courier")

x + aes(upper = "#barbarplots",
        lower = "friends don't let friends make bar plots",
        color = firebrick, font = Courier, size=1.5)
#install.packages("memery")
library(memery)

pic <- system.file("philosoraptor.jpg", package = "memery")

txt <- c("What to call my R package?", "Perhaps...")

memery::meme(pic, txt[1], "dino1.jpg")

web <- "https://imgflip.com/s/meme/Philosoraptor.jpg"

memery::meme(web, txt[2], "dino2.png")

txt2 <- c("Am I in your way?", "I'm too cute to work!")

memery::meme("kitten.jpg", txt2, "kitty1.jpg")

clrs <- c("blue", "pink")

memery::meme("kitten.jpg", txt2, "kitty2.jpg", col = clrs[1], shadow = clrs[2])


img <- "https://media.giphy.com/media/OUwzqE4ZOk5Bm/giphy.gif"

lab <- c("memery", "magic")

pos <- list(w = rep(0.9, 2), h = rep(0.3, 2), x = rep(0.5, 2), y = c(0.9, 0.75))

meme_gif(img, lab, "HP.gif", size = c(1.5, 0.75), label_pos = pos, fps = 20)

meme::meme("kitten.jpg", "am i in your way?") 
