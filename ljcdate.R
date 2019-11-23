library(dplyr)
library(magrittr)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)

# data join ----
dtdate<-read.csv("./進度登記表.csv")
db<-read.csv("./欖仁溪點位.csv")
dtdate<-mutate(dtdate,x1y1x2y2=paste0(X1,Y1,X2,Y2))
db<-mutate(db,x1y1x2y2=paste0(x1,y1,x2,y2))
dt<-left_join(db,dtdate,by="x1y1x2y2") %>%
  filter(.,!is.na(x4))
dt$Date<-as.Date(dt$Date)
nDate<-nrow(distinct(dt,Date))

# gganimate ----
p<-ggplot(dt,aes(x4,y4))+
  geom_point(color="grey40",size=0.5)+
  coord_fixed()+
  scale_x_continuous(breaks = seq(0,300,10))+
  scale_y_continuous(breaks = seq(0,220,10))+
  theme(panel.grid.minor = element_line(colour = "white"),
        axis.text = element_text(size = 4.5),
        axis.title = element_text(size = 4.5))+
  transition_manual(frames = Date,cumulative = TRUE)
animate(p, nframes = nDate,fps = 10, width=2000, height=2000, res=300, renderer = gifski_renderer("ljcdate.gif"))
