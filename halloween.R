library("RSocrata")
library(dplyr)
library(lubridate)
library(ggplot2)
library(jsonlite)

keys<-fromJSON("../keys.json")$chicago_data

df <- read.socrata(
  "https://data.cityofchicago.org/resource/6zsd-86xi.json",
  app_token = keys$token,
  email     = keys$email,
  password  = keys$pw
)


myagg<-aggregate(date ~ primary_type, df, length)
df<-mutate(df, is_halloween = (month(date)==10 & day(date)==31))
hall_nums<-as.data.frame(table(df[which(df$is_halloween==T),c('primary_type')]))
hall_nums<-mutate(hall_nums, perc = (Freq/sum(Freq))*100)
nonhall_nums<-as.data.frame(table(df[which(df$is_halloween==F),c('primary_type')]))
nonhall_nums<-mutate(nonhall_nums, perc = (Freq/sum(Freq))*100)

mytable<-merge(hall_nums,nonhall_nums,by='Var1',all=T)
names(mytable) <- c('Type','Halloween_freq','Halloween_perc','Other_freq','Other_perc')
mytable<-mutate(mytable, diff = Halloween_perc - Other_perc)
mytable<-arrange(mytable,desc(diff))
mytable<-mytable[which(!is.na(mytable$diff)),]
#mytable<-mytable[which(abs(mytable$diff) > 0.2 ),]
mytable<- mutate(mytable, sign = ifelse(diff >= 0, "pos", "neg"))

p<-ggplot(data = mytable, aes(x = reorder(Type, diff),
                              y = diff,
                              fill = sign,
                              label = format(diff,digits=2))) +
  #geom_text(nudge_y = -1, color = "black") +
  scale_fill_manual(values = c("pos" = "black", "neg" = "orange")) +
  geom_col() +
  coord_flip() +
  labs(y = "Difference (% of all crimes)",
       x = "Crime Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")
p
