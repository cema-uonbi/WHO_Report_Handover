## create the begining curve
library(gridExtra)
library(forecast)
new_case <- WHO_cases_and_deaths[,c(1,2,5)]
new_case <- spread(new_case,country,cases)
new_case$sum <- rowSums(new_case[,2:48])
smoothCounts <- new_case[,c(1,49)] %>% 
  mutate_at(c(2), ~zoo::rollmean(., k=7,, align = "right", fill = NA))%>%
  rename('rolling'='sum')
smoothCounts$index <- '1'

break.vec <- seq(from = as.Date("2020-02-25"), to = Sys.Date()-1,  
                 by = "2 months")
p_case <- ggplot() +
  geom_bar(data=new_case, aes(x=date, y=sum),stat="identity", position=position_dodge(),colour = 'grey')+
  geom_line(data=smoothCounts, aes(x=date,y=rolling,colour=index),cex=1.5)+ 
  xlab("Date") + ylab("Count") +
  scale_x_date(breaks =break.vec, date_labels = "%b %d")+
  theme(axis.title= element_text(size=25, face= "bold", vjust=0.5, hjust=0.5)) +
  theme(axis.text = element_text(color = "black", size=20)) +
  theme(axis.line = element_line(size = 0.8, colour = "black")) +
  theme(panel.background = element_rect(fill = "transparent", color = "transparent"))+
  theme(legend.position = c(0.3, 0.9),legend.text = element_text(size = 20))+
  scale_color_manual(values = c('1' = 'blue'),name = '',labels=c("7 Day Rolling Average")) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  ggtitle('Daily New Cases') +
  theme(plot.title = element_text(hjust = 0.5,size = 28,face = "bold"))

new_death <- WHO_cases_and_deaths[,c(1,2,6)]
new_death <- spread(new_death,country,deaths)
new_death$sum <- rowSums(new_death[,2:48])
smoothCounts1 <- new_death[,c(1,49)] %>%
  mutate_at(c(2), ~zoo::rollmean(., k=7,, align = "right", fill = NA))%>%
  dplyr::rename('rolling' = 'sum')
smoothCounts1$index <- '1'

p_death <- ggplot() +
  geom_bar(data=new_death, aes(x=date, y=sum),stat="identity", position=position_dodge(),colour = 'grey')+
  geom_line(data=smoothCounts1, aes(x=date,y=rolling,colour = index),cex=1.5)+
  xlab("Date") + ylab("Count") +
  scale_x_date(breaks =break.vec, date_labels = "%b %d")+
  theme(axis.title= element_text(size=25, face= "bold", vjust=0.5, hjust=0.5)) +
  theme(axis.text = element_text(color = "black", size=20)) +
  theme(axis.line = element_line(size = 0.8, colour = "black")) +
  theme(panel.background = element_rect(fill = "transparent", color = "transparent"))+
  theme(legend.position = c(0.3, 0.9),legend.text = element_text(size = 20))+
  scale_color_manual(values = c('1' = 'red'),name = '',labels=c("7 Day Rolling Average")) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  ggtitle('Daily New Deaths') +
  theme(plot.title = element_text(hjust = 0.5,size = 28,face = "bold"))


png(file = "input_files/WHO_Africa_epicurve.png", width=1200, height=650, pointsize=22)
grid.arrange(p_case, p_death, ncol = 2)
dev.off()