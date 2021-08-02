# Purpose: produce a heatmap that shows all pairwise country comparison of the epidemic progression (number of days ahead/behind)
# Which can be based on either cumulative cases, or deaths, or per 10k cases, or per 10k deaths.
# Currently attempted to do this with cumulative number of cases per 10k population.
# ---> PROBLEM: There is an issue with the ordering of countries.


today<- Sys.Date() - 1
library(plyr)
load(paste0('/Users/s1687811/Documents/GitHub/WHO_covid19_report/output/WHO_report_analysis_', as.character(today), '.RData'))


# PAIRWISE EPIDEMIC HEATMAP ----

d.10k<- # Cumulative cases per 10k
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative cases per 10k popula') %>%
    rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
`Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame()


pairs<- expand.grid(colnames(d.10k[,-1]), colnames(d.10k[,-1]))
pairs$Var1<- as.character(pairs$Var1)
pairs$Var2<- as.character(pairs$Var2)
pairs$time.diff<- NA
for(i in 1:nrow(pairs)){

  pairs$time.diff[i]<- epidemic.diff(d.10k, focal.country = as.character(pairs[i,1]), vs.country =  as.character(pairs[i,2]))

}

time.diff.df<-
  pairs[order(pairs$time.diff, na.last = TRUE, decreasing = TRUE), ] %>%
  rename(`focal country` = Var1,
         `compared to` = Var2,
         `Time difference (days)` = time.diff)

time.diff.df$`focal country`<- who.info.tab[match(time.diff.df$`focal country`, who.info.tab$country), 'name_plot']
time.diff.df$`compared to`<- who.info.tab[match(time.diff.df$`compared to`, who.info.tab$country), 'name_plot']

time.diff.df$epidemic.diff.text<- formatC(time.diff.df$`Time difference (days)`, digits = 1, format = "f")
time.diff.df$`focal country`<- factor(time.diff.df$`focal country`, levels = rev(unique(as.character(time.diff.df$`focal country`))))
time.diff.df$`compared to`<- factor(time.diff.df$`compared to`, levels = unique(as.character(time.diff.df$`compared to`)))

upper<- time.diff.df[-which(na.omit(time.diff.df$`Time difference (days)`) < 0),] # This is the dataframe used for the heatmap in final report

ggplot(time.diff.df, aes(x = `compared to`, y = `focal country`, fill= `Time difference (days)`)) +
  geom_tile()+
  xlab('')+ylab('')+
  scale_fill_gradient2(low="forestgreen", high="firebrick", mid = "white", midpoint = 0) +
  #geom_text(aes(label = epidemic.diff.text, x = `compared to`, y = `focal country`), size = 10, fontface = 2)+
  scale_x_discrete(position = "top")+
  theme_bw()+
  labs(colour = 'yo')+
  theme(legend.position="bottom",
        #legend.text = element_text(size = 35),
        legend.title = element_text(face = 2, size = 10),
        panel.border= element_rect(color = 'black', size = 0.5),
        axis.text.y = element_text(face="bold", colour="black", size=15),
        axis.text.x = element_text(colour="black", face="bold", size=15, angle = 90, hjust = 0, vjust = 0.5),
        #axis.title.y = element_text(face="bold", colour="black", size=25),
        #axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))



ggplot(upper, aes(x = `compared to`, y = `focal country`, fill= `Time difference (days)`)) +
  geom_tile()+
  xlab('')+ylab('')+
  scale_fill_gradient(low="white", high="firebrick") +
  #geom_text(aes(label = epidemic.diff.text, x = `compared to`, y = `focal country`), size = 10, fontface = 2)+
  scale_x_discrete(position = "top")+
  theme_bw()+
  labs(colour = 'yo')+
  theme(legend.position="bottom",
        #legend.text = element_text(size = 35),
        legend.title = element_text(face = 2, size = 10),
        panel.border= element_rect(color = 'black', size = 0.5),
        axis.text.y = element_text(face="bold", colour="black", size=15),
        axis.text.x = element_text(colour="black", face="bold", size=15, angle = 90, hjust = 0, vjust = 0.5),
        #axis.title.y = element_text(face="bold", colour="black", size=25),
        #axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))


# Tentative legend: Pairwise epidemic progression comparison across WHO Africa region**. The reported numbers are the numbers of days ahead the countries in horizontal entries, arranged from most ahead to least ahead, are relative to the countries in vertical entries. The full matrix is symetric. Only the upper half is represented. Grey shades appear in each case of (i) a country compared to itself or (ii) the two countries compared have not reached similar epidemic progression yet.














