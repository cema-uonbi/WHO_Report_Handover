#Changing breaks if error with weekly ratio map 

# Map WR CASES ----
africa@data %<>% left_join(who_WR_data %>% select(-country), by=c("ISO_A3"="countryterritoryCode"))
# group country cases into similar values
breaks <- classIntervals(africa@data$WR_cases, n = 7, style = "jenks", na.rm=T)$brks
#breaks[1]= 0.0000001
# find groupings below 1 and above one to set red/green colours
groups_less_than_one <- sum(breaks < 1)
print(groups_less_than_one)
breaks[(groups_less_than_one + 1)] = 0.999999
print(breaks)
palredgreen <- brewer.pal(groups_less_than_one, name = "Greens")
print(palredgreen)
palredgreen <- c(rev(palredgreen)[1:(groups_less_than_one - 1)],brewer.pal(7 - (groups_less_than_one - 1), name = "Reds"))
print(palredgreen)
palredgreen<-c("#FFFFFF",palredgreen)
print(palredgreen)
#breaks <- c(0,breaks)
print(breaks)
png(filename = paste0('./output/Map_WR_cases_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "WR_cases", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palredgreen, legend.title.txt = "Ratio", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'Non reported or adjusted < 7 days ago', adj = 0)
dev.off()



#Adding weekly ratio summary to Rmarkdown 


\newpage

## Weekly Ratio Summary 

The following is a list of countries for which the value for all 7 of last week's values for weekly ratio of cases were below 1, and the same was true for the weekly ratio of deaths.

```{r echo = FALSE, results = 'asis'}
seven_day_WR_cases_and_deaths <-  WHO_cases_and_deaths_weekly_ratio 
  seven_day_WR_cases_and_deaths[seven_day_WR_cases_and_deaths < 0] <- NA
  seven_day_WR_cases_and_deaths <- filter( seven_day_WR_cases_and_deaths,date > today - 7) %>%
  select(date,country,ratio_c,ratio_d) %>%
 filter(ratio_c <1 & ratio_d<1) %>%
  group_by(country) %>%
filter(n() == 7)
seven_day_WR_cases_and_deaths <-seven_day_WR_cases_and_deaths[!duplicated(seven_day_WR_cases_and_deaths$country), ]

kable(seven_day_WR_cases_and_deaths$country, "latex" , col.names = NULL, booktabs= T) %>%
  kable_styling(position = "left")%>%
  sub("\\\\toprule", "", .) %>%
  sub("\\\\bottomrule", "", .)
```