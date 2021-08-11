# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   WHO report - analysis script     #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# 1) SET UP ----

setwd("~/Dropbox/CEMA_GitHub/WHO_Report_Handover/3")
data <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") %>%
  mutate(Date_reported=as.Date(Date_reported, format = "%Y-%m-%d"))

today<- max(data$Date_reported)
#today<- Sys.Date()-1 # Set date as to that of the data to fetch.

iter = 1000 # Number of iterations for the poisson error simulation (bootstrap), Set to 1000. Or 10 for a quick test.
set.seed(as.numeric(today)) # setting seed allows repeatability of poisson error simulations. Use the date as a reference point for the seed.

#today

source('script/sourced_functions_Weekly_Ratio_reports.R') # Source several functions used in the below script. See README file for details. 

time_window<- 7 # Time window over which doubling time is calculated
t2.define<- today
t1.define<- t2.define - time_window

# library to read excel files
library(readxl)
# large library of generic data science tools for manipulating data
library(tidyverse)
library(magrittr)
# import a relatively sensible colour palette
library(RColorBrewer)
# class interval library to group countries together with similar cases/deaths counts 
# so that they are given the same colour
library(classInt)
# libraries to create all of the maps (require a lot of other packages to work)
library(sf)
library(geojsonio)
library(cartography)
# library with useful function for reading/manipulating dates
library(lubridate)
# library to manipulate images
library(magick)
# rolling windows for weekly ratios
library(RcppRoll)

# LOADING DATA ----

# Load data and make various versions of them (wide, long, log10, linear, etc. that are used later in the script or in the Rmd  file)

# WHO list of countries & their popsize.
# Various encoding of the countries name used for different purpose (match various data sources, for maps, shortened for figures etc.)
who_country_aliases_and_populations<-
  read.csv('input_files/WHO_country_list.csv', stringsAsFactors = FALSE) %>%
  rename( popsize = pop)


## Download data from https://covid19.who.int/table and read it in
data <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") %>%
  #rename(Date_reported=Date_reported) %>%
  mutate(Date_reported=as.Date(Date_reported, format = "%Y-%m-%d"))
today<- max(data$Date_reported)
## Extract data for WHO AFRO
africa_data <- 
  data[which(data$WHO_region == 'AFRO'& !(data$Country == "Réunion") & !(data$Country == "Mayotte")),] %>%## we didn't include the two territories before
  select(.,c(1,3,6,8))%>%
  rename("date"="Date_reported",
             'country'="Country",
             'cum_cases'="Cumulative_cases" ,
             "cum_deaths"="Cumulative_deaths")

# All columns in WHO_cases_and_deaths are WHO countries.
# After modifying "Côte d'Ivoire" into "Cote d'Ivoire" (without the ^), and `São Tomé and Príncipe into Sao Tome and Principe, then the variable "country" of the who_country_aliases_and_populations is the one that matches the countries name of the data
# + formatting variable names to fit in rest of the script

WHO_cases_and_deaths<- africa_data %>% 
  mutate_at('country', ~replace(., .=="Côte d’Ivoire" , "Cote d'Ivoire"))%>% 
  # revert back to per day deaths / cases
  group_by(country) %>%
  mutate(cum_cases=as.integer(cum_cases),cum_deaths=as.integer(cum_deaths),cases = as.integer(c(0,diff(cum_cases))), deaths = as.integer(c(0,diff(cum_deaths)))) %>%
  ungroup() %>%
  # convert to per 10k pop
  inner_join(who_country_aliases_and_populations %>% 
               select(country,popsize)) %>% 
  mutate(popsize = popsize / 10000,cum_cases_per_10k=cum_cases / popsize,cum_deaths_per_10k=cum_deaths / popsize,cases_per_10k=cases / popsize,deaths_per_10k=deaths / popsize)


# check data is complete
if(!(WHO_cases_and_deaths %>% 
  group_by(country) %>% 
  summarise(n()) %>%
  distinct(n()) %>%
  length()) == 1) stop('Some countries are missing reporting dates')

if(sum(is.na(WHO_cases_and_deaths$cases)) > 0 | sum(is.na(WHO_cases_and_deaths$deaths)) > 0) stop('Some countries have NA entries')

who_countrywide_data <-  
  WHO_cases_and_deaths[which(WHO_cases_and_deaths$date == as.Date(format(today,"%Y/%m/%d"))),]%>% 
  select(.,c(1,2,3,4,8,9)) %>%
  inner_join(who_country_aliases_and_populations %>% 
               select(country,ISO3))  


  WHO_cases_and_deaths_weekly_ratio <- WHO_cases_and_deaths %>%
  left_join(weekly_ratios_2(WHO_cases_and_deaths,"cases")) %>%
  left_join(weekly_ratios_2(WHO_cases_and_deaths,"deaths"), suffix= c("_c","_d"), by = c("country", "date"))


# Creates the begining image / logo showing the WHO AFRO Region
 africa <- geojson_read("./input_files/Africa1.geojson", what="sp")
 africa@data %<>% left_join(who_countrywide_data, by=c("ISO_A3"="ISO3"))
 africa@data$WHOCountry <- ifelse(is.na(africa@data$country),0,1)
 png(file = "input_files/WHO_Africa3.png", width=3246, height=2880, pointsize=22)
typoLayer(spdf = africa, var = "WHOCountry", col = c("darkorange2", "white"), legend.pos = "n")
text(-34, -43,"Total Reported Cases", adj = 0,col="#08519C",font=2,cex=6)
text(-18, -48,paste0(WHO_cases_and_deaths %>% filter(date == today) %>% pull(cum_cases) %>% sum()), adj = 0,col="#08519C",font=2,cex=6)
text(24, -43,"Total Reported Deaths", adj = 0,col="#CB181D",font=2,cex=6)
text(44, -48,paste0(WHO_cases_and_deaths %>% filter(date == today) %>% pull(cum_deaths) %>% sum()), adj = 0,col="#CB181D",font=2,cex=6)
 dev.off()

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
 
 break.vec <- seq(from = as.Date("2020-02-25"), to = today,  
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
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
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
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
   ggtitle('Daily New Deaths') +
   theme(plot.title = element_text(hjust = 0.5,size = 28,face = "bold"))
 
 
 png(file = "input_files/WHO_Africa_epicurve.png", width=1200, height=650, pointsize=22)
 grid.arrange(p_case, p_death, ncol = 2)
 dev.off()
 
 WHO_cases_and_deaths_negative <- WHO_cases_and_deaths %>%
   mutate(cases_positive = (cases > -1), deaths_positive = (deaths > -1)) %>%
   group_by(country) %>%
   mutate(calc_cases_dt = eight_day_pos_window(cases_positive), calc_deaths_dt = eight_day_pos_window(deaths_positive)) %>%
   ungroup()
 
 # simulated datasets from bootstrapping
 WHO_cases_and_deaths_simulated <- WHO_cases_and_deaths_negative %>%
   group_by(country,date) %>%
   mutate(sim_deaths = map(iter,rpois_error,deaths,deaths_positive),sim_cases = map(iter,rpois_error,cases,cases_positive)) %>%
   group_by(country) %>%
   mutate(sim_cum_deaths = sim_cum_calc_per_pop(sim_deaths,1,deaths_positive),
          sim_cum_cases = sim_cum_calc_per_pop(sim_cases,1,cases_positive),
          sim_cum_deaths_per_10k = sim_cum_calc_per_pop(sim_deaths,popsize,deaths_positive),
          sim_cum_cases_per_10k = sim_cum_calc_per_pop(sim_cases,popsize,cases_positive)) %>%
   ungroup()

# The TWO NEXT SECTIONS are to collect the Last day incidence of the observed data but ALSO OF THE SIMULATED DATA
# that, to be able to report a 95%CI on the observed data
# Done on the raw number of cases and the per 10k pop 
print(WHO_cases_and_deaths_simulated)
# Last day Incidence & CI, raw ----
WHO_latest_day_cases_and_deaths_simulated <- WHO_cases_and_deaths_simulated %>%
  filter(date == today) %>%
  group_by(country)%>%
  transmute(
            last_day_case_ci_high =round(quantile(sim_cum_cases[[1]], c(0.95), method = 6,na.rm = TRUE))[[1]],
            last_day_deaths_ci_high =round(quantile(sim_cum_deaths[[1]], c(0.95), method = 6,na.rm = TRUE))[[1]],
            last_day_case_ci_low =round(quantile(sim_cum_cases[[1]], c(0.05), method = 6,na.rm = TRUE))[[1]],
            last_day_deaths_ci_low =round(quantile(sim_cum_deaths[[1]], c(0.05), method = 6,na.rm = TRUE))[[1]],
            last_day_case_ci_high_per_10k =round(quantile(sim_cum_cases_per_10k[[1]], c(0.95), method = 6,na.rm = TRUE))[[1]],
            last_day_deaths_ci_high_per_10k =round(quantile(sim_cum_deaths_per_10k[[1]], c(0.95), method = 6,na.rm = TRUE))[[1]],
            last_day_case_ci_low_per_10k =round(quantile(sim_cum_cases_per_10k[[1]], c(0.05), method = 6,na.rm = TRUE))[[1]],
            last_day_deaths_ci_low_per_10k =round(quantile(sim_cum_deaths_per_10k[[1]], c(0.05), method = 6,na.rm = TRUE))[[1]]) %>%
  inner_join(
    WHO_cases_and_deaths %>%
      filter(date == today) %>%
      transmute(
        country,
        last_day_case_obs = cum_cases,
        last_day_deaths_obs = cum_deaths,
        last_day_case_obs_per_10k = cum_cases_per_10k,
        last_day_deaths_obs_per_10k = cum_deaths_per_10k
      )
  ) %>%
  ungroup()



# By-COUNTRY PLOTS FUNC (2) ----

# MAPS  ----

who_WR_data<- # Assemble the weekly ratios formatted for maps plotting
  who_country_aliases_and_populations[,c('ISO3', 'country')] %>%
  left_join(WHO_cases_and_deaths_weekly_ratio %>%
              filter(date == today) %>%
              select(country,ratio_c,ratio_d), by = 'country') %>%
  rename(WR_cases = ratio_c,
         WR_deaths = ratio_d,
         countryterritoryCode = ISO3)

# Set the NA, -1 and Inf WR to zero, so that they appear in white on the maps
who_WR_data$WR_cases[who_WR_data$WR_cases == 0]<- 0.00000001 
who_WR_data$WR_deaths[who_WR_data$WR_deaths == 0]<- 0.0000001
who_WR_data$WR_cases[!is.finite(who_WR_data$WR_cases)]<- 0 
who_WR_data$WR_deaths[!is.finite(who_WR_data$WR_deaths)]<- 0
who_WR_data$WR_cases[who_WR_data$WR_cases < 0]<- 0 
who_WR_data$WR_deaths[who_WR_data$WR_deaths < 0]<- 0


# PRODUCE THE MAPS FOR THE REPORT ----

# Maps quick achk solution for legend: overplot a white point and then a text, to replace the "0" on the scale by a more informative legend text (0s are not real zeros here, they are NA and Inf, but the NA variable is reserved for non-WHO countries here)

# Map CASES ----
breaks <- classIntervals(africa@data$cum_cases, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)

#breaks[2]<-1
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_cum_cases_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "cum_cases", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palblue,
           legend.title.txt = "",
           legend.title.cex = 1,
           legend.values.cex = 1, 
           legend.pos = c(-30,-35)) # -30, -40

points(-23.3, -31, pch = 16, col = 'white', cex = 2)
dev.off()


# Map CASES 10k ----
breaks <- classIntervals(africa@data$cum_cases_per_10k, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
#breaks[2]<-0.000001
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_cases_10k_pop_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "cum_cases_per_10k", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palblue,legend.title.txt = "", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No case reported', adj = 0)
dev.off()



# Map DEATHS ----
breaks <- classIntervals(africa@data$cum_deaths, n = 6, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
# breaks
# breaks[2]<-3


# breaks<- c(1,2,688,1543,2083,2877,42550)
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_cum_deaths_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "cum_deaths", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palred,legend.title.txt = "", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No death reported', adj = 0)
dev.off()

# Map DEATHS 10k ----
breaks <- classIntervals(africa@data$cum_deaths_per_10k, n = 6, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
#breaks[2]<-0.0000001
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_deaths_10k_pop_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "cum_deaths_per_10k", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palred,legend.title.txt = "", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No death reported', adj = 0)
dev.off()


# Map WR CASES ----
africa@data %<>% left_join(who_WR_data %>% select(-country), by=c("ISO_A3"="countryterritoryCode"))


# group country cases into similar values
breaks <- classIntervals(africa@data$WR_cases, n = 7, style = "jenks", na.rm=T)$brks

breaks<- breaks[-1]
#breaks[1]<-0.0000001

breaks<- sort(breaks, decreasing = FALSE)
#breaks <- c(0.00000001, 0.64534884, 0.96096096, 1.26837061, 1.57464641, 2.02946593, 2.93442623)
 

# find groupings below 1 and above one to set red/green colours
groups_less_than_one <- sum(breaks < 1)
breaks[(groups_less_than_one + 1)] = 0.999999

palredgreen <- brewer.pal(groups_less_than_one, name = "Greens")
palredgreen <- c(rev(palredgreen)[1:groups_less_than_one],brewer.pal(7 - groups_less_than_one, name = "Reds"))
palredgreen<-c("#FFFFFF",palredgreen)
breaks <- c(0,breaks)
png(filename = paste0('output/Map_WR_cases_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "WR_cases", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palredgreen, legend.title.txt = "Ratio", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'Non reported or adjusted < 7 days ago', adj = 0)
dev.off()

# Map WR DEATHS ----
breaks <- classIntervals(africa@data$WR_deaths, n = 4, style = "jenks", na.rm=T)$brks

breaks[1]<-0.0000001
breaks<- sort(breaks, decreasing = FALSE)
#breaks <- c(0.0000001, 0.001, 1, 2, 4.5, 13)
#breaks = breaks[breaks!= 0]
breaks

# find groupings below 1 and above one to set red/green colours

groups_less_than_one <- sum(breaks < 1)
breaks[(groups_less_than_one + 1)] = 0.99999
palredgreen <- brewer.pal(groups_less_than_one, name = "Greens")
palredgreen <- c(rev(palredgreen)[1:groups_less_than_one],brewer.pal(4 - groups_less_than_one, name = "Reds"))
palredgreen<-c("#FFFFFF",palredgreen)
breaks <- c(0,breaks)
png(filename = paste0('output/Map_WR_deaths_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "WR_deaths", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palredgreen,legend.title.txt = "Ratio", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'Non reported or adjusted < 7 days ago', adj = 0)
dev.off()


# ASSEMBLE MAPS ----

# crop images and create 3x6 plot
# This assumes images are 1920x1240, will centre-crop to 1080x960 
#Read images
image1 <- image_read(paste0("output/Map_cum_cases_", today, "_.png"))
image2 <- image_read(paste0("output/Map_cases_10k_pop_", today, "_.png"))
image3 <- image_read(paste0("output/Map_cum_deaths_", today, "_.png"))
image4 <- image_read(paste0("output/Map_deaths_10k_pop_", today, "_.png"))
image5 <- image_read(paste0("output/Map_WR_cases_", today, "_.png"))
image6 <- image_read(paste0("output/Map_WR_deaths_", today, "_.png"))

#Crop images
image1_crop <- image_crop(image1, "1080x960+420+140")
image2_crop <- image_crop(image2, "1080x960+420+140")
image3_crop <- image_crop(image3, "1080x960+420+140")
image4_crop <- image_crop(image4, "1080x960+420+140")
image5_crop <- image_crop(image5, "1080x960+420+140")
image6_crop <- image_crop(image6, "1080x960+420+140")


# Save a 3*2 plots image, with added titles for each map
png(file = paste0("./output/6Maps_WHO_Africa_", today, "_.png"), width=1080*2, height=960*3, pointsize=22)
par(mai=rep(0.5,4)) # no margins
layout(matrix(1:6, ncol=2, byrow=TRUE))
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'CUMULATIVE REPORTED CASES', cex.main = 2)
rasterImage(image1_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'CUMULATIVE REPORTED DEATHS', cex.main = 2)
rasterImage(image3_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'CUMULATIVE REPORTED CASES PER 10k POPULATION', cex.main = 2)
rasterImage(image2_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'CUMULATIVE REPORTED DEATHS PER 10k POPULATION', cex.main = 2)
rasterImage(image4_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'WEEKLY RATIO CASES', cex.main = 2)
rasterImage(image5_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'WEEKLY RATIO DEATHS', cex.main = 2)
rasterImage(image6_crop, 0, 0, 1,1)
dev.off()



# SAVE OUTPUT RData ----
save.image(paste0('output/WHO_report_analysis_', today, '.RData'))


#Seven Day ratios

seven_day_WR_cases_and_deaths <-  WHO_cases_and_deaths_weekly_ratio 
  seven_day_WR_cases_and_deaths[seven_day_WR_cases_and_deaths < 0] <- NA
  seven_day_WR_cases_and_deaths <- filter( seven_day_WR_cases_and_deaths,date > today - 7) %>%
  select(date,country,ratio_c,ratio_d) %>%
 filter(ratio_c <1 & ratio_d<1) %>%
  group_by(country) %>%
filter(n() == 7)
seven_day_WR_cases_and_deaths <-seven_day_WR_cases_and_deaths[!duplicated(seven_day_WR_cases_and_deaths$country), ]




