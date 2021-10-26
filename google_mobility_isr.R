library(dplyr)
library(reshape2)
library(ggplot2)
library(RcppRoll)

#d1 <- read.csv('C:/Users/dmw63/Downloads/Global_Mobility_Report.csv')
#isr <- d1[d1$country_region=='Israel',]
#saveRDS(isr, 'google_mobility_isr.rds')

isr <- readRDS('google_mobility_isr.rds')

isr.south <- isr[isr$sub_region_2=="Be'er Sheva",]

isr.south$rave7_retail_rec = roll_mean(isr.south$retail_and_recreation_percent_change_from_baseline, n=7, align='right', fill=NA) 
isr.south$rave7_grocery = roll_mean(isr.south$grocery_and_pharmacy_percent_change_from_baseline, n=7, align='right', fill=NA) 
isr.south$rave7_parks = roll_mean(isr.south$parks_percent_change_from_baseline, n=7, align='right', fill=NA) 

isr.south$rave7_transit = roll_mean(isr.south$transit_stations_percent_change_from_baseline, n=7, align='right', fill=NA) 
isr.south$rave7_workplace = roll_mean(isr.south$workplaces_percent_change_from_baseline, n=7, align='right', fill=NA) 
isr.south$rave7_residential = roll_mean(isr.south$residential_percent_change_from_baseline, n=7, align='right', fill=NA) 


isr.south$date <- as.Date(isr.south$date)
plot(isr.south$date, isr.south$rave7_retail_rec, type='l')
abline(h=0, lty=2, col='gray')

isr.m <- melt(isr.south[,c('date','rave7_retail_rec','rave7_grocery','rave7_parks','rave7_transit','rave7_workplace','rave7_residential')], id.vars=c('date') )

p1 <- ggplot(isr.m, aes(x=date, y=value)) +
  geom_line() +
  ylab("Percent change mobility") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  geom_hline(yintercept=0, col='gray', lty=2)+
  geom_vline(xintercept=as.Date(c('2020-03-14', '2020-05-05', '2020-10-01', '2021-02-28' )), col='gray', lty=2)+
  
    facet_wrap(~variable )
p1
