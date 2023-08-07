## assess burned area sizes
## dhemerson.costa@ipam.org.br

## read libraries
library(ggplot2)
library(ggrepel)
options(scipen=999)

## read table
data <- read.table('./table/synthesis_col2.csv', 
                   sep=',',
                   dec='.',
                   header= TRUE)


## insert time periods
data$time <- gsub('1985|1986|1987|1988|1989|1990|1991|1992|1993|1994', '1985-1994', 
                 gsub('1995|1996|1997|1998|1999|2000|2001|2002|2003|2004', '1995-2004',
                      gsub('2005|2006|2007|2008|2009|2010|2011|2012|2013|2014', '2005-2014',
                           gsub('2015|2016|2017|2018|2019|2020|2021|2022', '2015-2022',
                                data$year))))


## aggregate per ecoreigon and time-period
for (i in 1:length(unique(data$ecoregion))) {
  z <- subset(data, ecoregion == unique(data$ecoregion)[i])
  ## get parameters for the entire time-series
  z <- as.data.frame(rbind(cbind(
    ecoregion= unique(z$ecoregion),
    time= 'All',
    n_ba= sum(z$n_ba),
    sd_n= sd(z$n_ba),
    sum_ba= sum(z$sum_ba),
    mean_ba= mean(z$mean_ba),
    sd_ba= sd(z$mean_ba),
    max_ba= max(z$max_ba)
  )))
  
  for (j in 1:length(unique(data$time))) {
    ## get data
    x <- subset(data, ecoregion == unique(data$ecoregion)[i] & time == unique(data$time)[j])
    
    ## get pareamenters
    x <- as.data.frame(rbind(cbind(
            ecoregion= unique(x$ecoregion),
            time= unique(x$time),
            n_ba= sum(x$n_ba),
            sd_n= sd(x$n_ba),
            sum_ba= sum(x$sum_ba),
            mean_ba= mean(x$mean_ba),
            sd_ba= sd(x$mean_ba),
            max_ba= max(x$max_ba)
          )))
    
    ## store
    if(exists('store') == FALSE) {
      store <- x
    } else {
      store <- rbind(store, x)
    }
  }
  store <- rbind(store, z)
  rm(x, z)
}

## subset
time <- subset(store, time != 'All')

## plot
ggplot(data= time, mapping= aes(x= time, y= as.numeric(mean_ba), fill= time)) +
  geom_bar(stat='identity') +
  facet_wrap(~ecoregion) +
  theme_bw()


## get comparations with historical mean
for (k in 1:length(unique(store$ecoregion))) {
  x <- subset(store, ecoregion== unique(store$ecoregion)[k] & time == 'All')
  y <- subset(store, ecoregion== unique(store$ecoregion)[k] & time != 'All')
  
  for (l in 1:length(unique(y$time))) {
    z <- subset(y, time == unique(y$time)[l])
    
    z <- as.data.frame(rbind(cbind(
      ecoregion= unique(z$ecoregion),
      time= unique(z$time),
      abs_diff= as.numeric(z$mean_ba) - as.numeric(x$mean_ba),
      rel_diff= round((as.numeric(z$mean_ba) - as.numeric(x$mean_ba)) / as.numeric(x$mean_ba) * 100, digits=1)
    )))
    
    if(exists('hist_mean') == FALSE) {
      hist_mean <- z
    } else {
      hist_mean <- rbind(hist_mean, z)
    }
  }
  rm(x, y, z)
}

## get difference from first to last
for (m in 1:length(unique(store$ecoregion))) {
  x <- subset(store, ecoregion == unique(store$ecoregion)[m])
  ii <- subset(x, time == '1985-1994')
  ij <- subset(x, time == '2015-2022')
  
  y <- as.data.frame(rbind(cbind(
      ecoregion= unique(x$ecoregion),
      abs_diff= as.numeric(ij$mean_ba) - as.numeric(ii$mean_ba),
      rel_diff= round(abs(as.numeric(ij$mean_ba) - as.numeric(ii$mean_ba)) / as.numeric(ii$mean_ba) * 100, digits=1)
    )))
  
  if(exists('time_diff') == FALSE) {
    time_diff <- y
  } else {
    time_diff <- rbind(time_diff, y)
  }

  rm (x, ii, ij, y)
}

## get historical means
hist <- subset(store, time == 'All')

x11()
ggplot(data= hist_mean, mapping= aes(x= time, y= as.numeric(rel_diff), fill= as.numeric(rel_diff))) +
  geom_bar(stat='identity', alpha= 0.6) +
  scale_fill_fermenter('Difference (%)', palette = 'RdYlGn', direction= -1,
                       breaks=c(-20, -10, 0, 10, 20)) + 
  facet_wrap(~ecoregion) +
  geom_hline(mapping=aes(yintercept=0), linetype= 'solid', col='gray70') +
  theme_bw() +
  ylab('Relative difference to historical mean (%)') +
  xlab('Time interval') +
  geom_text(mapping=aes(label= paste0(rel_diff,'%', '\n', round(as.numeric(abs_diff), digits=1), ' ha')),
            position= position_stack(0.5), size= 4) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_text(data= hist, mapping=aes(x= '1995-2004', y= 45, fill= NULL, 
                                   label= paste0('Historical mean: ', round(as.numeric(mean_ba), digits=1), ' ha')),
             col= 'blue', size=3, hjust=0.6) +
  geom_text(data= time_diff, mapping= aes(x= '1995-2004', y= 35, fill= NULL,
                                         label= paste0('T4-T1: ', round(as.numeric(abs_diff), digits=1), ' ha',
                                                       ' (', rel_diff, '%', ')')),
           col= 'purple4', size=3, hjust=0.6)
