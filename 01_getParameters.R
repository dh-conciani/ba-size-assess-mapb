## assess burned area sizes i
## dhemerson.costa@ipam.org.br

## read libraries
library(ggplot2)

## read table
data <- read.table('./table/ba_size_mapbiomas_col2.csv', 
                   sep=',',
                   dec='.',
                   header= TRUE,
                   encoding = 'UTF-8')


 ## remove columns
data <- data[, !names(data) %in% c('system.index', '.geo')]

## area to ha
data$area_ha <- data$`Área.km²` * 100

## get statistical parameters of ba_size per ecoregion and year
for (i in 1:length(unique(data$Ecoregião))) {
  print(paste0(unique(data$Ecoregião)[i]))
  print(paste0('region ', i, ' of ', length(unique(data$Ecoregião))))
  for (k in 1:length(unique(data$Ano))) {
    ## get data
    x <- subset(data, Ecoregião == unique(data$Ecoregião)[i] & Ano == unique(data$Ano)[k])
    print(paste0('year ', k, ' of ', length(unique(data$Ano))))
    ## get pareamenters
    x <- as.data.frame(rbind(cbind(
      ecoregion= unique(x$Ecoregião),
      year= unique(x$Ano),
      n_ba= nrow(x), 
      sum_ba= sum(x$area_ha),
      mean_ba= mean(x$area_ha),
      median_ba= median(x$area_ha),
      max_ba= max(x$area_ha),
      min_ba= min(x$area_ha)
    )))
    ## store
    if (exists('store') == FALSE) {
      store <- x
    } else {
      store <- rbind(x, store)
    }
  }
  rm(x)
}

## export
write.csv(store, './table/synthesis_col2.csv')

