## assess burned area sizes
## dhemerson.costa@ipam.org.br

## read libraries
library(ggplot2)
options(scipen=999)

## read table
data <- read.table('./table/synthesis_col2.csv', 
                   sep=',',
                   dec='.',
                   header= TRUE)

## sum of ba
ggplot(data, mapping=aes(x= year, y= sum_ba/1e6)) +
  geom_area(stat= 'identity', alpha=0.2, size=1) +
  facet_wrap(~ecoregion, scales= 'free_y') +
  theme_bw() +
  geom_smooth(method='lm', se= FALSE, col= 'red', size=1) +
  xlab(NULL) + 
  ylab('Total burned area (Mha)')

## number of fires
ggplot(data, mapping=aes(x= year, y= n_ba)) +
  geom_area(stat= 'identity', alpha=0.2, size=1) +
  facet_wrap(~ecoregion, scales= 'free_y') +
  theme_bw() +
  geom_smooth(method='lm', se= FALSE, col= 'red', size=1) +
  xlab(NULL) + 
  ylab('Mean burned area size (ha)')

## mean size
ggplot(data, mapping=aes(x= year, y= mean_ba)) +
  geom_area(stat= 'identity', alpha=0.2, size=1) +
  facet_wrap(~ecoregion, scales= 'free_y') +
  theme_bw() +
  geom_smooth(method='lm', se= FALSE, col= 'red', size=1) +
  xlab(NULL) + 
  ylab('Mean burned area size (ha)')

## max size
ggplot(data, mapping=aes(x= year, y= max_ba)) +
  geom_area(stat= 'identity', alpha=0.2, size=1) +
  facet_wrap(~ecoregion, scales= 'free_y') +
  theme_bw() +
  geom_smooth(method='lm', se= FALSE, col= 'red', size=1) +
  xlab(NULL) + 
  ylab('Greatest scar')
