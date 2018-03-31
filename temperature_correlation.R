### DESCRIPTION ###############################################################
# Assess correlation between temperature and food consumption
#
#

### LIBRARIES #################################################################
library(CatterPlots);
library(hedgehog);
library(dplyr);

options(stringsAsFactors = FALSE);
rm(list = ls(all.names = TRUE));

source.directory('helper-functions');

### MAIN ######################################################################
temperatures <- read.csv('data/toronto_temperatures.csv');

# restrict to dates we have complete data
temperatures <- temperatures %>%
    select(Date.Time, Max.Temp..Â.C., Min.Temp..Â.C., Mean.Temp..Â.C.) %>%
    mutate_at('Date.Time', as.Date) %>%
    filter(Date.Time >= as.Date('2017-04-28') & Date.Time <= as.Date('2017-07-07'));
names(temperatures) <- c('date', 'max_temp', 'min_temp', 'mean_temp');

# add food consumption data
food.by.date <- get.consumption.by.day('total');
temperatures <- merge(temperatures, food.by.date);


options(bitmapType = 'cairo');

png(
    'plots/temperature_panel.png',
    width = 8,
    height = 5,
    units = 'in',
    res = 500
    );

par(mar = c(1, 4, 0.5, 0.1), mfrow = c(2, 1) );

plot(
	temperatures$mean_temp, 
	type = 'l', 
	bty = 'n',
	xaxt = 'n',
	xlab = '',
	ylab = 'Mean temperature (°C)',
	ylim = c(0, 27),
	lwd = 2,
	xaxs = 'i'
	);

barplot(
	temperatures$consumption,
	ylab = 'Food consumed (g)',
	xaxs = 'i',
	col = 'khaki3'
	);

mtext(
    'May',
    side = 1,
    at = 1.2*which(as.Date('2017-05-15') == temperatures$date)
    );
mtext(
    'June',
    side = 1,
    at = 1.2*which(as.Date('2017-06-15') == temperatures$date)
    );

dev.off();


png(
    'plots/temperature_correlation.png',
    width = 6,
    height = 5,
    units = 'in',
    res = 500
    );

par(mar = c(4, 4, 0.1, 0.1));

purr <- multicat(
    temperatures$mean_temp, 
    temperatures$consumption,
    catcolor = 'maroon',
    size = 0.05,
    xlim = c(0, 27),
    xat = seq(0, 27, by = 5),
    xlab = 'Mean temperature (°C)',
    ylim = c(0, 380),
    yat = seq(0, 400, by = 100),
    ylab = 'Food consumed (g)',
    cat = sample(
        1:10, 
        nrow(temperatures), 
        replace = TRUE
    ),
    canvas = c(0, 1.04, 0, 1.0),
    bty = 'L'
    );

correlation.test <- cor.test(
	x = temperatures$mean_temp,
	y = temperatures$consumption,
	method = 'spearman'
	);

text(
	labels = bquote( rho == .(round(correlation.test$estimate, 3) ) ),
	x = 0.03,
	y = 0.1,
	adj = c(0, 0.5)
	);

text(
	labels = bquote( P == .(scientific.notation(correlation.test$p.value) ) ),
	x = 0.03,
	y = 0.05,
	adj = c(0, 0.5)
	);

dev.off();

lm.model <- lm(
	consumption ~ mean_temp,
	temperatures
	);

print( summary(lm.model) );

