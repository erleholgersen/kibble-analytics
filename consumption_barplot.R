### DESCRIPTION ###############################################################
# Plot kibble and wet food consumption by date
#

### PREAMBLE ##################################################################
library(CatterPlots);
library(lubridate);
library(hedgehog);
library(dplyr);

set.seed(1234);

# clear workspace
rm(list = ls(all = TRUE));

options(stringsAsFactors = FALSE);

# load helper functions
source.directory('helper-functions');

### MAIN ######################################################################

kibble.by.date <- get.consumption.by.day('kibble');
wet.food.by.date <- get.consumption.by.day('wet-food');

# make plot

options(bitmapType = 'cairo');

png(
    'plots/kibble_dashboard.png',
    width = 10,
    height = 5.5,
    units = 'in',
    res = 500
);

par(mfrow = c(2, 1), mar = c(1, 4, 0.2, 0.2));

wet.food.colour <- '#ecc745';
kibble.colour <- '#425d72';

# somewhat hack-y approach to lines
month.splits <- 1.2*c(4, 35, 65) + 0.1;

barplot(
    kibble.by.date$consumption, 
    ylab = 'Kibble consumed (g)', 
    space = 0.2,
    xaxs = 'i',
    col = kibble.colour
    );
abline(
    v = month.splits,
    lty = 3,
    lwd = 1.5,
    col = 'dimgrey'
    );


barplot(
    wet.food.by.date$consumption, 
    ylab = 'Wet food consumed (g)', 
    xaxs = 'i',
    space = 0.2,
    col = wet.food.colour
    );
abline(
    v = month.splits,
    lty = 3,
    lwd = 1.5,
    col = 'dimgrey'
);

mtext(
    'May',
    side = 1,
    at = 1.2*which(as.Date('2017-05-15') == wet.food.by.date$date)
    );
mtext(
    'June',
    side = 1,
    at = 1.2*which(as.Date('2017-06-15') == wet.food.by.date$date)
    );

dev.off();