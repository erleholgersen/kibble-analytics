### DESCRIPTION ###############################################################
# CatterPlot with overlaid isotonic regression
#

### LIBRARIES #################################################################
library(CatterPlots);
library(lubridate);
library(hedgehog);

options(stringsAsFactors = FALSE);

source.directory('helper-functions');

### PARAMETERS ################################################################

xmax <- 24;
cat.size <- 0.028;

# colours
wet.food.colour <- '#ecc745';
kibble.colour <- '#425d72';


### PREPARE DATA ##############################################################

load('data/2018-03-30_kibble_and_wet_food.RData');

# restrict to points we want in plot
wet.food <- wet.food[wet.food$time_elapsed <= xmax & wet.food$time_elapsed > 0, ];
kibble <- kibble[kibble$time_elapsed <= xmax & kibble$time_elapsed > 0 & !is.na(kibble$food_consumed), ];


# round up negative numbers to zero - measurement error
kibble$food_consumed <- pmax(kibble$food_consumed, 0);

### MAKE PLOT #################################################################

# x for plotting
x <- seq(0, 24, by = 0.01);

wet.food.regression <- isoreg(
    y = wet.food$food_consumed,
    x = wet.food$time_elapsed
);

kibble.regression <- isoreg(
    y = kibble$food_consumed,
    x = kibble$time_elapsed
    );

hack.args <- list(
    xlim = c(0, 24),
    ylim = c(0, 150)
    );

scaled.wet.food.regression <- CatterPlots:::scaleData(
    x,
    as.stepfun(wet.food.regression)(x),
    args = hack.args
    );

scaled.kibble.regression <- CatterPlots:::scaleData(
    x,
    as.stepfun(kibble.regression)(x),
    args = hack.args
    );

options(bitmapType = 'cairo');

png(
    'plots/wet_food_kibble_isotonic.png',
    width = 8,
    height = 5,
    units = 'in',
    res = 500
);

par(
    mar = c(4.5, 4, 1, 7.5),
    xpd = TRUE
);

purr <- multicat(
    wet.food$time_elapsed, 
    wet.food$food_consumed,
    catcolor = list(wet.food.colour),
    size = cat.size,
    xlim = c(0, 24),
    ylim = c(0, 150),
    cat = sample(
        1:10, 
        nrow(wet.food), 
        replace = TRUE
    ),
    canvas = c(0, 1, 0, 1),
    bty = 'L',
    xat = seq(0, 24, length.out = 5),
    yat = seq(0, 150, length.out = 4),
    xlab = 'Hours since wet food distributed',
    ylab = 'Food consumed (g)'
    # yaxtlab = seq(0, 150, length.out = 4)
);


lines(
    scaled.wet.food.regression$xscale,
    scaled.wet.food.regression$yscale,
    lwd = 3,
    col = wet.food.colour
);

morecats(
    purr,
    kibble$time_elapsed, 
    kibble$food_consumed,
    catcolor = list(kibble.colour),
    cat = sample(
        1:10, 
        nrow(wet.food), 
        replace = TRUE
    ),
    size = cat.size,
);

lines(
    scaled.kibble.regression$xscale,
    scaled.kibble.regression$yscale,
    lwd = 3,
    col = kibble.colour
);

# add legend
legend(
    1.03, 
    0.6,
    legend = c('Wet food', 'Kibble'),
    fill = c(wet.food.colour, kibble.colour),
    bty = 'n'
);

dev.off();

