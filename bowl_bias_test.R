### DESCRIPTION ###############################################################
# Test for preference between bowls
#

### PREAMBLE ##################################################################
library(CatterPlots);
library(lubridate);
library(hedgehog);
library(tidyr);
library(dplyr);
library(nlme);


set.seed(1234);

# clear workspace
rm(list = ls(all = TRUE));

options(stringsAsFactors = FALSE);

# load helper functions
source.directory('helper-functions');


### MAIN ######################################################################

load('data/2018-03-30_kibble_and_wet_food_intervals.RData');

# minimum amount of food remaining
cutoff <- 5;

# remove rows without closest livingroom information
wet.food.intervals <- wet.food.intervals %>%
    filter( !is.na(closest_livingroom) & !refill_interval) %>%
    filter(symmetric_food_start >= cutoff & asymmetric_food_start >= cutoff) %>%
    filter( time_since_refill < 12 ) %>%
    group_by(group) %>% 
    mutate(
        asymmetric_total = cumsum(asymmetric_food_consumed), 
        symmetric_total = cumsum(symmetric_food_consumed)
        ) %>%
    ungroup() %>%
    arrange(start);


wet.food.long <- wet.food.intervals %>% 
    select(group, start, end, time_since_refill, duration, closest_livingroom, asymmetric_total, symmetric_total) %>%
    gather(key = 'bowl', value = 'food_consumed', asymmetric_total, symmetric_total);

# rename values in bowl and closest_livingroom columns
wet.food.long$bowl <- gsub('_total', '', wet.food.long$bowl)
wet.food.long$closest_livingroom <- wet.food.long$closest_livingroom == wet.food.long$bowl;


lm.model <- lm( 
    food_consumed ~ bowl + closest_livingroom*time_since_refill + bowl*time_since_refill,
    wet.food.long
    );


lme.model <- lme(
    food_consumed ~ bowl + closest_livingroom*time_since_refill + bowl*time_since_refill,
    random =  ~bowl|group,
    data = wet.food.long
    );

print( summary(lme.model) );



# # plot all four categories
colour.scheme <- c('#a6cee3', '#1f78b4', '#fdbf6f', '#ff7f00');
colour <- rep(NA, nrow(wet.food.long));

colour[ 'symmetric' == wet.food.long$bowl & !wet.food.long$closest_livingroom ] <- colour.scheme[1];
colour[ 'symmetric' == wet.food.long$bowl & wet.food.long$closest_livingroom ] <- colour.scheme[2];
colour[ 'asymmetric' == wet.food.long$bowl & !wet.food.long$closest_livingroom ] <-  colour.scheme[3];
colour[ 'asymmetric' == wet.food.long$bowl & wet.food.long$closest_livingroom ] <-  colour.scheme[4];


cat.size <- 0.05;
xmax <- 12;
ymax <- 90;


png(
    'plots/bowl_bias_multicoloured.png',
    width = 8,
    height = 5.5,
    units = 'in',
    res = 500
    );


par(xpd = FALSE, mar = c(4, 4, 0.5, 0.5));
purr <- multicat(
    wet.food.long$time_since_refill, 
    wet.food.long$food_consumed,
    catcolor = as.list(colour),
    size = cat.size,
    xlim = c(0, xmax),
    ylim = c(0, ymax),
    cat = as.numeric( as.factor(colour) ),
    canvas = c(0, 1.05, 0, 1.05),
    bty = 'L',
    xat = seq(0, 12, length.out = 5),
    yat = seq(0, 100, length.out = 6),
    xlab = 'Hours since wet food distributed',
    ylab = 'Food consumed (g)'
    # yaxtlab = seq(0, 150, length.out = 4)
);

legend(
    'topleft',
    legend = c('Symmetric, kitchen side', 'Symmetric, living room side', 'Asymmetric, kitchen side', 'Asymmetric, living room side'),
    fill = colour.scheme,
    cex = 0.8,
    bty = 'n'
);

dev.off();


# plot with regression fit – only consider significant covariate
colour.scheme <- c('dimgray', '#cab2d6');

colour <- ifelse(
    wet.food.long$closest_livingroom,
    colour.scheme[1],
    colour.scheme[2]
    );
png(
    'plots/bowl_bias_multicoloured_with_fit.png',
    width = 8,
    height = 5.5,
    units = 'in',
    res = 500
);


par(xpd = FALSE, mar = c(4, 4, 0.5, 0.5));
purr <- multicat(
    wet.food.long$time_since_refill, 
    wet.food.long$food_consumed,
    catcolor = as.list(colour),
    size = cat.size,
    xlim = c(0, xmax),
    ylim = c(0, ymax),
    cat = as.numeric( as.factor(colour) ) + 7,
    canvas = c(0, 1.05, 0, 1.05),
    bty = 'L',
    xat = seq(0, 12, length.out = 5),
    yat = seq(0, 100, length.out = 6),
    xlab = 'Hours since wet food distributed',
    ylab = 'Food consumed (g)'
    # yaxtlab = seq(0, 150, length.out = 4)
);

legend(
    'topleft',
    legend = c('Living room side', 'Kitchen side'),
    fill = colour.scheme,
    cex = 0.8,
    bty = 'n'
    );

abline(
    a = (24.416994 + 14.078871)/ymax,
    b = 3.841742/xmax,
    col = colour.scheme[1],
    lty = 3,
    lwd = 3
    );

abline(
    a = 24.416994/ymax,
    b = 3.841742/xmax,
    col = colour.scheme[2],
    lty = 3,
    lwd = 3
    );

dev.off();