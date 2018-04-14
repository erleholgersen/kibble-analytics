### DESCRIPTION ###############################################################
# Plot number of measurements per day
#

### LIBRARIES #################################################################
library(dplyr);

rm(list = ls(all.names = TRUE));

### MAIN ######################################################################

load('data/2018-03-30_kibble_and_wet_food.RData');

wet.food$date <- as.Date( paste(wet.food$date, '2017'), format = '%b %d %Y' );
kibble$date <- as.Date( paste(kibble$date, '2017'), format = '%b %d %Y' );

kibble.day.counts <- kibble %>% 
    group_by(date) %>% 
    summarize(kibble.n = n());

wet.food.day.counts <- wet.food %>%
    group_by(date) %>% 
    summarize(wet.food.n = n());

combined.counts <- merge(
    kibble.day.counts, 
    wet.food.day.counts,
    all = TRUE
    );

combined.counts <- combined.counts %>% 
    mutate(n = kibble.n + wet.food.n);

## make plot

options(bitmapType = 'cairo');

png(
    'plots/measurement_count.png',
    width = 10,
    height = 4.5,
    units = 'in',
    res = 500
    );

par(mar = c(2.5, 4, 0.5, 0.5));

plot(
    n ~ date,
    combined.counts,
    pch = 19,
    type = 'b',
    xlim = c(as.Date('2017-04-22'), as.Date('2017-07-10')),
    ylim = c(0, 30),
    yaxs = 'i',
    xlab = '',
    ylab = 'Cat food measurements',
    bty = 'l'
    );

# annotate key events
text(
    x = as.Date('2017-04-27'),
    y = 28,
    labels = 'Picked up coffee scale\nfrom Waterloo',
    cex = 0.9,
    adj = c(0.5, 1),
    col = 'gray20'
    );

text(
    x = as.Date('2017-05-18'),
    y = 28,
    labels = "Submitted master's\nresearch project",
    cex = 0.9,
    adj = c(0.5, 1),
    col = 'gray20'
);


text(
    x =  as.Date('2017-06-20'),
    y = 28,
    labels = 'Got a job offer!',
    cex = 0.9,
    adj = c(0.5, 1),
    col = 'gray20'
    );

text(
    x =  as.Date('2017-07-08'),
    y = 28,
    labels = 'Moved out',
    cex = 0.9,
    adj = c(0.5, 1),
    col = 'gray20'
);

dev.off();

