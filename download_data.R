### DESCRIPTION ###############################################################
# Download data from Google Sheets
#
# This won't work unless you have access to the same Google Sheet...
#

### LIBRARIES #################################################################
library(hedgehog);

options(stringsAsFactors = FALSE, error = function() traceback(2) );

source.directory('helper-functions');

### MAIN ######################################################################

wet.food <- prepare.wet.food(add.refill.data = TRUE);
kibble <- prepare.kibble(add.wet.food.data = TRUE);

save(
    wet.food,
    kibble,
    file = file.path('data', date.stamp.file.name('kibble_and_wet_food.RData'))
    );


wet.food.intervals <- prepare.wet.food.intervals();
kibble.intervals <- prepare.kibble.intervals();

save(
	wet.food.intervals,
	kibble.intervals,
    file = file.path('data', date.stamp.file.name('kibble_and_wet_food_intervals.RData'))
    );
