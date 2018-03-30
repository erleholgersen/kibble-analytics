### prepare.kibble.intervals ######################################################################
# Description:
#   Get data on kibble consumed by time interval
# Input variable:
#   duration.unit   unit for interval duration, defaults to hours
# Output variable:
#   kibble.intervals    data frame containing time 
prepare.kibble.intervals <- function(duration.unit = 'hours') {

    library(lubridate);

    kibble <- prepare.kibble();

    n <- nrow(kibble);

    kibble.intervals <- data.frame(
        start = kibble$timestamp[1:(n-1)],
        end = kibble$timestamp[2:n],
        food_start = kibble$food_left[1:(n-1)],
        food_end = kibble$food_left[2:n],
        food_consumed = kibble$food_left[1:(n-1)] - kibble$food_left[2:n],
        # get indicator of refill happening at end of interval
        refill_interval = kibble$refilled[2:n]
        );
    
    # if refill interval, no idea how much food was consumed
    kibble.intervals$food_consumed[kibble.intervals$refill_interval] <- NA;

    # calculate duration of interval
    kibble.intervals$duration <- difftime(
        kibble.intervals$end, 
        kibble.intervals$start, 
        unit = duration.unit
        );

    # convert to numeric, who needs a difftime object?
    kibble.intervals$duration <- as.numeric(kibble.intervals$duration);

    kibble.intervals$consumption_per_unit <- kibble.intervals$food_consumed/kibble.intervals$duration;

    # add time since refill
    refill.intervals <- kibble.intervals$end[kibble.intervals$refill_interval];


    matched.indices <- findInterval(kibble.intervals$end, refill.intervals);
    matched.indices[0 == matched.indices] <- NA;
    matched.refill.intervals <- refill.intervals[matched.indices];

    kibble.intervals$time_since_refill <- difftime(
        kibble.intervals$end, 
        matched.refill.intervals, 
        unit = duration.unit
        );

    kibble.intervals$time_since_refill <- as.numeric(kibble.intervals$time_since_refill);

    return(kibble.intervals);

}