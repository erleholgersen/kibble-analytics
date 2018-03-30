### prepare.wet.food.intervals ####################################################################
# Description:
#   Get data on wet food consumed by time interval
# Input variable:
#   duration.unit   unit for interval duration, defaults to hours
# Output variable:
#   wet.food.intervals    data frame containing time intervals 
prepare.wet.food.intervals <- function(duration.unit = 'hours') {

    library(lubridate);

    # weight of bowls
    symmetric.weight <- 180.78;
    asymmetric.weight <- 203.54;
    
    wet.food <- prepare.wet.food(add.refill.data = TRUE);
    
    # add bowl specific data
    wet.food$symmetric_food_left <- wet.food$symmetric_bowl_weight - symmetric.weight;
    wet.food$asymmetric_food_left <- wet.food$asymmetric_bowl_weight - asymmetric.weight;

    n <- nrow(wet.food);

    wet.food.intervals <- data.frame(
        start = wet.food$timestamp[1:(n-1)],
        end = wet.food$timestamp[2:n],
        food_start = wet.food$food_left[1:(n-1)],
        food_end = wet.food$food_left[2:n],
        food_consumed = wet.food$food_left[1:(n-1)] - wet.food$food_left[2:n],
        symmetric_food_start = wet.food$symmetric_food_left[1:(n-1)],
        symmetric_food_end = wet.food$symmetric_food_left[2:n],
        symmetric_food_consumed = wet.food$symmetric_food_left[1:(n-1)] - wet.food$symmetric_food_left[2:n],
        asymmetric_food_start = wet.food$asymmetric_food_left[1:(n-1)],
        asymmetric_food_end = wet.food$asymmetric_food_left[2:n],
        asymmetric_food_consumed = wet.food$asymmetric_food_left[1:(n-1)] - wet.food$asymmetric_food_left[2:n],
        # get indicator of refill happening at end of interval
        refill_interval = wet.food$refilled[2:n],
        closest_livingroom =  wet.food$closest_livingroom[1:(n-1)],
        group = as.factor(wet.food$group[1:(n-1)])
        );

    # if refill interval, no idea how much food was consumed
    wet.food.intervals$food_consumed[wet.food.intervals$refill_interval] <- NA;

    # calculate duration of interval
    wet.food.intervals$duration <- difftime(
        wet.food.intervals$end, 
        wet.food.intervals$start, 
        unit = duration.unit
        );

    # convert to numeric, who needs a difftime object?
    wet.food.intervals$duration <- as.numeric(wet.food.intervals$duration);

    # add duration per unit
    wet.food.intervals$consumption_per_unit <- wet.food.intervals$food_consumed/wet.food.intervals$duration;

    # add time since refill
    refill.intervals <- wet.food.intervals$end[wet.food.intervals$refill_interval];

    # add time since refill
    matched.indices <- findInterval(wet.food.intervals$end, refill.intervals);
    matched.indices[0 == matched.indices] <- NA;
    matched.refill.intervals <- refill.intervals[matched.indices];

    wet.food.intervals$time_since_refill <- difftime(
        wet.food.intervals$end, 
        matched.refill.intervals, 
        unit = duration.unit
        );

    wet.food.intervals$time_since_refill <- as.numeric(wet.food.intervals$time_since_refill);

    return(wet.food.intervals);

}