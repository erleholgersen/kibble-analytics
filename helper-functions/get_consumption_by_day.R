### get.consumption.by.day ####################################################
# Description:
#   Get data on food consumed by dat
# Input variable:
#   food   type of food to consider, kibble, wet-food, or total
# Output variable:
#   consumption.by.day    data frame containing date and grams of food consumed
get.consumption.by.day <- function(food = c('kibble', 'wet-food', 'total') ) {

	food <- match.arg(food);

	# treat total case separately
	if( 'total' == food ) {
		kibble.by.day <- get.consumption.by.day('kibble');
		wet.food.by.day <- get.consumption.by.day('wet-food');

		names(kibble.by.day) <- c('date', 'kibble.consumption');
		names(wet.food.by.day) <- c('date', 'wet.food.consumption');

		food.by.day <- merge(
			kibble.by.day,
			wet.food.by.day,
			by = 'date',
			all = TRUE
			);

		food.by.day$kibble.consumption[ is.na(food.by.day$kibble.consumption) ] <- 0;
		food.by.day$wet.food.consumption[ is.na(food.by.day$wet.food.consumption) ] <- 0;

		food.by.day <- food.by.day %>%
			mutate(consumption = kibble.consumption + wet.food.consumption) %>%
			select(date, consumption);

		return(food.by.day);
	}

	library(dplyr);

	load('data/2018-03-30_kibble_and_wet_food_intervals.RData');

	# consider intervals that are split across days separately from the rest
	# assume food consumption is uniform within intervals.. probably not the most
	# realistic assumption, but should work
	if( 'kibble' == food ) {
		continuous.intervals <- kibble.intervals %>%
    		filter(as.Date(start) == as.Date(end)) %>%
    		select(start, end, food_consumed);

		split.intervals <- kibble.intervals %>% 
   			filter(as.Date(start) != as.Date(end)) %>%
    		mutate(length = as.numeric(end - start));

	} else if ( 'wet-food' == food ) {
		continuous.intervals <- wet.food.intervals %>%
    		filter(as.Date(start) == as.Date(end)) %>%
    		select(start, end, food_consumed);

		split.intervals <- wet.food.intervals %>% 
    		filter(as.Date(start) != as.Date(end)) %>%
    		mutate(length = as.numeric(end - start));
	}

	# treat start and end separately
	split.starts <- split.intervals %>%
	    mutate(end = as.POSIXct( paste0(as.Date(start) + 1, ' 00:00:00'), tz = 'GMT') ) %>%
	    mutate(length_fraction = as.numeric( difftime(end, start, units = 'hours') )/length ) %>%
	    mutate(food_consumed = food_consumed*length_fraction) %>%
	    select(start, end, food_consumed);

	split.ends <- split.intervals %>%
	    mutate(start = as.POSIXct( paste0(as.Date(end), ' 00:00:00'), tz = 'GMT') ) %>%
	    mutate( length_fraction = as.numeric( difftime(end, start, units = 'hours') )/length ) %>%
	    mutate(food_consumed = food_consumed*length_fraction) %>%
	    select(start, end, food_consumed);

	# merge everything back together
	combined.intervals <- rbind(continuous.intervals, split.starts, split.ends) %>%
	    arrange(start, end);

	food.by.date <- combined.intervals %>% 
	    group_by( as.Date(start) ) %>% 
	    summarize(consumption = sum(food_consumed, na.rm = TRUE));

	names(food.by.date) <- c('date', 'consumption');

	return(food.by.date);
}