### prepare.wet.food ##############################################################################
# Description:
#   Load and prepare wet food data from Google Sheets
# Input variable:
#   add.refill.data   logical indicating whether more information about meal should be added
# Output variable:
#   wet.food    data frame of wet food data after processing
prepare.wet.food <- function(add.refill.data = FALSE) {

    library(lubridate);

    
    library(googlesheets);

    # read data from Google Sheets
    cat.food.sheet <- gs_title('Cat Food Half-Life');
    
    wet.food <- gs_read(
        cat.food.sheet,
        'Wet food'
        );
    
    # convert to data frame
    wet.food <- as.data.frame(wet.food);
    
    # change refilled column to true/false
    #! should add check to verify that only Yes/No appears in column
    wet.food$refilled <- 'Yes' == wet.food$refilled;
    
    # need to impute food left for times I forgot to measure bowls after refilling them
    # assume normal distribution - might need to stratify by brand
    full.weight.mean <- mean(wet.food$food_left[wet.food$refilled], na.rm = TRUE);
    full.weight.sd <- sd(wet.food$food_left[wet.food$refilled], na.rm = TRUE);
    
    # generate missing values
    wet.food$food_left[is.na(wet.food$food_left)] <- rnorm(
        sum(is.na(wet.food$food_left)),
        mean = full.weight.mean,
        sd = full.weight.sd
        );
    
    # add datestamp column
    wet.food$timestamp <- as.POSIXct(
        paste(
            wet.food$date, 
            '2017',
            wet.food$time
        ),
        format = '%B %d %Y %H:%M:%S'
        );
    
    
    # if requested, add time since last refill and other info on meal
    if (add.refill.data) {
    
        last.refill <- NA;
        meal.weight <- NA;
        group.count <- 1;
        meal <- NA;
        brand <- NA;
        
        wet.food$time_elapsed <- rep(NA, nrow(wet.food));
        wet.food$food_consumed <- rep(NA, nrow(wet.food));
        wet.food$group <- rep(NA, nrow(wet.food));
        wet.food$meal <- rep(NA, nrow(wet.food));
        
        #! Check assumption: timestamp monotone increasing 
        
        # loop over rows, check if row represents refill, and add difference
        for (i in 1:nrow(wet.food)) {
            
            if (wet.food$refilled[i])  {
                last.refill <- wet.food$timestamp[i];
                meal.weight <- wet.food$food_left[i];

                
                # get information about meal & brand
                brand <- wet.food$brand[i];
                meal <- ifelse(
                    hour(wet.food$timestamp[i]) < 12, 
                    'breakfast',
                    'dinner'
                );
                
                # if not first row, increment group count
                if(1 != i) group.count <- group.count + 1;
                
            }
            
            wet.food$time_elapsed[i] <- difftime(
                wet.food$timestamp[i], 
                last.refill,
                units = 'hours'
                );
            
            wet.food$food_consumed[i] <- meal.weight - wet.food$food_left[i];
            
            wet.food$group[i] <- group.count;
            wet.food$meal[i] <- meal;
            wet.food$brand[i] <- brand;
        }
    }
    
    
    return(wet.food);
}