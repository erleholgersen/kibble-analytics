### prepare.kibble ################################################################################
# Description:
#   Load and prepare kibble data from Google Sheets
# Input variable:
#   add.refill.data   logical indicating whether information about time since refill should be added
#   add.wet.food.data   logical indicating whether information about wet food distribution should be added
# Output variable:
#   kibble    data frame of kibble data after processing
prepare.kibble <- function(add.refill.data = FALSE, add.wet.food.data = FALSE) {
    
    library(googlesheets);
    library(lubridate);

    
    # read data from Google Sheets
    cat.food.sheet <- gs_title('Cat Food Half-Life');
    
    kibble <- gs_read(
        cat.food.sheet,
        'Kibble'
    );
    
    # convert to data frame
    kibble <- as.data.frame(kibble);
    
    # change refilled column to true/false
    #! should add check to verify that only Yes/No appears in column
    kibble$refilled <- 'Yes' == kibble$refilled;
    
    # add timestamp column
    kibble$timestamp <- as.POSIXct(
        paste(
            kibble$date, 
            '2017',
            kibble$time
        ),
        format = '%B %d %Y %H:%M'
        );
    
    
    if (add.refill.data) {
        
        last.refill <- NA;
        group.count <- 1;
        
        kibble$time_elapsed<- rep(NA, nrow(kibble));
        
        #! Check assumption: timestamp monotone increasing 
        
        # loop over rows, check if row represents refill, and add difference
        for (i in 1:nrow(kibble)) {
            
            if (kibble$refilled[i])  {
                last.refill <- kibble$timestamp[i];

                # if not first row, increment group count
                if(1 != i) group.count <- group.count + 1;
                
            }
            
            kibble$time_elapsed[i] <- difftime(
                kibble$timestamp[i], 
                last.refill,
                units = 'hours'
            );
            
            kibble$group[i] <- group.count;
        
        }
    }
    
    if (add.wet.food.data) {
        
        wet.food <- prepare.wet.food();
        
        # get timestamps at which wet food was refilled
        wet.food.refill.timestamps <- wet.food$timestamp[wet.food$refilled];
        
        # get kibble left at wet food refill timestamps
        kibble.indices <- findInterval(wet.food.refill.timestamps, kibble$timestamp);
        kibble.indices <- c(NA, kibble.indices)
        

        # assemble to data frame
        wet.food.refills <- data.frame(
            refill.timestamp = wet.food.refill.timestamps,
            kibble.timestamp = kibble$timestamp[kibble.indices],
            kibble.weight = kibble$food_left[kibble.indices]
            );
        
        # loop over kibble rows to get information about kibble consumed since last wet food refill
        meal.weight <- NA;
        last.refill <- NA;
        
        consumption.backlog <- 0;
        
        kibble$food_consumed <- rep(NA, nrow(kibble));
        kibble$time_elapsed <- rep(NA, nrow(kibble));
        
        for (i in 1:nrow(kibble)) {
            
            if (kibble$refilled[i]) {
                if(!is.na(kibble$food_consumed[i - 1]) && !(kibble$timestamp[i - 1] %in% wet.food.refills$kibble.timestamp)) {
                    consumption.backlog <- kibble$food_consumed[i - 1];
                }
                meal.weight <- kibble$food_left[i];
            }
            
            
            kibble$time_elapsed[i] <- difftime(
                kibble$timestamp[i], 
                last.refill, 
                unit = 'hours'
                );            
            
            kibble$food_consumed[i] <- consumption.backlog + meal.weight - kibble$food_left[i];
    
            if (kibble$timestamp[i] %in% wet.food.refills$kibble.timestamp) {
                consumption.backlog <- 0;
                meal.weight <- kibble$food_left[i];
                
                last.refill <- wet.food.refills$refill.timestamp[match(kibble$timestamp[i], wet.food.refills$kibble.timestamp)];
                
            }
            
            
        }
        
        
    }
    
    
    return(kibble);
    
}



