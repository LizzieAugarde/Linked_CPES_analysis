######### Compare all rows with the same patient ID, identify duplicates, multiple responses for the same tumour and for different tumours #########
compare_rows <- function(data_frame) {
  data_frame$match_type <- NA
  unique_patient_ids <- unique(data_frame$PATIENTID)
  
  #loop through each patientid
  for (patient_id in unique_patient_ids) {
    patient_data <- data_frame[data_frame$PATIENTID == patient_id, ]
    
    #identify first response per patient
    first_response <- patient_data[1, 'row_number']
    
    #identify full duplicates
    duplicates <- all(apply(patient_data[, !names(patient_data) %in% c("row_number", "PATIENTID")], 1, function(x) all(x == patient_data[1, !names(patient_data) %in% c("row_number", "PATIENTID")])))
    
    #identify records where patient is responding in more than 1 year but diagnosis is the same in both years
    same_tumour_diff_year <- any(!is.na(patient_data$datayear[-1]) & patient_data$datayear[-1] != patient_data$datayear[1])
    
    #mark rows
    data_frame$match_type[first_response] <- "First response"
    
    if (exact_match_all && !differences_datayear) {
      data_frame$match_type[patient_data$row_number[-1]] <- "Duplicate record"
    } else if (!exact_match_all && differences_datayear) {
      data_frame$match_type[patient_data$row_number[-1]] <- "Same tumour different datayear"
    } else {
      data_frame$match_type[patient_data$row_number[-1]] <- "Different tumour different datayear"
    }
  }
  
  return(data_frame)
} 

######### Identify all rows with the same patient ID and rank them by the year, so the earliest response is ranked highest ###########
rank_multiple_responses <- function(data, match_column, rank_column) {
  #get unique PIDs
  unique_matches <- unique(data[[match_column]])
  
  #initialise empty list for ranking
  ranked_subsets <- list()
  
  #iterate through the PIDs
  for (match_value in unique_matches) {
    #subset by each PID
    subset_data <- data[data[[match_column]] == match_value, ]
    
    #rank the rows within each PID subset by the datayear
    subset_data <- subset_data[order(subset_data[[rank_column]], decreasing = FALSE), ]
    
    #add a rank variable
    subset_data$rank <- seq_len(nrow(subset_data))
    
    #add the ranked subset to the empty list
    ranked_subsets[[as.character(match_value)]] <- subset_data
  }
  
  #combine all the ranked PID subsets into a single data frame
  result <- do.call(rbind, ranked_subsets)
  
  return(result)
}
