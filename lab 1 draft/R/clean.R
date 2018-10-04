# Functions for cleaning the data

cleanDatesData <- function(date_df) {
  # Arguments:
  #   date_df: a data.frame in the format of the output of the 
  #     loadDatesData() function
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime)
  
  # convert the dates variable to lubridate format
  date_df <- date_df %>% 
    # separate date variable into two variables: time and date
           # remove times
    mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep)
 
  return(date_df)
}


cleanRedwoodData <- function(redwood_df) {
  # Arguments:
  #   redwood_df: a data.frame in the format of the output of the 
  #     loadRedwoodData() function
  # Returns:
  #   a data.frame similar to the input `redwood_df` but with result_time 
  #   converted to lubridate ymd_hms format
  
  # convert result_time to lubridate ymd_hms format
  redwood_df$result_time <- ymd_hms(redwood_df$result_time)
  
  return(redwood_df)
}

getAnalysisData <- function(cleaned_df) {
  # Arguments:
  #   cleaned_df: a data.frame after data cleaning
  # Returns:
  #   a data.frame similar to the input `redwood_df` but with result_time 
  #   converted to lubridate ymd_hms format
  
  analysis_df <- 
    select(cleaned_df,
           nodeid, Direc, Dist, Height, datetime, humidity, humid_temp, 
           hamatop, hamabot)
  
  analysis_df$hour <- 
    hour(analysis_df$datetime)
  
  return(analysis_df)
}

getRedwoodPlot <- function(sonoma_all_df, date_df, mote_location_df) {
  # Arguments:
  #   sonoma_all_df: a data.frame in the format of the output of the 
  #     cleanRedwoodData() function
  #   date_df: a data.frame in the format of the output of the 
  #     cleanDatesData() function
  #   mote_location_df: a data.frame in the format of the output of the 
  #     loadMoteLocationData() function
  # Returns:
  #   a cleaned data.frame of the sonoma redwood tree data for plotting
  #
  
  # Merge in dates data
  sonoma_all_df_epoch <- inner_join(sonoma_all_df, 
                                    select(date_df, number, datetime),
                                    by = c("epoch"="number"))
  rm(sonoma_all_df, date_df)
  
  # Merge in mote location data
  sonoma_all_df_mote <-
    inner_join(sonoma_all_df_epoch, mote_location_df, 
               by = c("nodeid" ="ID"))
  rm(sonoma_all_df_epoch)
  
  # Remove 78,302 observations that don't correspond to one of 33 interior motes
  sonoma_all_df_filter_motes <-
    filter(sonoma_all_df_mote, Tree == 'interior')
  rm(sonoma_all_df_mote, mote_location_df)
  
  # Drop missing data:
  sonoma_all_df_filter_missing <- 
    filter(sonoma_all_df_filter_motes, !is.na(humid_temp))
  rm(sonoma_all_df_filter_motes)
  
  # Label readings with voltage > 3 volts or < 2.4 volts as voltage outliers
  sonoma_all_df_filter_missing$voltage_outlier <-
    with(sonoma_all_df_filter_missing,
         ifelse(voltage <= 3 & voltage >= 2.4,
                "Normal Voltage",
                "Outlier Voltage"))
  
  # Remove voltage outliers
  sonoma_all_df_filter_voltage <-
    filter(sonoma_all_df_filter_missing,
           voltage_outlier != 'Outlier Voltage')
  rm(sonoma_all_df_filter_missing)
  
  # nodeid == 109 seems to be the problem node
  # Remove 345 observations for nodeid 109
  sonoma_all_df_filter_rmnode <-
    filter(sonoma_all_df_filter_voltage, nodeid != 109)
  rm(sonoma_all_df_filter_voltage)
  
  sonoma_all_df_plot <-
    select(sonoma_all_df_filter_rmnode,
           epoch, humidity, humid_temp, datetime)
  
  rm(sonoma_all_df_filter_rmnode)
  
  return(sonoma_all_df_plot)
}

