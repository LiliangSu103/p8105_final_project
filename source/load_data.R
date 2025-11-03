#' Cleans, filters, and saves Motor Vehicle Collision data.
#'
#' @param input_path The file path to the raw, downloaded CSV file.
#' @param output_prefix The character string to use as the base name 
#'        for the output file (e.g., "Manhattan_2016_2025").
#'
#' @return The filtered data frame (invisibly) and writes the data to a new CSV file.
#' 
#' 
load_data = function(data_name) {
  
  # Construct the Dynamic Output File Path
  output_file_path = str_c("./data/", data_name, "_filtered.csv")
  
  # Read the raw data
  data_processed = read_csv(str_c("./data/Motor_Vehicle_Collisions_-_", data_name, ".csv"), na = c("NA",".","")) |>
    janitor::clean_names() |>
    
    # Keep Manhattan records
    filter(borough == "MANHATTAN") |>
    
    # Separate the crash_date column (creates character columns)
    separate(
      crash_date,
      into = c("month", "day", "year"),
      sep = "/"
    ) |>
    
    # filter for the specified range (2016-2025)
    mutate(year = as.numeric(year)) |>
    filter(year %in% 2016:2025) |>
    
    # arrange
    arrange(year, month, day, collision_id)
  
  # Write to CSV
  write_csv(data_processed, output_file_path)
  
  return(data_processed)
}