# rm(list = ls()); gc()

# pacman::p_load(tidyverse, rio, here)

# Load the dataset
# df <- import(here('data', 'processed_data', 'processed_internet_access.csv'))

# # Some FIP codes have a leading 0, but Excel removes leading 0s.If the FIP code is not 5-digit, we add a leading 0 to make it a valid FIP code
# df <- df |> 
#   mutate(fips_cd = ifelse(nchar(fips_cd < 5), str_pad(fips_cd, width = 5, pad = "0", side = "left"), fips_cd))


# Helper function to check FIPS codes and create a dataframe of invalid FIPS codes within the function
detect_invalid_fips <- function(df) {
  # Create a dataframe of invalid FIPS codes
  invalid_fips <- df %>% 
    filter(nchar(fips_cd) != 5)
  
  # Print any invalid FIPS codes if they exist
  if (nrow(invalid_fips) > 0) {
    print("Warning: There are FIPS codes that are not 5 digits:")
    print(invalid_fips)
    
    # Assign the invalid FIPS dataframe to the global environment
    assign("invalid_fips_df", invalid_fips, envir = .GlobalEnv)
    
    # Save the invalid FIPS codes dataframe
    write_csv(invalid_fips, here::here('data', 'fip_errors', 'invalid_fips.csv'))
  } else {
    print("All FIPS codes are valid (5 digits).")
    # Remove invalid_fips_df from the global environment if it exists and there are no invalid FIPS codes
    if (exists("invalid_fips_df", envir = .GlobalEnv)) {
      rm(invalid_fips_df, envir = .GlobalEnv)
    }
  }
}

# detect_invalid_fips(df = df)