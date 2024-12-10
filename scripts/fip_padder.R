# Helper function to add padding to FIP codes with zero padding to the left
# Valid US County FIP codes are 5-digit codes
add_fips_padding <- function(df) {
  df <- df |> 
    mutate(fips_cd = ifelse(nchar(fips_cd) < 5, str_pad(fips_cd, width = 5, pad = "0", side = "left"), fips_cd))

  return(df)
}