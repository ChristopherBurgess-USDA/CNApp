#' load_std_data 
#'
#' @description A utils function which loads the data from google
#'
#' @return Returns the data
#'
#' @noRd

read_google_std = function(std_type){
  read_sheet(
    "1ON6vDrDc8fmQKMlF--SCl_sA1OCeU5V81YDtvyp8Xc8",
    sheet = paste("Elementar", std_type, sep = " ")
  ) %>%
    pivot_longer(
      -Date,
      names_to = "element", values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    return()
}

