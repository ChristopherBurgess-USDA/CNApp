#' load_data 
#'
#' @description A utils function which contains all the inport data functions
#'
#' @return Returns the imported data
#'
#' @noRd


load_data = function(input_path){
  read_html(input_path) %>%
    html_element("table") %>%
    html_table(header = T) %>%
    select(
      Name, date_time = contains("Date"), contains("%") 
    ) %>%
    filter(!(Name %in% c("Empty", ""))) %>%
    mutate(
      date_time = parse_date_time(date_time, "mdYHM")
    )
}

