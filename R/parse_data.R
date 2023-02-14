std_lookup = function(std_data, target_ele, in_data){
  std_data = filter(std_data, element == target_ele)
  mutate(in_data, qc_check = case_when(
    value >= pull(std_data, act_high) ~ "Action High",
    value <= pull(std_data, act_low) ~ "Action Low",
    value >= pull(std_data, warn_high) ~ "Warning High",
    value <= pull(std_data, warn_low) ~ "Warning Low",
    T ~ "Normal"
  )
  ) %>%
    return()
}


sample_std_merge = function(in_data, in_interval, in_check){
  filter(in_data, date_time %within% in_interval) %>%
    mutate(std_check = in_check) %>%
    select(Name, date_time, std_check) %>%
    return()
}

check_std = function(in_data, std_data) {
  
  in_data %>%
    filter(str_detect(Name, "NAPT")) %>%
    pivot_longer(-c("Name", "date_time"), names_to = "element", values_to = "value") %>%
    mutate(
      element = str_sub(element, 1, 1)
    ) %>%
    nest_by(element) %>%
    mutate(
      std_check = list(std_lookup(std_data, element, data))
    ) %>%
    select(-data) %>%
    unnest(std_check) %>%
    return()

}

check_sample = function(in_data, in_std){
  samples = filter(in_data, !str_detect(Name, "NAPT")) 
  in_std %>%
    select(-value) %>%
    pivot_wider(names_from = element, values_from = qc_check) %>%
    mutate(
      std_qc_1 = if_else(
        C == "Normal" & N == "Normal" & S == "Normal", "Pass", "Check STD"
      ),
      std_interval = interval(date_time, lead(date_time)),
      std_check = if_else(lead(std_qc_1) == "Pass" & std_qc_1 == "Pass", "STD Pass", "Check STD")
    ) %>%
    filter(!is.na(std_check)) %>%
    rowwise() %>%
    mutate(
      checked_data = list(sample_std_merge(samples, std_interval, std_check))
    ) %>%
    select(checked_data) %>%
    unnest(checked_data) %>%
    left_join(samples) %>%
    return()
}

update_std <- function(std_data, run_std){
  run_std <- select(run_std, date_time, element, value) %>%
    pivot_wider(names_from = element, values_from = value)
  bind_rows(std_data, run_std)
}
