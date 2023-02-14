std_table_create = function(tab_data){
  tab_df = tab_data %>%
    rename(`QC Check` = qc_check, Value = value) %>%
    mutate(
      hour  = hour(date_time),
      min = minute(date_time),
      Name = paste0(Name, " (", hour, ":", min, ")"),
      Value = as.character(Value)
    ) %>%
    select(-date_time, -hour, -min) %>%
    pivot_longer(
      -one_of(c("element", "Name")),
      names_to = "measure",
      values_to = "temp"
    ) %>%
    unite(c_name, element, measure, sep = ";") %>%
    pivot_wider(names_from = c_name, values_from = temp) %>%
    ungroup()
  
  c_names = colnames(tab_df)[str_detect(colnames(tab_df), "QC Check")]
  
  cell_coloring = function(col, compare){
    cells_body(
      columns = !!sym(col),
      rows = str_detect(!!sym(col), compare)
    )
  }
  
  gt(tab_df, rowname_col = "element") %>%
    tab_style(
      style = list(cell_fill(color = '#FDE725FF'), cell_text(weight = 'bold')),
      locations = lapply(c_names, cell_coloring, compare = "Warning")
    ) %>%
    tab_style(
      style = list(cell_fill(color = '#F2637F'), cell_text(weight = 'bold')),
      locations = lapply(c_names, cell_coloring, compare = "Action")
    ) %>%
    tab_spanner_delim(delim = ";") %>%
    tab_header(title = "Run Standard Check") %>%
    return()
}

std_graph_format = function(std_data, h_lines, date_range){
  ## The function graphs takes the filtered data and a y variable to plot
  ## Paramters: data (tibble) and y variable (string)
  ## Returns: ggplot
  std_data %>%
    pivot_longer(-date_time, names_to = "element", values_to = "value") %>%
    left_join(h_lines) %>%
    ggplot(aes(x = date_time, y = value)) +
    geom_point(size = 3) +
    geom_hline(aes(yintercept = warn_low)) +
    geom_hline(aes(yintercept = warn_high)) +
    facet_wrap(~element, ncol = 1, scales = "free_y") +
    theme_cowplot(16) +
    xlim(c(date_range[1], date_range[2])) +
    labs(x = "Time", y = "Element Percentage") +
    panel_border() +
    background_grid() %>%
    return()
}


sam_graph_format = function(g_data){
  ## The function graphs takes the filtered data and a y variable to plot
  ## Paramters: data (tibble) and y variable (string)
  ## Returns: ggplot
  g_data %>%
    pivot_longer(contains("%"), names_to = "element", values_to = "value") %>%
    ggplot(aes(x = date_time, y = value)) +
    geom_point(size = 3) +
    facet_wrap(~element, ncol = 1, scales = "free_y") +
    theme_cowplot(16) +
    labs(x = "Time", y = "Element Percentage", title = "Sample Data") +
    panel_border() +
    background_grid() %>%
    return()
}

