staten_island <- function(df_customers) {
  df_customers |>
    transmute(customerid, city = map_chr(str_split(citystatezip, ","), 1)) |>
    filter(str_detect(city, regex("Staten\\s+Island", ignore_case = TRUE)))
}