spider_hat <- function(df_customers, df_contractor, dog_years) {
  df_customers |>
    filter(year(birthdate) %in% dog_years) |>
    filter(
      case_when(
        month(birthdate) == 3 ~ day(birthdate) >= 21,
        month(birthdate) == 4 ~ day(birthdate) <= 20,
        TRUE ~ FALSE)) |>
    filter(str_detect(citystatezip, df_contractor$citystatezip))
}