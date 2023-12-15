spider_hat <- function(df_customers, df_contractor, df_rabbit_years) {
  df_spider_hat <- df_customers |>
    filter(
      map_lgl(
        birthdate,
        ~ any(
          df_rabbit_years$start_date <= .x & df_rabbit_years$end_date >= .x)),
      case_when(
        month(birthdate) == 6 ~ day(birthdate) >= 21,
        month(birthdate) == 7 ~ day(birthdate) <= 23,
        TRUE ~ FALSE),
      str_detect(citystatezip, df_contractor$citystatezip)) |>
    select(customerid, name, citystatezip, phone) |>
    verify(length(customerid) == 1)
}