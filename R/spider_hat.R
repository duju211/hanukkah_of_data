spider_hat <- function(df_customers, df_contractor, df_chinese_year) {
  df_customers |>
    filter(
      map_lgl(
        birthdate,
        ~ any(
          df_chinese_year$start_date <= .x & df_chinese_year$end_date >= .x)),
      case_when(
        month(birthdate) == 9 ~ day(birthdate) >= 23,
        month(birthdate) == 10 ~ day(birthdate) <= 23,
        TRUE ~ FALSE),
      str_detect(citystatezip, df_contractor$citystatezip)) |>
    select(customerid, name, citystatezip, phone) |>
    verify(length(customerid) == 1)
}