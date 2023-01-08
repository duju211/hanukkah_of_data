contractor <- function(df_orders, df_customers, coffee_bagel_regex) {
  df_customers_after_2017 <- df_orders |>
    filter(year(ordered) > 2017) |>
    distinct(customerid)
  
  df_rel_orders <- df_orders |>
    filter(year(ordered) <= 2017) |>
    anti_join(df_customers_after_2017, by = "customerid") |>
    unnest(items) |>
    filter(str_detect(desc, coffee_bagel_regex)) |>
    semi_join(filter(df_customers, initials == "jd"), by = "customerid")
  
  df_customers |>
    semi_join(df_rel_orders, by = "customerid")
}