contractor <- function(df_orders_raw, df_order_items_coffee_bagel,
                       df_customers) {
  df_rel_orders <- df_orders_raw |>
    filter(year(ordered) == 2017) |>
    anti_join(filter(df_orders_raw, year(ordered) > 2017), by = "customerid") |>
    semi_join(df_order_items_coffee_bagel, by = "orderid") |>
    semi_join(filter(df_customers, initials == "jd"), by = "customerid")
  
  df_customers |>
    semi_join(df_rel_orders, by = "customerid")
}