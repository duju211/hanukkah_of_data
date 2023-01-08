frugal_cousin <- function(df_orders, df_customers) {
  df_customer_id <- df_orders |>
    mutate(
      profit = map_dbl(items, ~ sum(.x$unit_price - .x$wholesale_cost))) |>
    group_by(customerid) |>
    summarise(profit = sum(profit)) |>
    top_n(n = 1, wt = -profit)
  
  df_customers |>
    semi_join(df_customer_id, "customerid")
}