frugal_cousin <- function(df_orders, df_order_items_margin) {
  df_orders |>
    left_join(df_order_items_margin, by = "orderid") |>
    group_by(customerid) |>
    summarise(customer_margin = mean(margin)) |>
    arrange(customer_margin) |>
    slice(1)
}