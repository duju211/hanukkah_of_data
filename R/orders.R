orders <- function(df_orders_raw, df_order_items) {
  df_orders_raw |>
    select(-items) |>
    nest_join(df_order_items, by = "orderid", name = "items")
}