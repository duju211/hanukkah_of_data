collector <- function(df_orders, df_order_items, df_collectibles) {
  df_order_items |>
    semi_join(df_collectibles, by = "sku") |>
    left_join(df_orders, by = "orderid") |>
    group_by(customerid) |>
    summarise(anz_coll = n_distinct(sku)) |>
    filter(anz_coll == nrow(df_collectibles))
}