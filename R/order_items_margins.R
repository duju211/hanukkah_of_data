order_items_margin <- function(df_order_items, df_products) {
  df_order_items |>
    left_join(df_products, by = "sku") |>
    group_by(orderid) |>
    summarise(margin = sum(unit_price - wholesale_cost))
}