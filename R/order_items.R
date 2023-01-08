order_items <- function(df_order_items_raw, df_products) {
  df_order_items_raw |>
    left_join(df_products, by = "sku")
}