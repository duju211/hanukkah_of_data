order_items_coffee_bagel <- function(df_order_items_raw,
                                     df_products_coffee_bagel) {
  df_order_items_raw |>
    semi_join(df_products_coffee_bagel, by = "sku")
}