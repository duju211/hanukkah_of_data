order_items_pastries <- function(df_order_items, df_products) {
  df_products_pastries <- df_products |>
    filter(str_detect(sku, regex("bky", ignore_case = TRUE)))
  
  df_order_items |>
    semi_join(df_products_pastries, by = "sku")
}