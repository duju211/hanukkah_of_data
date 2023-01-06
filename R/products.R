products <- function(df_products_raw) {
  df_products_raw |>
    mutate(category = str_remove(sku, "\\d+"))
}