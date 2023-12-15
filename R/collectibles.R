collectibles <- function(df_products) {
  df_products |>
    filter(str_detect(sku, "COL"))
}