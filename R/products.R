products <- function(df_products_raw) {
  df_products_raw |>
    mutate(
      category = str_remove(sku, "\\d+"),
      color = str_extract(desc, "\\(.+\\)"),
      desc_product = str_trim(
        str_remove_all(str_remove(desc, color), "\\)|\\(")))
}