products <- function(df_products_raw) {
  df_products_raw |>
    mutate(
      category = str_remove(sku, "\\d+"),
      add_info = str_extract(desc, "\\(.+\\)"),
      desc = if_else(!is.na(add_info), str_remove(desc, add_info), desc),
      across(c(add_info, desc), ~ str_trim(str_remove_all(.x, "\\)|\\("))))
}