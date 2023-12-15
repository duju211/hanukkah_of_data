color_orders <- function(df_orders, df_order_items, df_products) {
  df_orders |>
    left_join(df_order_items, by = c("orderid")) |>
    left_join(df_products, by = "sku") |>
    mutate(
      color = str_remove_all(str_extract(desc, "\\(.+\\)"), "\\(|\\)"),
      day = as_date(floor_date(ordered, unit = "day"))) |>
    filter(!is.na(color))
}