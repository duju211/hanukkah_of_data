coffee_bagels <- function(df_products, df_order_items) {
  df_coffee <- df_products |>
    filter(str_detect(desc, regex("coffee", ignore_case = TRUE)))
  
  df_bagel <- df_products |>
    filter(str_detect(desc, regex("bagel", ignore_case = TRUE)))
  
  bind_rows(
    list(bagel = df_bagel, coffee = df_coffee), .id = "coffee_bagel") |>
    left_join(df_order_items, by = "sku")
}