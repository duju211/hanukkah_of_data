products_coffee_bagel <- function(df_products_raw, coffee_bagel_regex) {
  df_products_raw |>
    filter(str_detect(desc, coffee_bagel_regex))
}