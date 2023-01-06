products_coffee_bagel <- function(df_products, coffee_bagel_regex) {
  df_products |>
    filter(str_detect(desc, coffee_bagel_regex))
}