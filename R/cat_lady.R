cat_lady <- function(df_products, df_orders, df_customers) {
  df_products_cat <- df_products |>
    filter(str_detect(desc, regex("cat", ignore_case = TRUE))) |>
    filter(str_detect(desc, regex("senior", ignore_case = TRUE)))
  
  df_customers_queens <- df_customers |>
    filter(str_detect(citystatezip, regex("queens", ignore_case = TRUE)))
  
  df_customer_id <- df_orders |>
    semi_join(df_customers_queens, by = "customerid") |>
    unnest(items) |>
    semi_join(df_products_cat, by = "sku") |>
    group_by(customerid) |>
    summarise(sku = str_flatten(unique(sku), ", "), anz = n()) |>
    filter(anz > 2)
  
  df_customers |>
    semi_join(df_customer_id, by = "customerid")
}