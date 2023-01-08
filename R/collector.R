collector <- function(df_products, df_orders, df_customers, product_collect) {
  df_products_collect <- df_products |>
    filter(category == product_collect)
  
  df_orders_unnested <- df_orders |>
    unnest(items)
  
  df_customer_id <- df_orders_unnested |>
    semi_join(df_products_collect, by = c("sku")) |>
    group_by(customerid) |>
    summarise(anz_col = n_distinct(sku)) |>
    arrange(desc(anz_col)) |>
    slice(1)
  
  df_customers |>
    semi_join(df_customer_id, by = "customerid")
}