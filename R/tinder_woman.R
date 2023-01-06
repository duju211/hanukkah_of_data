tinder_woman <- function(df_products, df_order_items_raw, df_orders_raw,
                         df_customers, product_pastries) {
  df_products_pastries <- df_products |>
    filter(category == product_pastries)
  
  df_order_items_pastries <- df_order_items_raw |>
    semi_join(df_products_pastries, by = "sku")
  
  df_first_pastries <- df_orders_raw |>
    semi_join(df_order_items_pastries, by = "orderid") |>
    mutate(day = floor_date(ordered, unit = "day")) |>
    group_by(day) |>
    filter(ordered == min(ordered)) |>
    ungroup() |>
    count(customerid, sort = TRUE)
  
  df_customers |>
    semi_join(slice(df_first_pastries, 1), by = "customerid")
}