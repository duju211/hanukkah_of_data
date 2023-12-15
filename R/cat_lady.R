cat_lady <- function(df_order_items, df_orders, df_senior_cat_food,
                     df_staten_island) {
  df_order_items |>
    semi_join(df_senior_cat_food, by = "sku") |>
    left_join(select(df_orders, orderid, customerid), by = "orderid") |>
    semi_join(df_staten_island, by = "customerid") |>
    count(customerid, sort = TRUE) |>
    slice(1)
}