ex_boyfriend <- function(df_frugal_cousin, df_orders, df_customers) {
  df_orders_color <- df_orders |>
    semi_join(df_frugal_cousin, by = "customerid") |>
    unnest(items) |>
    mutate(day = floor_date(ordered, "day")) |>
    filter(!is.na(color))
  
  df_order_rel <- df_orders |>
    mutate(day = floor_date(ordered, "day")) |>
    semi_join(df_orders_color, by = "day") |>
    unnest(items) |>
    inner_join(
      df_orders_color, by = c("desc_product", "day"),
      suffix = c("_male", "_female")) |>
    filter(
      color_male != color_female,
      ordered_male >= ordered_female - dminutes(10)
      & ordered_male <= ordered_female + dminutes(10))
  
  df_customers |>
    semi_join(df_order_rel, by = c("customerid" = "customerid_male"))
}