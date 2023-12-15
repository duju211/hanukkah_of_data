ex_boyfriend <- function(df_color_orders, df_frugal_cousin) {
  df_color_orders_fc <- df_color_orders |>
    semi_join(df_frugal_cousin, by = "customerid") |>
    mutate(start = ordered - dminutes(1), end = ordered + dminutes(1))
  
  df_color_orders |>
    anti_join(df_color_orders_fc, by = join_by(customerid)) |>
    inner_join(select(df_color_orders_fc, day, start, end), by = join_by(day)) |>
    filter(ordered >= start & ordered <= end)
}