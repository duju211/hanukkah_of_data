order_contractor <- function(df_orders, df_coffee_bagels, df_initials) {
  df_orders |>
    filter(year(ordered) == 2017) |>
    inner_join(df_coffee_bagels, by = "orderid") |>
    group_by(customerid, day = floor_date(ordered, unit = "day")) |>
    summarise(
      coffee = any(coffee_bagel == "coffee"),
      bagel = any(coffee_bagel == "bagel"), .groups = "drop_last") |>
    summarise(coffee_and_bagel = any(coffee & bagel)) |>
    filter(coffee_and_bagel) |>
    semi_join(df_initials, by = "customerid")
}