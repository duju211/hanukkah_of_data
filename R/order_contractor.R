order_contractor <- function(df_orders, df_coffee_bagels, df_initials_jd) {
  df_orders |>
    filter(year(ordered) == 2017) |>
    inner_join(df_coffee_bagels, by = "orderid") |>
    group_by(customerid) |>
    summarise(
      coffee = any(coffee_bagel == "coffee"),
      bagel = any(coffee_bagel == "bagel")) |>
    filter(coffee & bagel) |>
    semi_join(df_initials_jd, by = "customerid")
}