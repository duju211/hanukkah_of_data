tinder_woman <- function(df_orders, df_order_items_pastries, df_customers) {
  df_order_items_pastries |>
    left_join(df_orders, by = "orderid") |>
    filter(hour(ordered) < 9) |>
    arrange(ordered) |>
    group_by(day = floor_date(ordered, "day")) |>
    summarise(
      earliest_order = min(ordered),
      customerid = unique(customerid[ordered == earliest_order])) |>
    count(customerid, sort = TRUE) |>
    slice(1) |>
    left_join(
      select(df_customers, customerid, name, phone),
      by = c("customerid"))
}