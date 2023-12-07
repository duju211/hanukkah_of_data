customers <- function(df_customers_raw) {
  df_customers_raw |>
    distinct(customerid, .keep_all = TRUE)
}