customers <- function(customers_path) {
  df_customers_raw <- read_csv(customers_path, col_types = "ccccDc")
  
  df_customers_raw |>
    assert(is_uniq, customerid) |>
    mutate(phone = str_remove_all(phone, "-")) |>
    separate_wider_delim(
      name, delim = regex("\\s+"),
      names = c("first_name", "last_name", NA_character_),
      too_few = "align_start")
}