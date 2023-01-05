customers_raw <- function(customers_path) {
  read_csv(customers_path, col_types = "ccccDc")
}