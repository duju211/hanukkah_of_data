order_items_raw <- function(order_items_path) {
  read_csv(order_items_path, col_types = "ccdd")
}