source("libraries.R")

walk(dir_ls("R"), source)

list(
  tar_target(customers_path, "csv/noahs-customers.csv", format = "file"),
  tar_target(products_path, "csv/noahs-products.csv", format = "file"),
  tar_target(order_items_path, "csv/noahs-orders_items.csv", format = "file"),
  tar_target(orders_path, "csv/noahs-orders.csv", format = "file"),
  tar_target(coffee_bagel_regex, regex("bagel|coffee", ignore_case = TRUE)),
  tar_target(dog_years, 2018 - (1:20 * 12)),
  
  tar_target(df_customers_raw, customers_raw(customers_path)),
  tar_target(df_customers, customers(df_customers_raw)),
  tar_target(df_products_raw, products_raw(products_path)),
  tar_target(
    df_products_coffee_bagel,
    products_coffee_bagel(df_products_raw, coffee_bagel_regex)),
  tar_target(df_order_items_raw, order_items_raw(order_items_path)),
  tar_target(
    df_order_items_coffee_bagel,
    order_items_coffee_bagel(df_order_items_raw, df_products_coffee_bagel)),
  tar_target(df_orders_raw, orders_raw(orders_path)),
  
  tar_target(df_phone_letter, phone_letter()),
  tar_target(df_investigator, investigator(df_customers, df_phone_letter)),
  tar_target(
    df_contractor,
    contractor(df_orders_raw, df_order_items_coffee_bagel, df_customers)),
  tar_target(df_spider_hat, spider_hat(df_customers, df_contractor, dog_years))
)