source("libraries.R")

walk(dir_ls("R"), source)

list(
  tar_target(customers_path, "csv/noahs-customers.csv", format = "file"),
  tar_target(products_path, "csv/noahs-products.csv", format = "file"),
  tar_target(order_items_path, "csv/noahs-orders_items.csv", format = "file"),
  tar_target(orders_path, "csv/noahs-orders.csv", format = "file"),
  tar_target(coffee_bagel_regex, regex("bagel|coffee", ignore_case = TRUE)),
  tar_target(dog_years, det_dog_years()),
  tar_target(product_pastries, "BKY"),
  tar_target(product_collect, "COL"),
  
  tar_target(txt_csv, csv_txt()),
  tar_target(df_customers_raw, customers_raw(customers_path)),
  tar_target(df_customers, customers(df_customers_raw)),
  tar_target(df_products_raw, products_raw(products_path)),
  tar_target(df_products, products(df_products_raw)),
  tar_target(df_order_items_raw, order_items_raw(order_items_path)),
  tar_target(df_order_items, order_items(df_order_items_raw, df_products)),
  tar_target(df_orders_raw, orders_raw(orders_path)),
  tar_target(df_orders, orders(df_orders_raw, df_order_items)),
  
  tar_target(df_phone_letter, phone_letter()),
  tar_target(df_investigator, investigator(df_customers, df_phone_letter)),
  tar_target(
    df_contractor,
    contractor(df_orders, df_customers, coffee_bagel_regex)),
  tar_target(df_spider_hat, spider_hat(df_customers, df_contractor, dog_years)),
  tar_target(
    df_tinder_woman,
    tinder_woman(
      df_products, df_orders, df_customers, product_pastries)),
  tar_target(
    df_cat_lady, cat_lady(df_products, df_orders, df_customers)),
  tar_target(df_frugal_cousin, frugal_cousin(df_orders, df_customers)),
  tar_target(
    df_ex_boyfriend, ex_boyfriend(df_frugal_cousin, df_orders, df_customers)),
  tar_target(
    df_collector,
    collector(df_products, df_orders, df_customers, product_collect)),
  
  tar_render(blog_post, "blog_post.Rmd")
)