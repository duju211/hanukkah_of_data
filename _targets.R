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
  tar_target(searched_initials, "ds"),
  
  tar_target(txt_csv, csv_txt()),
  tar_target(df_customers_raw, customers_raw(customers_path)),
  tar_target(df_customers, customers(df_customers_raw)),
  tar_target(df_products_raw, products_raw(products_path)),
  tar_target(df_order_items_raw, order_items_raw(order_items_path)),
  tar_target(df_orders_raw, orders_raw(orders_path)),
  
  #day1
  tar_target(df_phone_letter, phone_letter()),
  tar_target(df_last_names, last_names(df_customers)),
  tar_target(df_investigator, investigator(df_last_names, df_phone_letter)),
  
  #day2
  tar_target(df_initials, initials(df_customers, searched_initials)),
  tar_target(
    df_coffee_bagels, coffee_bagels(df_products_raw, df_order_items_raw)),
  tar_target(
    df_order_contractor, order_contractor(
      df_orders_raw, df_coffee_bagels, df_initials)),
  tar_target(
    df_contractor,
    select(
      semi_join(df_customers, df_order_contractor, by = "customerid"),
      customerid, name, phone, citystatezip)),
  
  #day3
  tar_target(df_rabbit_years, rabbit_years()),
  tar_target(df_goat_years, goat_years()),
  tar_target(
    df_spider_hat, spider_hat(df_customers, df_contractor, df_goat_years)),
  
  #day4
  tar_target(
    df_order_items_pastries,
    order_items_pastries(df_order_items_raw, df_products_raw)),
  tar_target(
    df_tinder_woman,
    tinder_woman(df_orders_raw, df_order_items_pastries, df_customers)),
  
  #day5
  tar_target(df_staten_island, staten_island(df_customers)),
  tar_target(df_senior_cat_food, senior_cat_food(df_products_raw)),
  tar_target(
    df_cat_lady,
    cat_lady(
      df_order_items_raw, df_orders_raw, df_senior_cat_food, df_staten_island)),
  
  #day6
  tar_target(
    df_order_items_margin,
    order_items_margin(df_order_items_raw, df_products_raw)),
  tar_target(
    df_frugal_cousin, frugal_cousin(df_orders_raw, df_order_items_margin)),
  
  #day7
  tar_target(
    df_color_orders,
    color_orders(df_orders_raw, df_order_items_raw, df_products_raw)),
  tar_target(
    df_ex_boyfriend, ex_boyfriend(df_color_orders, df_frugal_cousin)),
  
  #day8
  tar_target(df_collectibles, collectibles(df_products_raw)),
  tar_target(
    df_collector,
    collector(df_orders_raw, df_order_items_raw, df_collectibles)),
  
  tar_render(blog_post, "blog_post.Rmd"),
  tar_render(
    readme, "blog_post.Rmd", output_file = "README.md",
    output_format = "md_document")
)
