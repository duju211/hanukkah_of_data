library(tidyverse)
library(lubridate)


# ---- orders ------------------------------------------------------------------
df_orders <- read_csv("csv/noahs-orders.csv")

df_products <- read_csv("csv/noahs-products.csv")

df_products_coffee_bagel <- df_products |>
  filter(str_detect(desc, regex("bagel|coffee", ignore_case = TRUE)))

df_orders_coffee_bagel <- read_csv("csv/noahs-orders_items.csv") |>
  semi_join(df_products_coffee_bagel, by = "sku")

# ---- customers ---------------------------------------------------------------
df_customers_raw <- read_csv("csv/noahs-customers.csv")

df_customers <- df_customers_raw |>
  mutate(
    name_split = str_split(name, "\\s+"),
    first_name = map_chr(name_split, ~ str_flatten(.x[.x != last(.x)])),
    last_name = map_chr(name_split, last),
    initials = str_to_lower(str_glue(
      "{str_sub(first_name, end = 1)}{str_sub(last_name, end = 1)}")))

df_customers_jd <- df_customers |>
  filter(initials == "jd")

df_orders_late <- df_orders |>
  filter(year(ordered) > 2017)
  
df_cleaner <- df_orders |>
  filter(year(ordered) == 2017) |>
  anti_join(df_orders_late, by = "customerid") |>
  semi_join(df_orders_coffee_bagel, by = "orderid") |>
  semi_join(df_customers_jd, by = "customerid") |>
  count(customerid)

df_customers |>
  semi_join(df_cleaner, by = "customerid") |>
  pull(phone)
  