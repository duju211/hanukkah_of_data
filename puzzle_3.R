library(tidyverse)
library(lubridate)

df_customers_raw <- read_csv("csv/noahs-customers.csv")

df_customers <- df_customers_raw |>
  mutate(
    name_split = str_split(name, "\\s+"),
    first_name = map_chr(name_split, ~ str_flatten(.x[.x != last(.x)])),
    last_name = map_chr(name_split, last),
    initials = str_to_lower(str_glue(
      "{str_sub(first_name, end = 1)}{str_sub(last_name, end = 1)}")))

df_customer_origin <- filter(df_customers, customerid == 4164)
customer_origin_zip <- as.character(parse_number(pull(df_customer_origin, citystatezip)))

dog_years <- 2018 - (1:20 * 12)

df_customer_spider <- df_customers |>
  mutate(birth_month = month(birthdate), birth_day = day(birthdate)) |>
  filter(year(birthdate) %in% dog_years) |>
  filter(
    case_when(
      birth_month == 3 ~ birth_day >= 21,
      birth_month == 4 ~ birth_day <= 20,
      TRUE ~ FALSE)) |>
  filter(str_detect(citystatezip, customer_origin_zip))

pull(df_customer_spider, phone)
