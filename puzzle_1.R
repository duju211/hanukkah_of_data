# ---- libraries ---------------------------------------------------------------
library(tidyverse)

# ---- customers ---------------------------------------------------------------
df_customers_raw <- read_csv("csv/noahs-customers.csv")

df_customers <- df_customers_raw |>
  mutate(
    phone = map_chr(str_extract_all(phone, "\\d"), str_flatten),
    name_split = str_split(name, "\\s+"),
    first_name = map_chr(name_split, ~ str_flatten(.x[.x != last(.x)])),
    last_name = map_chr(name_split, last))

# ---- letters -----------------------------------------------------------------
df_letters <- tibble(letter = letters) |>
  mutate(
    nr = ((row_number() - 1) %/% 3) + 2,
    nr = as.character(if_else(nr == 10, 0, nr)))

# ---- sol ---------------------------------------------------------------------
df_customers_pro <- df_customers |>
  transmute(
    name_pro = str_split(str_to_lower(str_remove_all(
      last_name, "\\s"), "\\s"), ""),
    customerid) |>
  unnest(name_pro) |>
  left_join(df_letters, by = c("name_pro" = "letter")) |>
  group_by(customerid) |>
  summarise(phone_pro = str_flatten(nr))

df_investigator <- df_customers |>
  select(customerid, name, phone) |>
  left_join(df_customers_pro, by = "customerid") |>
  mutate(phone_extracted = str_extract(phone, phone_pro)) |>
  filter(str_length(phone_extracted) == str_length(phone))

df_customers_raw |>
  semi_join(df_investigator, by = "customerid") |>
  pull(phone)
  
