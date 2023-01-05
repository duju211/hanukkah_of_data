customers <- function(df_customers_raw) {
  df_customers_raw |>
    mutate(
      phone_chr = map_chr(str_extract_all(phone, "\\d"), str_flatten),
      name_split = str_split(name, "\\s+"),
      first_name = map_chr(name_split, ~ str_flatten(.x[.x != last(.x)])),
      last_name = map_chr(name_split, last),
      initials = str_to_lower(str_glue(
        "{str_sub(first_name, end = 1)}{str_sub(last_name, end = 1)}"))) |>
    select(where(negate(is_list)))
}