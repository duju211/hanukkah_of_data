last_names <- function(df_customers) {
  df_customers |>
    transmute(
      customerid,
      last_name = map_chr(str_split(name, "\\s+"), last),
      letter = str_split(last_name, ""),
      phone_chr = str_remove_all(phone, "-")) |>
    unnest(letter) |>
    mutate(letter = str_to_lower(letter))
}