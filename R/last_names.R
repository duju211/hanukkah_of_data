last_names <- function(df_customers) {
  df_customers |>
    transmute(
      customerid,
      name_split = str_split(name, "\\s+"),
      name_split = map(
        name_split,
        ~ str_subset(.x, regex("^[A-Z][a-z]+$"))),
      last_name = map_chr(name_split, last),
      letter = str_split(last_name, ""),
      phone_chr = str_remove_all(phone, "-")) |>
    unnest(letter) |>
    mutate(letter = str_to_lower(letter))
}