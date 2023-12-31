initials <- function(df_customers, searched_initials) {
  df_customers |>
    mutate(
      name_split = str_split(name, "\\s+"),
      name_split = map(
        name_split, ~ str_subset(.x, regex("^[a-z]+$", ignore_case = TRUE))),
      first_name = map_chr(name_split, first),
      last_name = map_chr(name_split, last)) |>
    transmute(
      customerid,
      initials = str_to_lower(str_c(
        str_sub(first_name, end = 1), str_sub(last_name, end = 1)))) |>
    filter(initials == searched_initials)
}