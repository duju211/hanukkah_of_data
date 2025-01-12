initials <- function(df_customers, searched_initials) {
  df_customers |>
    transmute(
      customerid,
      initials = str_to_lower(str_c(
        str_sub(first_name, end = 1), str_sub(last_name, end = 1)))) |>
    filter(initials == searched_initials)
}