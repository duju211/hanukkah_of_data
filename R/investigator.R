investigator <- function(df_customers, df_phone_letter) {
  df_sol <- df_customers |>
    mutate(
      customerid, phone, last_name = str_split(str_to_lower(last_name), ""),
      .keep = "none") |>
    unnest(last_name) |>
    left_join(
      df_phone_letter, by = join_by(last_name == letter),
      relationship = "many-to-one") |>
    summarise(phone_pro = str_flatten(nr), .by = c(customerid, phone)) |>
    filter(phone == phone_pro)

  stopifnot(nrow(df_sol) == 1)
    
  semi_join(df_customers, df_sol, by = join_by(customerid))
}