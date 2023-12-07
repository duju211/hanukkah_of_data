investigator <- function(df_last_names, df_phone_letter) {
  df_last_names |>
    left_join(df_phone_letter, by = "letter") |>
    group_by(customerid, phone_chr) |>
    summarise(phone_pro = str_flatten(nr), .groups = "drop") |>
    filter(phone_chr == phone_pro)
}