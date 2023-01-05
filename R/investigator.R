investigator <- function(df_customers, df_phone_letter) {
  df_customers_pro <- df_customers |>
    transmute(
      name_pro = str_split(str_to_lower(str_remove_all(
        last_name, "\\s"), "\\s"), ""),
      customerid) |>
    unnest(name_pro) |>
    left_join(df_phone_letter, by = c("name_pro" = "letter")) |>
    group_by(customerid) |>
    summarise(phone_pro = str_flatten(nr))
  
  df_customers |>
    left_join(df_customers_pro, by = "customerid") |>
    filter(str_length(str_extract(phone_chr, phone_pro)) == str_length(phone_chr))
}