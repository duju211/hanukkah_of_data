senior_cat_food <- function(df_products) {
  df_products |>
    filter(
      str_detect(desc, regex("cat\\s+food", ignore_case = TRUE)),
      str_detect(desc, regex("senior", ignore_case = TRUE)))
}