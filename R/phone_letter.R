phone_letter <- function() {
  tibble(letter = letters) |>
    mutate(
      nr = as.character(case_when(
        letter %in% c("a", "b", "c") ~ 2,
        letter %in% c("d", "e", "f") ~ 3,
        letter %in% c("g", "h", "i") ~ 4,
        letter %in% c("j", "k", "l") ~ 5,
        letter %in% c("m", "n", "o") ~ 6,
        letter %in% c("p", "q", "r", "s") ~ 7,
        letter %in% c("t", "u", "v") ~ 8,
        letter %in% c("w", "x", "y", "z") ~ 9,
        TRUE ~ NA_real_)))
}