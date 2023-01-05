phone_letter <- function() {
  tibble(letter = letters) |>
    mutate(
      nr = ((row_number() - 1) %/% 3) + 2,
      nr = as.character(if_else(nr == 10, 0, nr)))
}