csv_txt <- function() {
  tibble(file = dir_ls("csv/")) |>
    mutate(file = path_file(file), file_txt = str_glue("'{file}'")) |>
    pull(file_txt) |>
    glue_collapse(", ", last = " and ")
}