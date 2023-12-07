<img src="hanukkah.png" width="1024" />

Title photo from
<a href="https://unsplash.com/@gaellemarcel?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Gaelle
Marcel</a> on
<a href="https://unsplash.com/de/fotos/wkn_KHBExcE?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>

# Challenge

‘Hanukkah of Data’ is a [data
challenge](https://hanukkah.bluebird.sh/about/) where you have to solve
8 puzzles surrounding a fictional data set.

To solve the puzzles we use the following R libraries:

    library(tidyverse)

## Day 1

To find the rug, we will need to contact a private investigator. The
last name of the investigator can be spelled by using the letters
printed on the phone buttons. For example: 2 has “ABC”, and 3 “DEF”,
etc.

The key pad can be represented in R like this:

    phone_letter <- function() {
      tibble(letter = letters) |>
        mutate(
          nr = ((row_number() - 1) %/% 3) + 2,
          nr = as.character(if_else(nr == 10, 0, nr)))
    }

We then need to find the last name of each person. Transform the data so
that every letter is one row:

    last_names <- function(df_customers) {
      df_customers |>
        transmute(
          customerid,
          last_name = map_chr(str_split(name, "\\s+"), last),
          letter = str_split(last_name, ""),
          phone_chr = str_remove_all(phone, "-")) |>
        unnest(letter) |>
        mutate(letter = str_to_lower(letter))
    }

    investigator <- function(df_last_names, df_phone_letter) {
      df_customers_pro <- df_last_names |>
        left_join(df_phone_letter, by = "letter") |>
        group_by(customerid, phone_chr) |>
        summarise(phone_pro = str_flatten(nr), .groups = "drop") |>
        filter(phone_chr == phone_pro)
    }
