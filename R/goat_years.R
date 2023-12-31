goat_years <- function() {
  df_goat_raw <- tibble::tribble(
           ~Start.date,          ~End.date, ~Heavenly.branch,
    "13 February 1907",  "1 February 1908",      "Fire Goat",
     "1 February 1919", "19 February 1920",     "Earth Goat",
    "17 February 1931",  "5 February 1932",     "Metal Goat",
     "5 February 1943",  "24 January 1944",     "Water Goat",
     "24 January 1955", "11 February 1956",      "Wood Goat",
     "9 February 1967",  "29 January 1968",      "Fire Goat",
     "28 January 1979", "15 February 1980",     "Earth Goat",
    "15 February 1991",  "3 February 1992",     "Metal Goat",
     "1 February 2003",  "21 January 2004",     "Water Goat",
    "19 February 2015",  "7 February 2016",      "Wood Goat",
     "6 February 2027",  "25 January 2028",      "Fire Goat",
     "24 January 2039", "11 February 2040",     "Earth Goat",
    "11 February 2051",  "31 January 2052",     "Metal Goat",
     "29 January 2063", "16 February 2064",     "Water Goat",
    "15 February 2075",  "4 February 2076",      "Wood Goat",
     "3 February 2087",  "23 January 2088",      "Fire Goat",
     "21 January 2099",  "8 February 2100",     "Earth Goat") |>
    clean_names()
  
  df_goat_raw |>
    mutate(
      across(c(start_date, end_date), ~ parse_date(.x, "%d %B %Y")))
}