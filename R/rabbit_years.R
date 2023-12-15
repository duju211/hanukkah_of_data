rabbit_years <- function() {
  df_rabbit_raw <- tribble(
    ~Start.date,          ~End.date, ~Heavenly.branch,
    "29 January 1903", "15 February 1904",     "Water Hare",
    "14 February 1915",  "3 February 1916",      "Wood Hare",
    "2 February 1927",  "22 January 1928",      "Fire Hare",
    "19 February 1939",  "7 February 1940",     "Earth Hare",
    "6 February 1951",  "26 January 1952",     "Metal Hare",
    "25 January 1963", "12 February 1964",     "Water Hare",
    "11 February 1975",  "30 January 1976",      "Wood Hare",
    "29 January 1987", "16 February 1988",      "Fire Hare",
    "16 February 1999",  "4 February 2000",     "Earth Hare",
    "3 February 2011",  "22 January 2012",     "Metal Hare",
    "22 January 2023",  "9 February 2024",     "Water Hare",
    "8 February 2035",  "27 January 2036",      "Wood Hare",
    "26 January 2047", "13 February 2048",      "Fire Hare",
    "11 February 2059",  "1 February 2060",     "Earth Hare",
    "31 January 2071", "18 February 2072",     "Metal Hare",
    "17 February 2083",  "5 February 2084",     "Water Hare",
    "5 February 2095",  "24 January 2096",      "Wood Hare") |>
    clean_names()
  
  df_rabbit_raw |>
    mutate(
      across(c(start_date, end_date), ~ parse_date(.x, "%d %B %Y")))
}