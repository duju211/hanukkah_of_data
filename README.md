Title photo from
<a href="https://unsplash.com/de/@diana_pole?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash">Diana
Polekhina</a> on
<a href="https://unsplash.com/de/fotos/rote-und-silberne-handwerkzeuge-F6tbedzUQvw?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash">Unsplash</a>.

# Challenge

‘Hanukkah of Data’ is a [data
challenge](https://hanukkah.bluebird.sh/about/) where you have to solve
8 puzzles surrounding a fictional data set. I have already participated
in [last year’s](https://www.datannery.com/posts/hanukkah-of-data/)
challenge, but it was a lot of fun to revisit the old puzzles and rework
some of my solutions.

Especially the ‘speed-run’ challenge had some twists in it, therefore I
will talk about these puzzles in this blog post. The puzzles are mostly
the same as in the normal version, but the data has some more difficult
edge cases in it.

To solve the puzzles we use the following R libraries:

    library(tarchetypes)
    library(conflicted)
    library(tidyverse)
    library(lubridate)
    library(distill)
    library(assertr)
    library(targets)
    library(janitor)
    library(fs)
    library(gt)

    conflict_prefer("filter", "dplyr")
    conflict_prefer("lag", "dplyr")

I’ve already used the `tidyverse` in last years challenge. This year I
am also using `assertr` to detect problems with my solutions as early as
possible.

Behind the scenes the whole analysis is created as a `targets` pipeline.
So if you want to reproduce the analysis, you have to perform the
following steps:

-   Clone the [repository](https://github.com/duju211/hanukkah_of_data)
-   Run `renv::restore()` to restore the R package versions
-   Run `targets::tar_make()` to run the pipeline

## Day 1

To find the rug, we will need to contact a private investigator. The
last name of the investigator can be spelled by using the letters
printed on the phone buttons. For example: 2 has “ABC”, and 3 “DEF”,
etc.

Before we can begin, load in customer data. It can be found under:

    customers_path <- "csv/noahs-customers.csv"

Read in CSV data and define appropriate column types. Make sure every
`customerid` is uniq and remove non digits from phone number. We also
need to find the last name of each person. This can be a little bit
tricky. Names can also include special suffixes like `Jr.` or roman
numbers like `III`. Therefore we use a regex to filter for last names
that start with a upper case letter and end with one or more lower case
letters.

    customers <- function(customers_path) {
      df_customers_raw <- read_csv(customers_path, col_types = "ccccDc")
      
      df_customers_raw |>
        assert(is_uniq, customerid) |>
        mutate(phone = str_remove_all(phone, "-")) |>
        separate_wider_delim(
          name, delim = regex("\\s+"),
          names = c("first_name", "last_name", NA_character_),
          too_few = "align_start")
    }

    df_customers <- customers(customers_path)

The key pad can be represented in R like this:

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

    df_phone_letter <- phone_letter()

By combining both data sources, we can answer the question:

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

    df_investigator <- investigator(df_customers, df_phone_letter)

<div id="aelxxtwgic" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false" style="-webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale; font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji'; display: table; border-collapse: collapse; line-height: normal; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;" bgcolor="#FFFFFF">
  <thead style="border-style: none;">
    
    <tr class="gt_col_headings" style="border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="customerid" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">customerid</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="first_name" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">first_name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="last_name" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">last_name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="address" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">address</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="citystatezip" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">citystatezip</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="birthdate" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">birthdate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="phone" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">phone</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="timezone" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">timezone</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="lat" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">lat</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="long" style="border-style: none; color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">long</th>
    </tr>
  </thead>
  <tbody class="gt_table_body" style="border-style: none; border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
    <tr style="border-style: none;"><td headers="customerid" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">4249</td>
<td headers="first_name" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Tracy</td>
<td headers="last_name" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Rosenkranz</td>
<td headers="address" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">221 Banker St</td>
<td headers="citystatezip" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Brooklyn, NY 11222</td>
<td headers="birthdate" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">1954-06-12</td>
<td headers="phone" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">7673657269</td>
<td headers="timezone" class="gt_row gt_left" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">America/New_York</td>
<td headers="lat" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">40.7257</td>
<td headers="long" class="gt_row gt_right" style="border-style: none; padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">-73.9555</td></tr>
  </tbody>
  
  
</table>
</div>

## Day 2

Now we are looking for a contractor, to whom the rug was given to by a
cleaning company.

Define the initials to search for:

    searched_initials <- "ds"

Look for customers with the searched initials:

    initials <- function(df_customers, searched_initials) {
      df_customers |>
        transmute(
          customerid,
          initials = str_to_lower(str_c(
            str_sub(first_name, end = 1), str_sub(last_name, end = 1)))) |>
        filter(initials == searched_initials)
    }

    df_initials <- initials(df_customers, searched_initials)

We then look for products that are ‘coffee’ or ‘bagels’.

Before doing so read in products data:

    products_path <- "csv/noahs-products.csv"

    products_raw <- function(products_path) {
      read_csv(products_path)
    }

    df_products_raw <- products_raw(products_path)

Join the order items to the filtered products:

    coffee_bagels <- function(df_products, df_order_items) {
      df_coffee <- df_products |>
        filter(str_detect(desc, regex("coffee", ignore_case = TRUE)))
      
      df_bagel <- df_products |>
        filter(str_detect(desc, regex("bagel", ignore_case = TRUE)))
      
      bind_rows(
        list(bagel = df_bagel, coffee = df_coffee), .id = "coffee_bagel") |>
        left_join(df_order_items, by = "sku")
    }

Look for 2017 orders where coffee or bagels were bought. Keep only those
were the customer has the above mentioned initials.

    order_contractor <- function(df_orders, df_coffee_bagels, df_initials) {
      df_orders |>
        filter(year(ordered) == 2017) |>
        inner_join(df_coffee_bagels, by = "orderid") |>
        group_by(customerid, day = floor_date(ordered, unit = "day")) |>
        summarise(
          coffee = any(coffee_bagel == "coffee"),
          bagel = any(coffee_bagel == "bagel"), .groups = "drop_last") |>
        summarise(coffee_and_bagel = any(coffee & bagel)) |>
        filter(coffee_and_bagel) |>
        semi_join(df_initials, by = "customerid") |>
        verify(length(customerid) == 1)
    }

## Day 3

Search for the neighbor with the spider hat. The filtering conditions
are the following:

-   Born in goat year
-   Born in libra zodiac
-   Lives in neighborhood to contractor

These are the specific filter conditions for the speed run. For the
other versions of the game, these are different. But the general
filtering stays the same.

Define goat years (pasted from wikipedia):

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

    spider_hat <- function(df_customers, df_contractor, df_chinese_year) {
      df_customers |>
        filter(
          map_lgl(
            birthdate,
            ~ any(
              df_chinese_year$start_date <= .x & df_chinese_year$end_date >= .x)),
          case_when(
            month(birthdate) == 9 ~ day(birthdate) >= 23,
            month(birthdate) == 10 ~ day(birthdate) <= 23,
            TRUE ~ FALSE),
          str_detect(citystatezip, df_contractor$citystatezip)) |>
        select(customerid, first_name, last_name, citystatezip, phone) |>
        verify(length(customerid) == 1)
    }

## Day 4

Look for order items that are ‘pastries’:

    order_items_pastries <- function(df_order_items, df_products) {
      df_products_pastries <- df_products |>
        filter(str_detect(sku, regex("bky", ignore_case = TRUE)))
      
      df_order_items |>
        semi_join(df_products_pastries, by = "sku")
    }

Look for persons that order pastries early in the morning:

    tinder_woman <- function(df_orders, df_order_items_pastries, df_customers) {
      df_order_items_pastries |>
        left_join(df_orders, by = "orderid") |>
        filter(hour(ordered) < 9) |>
        arrange(ordered) |>
        group_by(day = floor_date(ordered, "day")) |>
        summarise(
          earliest_order = min(ordered),
          customerid = unique(customerid[ordered == earliest_order])) |>
        count(customerid, sort = TRUE) |>
        slice(1) |>
        left_join(
          select(df_customers, customerid, first_name, last_name, phone),
          by = c("customerid"))
    }

## Day 5

Look for people that live in Staten Island (not needed for the
speedrun):

    staten_island <- function(df_customers) {
      df_customers |>
        transmute(customerid, city = map_chr(str_split(citystatezip, ","), 1)) |>
        filter(str_detect(city, regex("Staten\\s+Island", ignore_case = TRUE)))
    }

Look for products that represent cat food for senior cats:

    senior_cat_food <- function(df_products) {
      df_products |>
        filter(
          str_detect(desc, regex("cat\\s+food", ignore_case = TRUE)),
          str_detect(desc, regex("senior", ignore_case = TRUE)))
    }

Combine the information and look for the searched woman:

    cat_lady <- function(df_order_items, df_orders, df_senior_cat_food,
                         df_staten_island) {
      df_order_items |>
        semi_join(df_senior_cat_food, by = "sku") |>
        left_join(select(df_orders, orderid, customerid), by = "orderid") |>
        #semi_join(df_staten_island, by = "customerid") |>
        count(customerid, sort = TRUE) |>
        slice(1)
    }

## Day 6

Calculate margin for each order item

    order_items_margin <- function(df_order_items, df_products) {
      df_order_items |>
        left_join(df_products, by = "sku") |>
        group_by(orderid) |>
        summarise(margin = sum(unit_price - wholesale_cost))
    }

Determine customer with the lowest total margin:

    frugal_cousin <- function(df_orders, df_order_items_margin) {
      df_orders |>
        left_join(df_order_items_margin, by = "orderid") |>
        group_by(customerid) |>
        summarise(customer_margin = mean(margin)) |>
        arrange(customer_margin) |>
        slice(1)
    }

## Day 7

Find all orders that contain a colored item:

    color_orders <- function(df_orders, df_order_items, df_products) {
      df_orders |>
        left_join(df_order_items, by = c("orderid")) |>
        left_join(df_products, by = "sku") |>
        mutate(
          color = str_remove_all(str_extract(desc, "\\(.+\\)"), "\\(|\\)"),
          day = as_date(floor_date(ordered, unit = "day"))) |>
        filter(!is.na(color))
    }

Search for orders that happened in close proximity to the orders of the
frugal cousin:

    ex_boyfriend <- function(df_color_orders, df_frugal_cousin) {
      df_color_orders_fc <- df_color_orders |>
        semi_join(df_frugal_cousin, by = "customerid") |>
        mutate(start = ordered - dminutes(0.3), end = ordered + dminutes(0.3))
      
      df_color_orders |>
        anti_join(df_color_orders_fc, by = join_by(customerid)) |>
        inner_join(
          select(df_color_orders_fc, day, start, end), by = join_by(day)) |>
        filter(ordered >= start & ordered <= end) |>
        verify(length(customerid) == 1)
    }

# Day 8

Look for products that are collectibles

    collectibles <- function(df_products) {
      df_products |>
        filter(str_detect(sku, "COL"))
    }

Find the person who has all the collectibles

    collector <- function(df_orders, df_order_items, df_collectibles) {
      df_order_items |>
        semi_join(df_collectibles, by = "sku") |>
        left_join(df_orders, by = "orderid") |>
        group_by(customerid) |>
        summarise(anz_coll = n_distinct(sku)) |>
        filter(anz_coll == nrow(df_collectibles)) |>
        verify(length(customerid) == 1)
    }

# Conclusion

As last year, I had a lot of fun solving the Hanukkah of Data
challenges. I revisited my previous solutions and improved them to solve
the new challenges. By using functions from the `assertr` package, I
could spot difficulties early. Especially during the speed run at the
end of the challenge, this type of assertive programming made it more
easy for me, to adjust my solutions to more challenging data and edge
cases. I’m already looking forward to the challenges next year :-).
