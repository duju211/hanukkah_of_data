# Challenge

‘Hanukkah of Data’ is a [data
challenge](https://hanukkah.bluebird.sh/about/) where you have to solve
8 puzzles surrounding a fictional dataset.

We are asked to find a rug for our granduncle Noah who owns a store. The
store has data about customer, products and orders. With the help of the
data ([CSV
files](https://github.com/duju211/hanukkah_of_data/tree/master/csv)) we
are asked to solve a total of eight puzzles.

# Data

The data for this analysis consists of four CSV files:
‘noahs-customers.csv’, ‘noahs-orders.csv’, ‘noahs-orders\_items.csv’ and
‘noahs-products.csv’. The data is read into R with the help of the
`readr::read_csv` function, specifying the correct column types as
arguments in the function calls.

## Customers

There are a total of 11080 customers in the raw data. Perform the
following preprocessing steps:

-   Determine the Initials by splitting the name into first and last
    name
-   Extract all digits from the phone number and save in column
    `phone_chr`
-   Remove duplicates

<!-- -->

    customers <- function(df_customers_raw) {
      df_customers_raw |>
        mutate(
          phone_chr = map_chr(str_extract_all(phone, "\\d"), str_flatten),
          name_split = str_split(name, "\\s+"),
          first_name = map_chr(name_split, ~ str_flatten(.x[.x != last(.x)])),
          last_name = map_chr(name_split, last),
          initials = str_to_lower(str_glue(
            "{str_sub(first_name, end = 1)}{str_sub(last_name, end = 1)}"))) |>
        select(where(negate(is_list))) |>
        distinct(customerid, .keep_all = TRUE)
    }

    ## # A tibble: 11,079 × 10
    ##    customerid name      address citys…¹ birthdate  phone phone…² first…³ last_…⁴
    ##    <chr>      <chr>     <chr>   <chr>   <date>     <chr> <chr>   <chr>   <chr>  
    ##  1 1001       Jack Qui… 201 E … Los An… 1960-05-14 805-… 805287… Jack    Quinn  
    ##  2 1002       David Po… 224C T… Staten… 1978-04-04 516-… 516768… David   Powell 
    ##  3 1003       Carrie G… 1608 W… Tampa,… 1969-01-21 727-… 727209… Carrie  Green  
    ##  4 1004       Steven M… 178½ E… Manhat… 1953-08-17 607-… 607941… Steven  Miller 
    ##  5 1005       Christin… 270 W … Bronx,… 1983-06-06 212-… 212759… Christ… Powers 
    ##  6 1006       Amanda R… 183-48… Saint … 1962-07-08 914-… 914421… Amanda  Ramirez
    ##  7 1007       Mark Col… 14-47 … Colleg… 1967-04-14 585-… 585554… Mark    Collins
    ##  8 1008       Jill Ste… 735A A… Manhat… 1959-06-11 516-… 516307… Jill    Stevens
    ##  9 1009       Samuel F… 56 Ric… Brookl… 1988-10-24 929-… 929869… Samuel  Fowler 
    ## 10 1010       Brenda J… 2821 B… Bronx,… 1960-09-07 914-… 914205… Brenda  Johnson
    ## # … with 11,069 more rows, 1 more variable: initials <chr>, and abbreviated
    ## #   variable names ¹​citystatezip, ²​phone_chr, ³​first_name, ⁴​last_name

## Products

    products <- function(df_products_raw) {
      df_products_raw |>
        mutate(
          category = str_remove(sku, "\\d+"),
          add_info = str_extract(desc, "\\(.+\\)"),
          desc = if_else(!is.na(add_info), str_remove(desc, add_info), desc),
          across(c(add_info, desc), ~ str_trim(str_remove_all(.x, "\\)|\\("))))
    }

    ## # A tibble: 1,124 × 5
    ##    sku     desc                                    wholesale_c…¹ categ…² add_i…³
    ##    <chr>   <chr>                                           <dbl> <chr>   <chr>  
    ##  1 DLI0002 Smoked Whitefish Sandwich                        9.33 DLI     <NA>   
    ##  2 PET0005 Vegan Cat Food, Turkey & Chicken                 4.35 PET     <NA>   
    ##  3 HOM0018 Power Radio                                     21.8  HOM     red    
    ##  4 KIT0034 Azure Ladle                                      2.81 KIT     <NA>   
    ##  5 PET0041 Gluten-free Cat Food, Pumpkin & Pumpkin          4.6  PET     <NA>   
    ##  6 PET0045 Gluten-free Cat Food, Salmon & Shrimp            4.32 PET     <NA>   
    ##  7 TOY0048 Electric Doll                                   10.2  TOY     <NA>   
    ##  8 CMP0061 Network Printer                                136.   CMP     <NA>   
    ##  9 DLI0066 Pickled Herring Sandwich                         9.94 DLI     <NA>   
    ## 10 TOY0085 Noah's Toy Soldier                              12.0  TOY     <NA>   
    ## # … with 1,114 more rows, and abbreviated variable names ¹​wholesale_cost,
    ## #   ²​category, ³​add_info

## Orders

    orders <- function(df_orders_raw, df_order_items) {
      df_orders_raw |>
        select(-items) |>
        nest_join(df_order_items, by = "orderid", name = "items")
    }

    ## # A tibble: 214,207 × 6
    ##    orderid customerid ordered             shipped             total items   
    ##    <chr>   <chr>      <dttm>              <dttm>              <dbl> <list>  
    ##  1 1001    4308       2017-01-31 00:32:19 2017-01-31 07:15:00  25.5 <tibble>
    ##  2 1002    11683      2017-01-31 00:58:31 2017-01-31 18:00:00  35.3 <tibble>
    ##  3 1003    5676       2017-01-31 01:34:40 2017-01-31 09:00:00  30.8 <tibble>
    ##  4 1004    3097       2017-01-31 02:31:24 2017-01-31 19:45:00  77.6 <tibble>
    ##  5 1005    10374      2017-01-31 02:46:09 2017-01-31 14:45:00 109.  <tibble>
    ##  6 1006    9241       2017-01-31 03:56:04 2017-01-31 19:00:00  15.1 <tibble>
    ##  7 1007    7189       2017-01-31 07:00:00 2017-01-31 20:15:00  11.0 <tibble>
    ##  8 1008    7228       2017-01-31 07:17:53 2017-01-31 07:17:53  33.1 <tibble>
    ##  9 1009    1125       2017-01-31 08:14:34 2017-01-31 13:15:00  17.2 <tibble>
    ## 10 1010    7340       2017-01-31 08:32:57 2017-01-31 19:30:00   4.6 <tibble>
    ## # … with 214,197 more rows

# Puzzles

## Puzzle 1

To find the rug, we will need to contact a private investigator. The
last name of the investigator can be spelled by using the letters
printed on the phone buttons. For example: 2 has “ABC”, and 3 “DEF”,
etc.

The key pad of the phone can be represented in R like this:

    phone_letter <- function() {
      tibble(letter = letters) |>
        mutate(
          nr = ((row_number() - 1) %/% 3) + 2,
          nr = as.character(if_else(nr == 10, 0, nr)))
    }

    ## # A tibble: 26 × 2
    ##    letter nr   
    ##    <chr>  <chr>
    ##  1 a      2    
    ##  2 b      2    
    ##  3 c      2    
    ##  4 d      3    
    ##  5 e      3    
    ##  6 f      3    
    ##  7 g      4    
    ##  8 h      4    
    ##  9 i      4    
    ## 10 j      5    
    ## # … with 16 more rows

By combining this representation with the data on hand, we can determine
the investigator:

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

<div id="ttmpfumjbi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ttmpfumjbi .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ttmpfumjbi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ttmpfumjbi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ttmpfumjbi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ttmpfumjbi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ttmpfumjbi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ttmpfumjbi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ttmpfumjbi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ttmpfumjbi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ttmpfumjbi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ttmpfumjbi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ttmpfumjbi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ttmpfumjbi .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ttmpfumjbi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ttmpfumjbi .gt_from_md > :first-child {
  margin-top: 0;
}

#ttmpfumjbi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ttmpfumjbi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ttmpfumjbi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ttmpfumjbi .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ttmpfumjbi .gt_row_group_first td {
  border-top-width: 2px;
}

#ttmpfumjbi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ttmpfumjbi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ttmpfumjbi .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ttmpfumjbi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ttmpfumjbi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ttmpfumjbi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ttmpfumjbi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ttmpfumjbi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ttmpfumjbi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ttmpfumjbi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ttmpfumjbi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ttmpfumjbi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ttmpfumjbi .gt_left {
  text-align: left;
}

#ttmpfumjbi .gt_center {
  text-align: center;
}

#ttmpfumjbi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ttmpfumjbi .gt_font_normal {
  font-weight: normal;
}

#ttmpfumjbi .gt_font_bold {
  font-weight: bold;
}

#ttmpfumjbi .gt_font_italic {
  font-style: italic;
}

#ttmpfumjbi .gt_super {
  font-size: 65%;
}

#ttmpfumjbi .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#ttmpfumjbi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ttmpfumjbi .gt_indent_1 {
  text-indent: 5px;
}

#ttmpfumjbi .gt_indent_2 {
  text-indent: 10px;
}

#ttmpfumjbi .gt_indent_3 {
  text-indent: 15px;
}

#ttmpfumjbi .gt_indent_4 {
  text-indent: 20px;
}

#ttmpfumjbi .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="customerid">customerid</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="name">name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="phone">phone</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="customerid" class="gt_row gt_right">3188</td>
<td headers="name" class="gt_row gt_left">Sam Guttenberg</td>
<td headers="phone" class="gt_row gt_right">488-836-2374</td></tr>
  </tbody>
  
  
</table>
</div>

## Puzzle 2

Now we are looking for a contractor, to whom the rug was given to by a
cleaning company. The following is known:

-   The contractor has the initials ‘jd’
-   The cleaning company and the contractor had meetings at Noah’s over
    coffee and bagels
-   The cleaning company stopped outsourcing a few years ago

<!-- -->

    contractor <- function(df_orders, df_customers, coffee_bagel_regex) {
      df_customers_after_2017 <- df_orders |>
        filter(year(ordered) > 2017) |>
        distinct(customerid)
      
      df_rel_orders <- df_orders |>
        filter(year(ordered) <= 2017) |>
        anti_join(df_customers_after_2017, by = "customerid") |>
        unnest(items) |>
        filter(str_detect(desc, coffee_bagel_regex)) |>
        semi_join(filter(df_customers, initials == "jd"), by = "customerid")
      
      df_customers |>
        semi_join(df_rel_orders, by = "customerid")
    }

<div id="jghctfqlyy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jghctfqlyy .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jghctfqlyy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jghctfqlyy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jghctfqlyy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jghctfqlyy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jghctfqlyy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jghctfqlyy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jghctfqlyy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jghctfqlyy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jghctfqlyy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jghctfqlyy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jghctfqlyy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jghctfqlyy .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#jghctfqlyy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jghctfqlyy .gt_from_md > :first-child {
  margin-top: 0;
}

#jghctfqlyy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jghctfqlyy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jghctfqlyy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#jghctfqlyy .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#jghctfqlyy .gt_row_group_first td {
  border-top-width: 2px;
}

#jghctfqlyy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jghctfqlyy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jghctfqlyy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jghctfqlyy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jghctfqlyy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jghctfqlyy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jghctfqlyy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jghctfqlyy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jghctfqlyy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jghctfqlyy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jghctfqlyy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jghctfqlyy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jghctfqlyy .gt_left {
  text-align: left;
}

#jghctfqlyy .gt_center {
  text-align: center;
}

#jghctfqlyy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jghctfqlyy .gt_font_normal {
  font-weight: normal;
}

#jghctfqlyy .gt_font_bold {
  font-weight: bold;
}

#jghctfqlyy .gt_font_italic {
  font-style: italic;
}

#jghctfqlyy .gt_super {
  font-size: 65%;
}

#jghctfqlyy .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#jghctfqlyy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jghctfqlyy .gt_indent_1 {
  text-indent: 5px;
}

#jghctfqlyy .gt_indent_2 {
  text-indent: 10px;
}

#jghctfqlyy .gt_indent_3 {
  text-indent: 15px;
}

#jghctfqlyy .gt_indent_4 {
  text-indent: 20px;
}

#jghctfqlyy .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="customerid">customerid</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="name">name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="citystatezip">citystatezip</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="phone">phone</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="customerid" class="gt_row gt_right">4164</td>
<td headers="name" class="gt_row gt_left">Jeremy Davis</td>
<td headers="citystatezip" class="gt_row gt_left">South Ozone Park, NY 11420</td>
<td headers="phone" class="gt_row gt_right">212-771-8924</td></tr>
  </tbody>
  
  
</table>
</div>

## Puzzle 3

We are searching for the neighbor of the contractor. The neighbor has
the following characteristics:

-   Star Sign: Aries
    -   March 21 - April 19
-   Born in the year of the Dog
    -   Every 12 years (2018 for example)
-   Lives in the neighborhood

Dog years are determined by the following function:

    det_dog_years <- function() {
      2018 - (1:9 * 12)
    }

Resulting in: 2006, 1994, 1982, 1970, 1958, 1946, 1934, 1922, 1910

With this information we can find out who the neighbor is:

    spider_hat <- function(df_customers, df_contractor, dog_years) {
      df_customers |>
        filter(year(birthdate) %in% dog_years) |>
        filter(
          case_when(
            month(birthdate) == 3 ~ day(birthdate) >= 21,
            month(birthdate) == 4 ~ day(birthdate) <= 20,
            TRUE ~ FALSE)) |>
        filter(str_detect(citystatezip, df_contractor$citystatezip))
    }

<div id="tthvqnsptf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color: #333333; font-size: 16px; font-weight: normal; font-style: normal; background-color: #FFFFFF; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;" bgcolor="#FFFFFF">
  
  <thead class="gt_col_headings" style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="customerid" style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">customerid</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="name" style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="citystatezip" style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#FFFFFF" valign="bottom" align="left">citystatezip</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="birthdate" style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">birthdate</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="phone" style="color: #333333; background-color: #FFFFFF; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#FFFFFF" valign="bottom" align="right">phone</th>
    </tr>
  </thead>
  <tbody class="gt_table_body" style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
    <tr><td headers="customerid" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">2274</td>
<td headers="name" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Brent Nguyen</td>
<td headers="citystatezip" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">South Ozone Park, NY 11420</td>
<td headers="birthdate" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">1958-03-25</td>
<td headers="phone" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #D3D3D3; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">516-636-7397</td></tr>
  </tbody>
  
  
</table>
</div>
