Tidy Data
================
Ashley Tseng
9/24/2019

## Wide to long

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names()

pulse_data
```

    ## # A tibble: 1,087 x 7
    ##       id   age sex   bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##    <dbl> <dbl> <chr>        <dbl>         <dbl>         <dbl>         <dbl>
    ##  1 10003  48.0 male             7             1             2             0
    ##  2 10015  72.5 male             6            NA            NA            NA
    ##  3 10022  58.5 male            14             3             8            NA
    ##  4 10026  72.7 male            20             6            18            16
    ##  5 10035  60.4 male             4             0             1             2
    ##  6 10050  84.7 male             2            10            12             8
    ##  7 10078  31.3 male             4             0            NA            NA
    ##  8 10088  56.9 male             5            NA             0             2
    ##  9 10091  76.0 male             0             3             4             0
    ## 10 10092  74.2 fema…           10             2            11             6
    ## # … with 1,077 more rows

``` r
pivot_longer(
  pulse_data,
  bdi_score_bl:bdi_score_12m,
  names_to = "visit",
  values_to = "bdi"
)
```

    ## # A tibble: 4,348 x 5
    ##       id   age sex   visit           bdi
    ##    <dbl> <dbl> <chr> <chr>         <dbl>
    ##  1 10003  48.0 male  bdi_score_bl      7
    ##  2 10003  48.0 male  bdi_score_01m     1
    ##  3 10003  48.0 male  bdi_score_06m     2
    ##  4 10003  48.0 male  bdi_score_12m     0
    ##  5 10015  72.5 male  bdi_score_bl      6
    ##  6 10015  72.5 male  bdi_score_01m    NA
    ##  7 10015  72.5 male  bdi_score_06m    NA
    ##  8 10015  72.5 male  bdi_score_12m    NA
    ##  9 10022  58.5 male  bdi_score_bl     14
    ## 10 10022  58.5 male  bdi_score_01m     3
    ## # … with 4,338 more rows

“names\_to”: The columns that I currently have are going to become their
own variable “visit”. “values\_to”: Then take the values from those
columns and put them their new column. “pivot\_longer” results in two
new columns; wide to long format.

We didn’t like that pivot\_longer was a separate action from the haven
and janitor action; let’s pipe it all together:

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names() %>%
  pivot_longer(
  bdi_score_bl:bdi_score_12m,
  names_to = "visit",
  names_prefix = "bdi_score_",
  values_to = "bdi"
) %>% 
  mutate(
    visit = recode(visit, "bl" = "00m")
  )
```
