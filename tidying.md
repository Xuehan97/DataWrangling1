tidying
================
Xuehan Yang
2021/9/30

``` r
library(tidyverse)
library(readxl)
options(tibble.print_min = 5)
```

## pivot longer

Load the Pulse data

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% janitor::clean_names()
```

Let’s try to pivot

``` r
pulse_tidy_data = 
  pivot_longer(
    pulse_df,
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    values_to = "bdi",
    names_prefix = "bdi_score_"
  ) %>% 
  mutate(
    visit = replace(visit, visit == "bl","00m")
  ) %>% 
  relocate(visit)

pulse_tidy_data  #see baseline and following bdi
```

    ## # A tibble: 4,348 x 5
    ##   visit    id   age sex     bdi
    ##   <chr> <dbl> <dbl> <chr> <dbl>
    ## 1 00m   10003  48.0 male      7
    ## 2 01m   10003  48.0 male      1
    ## 3 06m   10003  48.0 male      2
    ## 4 12m   10003  48.0 male      0
    ## 5 00m   10015  72.5 male      6
    ## # ... with 4,343 more rows

## pivot wider

make up a results data tavle

``` r
analysis_df = 
  tibble(
    group = c("treatment","treatment","control","control"),
    time = c("a","b","a","b"),
    group_mean = c(4,8,3,6)
  )

analysis_df %>% 
  pivot_wider(
    names_from = "time",
    values_from = "group_mean"
  ) %>% knitr::kable() #human readable
```

| group     |   a |   b |
|:----------|----:|----:|
| treatment |   4 |   8 |
| control   |   3 |   6 |

## binding rows

import the LotR movie words stuff

``` r
fellowship_df = 
  read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "fellowship_rings")

two_towers_df = 
  read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "two_towers")

return_df = 
  read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>% 
  mutate(movie = "return_rings")
```

``` r
lotr_df =
  bind_rows(fellowship_df,two_towers_df,return_df) %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words"
  ) %>% 
  relocate(movie)
```

(never use `rbind()`, instead using `bind_rows()`)

## joins

Look at FAS data

``` r
litters_df = 
  read_csv(file = "./data/FAS_litters.csv") %>% 
  janitor::clean_names() %>% 
  separate(group, into = c("dose","day_of_tx"),3) %>%  # split into differrent columns
  relocate(litter_number) %>% 
  mutate(dose = str_to_lower(dose))
```

    ## Rows: 49 Columns: 8

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df = 
  read_csv("./data/FAS_pups.csv") %>% 
  janitor::clean_names() %>% 
  mutate(sex = recode(sex, `1` = "male",`2` = "female")) #`` forcing R to recognise
```

    ## Rows: 313 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

Let’s join these up!

``` r
fas_df = 
  left_join(
    pups_df,litters_df,by = "litter_number") %>% 
  relocate(litter_number, dose, day_of_tx)
```

(never use `spread()`, instead using `pivot_wider()`) (never use
`gather()`, instead using `pivot_longer()`)
