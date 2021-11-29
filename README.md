Time Series Analysis and Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Data Scientist/IT Manager
29 November, 2021

## Get Data

``` r
get_cran_data()
get_package_release_data()
csv_to_rds()
downloads_tbl <- downloads_processed_tbl()
pkg_tbl <- readRDS("01_data/pkg_release_tbl.rds") %>%
    mutate(date = as.Date(date))
glimpse(downloads_tbl)
```

    ## Rows: 23,710
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,~
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M~
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:~
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4~
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N~
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA~
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N~
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR~
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0~
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", ~
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638~

The last day in the dataset is 2021-11-27 18:56:46, the file was birthed
on: 2021-11-29 11:38:26, and is 45.69 hours old. Consider updating the
cran log file from the package-downloads project.

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | downloads\_tbl |
| Number of rows                                   | 23710          |
| Number of columns                                | 11             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| character                                        | 6              |
| Date                                             | 1              |
| numeric                                          | 2              |
| POSIXct                                          | 1              |
| Timespan                                         | 1              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| r\_version     |      15970 |           0.33 |   5 |   5 |     0 |        28 |          0 |
| r\_arch        |      15970 |           0.33 |   3 |   7 |     0 |         5 |          0 |
| r\_os          |      15970 |           0.33 |   7 |  15 |     0 |         9 |          0 |
| package        |          0 |           1.00 |   8 |  13 |     0 |         5 |          0 |
| version        |          0 |           1.00 |   5 |   5 |     0 |        12 |          0 |
| country        |       2040 |           0.91 |   2 |   2 |     0 |        97 |          0 |

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 2020-11-23 | 2021-11-27 | 2021-07-15 |       370 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |       mean |         sd |  p0 |      p25 |      p50 |     p75 |    p100 | hist  |
|:---------------|-----------:|---------------:|-----------:|-----------:|----:|---------:|---------:|--------:|--------:|:------|
| size           |          0 |              1 | 1525455.03 | 1875798.84 | 357 | 27383.25 | 238424.5 | 3245134 | 5677952 | ▇▁▂▂▁ |
| ip\_id         |          0 |              1 |    8028.96 |   14895.76 |   1 |   219.00 |   2989.0 |    8425 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| date\_time     |          0 |              1 | 2020-11-23 09:00:41 | 2021-11-27 18:56:46 | 2021-07-15 05:42:20 |     13831 |

**Variable type: Timespan**

| skim\_variable | n\_missing | complete\_rate | min | max | median | n\_unique |
|:---------------|-----------:|---------------:|----:|----:|-------:|----------:|
| time           |          0 |              1 |   0 |  59 |     10 |        60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

``` r
data_tbl <- downloads_tbl %>%
    select(-r_version, -r_arch, -r_os)
```

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a verticle line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->
