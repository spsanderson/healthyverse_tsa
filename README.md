Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
30 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 138,561
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2025-04-28 23:49:44, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6348.23
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 138561        |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     99781 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99781 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99781 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11719 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-28 | 2023-06-12 | 1618 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134187.81 | 1520594.97 | 355 | 14701 | 278219 | 2367754 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10385.85 | 18456.28 | 1 | 291 | 3058 | 11666 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-28 23:49:44 | 2023-06-12 12:50:33 | 84501 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 29S |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->![](man/figures/README-initial_ts_plot-2.png)<!-- -->

Now lets take a look at some time series decomposition graphs.

![](man/figures/README-ts_decomp_plt-1.png)<!-- -->![](man/figures/README-ts_decomp_plt-2.png)<!-- -->![](man/figures/README-ts_decomp_plt-3.png)<!-- -->![](man/figures/README-ts_decomp_plt-4.png)<!-- -->

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

    ## 
    ## Call:
    ## stats::lm(formula = .formula, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -146.94  -35.67  -10.77   26.72  814.71 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.949e+02  7.174e+01
    ## date                                                1.182e-02  3.805e-03
    ## lag(value, 1)                                       1.029e-01  2.468e-02
    ## lag(value, 7)                                       9.537e-02  2.557e-02
    ## lag(value, 14)                                      9.391e-02  2.561e-02
    ## lag(value, 21)                                      6.779e-02  2.575e-02
    ## lag(value, 28)                                      6.593e-02  2.563e-02
    ## lag(value, 35)                                      6.729e-02  2.578e-02
    ## lag(value, 42)                                      4.792e-02  2.584e-02
    ## lag(value, 49)                                      6.687e-02  2.569e-02
    ## month(date, label = TRUE).L                        -1.034e+01  5.134e+00
    ## month(date, label = TRUE).Q                         2.618e+00  5.197e+00
    ## month(date, label = TRUE).C                        -1.209e+01  5.189e+00
    ## month(date, label = TRUE)^4                        -6.662e+00  5.202e+00
    ## month(date, label = TRUE)^5                        -1.249e+01  5.200e+00
    ## month(date, label = TRUE)^6                        -2.916e+00  5.244e+00
    ## month(date, label = TRUE)^7                        -6.344e+00  5.172e+00
    ## month(date, label = TRUE)^8                        -4.607e+00  5.168e+00
    ## month(date, label = TRUE)^9                         5.590e+00  5.158e+00
    ## month(date, label = TRUE)^10                        4.384e+00  5.243e+00
    ## month(date, label = TRUE)^11                       -5.806e+00  5.336e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.166e+01  2.376e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.372e+00  2.500e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.717 0.006662 ** 
    ## date                                                 3.106 0.001931 ** 
    ## lag(value, 1)                                        4.170 3.21e-05 ***
    ## lag(value, 7)                                        3.730 0.000198 ***
    ## lag(value, 14)                                       3.667 0.000253 ***
    ## lag(value, 21)                                       2.633 0.008555 ** 
    ## lag(value, 28)                                       2.572 0.010195 *  
    ## lag(value, 35)                                       2.610 0.009132 ** 
    ## lag(value, 42)                                       1.854 0.063862 .  
    ## lag(value, 49)                                       2.602 0.009345 ** 
    ## month(date, label = TRUE).L                         -2.013 0.044255 *  
    ## month(date, label = TRUE).Q                          0.504 0.614546    
    ## month(date, label = TRUE).C                         -2.330 0.019939 *  
    ## month(date, label = TRUE)^4                         -1.281 0.200464    
    ## month(date, label = TRUE)^5                         -2.403 0.016400 *  
    ## month(date, label = TRUE)^6                         -0.556 0.578256    
    ## month(date, label = TRUE)^7                         -1.227 0.220146    
    ## month(date, label = TRUE)^8                         -0.891 0.372890    
    ## month(date, label = TRUE)^9                          1.084 0.278605    
    ## month(date, label = TRUE)^10                         0.836 0.403234    
    ## month(date, label = TRUE)^11                        -1.088 0.276785    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.908 1.02e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.349 0.000832 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.84 on 1546 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2447, Adjusted R-squared:  0.234 
    ## F-statistic: 22.77 on 22 and 1546 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

## NNS Forecasting

This is something I have been wanting to try for a while. The `NNS`
package is a great package for forecasting time series data.

[NNS GitHub](https://github.com/OVVO-Financial/NNS)

``` r
library(NNS)

data_list <- base_data |>
    select(package, value) |>
    group_split(package)

data_list |>
    imap(
        \(x, idx) {
            obj <- x
            x <- obj |> pull(value) |> tail(7*52)
            train_set_size <- length(x) - 56
            pkg <- obj |> pluck(1) |> unique()
            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            
            cat(paste0("Package: ", pkg, "\n"))
            NNS.ARMA.optim(
                variable = x,
                h = 28,
                training.set = train_set_size,
                #seasonal.factor = seq(12, 60, 7),
                seasonal.factor = sf,
                pred.int = 0.95,
                plot = TRUE
            )
            title(
                sub = paste0("\n",
                             "Package: ", pkg, " - NNS Optimization")
            )
        }
    )
```

    ## Package: healthyR
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.16469755787862"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.11464100812192"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.65465224269651"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.65465224269651"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.54304449332731"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.54304449332731"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.94422836242063"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.94422836242063"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.61975718721106"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.61975718721106"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.17898085120255"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.17898085120255"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.93164685347245"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.93164685347245"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.33307896136226"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.52095642669919"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.44383735862785"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 21, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.44383735862785"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.78481565274396"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 21, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.78481565274396"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.44587041736301"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 21, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.44587041736301"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.37513641987577"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.28238451656852"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.28238451656852"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.13473706162396"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.13473706162396"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.04774469450519"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.04774469450519"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.02263877569501"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.02263877569501"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.1464890136974"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.1464890136974"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.5209897995302"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.5209897995302"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.64883784372544"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.64883784372544"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.79857360515098"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.79857360515098"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.98438650588179"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.98438650588179"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.78297040481266"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.78297040481266"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.11673730488398"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.11673730488398"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.60550782770633"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.60550782770633"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.66998347402131"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.39656530360823"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.11033675156284"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84, 63, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.05135727117623"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 84, 63, 35, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.04610939249818"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 84, 63, 35, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.04610939249818"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 84, 63, 35, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.2480585356347"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 84, 63, 35, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.2480585356347"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 84, 63, 35, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.1904638994432"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 84, 63, 35, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.1904638994432"

![](man/figures/README-nns_forecasting-8.png)<!-- -->

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL
    ## 
    ## [[6]]
    ## NULL
    ## 
    ## [[7]]
    ## NULL
    ## 
    ## [[8]]
    ## NULL

## Pre-Processing

Now we are going to do some basic pre-processing.

``` r
data_padded_tbl <- base_data %>%
  pad_by_time(
    .date_var  = date,
    .pad_value = 0
  )

# Get log interval and standardization parameters
log_params  <- liv(data_padded_tbl$value, limit_lower = 0, offset = 1, silent = TRUE)
limit_lower <- log_params$limit_lower
limit_upper <- log_params$limit_upper
offset      <- log_params$offset

data_liv_tbl <- data_padded_tbl %>%
  # Get log interval transform
  mutate(value_trans = liv(value, limit_lower = 0, offset = 1, silent = TRUE)$log_scaled)

# Get Standardization Params
std_params <- standard_vec(data_liv_tbl$value_trans, silent = TRUE)
std_mean   <- std_params$mean
std_sd     <- std_params$sd

data_transformed_tbl <- data_liv_tbl %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  select(-value)
```

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
horizon <- 4*7

nested_data_tbl <- data_transformed_tbl %>%
    
    # 1. Extending: We'll predict n days into the future.
    extend_timeseries(
        .id_var        = package,
        .date_var      = date,
        .length_future = horizon
    ) %>%
    
    # 2. Nesting: We'll group by id, and create a future dataset
    #    that forecasts n days of extended data and
    #    an actual dataset that contains n*2 days
    nest_timeseries(
        .id_var        = package,
        .length_future = horizon
        #.length_actual = horizon*2
    ) %>%
    
   # 3. Splitting: We'll take the actual data and create splits
   #    for accuracy and confidence interval estimation of n das (test)
   #    and the rest is training data
    split_nested_timeseries(
        .length_test = horizon
    )

nested_data_tbl
```

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,611 × 2]> <tibble [28 × 2]> <split [1583|28]>
    ## 2 healthyR      <tibble [1,604 × 2]> <tibble [28 × 2]> <split [1576|28]>
    ## 3 healthyR.ts   <tibble [1,548 × 2]> <tibble [28 × 2]> <split [1520|28]>
    ## 4 healthyverse  <tibble [1,519 × 2]> <tibble [28 × 2]> <split [1491|28]>
    ## 5 healthyR.ai   <tibble [1,343 × 2]> <tibble [28 × 2]> <split [1315|28]>
    ## 6 TidyDensity   <tibble [1,194 × 2]> <tibble [28 × 2]> <split [1166|28]>
    ## 7 tidyAML       <tibble [802 × 2]>   <tibble [28 × 2]> <split [774|28]> 
    ## 8 RandomWalker  <tibble [224 × 2]>   <tibble [28 × 2]> <split [196|28]>

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_lm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_lm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl <- nested_modeltime_tbl[!is.na(nested_modeltime_tbl$package),]
```

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  filter(!is.na(package)) %>%
  knitr::kable()
```

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.6715418 | 150.10942 | 0.5627237 | 132.83668 | 0.8275844 | 0.0400611 |
| healthyR.data | 2 | LM | Test | 0.7342546 | 182.70408 | 0.6152745 | 138.18634 | 0.8468069 | 0.0261974 |
| healthyR.data | 3 | EARTH | Test | 0.8439645 | 312.16496 | 0.7072067 | 127.80703 | 0.9864043 | 0.0261974 |
| healthyR.data | 4 | NNAR | Test | 0.7494205 | 106.29014 | 0.6279828 | 184.24083 | 0.8836083 | 0.0817487 |
| healthyR | 1 | ARIMA | Test | 0.5667800 | 92.59919 | 0.6898236 | 143.88715 | 0.7112184 | 0.0000852 |
| healthyR | 2 | LM | Test | 0.5728537 | 96.21308 | 0.6972158 | 173.63075 | 0.6920379 | 0.0428055 |
| healthyR | 3 | EARTH | Test | 0.5763053 | 100.90487 | 0.7014167 | 146.45173 | 0.6931771 | 0.0428055 |
| healthyR | 4 | NNAR | Test | 0.5751968 | 89.48463 | 0.7000676 | 139.36212 | 0.7223871 | 0.0000333 |
| healthyR.ts | 1 | ARIMA | Test | 0.8121484 | 98.02245 | 0.6559635 | 178.29053 | 0.9678591 | 0.0272568 |
| healthyR.ts | 2 | LM | Test | 0.8205539 | 164.32949 | 0.6627525 | 123.46630 | 1.0721062 | 0.0272568 |
| healthyR.ts | 3 | EARTH | Test | 1.2377360 | 372.24376 | 0.9997059 | 137.12598 | 1.4340038 | 0.0272568 |
| healthyR.ts | 4 | NNAR | Test | 0.8063536 | 217.64708 | 0.6512831 | 174.59054 | 0.9549916 | 0.0706650 |
| healthyverse | 1 | ARIMA | Test | 0.7058354 | 371.35081 | 0.7798550 | 115.42906 | 0.8316779 | 0.0092860 |
| healthyverse | 2 | LM | Test | 0.7209791 | 475.62884 | 0.7965868 | 110.93866 | 0.8377903 | 0.0030764 |
| healthyverse | 3 | EARTH | Test | 2.7202532 | 2357.46771 | 3.0055212 | 137.57472 | 3.0168199 | 0.0030764 |
| healthyverse | 4 | NNAR | Test | 0.6576316 | 243.56496 | 0.7265962 | 116.13394 | 0.8477395 | 0.0427870 |
| healthyR.ai | 1 | ARIMA | Test | 0.8267357 | 124.53649 | 0.8748822 | 176.01264 | 1.0791625 | 0.0051998 |
| healthyR.ai | 2 | LM | Test | 0.8293720 | 141.67407 | 0.8776720 | 163.40139 | 1.0857462 | 0.3055342 |
| healthyR.ai | 3 | EARTH | Test | 0.8383156 | 151.80392 | 0.8871364 | 159.42726 | 1.0988237 | 0.3055342 |
| healthyR.ai | 4 | NNAR | Test | 0.8706119 | 162.73608 | 0.9213136 | 163.59691 | 1.1228166 | 0.0044542 |
| TidyDensity | 1 | ARIMA | Test | 0.5382949 | 308.68235 | 0.7812958 | 110.89007 | 0.6774086 | 0.0124889 |
| TidyDensity | 2 | LM | Test | 0.5731776 | 411.29948 | 0.8319255 | 100.99019 | 0.7391150 | 0.0048923 |
| TidyDensity | 3 | EARTH | Test | 0.5544699 | 278.71904 | 0.8047726 | 113.05221 | 0.6794148 | 0.0048923 |
| TidyDensity | 4 | NNAR | Test | 0.5437529 | 180.73800 | 0.7892176 | 134.38251 | 0.6876058 | 0.0290847 |
| tidyAML | 1 | ARIMA | Test | 0.6148599 | 254.64283 | 0.9600061 | 104.56550 | 0.7201643 | 0.0006736 |
| tidyAML | 2 | LM | Test | 0.6097907 | 276.65002 | 0.9520914 | 99.45981 | 0.7260921 | 0.0015150 |
| tidyAML | 3 | EARTH | Test | 0.5692576 | 180.63799 | 0.8888053 | 108.30534 | 0.6848760 | 0.0015150 |
| tidyAML | 4 | NNAR | Test | 0.6025894 | 258.91382 | 0.9408477 | 102.80497 | 0.7114501 | 0.0022396 |
| RandomWalker | 1 | ARIMA | Test | 1.3630456 | 128.18413 | 0.6091636 | 161.69557 | 1.6449183 | 0.1934925 |
| RandomWalker | 2 | LM | Test | 1.2710252 | 118.73939 | 0.5680384 | 164.85087 | 1.5147592 | 0.0033780 |
| RandomWalker | 3 | EARTH | Test | 1.2732088 | 115.49383 | 0.5690143 | 169.93001 | 1.5090506 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3956416 | 166.45981 | 0.6237312 | 171.91988 | 1.5254705 | 0.0048414 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)<!-- -->

### Best Model

``` r
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 10
    ##   package     .model_id .model_desc .type   mae  mape  mase smape  rmse      rsq
    ##   <fct>           <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR.d…         1 ARIMA       Test  0.672 150.  0.563  133. 0.828  0.0401 
    ## 2 healthyR            2 LM          Test  0.573  96.2 0.697  174. 0.692  0.0428 
    ## 3 healthyR.ts         4 NNAR        Test  0.806 218.  0.651  175. 0.955  0.0707 
    ## 4 healthyver…         1 ARIMA       Test  0.706 371.  0.780  115. 0.832  0.00929
    ## 5 healthyR.ai         1 ARIMA       Test  0.827 125.  0.875  176. 1.08   0.00520
    ## 6 TidyDensity         1 ARIMA       Test  0.538 309.  0.781  111. 0.677  0.0125 
    ## 7 tidyAML             3 EARTH       Test  0.569 181.  0.889  108. 0.685  0.00152
    ## 8 RandomWalk…         3 EARTH       Test  1.27  115.  0.569  170. 1.51  NA

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)<!-- -->

## Refitting and Future Forecast

Now that we have the best models, we can make our future forecasts.

``` r
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
    modeltime_nested_refit(
        control = control_nested_refit(verbose = TRUE)
    )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1583|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1576|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1520|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1491|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1315|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1166|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [774|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [196|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  mutate(across(.value:.conf_hi, .fns = ~ standard_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  )$standard_inverse_value)) %>%
  mutate(across(.value:.conf_hi, .fns = ~ liiv(
    x = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )$rescaled_v)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
