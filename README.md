# Time Series Analysis, Modeling and Forecasting of the Healthyverse Packages
Steven P. Sanderson II, MPH - Date:
2026-10-26

# Introduction

This analysis follows a *Nested Modeltime Workflow* from **`modeltime`**
along with using the **`NNS`** package. I use this to monitor the
downloads of all of my packages:

- [`healthyR`](https://www.spsanderson.com/healthyR/)
- [`healthyR.data`](https://www.spsanderson.com/healthyR.data/)
- [`healthyR.ts`](https://www.spsanderson.com/healthyR.ts/)
- [`healthyR.ai`](https://www.spsanderson.com/healthyR.ai/)
- [`healthyverse`](https://www.spsanderson.com/healthyverse/)
- [`TidyDensity`](https://www.spsanderson.com/TidyDensity/)
- [`tidyAML`](https://www.spsanderson.com/tidyAML/)
- [`RandomWalker`](https://www.spsanderson.com/RandomWalker/)

## Get Data

``` r
glimpse(downloads_tbl)
```

    Rows: 193,935
    Columns: 11
    $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2026-10-24 22:59:11, the file was
birthed on: 2025-10-31 10:47:59.603742, and at report knit time is
8600.19 hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 193935        |
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
| r_version     |    146562 |          0.24 |   1 |  17 |     0 |       55 |          0 |
| r_arch        |    146562 |          0.24 |   1 |   7 |     0 |        8 |          0 |
| r_os          |    146562 |          0.24 |   6 |  33 |     0 |       43 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       63 |          0 |
| country       |     18659 |          0.90 |   2 |   2 |     0 |      173 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2026-10-24 | 2024-04-03 | 2155 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1162548.23 | 1478697.54 | 355 | 47146 | 361718 | 2367747 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 13537.88 | 28874.95 | 1 | 189 | 2818 | 12780 | 429286 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2026-10-24 22:59:11 | 2024-04-03 16:47:06 | 126892 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 16M 1S |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)

![](man/figures/README-initial_ts_plot-2.png)

    [[1]]

![](man/figures/README-initial_ts_plot-3.png)

    [[2]]

![](man/figures/README-initial_ts_plot-4.png)

    [[3]]

![](man/figures/README-initial_ts_plot-5.png)

    [[4]]

![](man/figures/README-initial_ts_plot-6.png)

    [[5]]

![](man/figures/README-initial_ts_plot-7.png)

    [[6]]

![](man/figures/README-initial_ts_plot-8.png)

    [[7]]

![](man/figures/README-initial_ts_plot-9.png)

    [[8]]

![](man/figures/README-initial_ts_plot-10.png)

Now lets take a look at some time series decomposition graphs.

    [[1]]

![](man/figures/README-ts_decomp_plt-1.png)

    [[2]]

![](man/figures/README-ts_decomp_plt-2.png)

    [[3]]

![](man/figures/README-ts_decomp_plt-3.png)

    [[4]]

![](man/figures/README-ts_decomp_plt-4.png)

    [[5]]

![](man/figures/README-ts_decomp_plt-5.png)

    [[6]]

![](man/figures/README-ts_decomp_plt-6.png)

    [[7]]

![](man/figures/README-ts_decomp_plt-7.png)

    [[8]]

![](man/figures/README-ts_decomp_plt-8.png)

    [[1]]

![](man/figures/README-ts_decomp_plt-9.png)

    [[2]]

![](man/figures/README-ts_decomp_plt-10.png)

    [[3]]

![](man/figures/README-ts_decomp_plt-11.png)

    [[4]]

![](man/figures/README-ts_decomp_plt-12.png)

    [[5]]

![](man/figures/README-ts_decomp_plt-13.png)

    [[6]]

![](man/figures/README-ts_decomp_plt-14.png)

    [[7]]

![](man/figures/README-ts_decomp_plt-15.png)

    [[8]]

![](man/figures/README-ts_decomp_plt-16.png)

Seasonal Diagnostics:

    [[1]]

![](man/figures/README-ts_decomp_seasonal_plt-1.png)

    [[2]]

![](man/figures/README-ts_decomp_seasonal_plt-2.png)

    [[3]]

![](man/figures/README-ts_decomp_seasonal_plt-3.png)

    [[4]]

![](man/figures/README-ts_decomp_seasonal_plt-4.png)

    [[5]]

![](man/figures/README-ts_decomp_seasonal_plt-5.png)

    [[6]]

![](man/figures/README-ts_decomp_seasonal_plt-6.png)

    [[7]]

![](man/figures/README-ts_decomp_seasonal_plt-7.png)

    [[8]]

![](man/figures/README-ts_decomp_seasonal_plt-8.png)

ACF and PACF Diagnostics:

    [[1]]

![](man/figures/README-ts_decomp_acf_plt-1.png)

    [[2]]

![](man/figures/README-ts_decomp_acf_plt-2.png)

    [[3]]

![](man/figures/README-ts_decomp_acf_plt-3.png)

    [[4]]

![](man/figures/README-ts_decomp_acf_plt-4.png)

    [[5]]

![](man/figures/README-ts_decomp_acf_plt-5.png)

    [[6]]

![](man/figures/README-ts_decomp_acf_plt-6.png)

    [[7]]

![](man/figures/README-ts_decomp_acf_plt-7.png)

    [[8]]

![](man/figures/README-ts_decomp_acf_plt-8.png)

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

    Call:
    stats::lm(formula = .formula, data = df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -148.97  -38.42  -11.66   28.31  827.84 

    Coefficients:
                                                         Estimate Std. Error
    (Intercept)                                        -1.344e+02  4.544e+01
    date                                                8.814e-03  2.391e-03
    lag(value, 1)                                       9.934e-02  2.148e-02
    lag(value, 7)                                       7.133e-02  2.206e-02
    lag(value, 14)                                      7.530e-02  2.192e-02
    lag(value, 21)                                      8.711e-02  2.196e-02
    lag(value, 28)                                      7.941e-02  2.191e-02
    lag(value, 35)                                      3.630e-02  2.193e-02
    lag(value, 42)                                      6.041e-02  2.198e-02
    lag(value, 49)                                      6.701e-02  2.192e-02
    month(date, label = TRUE).L                        -7.924e+00  4.668e+00
    month(date, label = TRUE).Q                         2.013e+00  4.593e+00
    month(date, label = TRUE).C                        -1.535e+01  4.604e+00
    month(date, label = TRUE)^4                        -1.048e+01  4.637e+00
    month(date, label = TRUE)^5                        -5.554e+00  4.617e+00
    month(date, label = TRUE)^6                         8.831e-01  4.617e+00
    month(date, label = TRUE)^7                        -1.481e+00  4.543e+00
    month(date, label = TRUE)^8                        -3.816e+00  4.486e+00
    month(date, label = TRUE)^9                        -2.316e-01  4.466e+00
    month(date, label = TRUE)^10                       -3.321e-01  4.453e+00
    month(date, label = TRUE)^11                       -5.263e-01  4.447e+00
    fourier_vec(date, type = "sin", K = 1, period = 7) -1.055e+01  2.019e+00
    fourier_vec(date, type = "cos", K = 1, period = 7)  7.274e+00  2.075e+00
                                                       t value Pr(>|t|)    
    (Intercept)                                         -2.957 0.003142 ** 
    date                                                 3.687 0.000232 ***
    lag(value, 1)                                        4.625 3.97e-06 ***
    lag(value, 7)                                        3.234 0.001240 ** 
    lag(value, 14)                                       3.436 0.000602 ***
    lag(value, 21)                                       3.966 7.56e-05 ***
    lag(value, 28)                                       3.624 0.000298 ***
    lag(value, 35)                                       1.655 0.098019 .  
    lag(value, 42)                                       2.749 0.006035 ** 
    lag(value, 49)                                       3.057 0.002268 ** 
    month(date, label = TRUE).L                         -1.698 0.089720 .  
    month(date, label = TRUE).Q                          0.438 0.661330    
    month(date, label = TRUE).C                         -3.334 0.000870 ***
    month(date, label = TRUE)^4                         -2.260 0.023932 *  
    month(date, label = TRUE)^5                         -1.203 0.229129    
    month(date, label = TRUE)^6                          0.191 0.848314    
    month(date, label = TRUE)^7                         -0.326 0.744386    
    month(date, label = TRUE)^8                         -0.851 0.395077    
    month(date, label = TRUE)^9                         -0.052 0.958639    
    month(date, label = TRUE)^10                        -0.075 0.940562    
    month(date, label = TRUE)^11                        -0.118 0.905801    
    fourier_vec(date, type = "sin", K = 1, period = 7)  -5.223 1.94e-07 ***
    fourier_vec(date, type = "cos", K = 1, period = 7)   3.506 0.000464 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 59.9 on 2083 degrees of freedom
      (49 observations deleted due to missingness)
    Multiple R-squared:  0.1955,    Adjusted R-squared:  0.187 
    F-statistic: 23.01 on 22 and 2083 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)

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
#            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            seas <- t(
                sapply(
                    1:25, 
                    function(i) c(
                        i,
                        sqrt(
                            mean((
                                NNS.ARMA(x, 
                                         h = 28, 
                                         training.set = train_set_size, 
                                         method = "lin", 
                                         seasonal.factor = i, 
                                         plot=FALSE
                                         ) - tail(x, 28)) ^ 2)))
                    )
                )
            colnames(seas) <- c("Period", "RMSE")
            sf <- seas[which.min(seas[, 2]), 1]
            
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

    Package: healthyR
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 9.09281919742698"
    [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 9.09281919742698"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.57746074323282"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 7.57746074323282"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.0039933385296"
    [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.0039933385296"

![](man/figures/README-nns_forecasting-1.png)

    Package: healthyR.ai
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 24.7929677406685"
    [1] "BEST method = 'lin' PATH MEMBER = c( 7 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 24.7929677406685"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.95536327517294"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 7 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.95536327517294"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 7 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.0353702203128"
    [1] "BEST method = 'both' PATH MEMBER = c( 7 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.0353702203128"

![](man/figures/README-nns_forecasting-2.png)

    Package: healthyR.data
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 14.1087626062112"
    [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 14.1087626062112"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 10.8799602647175"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 10.8799602647175"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.6527281146528"
    [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.6527281146528"

![](man/figures/README-nns_forecasting-3.png)

    Package: healthyR.ts
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 14.0877453490794"
    [1] "BEST method = 'lin' PATH MEMBER = c( 13 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 14.0877453490794"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.9813364401029"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 13 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 9.9813364401029"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 13 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 12.7991628296033"
    [1] "BEST method = 'both' PATH MEMBER = c( 13 )"
    [1] "BEST both OBJECTIVE FUNCTION = 12.7991628296033"

![](man/figures/README-nns_forecasting-4.png)

    Package: healthyverse
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 24 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 27.887832200682"
    [1] "BEST method = 'lin' PATH MEMBER = c( 24 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 27.887832200682"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 24 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 22.2172312880256"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 24 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 22.2172312880256"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 24 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 33.1015176582596"
    [1] "BEST method = 'both' PATH MEMBER = c( 24 )"
    [1] "BEST both OBJECTIVE FUNCTION = 33.1015176582596"

![](man/figures/README-nns_forecasting-5.png)

    Package: RandomWalker
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 5.99376680274813"
    [1] "BEST method = 'lin' PATH MEMBER = c( 2 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 5.99376680274813"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 11.4252779594901"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 2 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 11.4252779594901"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 2 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 24.5575213671155"
    [1] "BEST method = 'both' PATH MEMBER = c( 2 )"
    [1] "BEST both OBJECTIVE FUNCTION = 24.5575213671155"

![](man/figures/README-nns_forecasting-6.png)

    Package: tidyAML
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 13.8276792014672"
    [1] "BEST method = 'lin' PATH MEMBER = c( 21 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 13.8276792014672"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.42086911859023"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 9.42086911859023"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 10.4709846688237"
    [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    [1] "BEST both OBJECTIVE FUNCTION = 10.4709846688237"

![](man/figures/README-nns_forecasting-7.png)

    Package: TidyDensity
    [1] "CURRNET METHOD: lin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 9 ) ...)"
    [1] "CURRENT lin OBJECTIVE FUNCTION = 14.2309236240723"
    [1] "BEST method = 'lin' PATH MEMBER = c( 9 )"
    [1] "BEST lin OBJECTIVE FUNCTION = 14.2309236240723"
    [1] "CURRNET METHOD: nonlin"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 9 ) ...)"
    [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.24929873879259"
    [1] "BEST method = 'nonlin' PATH MEMBER = c( 9 )"
    [1] "BEST nonlin OBJECTIVE FUNCTION = 5.24929873879259"
    [1] "CURRNET METHOD: both"
    [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 9 ) ...)"
    [1] "CURRENT both OBJECTIVE FUNCTION = 4.72948244047585"
    [1] "BEST method = 'both' PATH MEMBER = c( 9 )"
    [1] "BEST both OBJECTIVE FUNCTION = 4.72948244047585"

![](man/figures/README-nns_forecasting-8.png)

    [[1]]
    NULL

    [[2]]
    NULL

    [[3]]
    NULL

    [[4]]
    NULL

    [[5]]
    NULL

    [[6]]
    NULL

    [[7]]
    NULL

    [[8]]
    NULL

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
  group_by(package) %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  tk_augment_fourier(
    .date_var = date,
    .periods  = c(7, 14, 30, 90, 180),
    .K        = 2
  ) %>%
  tk_augment_timeseries_signature(
    .date_var = date
  ) %>%
  ungroup() %>%
  select(-c(value, -year.iso))
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

    # 0. Filter out column where package is NA
    filter(!is.na(package)) %>%
    
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

    # A tibble: 8 × 4
      package       .actual_data          .future_data       .splits          
      <fct>         <list>                <list>             <list>           
    1 healthyR.data <tibble [2,143 × 50]> <tibble [28 × 50]> <split [2115|28]>
    2 healthyR      <tibble [2,137 × 50]> <tibble [28 × 50]> <split [2109|28]>
    3 healthyR.ts   <tibble [2,073 × 50]> <tibble [28 × 50]> <split [2045|28]>
    4 healthyverse  <tibble [1,945 × 50]> <tibble [28 × 50]> <split [1917|28]>
    5 healthyR.ai   <tibble [1,878 × 50]> <tibble [28 × 50]> <split [1850|28]>
    6 TidyDensity   <tibble [1,731 × 50]> <tibble [28 × 50]> <split [1703|28]>
    7 tidyAML       <tibble [1,334 × 50]> <tibble [28 × 50]> <split [1306|28]>
    8 RandomWalker  <tibble [758 × 50]>   <tibble [28 × 50]> <split [730|28]> 

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ .
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_date) %>%
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
  add_recipe(recipe = recipe_date) %>%
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
| healthyR.data | 1 | ARIMA | Test | 0.8651133 | 113.0289 | 1.0154985 | 187.25304 | 1.0615497 | 0.0261935 |
| healthyR.data | 2 | LM | Test | 1.0890138 | 226.3037 | 1.2783204 | 184.27829 | 1.2690802 | 0.0000173 |
| healthyR.data | 3 | EARTH | Test | 0.8550263 | 487.4628 | 1.0036581 | 101.30996 | 1.0354231 | 0.0355169 |
| healthyR.data | 4 | NNAR | Test | 1.0773934 | 234.1398 | 1.2646800 | 174.65558 | 1.2593109 | 0.0969285 |
| healthyR | 1 | ARIMA | Test | 0.6919739 | 157.2021 | 0.9520086 | 154.19997 | 0.8825884 | 0.0185845 |
| healthyR | 2 | LM | Test | 0.8316224 | 250.9897 | 1.1441353 | 151.71484 | 1.0031851 | 0.0030209 |
| healthyR | 3 | EARTH | Test | 1.0054360 | 454.9649 | 1.3832657 | 125.90868 | 1.1743392 | 0.0006680 |
| healthyR | 4 | NNAR | Test | 0.6858478 | 157.6953 | 0.9435804 | 142.71807 | 0.8634280 | 0.0025810 |
| healthyR.ts | 1 | ARIMA | Test | 0.6363540 | 118.7825 | 0.8038915 | 164.69268 | 0.8622758 | 0.0330353 |
| healthyR.ts | 2 | LM | Test | 0.6694607 | 251.6952 | 0.8457144 | 150.24200 | 0.8773853 | 0.0344034 |
| healthyR.ts | 3 | EARTH | Test | 0.9872314 | 1068.8247 | 1.2471468 | 128.38547 | 1.1150418 | 0.0018060 |
| healthyR.ts | 4 | NNAR | Test | 0.6788221 | 249.3023 | 0.8575404 | 141.03236 | 0.8632452 | 0.0017984 |
| healthyverse | 1 | ARIMA | Test | 0.6974928 | 162.0295 | 0.8007411 | 73.82861 | 0.8731024 | 0.0435063 |
| healthyverse | 2 | LM | Test | 0.6874345 | 104.4969 | 0.7891938 | 92.36817 | 0.8798812 | 0.0333636 |
| healthyverse | 3 | EARTH | Test | 0.7129231 | 180.6754 | 0.8184554 | 73.24776 | 0.8993790 | 0.0009680 |
| healthyverse | 4 | NNAR | Test | 0.8200156 | 100.2923 | 0.9414006 | 119.70159 | 1.0183693 | 0.0014117 |
| healthyR.ai | 1 | ARIMA | Test | 0.6648459 | 156.1649 | 0.8415350 | 115.83029 | 0.7824593 | 0.0003253 |
| healthyR.ai | 2 | LM | Test | 0.9370796 | 171.0687 | 1.1861176 | 159.42885 | 1.1177953 | 0.0006216 |
| healthyR.ai | 3 | EARTH | Test | 1.1301316 | 496.4953 | 1.4304749 | 109.49169 | 1.3189933 | 0.0345781 |
| healthyR.ai | 4 | NNAR | Test | 0.7855307 | 131.7372 | 0.9942930 | 161.08670 | 0.9434796 | 0.0784469 |
| TidyDensity | 1 | ARIMA | Test | 0.5891822 | 128.7947 | 0.8729015 | 170.06557 | 0.7179079 | 0.0911543 |
| TidyDensity | 2 | LM | Test | 0.6640120 | 441.7173 | 0.9837653 | 131.61722 | 0.8352347 | 0.0331056 |
| TidyDensity | 3 | EARTH | Test | 0.5978309 | 146.7833 | 0.8857150 | 151.05347 | 0.7362319 | 0.0279920 |
| TidyDensity | 4 | NNAR | Test | 0.7926655 | 577.0494 | 1.1743717 | 128.06156 | 0.9958266 | 0.0252674 |
| tidyAML | 1 | ARIMA | Test | 0.6732975 | 209.0142 | 0.7809990 | 104.14576 | 0.8558773 | 0.0209386 |
| tidyAML | 2 | LM | Test | 0.7778321 | 221.6391 | 0.9022551 | 147.40018 | 0.9013967 | 0.0207695 |
| tidyAML | 3 | EARTH | Test | 1.2837890 | 494.6810 | 1.4891455 | 117.77041 | 1.5194251 | 0.0024572 |
| tidyAML | 4 | NNAR | Test | 0.6461430 | 193.2903 | 0.7495008 | 112.08761 | 0.8047833 | 0.0585898 |
| RandomWalker | 1 | ARIMA | Test | 0.4971538 | 157.6406 | 0.6374945 | 135.05533 | 0.6762756 | 0.0340740 |
| RandomWalker | 2 | LM | Test | 0.5267698 | 155.9874 | 0.6754708 | 128.76688 | 0.7110380 | 0.0004186 |
| RandomWalker | 3 | EARTH | Test | 0.7637426 | 441.1055 | 0.9793382 | 137.78441 | 0.8875769 | 0.0577803 |
| RandomWalker | 4 | NNAR | Test | 0.6156878 | 260.8952 | 0.7894894 | 132.90741 | 0.8137641 | 0.0025054 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  facet_wrap(~ package, nrow = 3) +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)

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

    # Nested Modeltime Table
      

    # A tibble: 8 × 10
      package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
      <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    1 healthyR.da…         3 EARTH       Test  0.855  487. 1.00  101.  1.04  3.55e-2
    2 healthyR             4 NNAR        Test  0.686  158. 0.944 143.  0.863 2.58e-3
    3 healthyR.ts          1 ARIMA       Test  0.636  119. 0.804 165.  0.862 3.30e-2
    4 healthyverse         1 ARIMA       Test  0.697  162. 0.801  73.8 0.873 4.35e-2
    5 healthyR.ai          1 ARIMA       Test  0.665  156. 0.842 116.  0.782 3.25e-4
    6 TidyDensity          1 ARIMA       Test  0.589  129. 0.873 170.  0.718 9.12e-2
    7 tidyAML              4 NNAR        Test  0.646  193. 0.750 112.  0.805 5.86e-2
    8 RandomWalker         1 ARIMA       Test  0.497  158. 0.637 135.  0.676 3.41e-2

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  facet_wrap(~ package, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)

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

    # Nested Modeltime Table
      

    # A tibble: 8 × 5
      package       .actual_data .future_data .splits           .modeltime_tables 
      <fct>         <list>       <list>       <list>            <list>            
    1 healthyR.data <tibble>     <tibble>     <split [2115|28]> <mdl_tm_t [1 × 5]>
    2 healthyR      <tibble>     <tibble>     <split [2109|28]> <mdl_tm_t [1 × 5]>
    3 healthyR.ts   <tibble>     <tibble>     <split [2045|28]> <mdl_tm_t [1 × 5]>
    4 healthyverse  <tibble>     <tibble>     <split [1917|28]> <mdl_tm_t [1 × 5]>
    5 healthyR.ai   <tibble>     <tibble>     <split [1850|28]> <mdl_tm_t [1 × 5]>
    6 TidyDensity   <tibble>     <tibble>     <split [1703|28]> <mdl_tm_t [1 × 5]>
    7 tidyAML       <tibble>     <tibble>     <split [1306|28]> <mdl_tm_t [1 × 5]>
    8 RandomWalker  <tibble>     <tibble>     <split [730|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(package) %>%
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
  filter_by_time(.date_var = .index, .start_date = max(.index) - 60) %>%
  ungroup() %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  facet_wrap(~ package, nrow = 3) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)
