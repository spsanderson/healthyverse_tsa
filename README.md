Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
21 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 134,652
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

The last day in the data set is 2025-03-19 23:25:24, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5387.83
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 134652        |
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
| r_version     |     96767 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96767 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96767 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11318 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-19 | 2023-05-21 | 1578 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135241.95 | 1525079.74 | 355 | 14701 | 260656.5 | 2367774 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10376.19 | 18361.28 | 1 | 305 | 3091.0 | 11801 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-19 23:25:24 | 2023-05-21 01:27:05 | 81730 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     10 |       60 |

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
    ## -152.89  -35.16  -10.17   26.69  810.92 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.953e+02  7.459e+01
    ## date                                                1.173e-02  3.958e-03
    ## lag(value, 1)                                       1.100e-01  2.493e-02
    ## lag(value, 7)                                       9.294e-02  2.583e-02
    ## lag(value, 14)                                      9.517e-02  2.586e-02
    ## lag(value, 21)                                      6.540e-02  2.591e-02
    ## lag(value, 28)                                      6.330e-02  2.591e-02
    ## lag(value, 35)                                      6.940e-02  2.600e-02
    ## lag(value, 42)                                      5.251e-02  2.612e-02
    ## lag(value, 49)                                      8.562e-02  2.607e-02
    ## month(date, label = TRUE).L                        -1.125e+01  5.161e+00
    ## month(date, label = TRUE).Q                         2.513e+00  5.191e+00
    ## month(date, label = TRUE).C                        -1.173e+01  5.251e+00
    ## month(date, label = TRUE)^4                        -7.643e+00  5.214e+00
    ## month(date, label = TRUE)^5                        -1.220e+01  5.204e+00
    ## month(date, label = TRUE)^6                        -2.711e+00  5.283e+00
    ## month(date, label = TRUE)^7                        -7.025e+00  5.181e+00
    ## month(date, label = TRUE)^8                        -4.132e+00  5.212e+00
    ## month(date, label = TRUE)^9                         5.377e+00  5.265e+00
    ## month(date, label = TRUE)^10                        4.606e+00  5.303e+00
    ## month(date, label = TRUE)^11                       -6.045e+00  5.328e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.177e+01  2.397e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.957e+00  2.527e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.618 0.008937 ** 
    ## date                                                 2.964 0.003082 ** 
    ## lag(value, 1)                                        4.412 1.10e-05 ***
    ## lag(value, 7)                                        3.598 0.000331 ***
    ## lag(value, 14)                                       3.680 0.000242 ***
    ## lag(value, 21)                                       2.524 0.011708 *  
    ## lag(value, 28)                                       2.443 0.014677 *  
    ## lag(value, 35)                                       2.669 0.007687 ** 
    ## lag(value, 42)                                       2.010 0.044614 *  
    ## lag(value, 49)                                       3.285 0.001044 ** 
    ## month(date, label = TRUE).L                         -2.180 0.029387 *  
    ## month(date, label = TRUE).Q                          0.484 0.628387    
    ## month(date, label = TRUE).C                         -2.233 0.025680 *  
    ## month(date, label = TRUE)^4                         -1.466 0.142868    
    ## month(date, label = TRUE)^5                         -2.345 0.019168 *  
    ## month(date, label = TRUE)^6                         -0.513 0.607898    
    ## month(date, label = TRUE)^7                         -1.356 0.175381    
    ## month(date, label = TRUE)^8                         -0.793 0.428049    
    ## month(date, label = TRUE)^9                          1.021 0.307239    
    ## month(date, label = TRUE)^10                         0.868 0.385287    
    ## month(date, label = TRUE)^11                        -1.134 0.256771    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.911 1.01e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.148 0.001675 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.49 on 1506 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.257,  Adjusted R-squared:  0.2462 
    ## F-statistic: 23.68 on 22 and 1506 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.74819545318425"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.45977742877245"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.33666978284281"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.33666978284281"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.04026478059586"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.04026478059586"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.61021934243001"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.61021934243001"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.34439838069685"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.18965191251395"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.10576932491019"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.09303726733227"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 63, 98, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.05019403195756"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77, 63, 98, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.05019403195756"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77, 63, 98, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.74564900297657"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77, 63, 98, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.74564900297657"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77, 63, 98, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.28383857706951"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77, 63, 98, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.28383857706951"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.39872202896405"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 0.995761406505228"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 0.995761406505228"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.57878513242653"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.57878513242653"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.20625380905094"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.20625380905094"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.53897429672667"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.10667725526018"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.10667725526018"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.49330995587998"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.49330995587998"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.20036110084478"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.20036110084478"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.54748369558906"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.40563084740933"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.38385697498082"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.38385697498082"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.77416053296837"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.77416053296837"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.8978854652778"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.8978854652778"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.55424318260632"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.55424318260632"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.53974785852554"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.53974785852554"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.57531797087854"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.57531797087854"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.92080478006954"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.92080478006954"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.34113305731489"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.34113305731489"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.82616418234966"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.82616418234966"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.69928706013562"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.44589639412086"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.44589639412086"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.11907993501868"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.11907993501868"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.63861338911689"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.63861338911689"

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
    ## 1 healthyR.data <tibble [1,571 × 2]> <tibble [28 × 2]> <split [1543|28]>
    ## 2 healthyR      <tibble [1,564 × 2]> <tibble [28 × 2]> <split [1536|28]>
    ## 3 healthyR.ts   <tibble [1,508 × 2]> <tibble [28 × 2]> <split [1480|28]>
    ## 4 healthyverse  <tibble [1,479 × 2]> <tibble [28 × 2]> <split [1451|28]>
    ## 5 healthyR.ai   <tibble [1,303 × 2]> <tibble [28 × 2]> <split [1275|28]>
    ## 6 TidyDensity   <tibble [1,154 × 2]> <tibble [28 × 2]> <split [1126|28]>
    ## 7 tidyAML       <tibble [762 × 2]>   <tibble [28 × 2]> <split [734|28]> 
    ## 8 RandomWalker  <tibble [184 × 2]>   <tibble [28 × 2]> <split [156|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7361937 | 88.86179 | 0.6572085 | 151.13185 | 0.9294268 | 0.2409203 |
| healthyR.data | 2 | LM | Test | 0.7811475 | 184.16121 | 0.6973392 | 136.64245 | 0.9259204 | 0.0433011 |
| healthyR.data | 3 | EARTH | Test | 0.9961934 | 204.93865 | 0.8893132 | 139.27266 | 1.2448491 | 0.0433011 |
| healthyR.data | 4 | NNAR | Test | 0.8307947 | 101.10672 | 0.7416599 | 166.77722 | 1.0387582 | 0.1646983 |
| healthyR | 1 | ARIMA | Test | 0.7265083 | 133.19208 | 0.7606915 | 164.06431 | 0.8700539 | 0.0276122 |
| healthyR | 2 | LM | Test | 0.7214915 | 105.36189 | 0.7554386 | 183.82648 | 0.8909076 | 0.0800564 |
| healthyR | 3 | EARTH | Test | 0.7218852 | 109.68661 | 0.7558508 | 172.90511 | 0.8928910 | 0.0800564 |
| healthyR | 4 | NNAR | Test | 0.7376589 | 139.39746 | 0.7723667 | 167.12720 | 0.8961976 | 0.0092488 |
| healthyR.ts | 1 | ARIMA | Test | 0.9674567 | 120.32533 | 0.6859432 | 122.94743 | 1.1922070 | 0.0903303 |
| healthyR.ts | 2 | LM | Test | 0.9644502 | 117.19549 | 0.6838116 | 124.86437 | 1.1874101 | 0.0903303 |
| healthyR.ts | 3 | EARTH | Test | 0.9631683 | 116.48531 | 0.6829027 | 125.14478 | 1.1859836 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9815367 | 105.70946 | 0.6959262 | 180.42370 | 1.1667409 | 0.0457400 |
| healthyverse | 1 | ARIMA | Test | 0.6585083 | 108.56179 | 0.9425052 | 105.40603 | 0.8072961 | 0.0483621 |
| healthyverse | 2 | LM | Test | 0.6539605 | 144.29232 | 0.9359960 | 92.07087 | 0.7992832 | 0.0600258 |
| healthyverse | 3 | EARTH | Test | 0.6608080 | 98.99415 | 0.9457967 | 105.03598 | 0.8283595 | 0.0600258 |
| healthyverse | 4 | NNAR | Test | 0.6878435 | 99.93073 | 0.9844919 | 111.98231 | 0.8635018 | 0.0713897 |
| healthyR.ai | 1 | ARIMA | Test | 0.8133778 | 106.30620 | 0.7995920 | 178.76885 | 0.9369874 | 0.1106621 |
| healthyR.ai | 2 | LM | Test | 0.8034141 | 92.23217 | 0.7897972 | 148.71588 | 0.9665301 | 0.0407376 |
| healthyR.ai | 3 | EARTH | Test | 0.9310084 | 141.55408 | 0.9152289 | 155.42226 | 1.0657405 | 0.0407376 |
| healthyR.ai | 4 | NNAR | Test | 0.7915203 | 101.40957 | 0.7781049 | 155.58821 | 0.9169612 | 0.0995748 |
| TidyDensity | 1 | ARIMA | Test | 0.6987931 | 158.29055 | 0.7508813 | 117.65109 | 0.8489700 | 0.0038191 |
| TidyDensity | 2 | LM | Test | 0.7176983 | 201.23612 | 0.7711957 | 110.59850 | 0.8542733 | 0.0715273 |
| TidyDensity | 3 | EARTH | Test | 0.6931386 | 152.87748 | 0.7448053 | 117.87615 | 0.8409832 | 0.0715273 |
| TidyDensity | 4 | NNAR | Test | 0.7081788 | 110.52648 | 0.7609666 | 143.24897 | 0.8848846 | 0.0263563 |
| tidyAML | 1 | ARIMA | Test | 0.6215143 | 262.58699 | 0.6827781 | 94.03838 | 0.7442637 | 0.0154981 |
| tidyAML | 2 | LM | Test | 0.6347241 | 260.49630 | 0.6972900 | 95.93136 | 0.7473431 | 0.0034442 |
| tidyAML | 3 | EARTH | Test | 0.6714601 | 104.82972 | 0.7376472 | 127.45397 | 0.8724442 | 0.0034442 |
| tidyAML | 4 | NNAR | Test | 0.6121652 | 252.06019 | 0.6725075 | 93.22320 | 0.7336145 | 0.0360311 |
| RandomWalker | 1 | ARIMA | Test | 0.9171231 | 82.42778 | 0.4767735 | 88.19567 | 1.2013340 | 0.3119075 |
| RandomWalker | 2 | LM | Test | 1.3442311 | 107.98127 | 0.6988089 | 190.61260 | 1.4428159 | 0.0046802 |
| RandomWalker | 3 | EARTH | Test | 1.2680642 | 94.15972 | 0.6592129 | 173.46675 | 1.4083717 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4251529 | 132.71719 | 0.7408767 | 158.85750 | 1.6103460 | 0.0002218 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         2 LM          Test  0.781 184.  0.697 137.  0.926 0.0433
    ## 2 healthyR              1 ARIMA       Test  0.727 133.  0.761 164.  0.870 0.0276
    ## 3 healthyR.ts           4 NNAR        Test  0.982 106.  0.696 180.  1.17  0.0457
    ## 4 healthyverse          2 LM          Test  0.654 144.  0.936  92.1 0.799 0.0600
    ## 5 healthyR.ai           4 NNAR        Test  0.792 101.  0.778 156.  0.917 0.0996
    ## 6 TidyDensity           3 EARTH       Test  0.693 153.  0.745 118.  0.841 0.0715
    ## 7 tidyAML               4 NNAR        Test  0.612 252.  0.673  93.2 0.734 0.0360
    ## 8 RandomWalker          1 ARIMA       Test  0.917  82.4 0.477  88.2 1.20  0.312

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1543|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1536|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1480|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1451|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1275|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1126|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [734|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [156|28]>  <mdl_tm_t [1 × 5]>

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
