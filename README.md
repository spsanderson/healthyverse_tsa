Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
29 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 138,510
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

The last day in the data set is 2025-04-27 23:45:34, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6324.16
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 138510        |
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
| r_version     |     99747 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99747 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99747 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11707 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-27 | 2023-06-12 | 1617 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134182.37 | 1520696.75 | 355 | 14701 | 278210 | 2367754 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10383.77 | 18452.45 | 1 | 292 | 3058 | 11662 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-27 23:45:34 | 2023-06-12 06:54:15 | 84455 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     25 |       60 |

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
    ## -146.65  -35.60  -10.61   26.64  814.59 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.975e+02  7.178e+01
    ## date                                                1.195e-02  3.807e-03
    ## lag(value, 1)                                       1.040e-01  2.470e-02
    ## lag(value, 7)                                       9.550e-02  2.557e-02
    ## lag(value, 14)                                      9.447e-02  2.561e-02
    ## lag(value, 21)                                      6.807e-02  2.575e-02
    ## lag(value, 28)                                      6.521e-02  2.564e-02
    ## lag(value, 35)                                      6.775e-02  2.578e-02
    ## lag(value, 42)                                      4.737e-02  2.585e-02
    ## lag(value, 49)                                      6.618e-02  2.570e-02
    ## month(date, label = TRUE).L                        -1.042e+01  5.135e+00
    ## month(date, label = TRUE).Q                         2.530e+00  5.198e+00
    ## month(date, label = TRUE).C                        -1.192e+01  5.191e+00
    ## month(date, label = TRUE)^4                        -6.677e+00  5.201e+00
    ## month(date, label = TRUE)^5                        -1.257e+01  5.201e+00
    ## month(date, label = TRUE)^6                        -2.734e+00  5.246e+00
    ## month(date, label = TRUE)^7                        -6.388e+00  5.172e+00
    ## month(date, label = TRUE)^8                        -4.685e+00  5.169e+00
    ## month(date, label = TRUE)^9                         5.769e+00  5.161e+00
    ## month(date, label = TRUE)^10                        4.193e+00  5.246e+00
    ## month(date, label = TRUE)^11                       -5.701e+00  5.337e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.172e+01  2.377e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.278e+00  2.502e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.751 0.006015 ** 
    ## date                                                 3.138 0.001731 ** 
    ## lag(value, 1)                                        4.211 2.69e-05 ***
    ## lag(value, 7)                                        3.736 0.000194 ***
    ## lag(value, 14)                                       3.688 0.000233 ***
    ## lag(value, 21)                                       2.643 0.008289 ** 
    ## lag(value, 28)                                       2.544 0.011071 *  
    ## lag(value, 35)                                       2.628 0.008676 ** 
    ## lag(value, 42)                                       1.833 0.067032 .  
    ## lag(value, 49)                                       2.575 0.010122 *  
    ## month(date, label = TRUE).L                         -2.029 0.042663 *  
    ## month(date, label = TRUE).Q                          0.487 0.626563    
    ## month(date, label = TRUE).C                         -2.297 0.021775 *  
    ## month(date, label = TRUE)^4                         -1.284 0.199414    
    ## month(date, label = TRUE)^5                         -2.417 0.015752 *  
    ## month(date, label = TRUE)^6                         -0.521 0.602382    
    ## month(date, label = TRUE)^7                         -1.235 0.216977    
    ## month(date, label = TRUE)^8                         -0.906 0.364823    
    ## month(date, label = TRUE)^9                          1.118 0.263788    
    ## month(date, label = TRUE)^10                         0.799 0.424228    
    ## month(date, label = TRUE)^11                        -1.068 0.285636    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.929 9.15e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.309 0.000958 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.83 on 1545 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2451, Adjusted R-squared:  0.2344 
    ## F-statistic:  22.8 on 22 and 1545 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.07357469971887"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.01946766743965"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.66186788619901"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.66186788619901"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.65996946757086"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.65996946757086"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.0061605636168"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.0061605636168"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.6020328382517"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.6020328382517"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.92033670871581"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.92033670871581"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.79159305322308"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.79159305322308"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.21612777281762"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.40652665950194"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.29690904348757"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 21, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.29690904348757"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.55175258137364"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 21, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.55175258137364"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.25839324547333"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 21, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.25839324547333"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.49380397827036"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.39621367065132"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.36892650226415"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.36892650226415"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.61942313202765"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.61942313202765"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.92250782128766"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.92250782128766"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.96823184815288"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.96823184815288"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.08326576157709"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.08326576157709"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.46733461785485"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.46733461785485"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.65237470208672"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.65237470208672"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.80661893142167"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.80661893142167"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.99024151389388"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.99024151389388"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.03773818160374"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.03773818160374"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.00142064113664"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.00142064113664"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.64607792266981"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.64607792266981"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.98873373286935"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.50933681297103"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.37143000481625"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.24901312004473"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84, 35, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.18941877823265"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63, 84, 35, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.18941877823265"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63, 84, 35, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.30212059382882"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63, 84, 35, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.30212059382882"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63, 84, 35, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.30537993017014"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63, 84, 35, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.30537993017014"

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
    ## 1 healthyR.data <tibble [1,610 × 2]> <tibble [28 × 2]> <split [1582|28]>
    ## 2 healthyR      <tibble [1,603 × 2]> <tibble [28 × 2]> <split [1575|28]>
    ## 3 healthyR.ts   <tibble [1,547 × 2]> <tibble [28 × 2]> <split [1519|28]>
    ## 4 healthyverse  <tibble [1,518 × 2]> <tibble [28 × 2]> <split [1490|28]>
    ## 5 healthyR.ai   <tibble [1,342 × 2]> <tibble [28 × 2]> <split [1314|28]>
    ## 6 TidyDensity   <tibble [1,193 × 2]> <tibble [28 × 2]> <split [1165|28]>
    ## 7 tidyAML       <tibble [801 × 2]>   <tibble [28 × 2]> <split [773|28]> 
    ## 8 RandomWalker  <tibble [223 × 2]>   <tibble [28 × 2]> <split [195|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6524969 | 131.03186 | 0.5284436 | 127.60086 | 0.7998733 | 0.1473772 |
| healthyR.data | 2 | LM | Test | 0.7472342 | 183.05175 | 0.6051693 | 138.90092 | 0.8656058 | 0.0024111 |
| healthyR.data | 3 | EARTH | Test | 0.9439637 | 388.24584 | 0.7644964 | 127.02198 | 1.1215601 | 0.0024111 |
| healthyR.data | 4 | NNAR | Test | 0.7512056 | 107.74088 | 0.6083857 | 178.60338 | 0.9038719 | 0.0618262 |
| healthyR | 1 | ARIMA | Test | 0.5755861 | 87.94608 | 0.6487710 | 154.16859 | 0.7529173 | 0.0061203 |
| healthyR | 2 | LM | Test | 0.6054449 | 96.71200 | 0.6824262 | 176.07459 | 0.7504434 | 0.0002436 |
| healthyR | 3 | EARTH | Test | 0.6064043 | 97.41448 | 0.6835076 | 150.64075 | 0.7478893 | 0.0002436 |
| healthyR | 4 | NNAR | Test | 0.5967754 | 89.04068 | 0.6726545 | 143.85622 | 0.7600938 | 0.0024092 |
| healthyR.ts | 1 | ARIMA | Test | 0.8479944 | 112.91900 | 0.6522461 | 152.61273 | 1.0618323 | 0.0020875 |
| healthyR.ts | 2 | LM | Test | 0.8631540 | 166.10110 | 0.6639062 | 122.93838 | 1.1568230 | 0.0020875 |
| healthyR.ts | 3 | EARTH | Test | 1.0920282 | 278.43534 | 0.8399478 | 141.99832 | 1.2511818 | 0.0020875 |
| healthyR.ts | 4 | NNAR | Test | 0.8456913 | 208.79709 | 0.6504746 | 172.31062 | 1.0289726 | 0.0669795 |
| healthyverse | 1 | ARIMA | Test | 0.7380161 | 363.62613 | 0.7866275 | 117.64602 | 0.8748932 | 0.0737893 |
| healthyverse | 2 | LM | Test | 0.7647698 | 477.78618 | 0.8151435 | 112.35954 | 0.8882387 | 0.0211214 |
| healthyverse | 3 | EARTH | Test | 0.7490431 | 294.52754 | 0.7983809 | 122.57559 | 0.9332157 | 0.0211214 |
| healthyverse | 4 | NNAR | Test | 0.7380951 | 262.58502 | 0.7867118 | 122.46918 | 0.9319427 | 0.0287878 |
| healthyR.ai | 1 | ARIMA | Test | 0.8379018 | 120.28645 | 0.9401958 | 177.61915 | 1.0891743 | 0.0189312 |
| healthyR.ai | 2 | LM | Test | 0.8541739 | 139.85824 | 0.9584544 | 162.56817 | 1.1063181 | 0.3550123 |
| healthyR.ai | 3 | EARTH | Test | 0.8594097 | 144.55833 | 0.9643294 | 160.68099 | 1.1148693 | 0.3550123 |
| healthyR.ai | 4 | NNAR | Test | 0.8910654 | 169.17443 | 0.9998498 | 162.29224 | 1.1373429 | 0.0012711 |
| TidyDensity | 1 | ARIMA | Test | 0.5503265 | 231.47099 | 0.7801248 | 111.61955 | 0.6855779 | 0.0089221 |
| TidyDensity | 2 | LM | Test | 0.5657808 | 337.75979 | 0.8020323 | 96.31598 | 0.7334962 | 0.0102334 |
| TidyDensity | 3 | EARTH | Test | 0.5646327 | 234.40887 | 0.8004049 | 110.22951 | 0.6871976 | 0.0102334 |
| TidyDensity | 4 | NNAR | Test | 0.5566351 | 136.33085 | 0.7890677 | 134.00288 | 0.6966032 | 0.0481589 |
| tidyAML | 1 | ARIMA | Test | 0.6464537 | 283.91540 | 1.0068221 | 107.45524 | 0.7478587 | 0.0042932 |
| tidyAML | 2 | LM | Test | 0.5844078 | 279.08595 | 0.9101884 | 97.04123 | 0.7129172 | 0.0063620 |
| tidyAML | 3 | EARTH | Test | 0.5389418 | 179.70801 | 0.8393772 | 105.72181 | 0.6550500 | 0.0063620 |
| tidyAML | 4 | NNAR | Test | 0.5850716 | 268.28960 | 0.9112222 | 101.65436 | 0.7035670 | 0.0085394 |
| RandomWalker | 1 | ARIMA | Test | 1.2670275 | 156.84871 | 0.5601073 | 146.19577 | 1.5578024 | 0.0027921 |
| RandomWalker | 2 | LM | Test | 1.2933705 | 113.60705 | 0.5717526 | 173.94457 | 1.5288375 | 0.0152137 |
| RandomWalker | 3 | EARTH | Test | 1.2927092 | 114.75817 | 0.5714602 | 172.10906 | 1.5305673 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4257858 | 184.81942 | 0.6302886 | 148.50597 | 1.7196483 | 0.0257689 |

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
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         1 ARIMA       Test  0.652 131.  0.528  128. 0.800 1.47e-1
    ## 2 healthyR             3 EARTH       Test  0.606  97.4 0.684  151. 0.748 2.44e-4
    ## 3 healthyR.ts          4 NNAR        Test  0.846 209.  0.650  172. 1.03  6.70e-2
    ## 4 healthyverse         1 ARIMA       Test  0.738 364.  0.787  118. 0.875 7.38e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.838 120.  0.940  178. 1.09  1.89e-2
    ## 6 TidyDensity          1 ARIMA       Test  0.550 231.  0.780  112. 0.686 8.92e-3
    ## 7 tidyAML              3 EARTH       Test  0.539 180.  0.839  106. 0.655 6.36e-3
    ## 8 RandomWalker         2 LM          Test  1.29  114.  0.572  174. 1.53  1.52e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1582|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1575|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1519|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1490|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1314|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1165|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [773|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [195|28]>  <mdl_tm_t [1 × 5]>

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
