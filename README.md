Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
28 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 138,321
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

The last day in the data set is 2025-04-26 23:01:08, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6299.42
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 138321        |
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
| r_version     |     99568 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99568 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99568 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11705 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-26 | 2023-06-10 | 1616 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134974.2 | 1521265.85 | 355 | 14701 | 278153 | 2367768 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10392.2 | 18461.35 | 1 | 300 | 3061 | 11721 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-26 23:01:08 | 2023-06-10 02:07:00 | 84351 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 21S |       60 |

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
    ## -147.24  -35.66  -10.74   26.68  814.26 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.893e+02  7.180e+01
    ## date                                                1.151e-02  3.808e-03
    ## lag(value, 1)                                       1.045e-01  2.467e-02
    ## lag(value, 7)                                       9.615e-02  2.554e-02
    ## lag(value, 14)                                      9.452e-02  2.558e-02
    ## lag(value, 21)                                      6.796e-02  2.572e-02
    ## lag(value, 28)                                      6.573e-02  2.561e-02
    ## lag(value, 35)                                      6.783e-02  2.575e-02
    ## lag(value, 42)                                      4.863e-02  2.582e-02
    ## lag(value, 49)                                      6.698e-02  2.568e-02
    ## month(date, label = TRUE).L                        -1.023e+01  5.129e+00
    ## month(date, label = TRUE).Q                         2.714e+00  5.192e+00
    ## month(date, label = TRUE).C                        -1.228e+01  5.188e+00
    ## month(date, label = TRUE)^4                        -6.578e+00  5.196e+00
    ## month(date, label = TRUE)^5                        -1.235e+01  5.195e+00
    ## month(date, label = TRUE)^6                        -3.048e+00  5.242e+00
    ## month(date, label = TRUE)^7                        -6.269e+00  5.166e+00
    ## month(date, label = TRUE)^8                        -4.471e+00  5.164e+00
    ## month(date, label = TRUE)^9                         5.395e+00  5.157e+00
    ## month(date, label = TRUE)^10                        4.535e+00  5.243e+00
    ## month(date, label = TRUE)^11                       -5.881e+00  5.332e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.171e+01  2.374e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.336e+00  2.499e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.637 0.008441 ** 
    ## date                                                 3.023 0.002546 ** 
    ## lag(value, 1)                                        4.235 2.42e-05 ***
    ## lag(value, 7)                                        3.765 0.000173 ***
    ## lag(value, 14)                                       3.694 0.000228 ***
    ## lag(value, 21)                                       2.642 0.008316 ** 
    ## lag(value, 28)                                       2.567 0.010366 *  
    ## lag(value, 35)                                       2.634 0.008521 ** 
    ## lag(value, 42)                                       1.883 0.059849 .  
    ## lag(value, 49)                                       2.609 0.009173 ** 
    ## month(date, label = TRUE).L                         -1.993 0.046388 *  
    ## month(date, label = TRUE).Q                          0.523 0.601200    
    ## month(date, label = TRUE).C                         -2.367 0.018076 *  
    ## month(date, label = TRUE)^4                         -1.266 0.205667    
    ## month(date, label = TRUE)^5                         -2.378 0.017526 *  
    ## month(date, label = TRUE)^6                         -0.581 0.561039    
    ## month(date, label = TRUE)^7                         -1.213 0.225171    
    ## month(date, label = TRUE)^8                         -0.866 0.386689    
    ## month(date, label = TRUE)^9                          1.046 0.295654    
    ## month(date, label = TRUE)^10                         0.865 0.387103    
    ## month(date, label = TRUE)^11                        -1.103 0.270166    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.932 9.02e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.336 0.000871 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.77 on 1544 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2463, Adjusted R-squared:  0.2355 
    ## F-statistic: 22.93 on 22 and 1544 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.13521414064336"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.03245580322317"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.61130582064192"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.61130582064192"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.55527074208074"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.55527074208074"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.92638291680517"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.92638291680517"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.03218351720814"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.49233551859612"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.49233551859612"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.99528801690689"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.99528801690689"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.62197185201329"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.62197185201329"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.15039477198717"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.34258242773018"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.21665051652139"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 21, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.21665051652139"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.52293252449011"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 21, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.52293252449011"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 21, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.19223699642033"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 21, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.19223699642033"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.44269862277876"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.33057540024806"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.27857349753535"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.27857349753535"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.54970850846028"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.54970850846028"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.84254669924374"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.84254669924374"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.74817326405301"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.74817326405301"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 13.0303394060134"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 13.0303394060134"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 9.30430133208958"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 9.30430133208958"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.69344975372"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.69344975372"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.46117755962562"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.46117755962562"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.03402532410919"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.03402532410919"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.91720113588312"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.91720113588312"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.71800290797759"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.71800290797759"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.43263222500199"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.43263222500199"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.07464435694497"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.58426816188603"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.40877020872532"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.33627715012833"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 84, 35, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.29735871258344"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63, 84, 35, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.29735871258344"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63, 84, 35, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.32404681200313"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63, 84, 35, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.32404681200313"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63, 84, 35, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.37122742476274"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63, 84, 35, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.37122742476274"

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
    ## 1 healthyR.data <tibble [1,609 × 2]> <tibble [28 × 2]> <split [1581|28]>
    ## 2 healthyR      <tibble [1,602 × 2]> <tibble [28 × 2]> <split [1574|28]>
    ## 3 healthyR.ts   <tibble [1,546 × 2]> <tibble [28 × 2]> <split [1518|28]>
    ## 4 healthyverse  <tibble [1,517 × 2]> <tibble [28 × 2]> <split [1489|28]>
    ## 5 healthyR.ai   <tibble [1,341 × 2]> <tibble [28 × 2]> <split [1313|28]>
    ## 6 TidyDensity   <tibble [1,192 × 2]> <tibble [28 × 2]> <split [1164|28]>
    ## 7 tidyAML       <tibble [800 × 2]>   <tibble [28 × 2]> <split [772|28]> 
    ## 8 RandomWalker  <tibble [222 × 2]>   <tibble [28 × 2]> <split [194|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6708133 | 141.01906 | 0.5324481 | 137.12751 | 0.8096096 | 0.0999997 |
| healthyR.data | 2 | LM | Test | 0.7473374 | 182.28615 | 0.5931880 | 138.81778 | 0.8660570 | 0.0011228 |
| healthyR.data | 3 | EARTH | Test | 0.9590136 | 388.57308 | 0.7612029 | 126.13442 | 1.1498961 | 0.0011228 |
| healthyR.data | 4 | NNAR | Test | 0.7440876 | 100.55576 | 0.5906086 | 174.02838 | 0.8988543 | 0.0551404 |
| healthyR | 1 | ARIMA | Test | 0.6261014 | 90.51251 | 0.6998058 | 161.48986 | 0.8063483 | 0.0031646 |
| healthyR | 2 | LM | Test | 0.6386202 | 97.45244 | 0.7137983 | 179.31319 | 0.7783418 | 0.0107400 |
| healthyR | 3 | EARTH | Test | 0.6388659 | 97.22534 | 0.7140729 | 156.48024 | 0.7733400 | 0.0107400 |
| healthyR | 4 | NNAR | Test | 0.6212456 | 89.36056 | 0.6943783 | 146.04413 | 0.7662187 | 0.0266731 |
| healthyR.ts | 1 | ARIMA | Test | 0.8868761 | 124.59802 | 0.6937262 | 152.38324 | 1.0937133 | 0.0054099 |
| healthyR.ts | 2 | LM | Test | 0.9038253 | 168.42498 | 0.7069840 | 127.52489 | 1.1855012 | 0.0054099 |
| healthyR.ts | 3 | EARTH | Test | 1.0258522 | 235.74450 | 0.8024351 | 140.32829 | 1.1852838 | 0.0054099 |
| healthyR.ts | 4 | NNAR | Test | 0.8684426 | 201.74307 | 0.6793073 | 175.22385 | 1.0411431 | 0.0731622 |
| healthyverse | 1 | ARIMA | Test | 0.7499696 | 332.24360 | 0.7828596 | 122.40207 | 0.8867590 | 0.0753356 |
| healthyverse | 2 | LM | Test | 0.7652688 | 474.13028 | 0.7988298 | 112.47632 | 0.8889840 | 0.0602195 |
| healthyverse | 3 | EARTH | Test | 0.7477005 | 288.63384 | 0.7804910 | 122.62949 | 0.9328576 | 0.0602195 |
| healthyverse | 4 | NNAR | Test | 0.7398562 | 242.15803 | 0.7723027 | 124.49639 | 0.9455104 | 0.0298673 |
| healthyR.ai | 1 | ARIMA | Test | 0.7270671 | 118.51000 | 0.8403671 | 177.51754 | 0.8900172 | 0.0132123 |
| healthyR.ai | 2 | LM | Test | 0.7335750 | 136.84576 | 0.8478891 | 156.99894 | 0.8927033 | 0.2581962 |
| healthyR.ai | 3 | EARTH | Test | 0.7376738 | 140.90216 | 0.8526267 | 155.53853 | 0.8984623 | 0.2581962 |
| healthyR.ai | 4 | NNAR | Test | 0.7757342 | 168.24025 | 0.8966181 | 162.77190 | 0.9321340 | 0.0001550 |
| TidyDensity | 1 | ARIMA | Test | 0.5679428 | 194.73505 | 0.7478732 | 108.95139 | 0.7054870 | 0.0710516 |
| TidyDensity | 2 | LM | Test | 0.6001895 | 262.90105 | 0.7903359 | 96.86325 | 0.7823519 | 0.0019018 |
| TidyDensity | 3 | EARTH | Test | 0.5964610 | 193.80004 | 0.7854262 | 109.60794 | 0.7286022 | 0.0019018 |
| TidyDensity | 4 | NNAR | Test | 0.5781100 | 114.54233 | 0.7612615 | 131.11120 | 0.7131711 | 0.0845147 |
| tidyAML | 1 | ARIMA | Test | 0.5954629 | 236.59885 | 0.8760909 | 103.22909 | 0.7144572 | 0.0810259 |
| tidyAML | 2 | LM | Test | 0.6256122 | 275.42491 | 0.9204488 | 99.38404 | 0.7706070 | 0.0836909 |
| tidyAML | 3 | EARTH | Test | 0.5926162 | 150.24208 | 0.8719026 | 119.71430 | 0.7412112 | 0.0836909 |
| tidyAML | 4 | NNAR | Test | 0.6129636 | 258.32706 | 0.9018391 | 103.04793 | 0.7428968 | 0.0198067 |
| RandomWalker | 1 | ARIMA | Test | 1.2525587 | 165.39351 | 0.5730267 | 147.77133 | 1.5512413 | 0.0010637 |
| RandomWalker | 2 | LM | Test | 1.2434140 | 111.14393 | 0.5688432 | 174.67099 | 1.4827637 | 0.0067311 |
| RandomWalker | 3 | EARTH | Test | 1.2406311 | 113.36165 | 0.5675700 | 170.53505 | 1.4850072 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4032914 | 206.18661 | 0.6419846 | 171.27007 | 1.5619836 | 0.0000044 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.671 141.  0.532  137. 0.810 0.100  
    ## 2 healthyR             4 NNAR        Test  0.621  89.4 0.694  146. 0.766 0.0267 
    ## 3 healthyR.ts          4 NNAR        Test  0.868 202.  0.679  175. 1.04  0.0732 
    ## 4 healthyverse         1 ARIMA       Test  0.750 332.  0.783  122. 0.887 0.0753 
    ## 5 healthyR.ai          1 ARIMA       Test  0.727 119.  0.840  178. 0.890 0.0132 
    ## 6 TidyDensity          1 ARIMA       Test  0.568 195.  0.748  109. 0.705 0.0711 
    ## 7 tidyAML              1 ARIMA       Test  0.595 237.  0.876  103. 0.714 0.0810 
    ## 8 RandomWalker         2 LM          Test  1.24  111.  0.569  175. 1.48  0.00673

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1581|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1574|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1518|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1489|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1313|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1164|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [772|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [194|28]>  <mdl_tm_t [1 × 5]>

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
