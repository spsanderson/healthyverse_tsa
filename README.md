Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
27 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 140,995
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

The last day in the data set is 2025-05-25 23:53:25, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.538792^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 140995        |
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
| r_version     |    101672 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    101672 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    101672 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12002 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-25 | 2023-06-28 | 1645 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133795.54 | 1518161.69 | 355 | 14701 | 289681 | 2367742 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10428.65 | 18579.32 | 1 | 279 | 3045 | 11666 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-25 23:53:25 | 2023-06-28 00:21:12 | 86266 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 51S |       60 |

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
    ## -148.16  -35.66  -11.08   26.58  814.86 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.745e+02  6.920e+01
    ## date                                                1.074e-02  3.668e-03
    ## lag(value, 1)                                       1.025e-01  2.444e-02
    ## lag(value, 7)                                       9.564e-02  2.530e-02
    ## lag(value, 14)                                      8.986e-02  2.529e-02
    ## lag(value, 21)                                      6.785e-02  2.540e-02
    ## lag(value, 28)                                      6.969e-02  2.532e-02
    ## lag(value, 35)                                      6.402e-02  2.541e-02
    ## lag(value, 42)                                      5.078e-02  2.553e-02
    ## lag(value, 49)                                      7.124e-02  2.545e-02
    ## month(date, label = TRUE).L                        -9.903e+00  5.114e+00
    ## month(date, label = TRUE).Q                         3.377e+00  5.145e+00
    ## month(date, label = TRUE).C                        -1.296e+01  5.141e+00
    ## month(date, label = TRUE)^4                        -6.962e+00  5.184e+00
    ## month(date, label = TRUE)^5                        -1.166e+01  5.132e+00
    ## month(date, label = TRUE)^6                        -3.414e+00  5.231e+00
    ## month(date, label = TRUE)^7                        -6.870e+00  5.112e+00
    ## month(date, label = TRUE)^8                        -3.975e+00  5.120e+00
    ## month(date, label = TRUE)^9                         5.572e+00  5.128e+00
    ## month(date, label = TRUE)^10                        3.867e+00  5.113e+00
    ## month(date, label = TRUE)^11                       -5.311e+00  5.256e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.166e+01  2.343e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.125e+00  2.465e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.521 0.011791 *  
    ## date                                                 2.928 0.003459 ** 
    ## lag(value, 1)                                        4.193 2.90e-05 ***
    ## lag(value, 7)                                        3.780 0.000162 ***
    ## lag(value, 14)                                       3.553 0.000392 ***
    ## lag(value, 21)                                       2.671 0.007631 ** 
    ## lag(value, 28)                                       2.753 0.005977 ** 
    ## lag(value, 35)                                       2.520 0.011849 *  
    ## lag(value, 42)                                       1.989 0.046905 *  
    ## lag(value, 49)                                       2.800 0.005179 ** 
    ## month(date, label = TRUE).L                         -1.937 0.052977 .  
    ## month(date, label = TRUE).Q                          0.656 0.511633    
    ## month(date, label = TRUE).C                         -2.520 0.011823 *  
    ## month(date, label = TRUE)^4                         -1.343 0.179466    
    ## month(date, label = TRUE)^5                         -2.271 0.023255 *  
    ## month(date, label = TRUE)^6                         -0.653 0.514092    
    ## month(date, label = TRUE)^7                         -1.344 0.179158    
    ## month(date, label = TRUE)^8                         -0.776 0.437622    
    ## month(date, label = TRUE)^9                          1.086 0.277426    
    ## month(date, label = TRUE)^10                         0.756 0.449528    
    ## month(date, label = TRUE)^11                        -1.011 0.312395    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.976 7.21e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.296 0.001004 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.74 on 1573 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2414, Adjusted R-squared:  0.2307 
    ## F-statistic: 22.75 on 22 and 1573 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.27702617951596"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.83565519329191"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 21, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.76686873529753"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 21, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.76686873529753"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 21, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.85077270996673"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 21, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.85077270996673"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 21, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.02650101027032"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 21, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.02650101027032"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.3154418391606"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.3154418391606"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.02326343418452"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.02326343418452"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.95619009130673"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.95619009130673"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.53849387142355"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.28804253327779"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.21170260020556"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 21 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.21170260020556"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.02373206455682"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 21 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.02373206455682"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 21 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.93259805576209"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 21 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.93259805576209"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.52627661121621"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.02382206060663"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.93094329200368"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.93094329200368"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.97094612422381"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.97094612422381"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.00941387860133"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.00941387860133"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.13830693285428"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.11237788494634"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.11237788494634"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.84055865048838"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.84055865048838"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.50079676473849"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.50079676473849"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.05794307137274"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.05794307137274"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.44495477569851"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.44495477569851"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.11610686977046"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.11610686977046"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.90751745055807"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.71878199868439"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.71878199868439"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.57265927279978"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.57265927279978"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.38767991977992"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.38767991977992"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.00613750045197"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.00613750045197"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.75170844426759"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.75170844426759"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.0673048071439"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.0673048071439"

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
    ## 1 healthyR.data <tibble [1,638 × 2]> <tibble [28 × 2]> <split [1610|28]>
    ## 2 healthyR      <tibble [1,631 × 2]> <tibble [28 × 2]> <split [1603|28]>
    ## 3 healthyR.ts   <tibble [1,575 × 2]> <tibble [28 × 2]> <split [1547|28]>
    ## 4 healthyverse  <tibble [1,545 × 2]> <tibble [28 × 2]> <split [1517|28]>
    ## 5 healthyR.ai   <tibble [1,370 × 2]> <tibble [28 × 2]> <split [1342|28]>
    ## 6 TidyDensity   <tibble [1,221 × 2]> <tibble [28 × 2]> <split [1193|28]>
    ## 7 tidyAML       <tibble [829 × 2]>   <tibble [28 × 2]> <split [801|28]> 
    ## 8 RandomWalker  <tibble [251 × 2]>   <tibble [28 × 2]> <split [223|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6126040 | 106.87475 | 0.7038249 | 127.84261 | 0.8236863 | 0.0089179 |
| healthyR.data | 2 | LM | Test | 0.6171409 | 141.17849 | 0.7090374 | 109.97978 | 0.8158816 | 0.0144666 |
| healthyR.data | 3 | EARTH | Test | 0.6298489 | 174.86492 | 0.7236376 | 102.39799 | 0.8424735 | 0.0144666 |
| healthyR.data | 4 | NNAR | Test | 0.7255342 | 99.50892 | 0.8335711 | 174.52868 | 0.9051547 | 0.0024235 |
| healthyR | 1 | ARIMA | Test | 0.6098928 | 95.39567 | 0.7122488 | 166.62706 | 0.7862104 | 0.0054860 |
| healthyR | 2 | LM | Test | 0.6033609 | 94.47549 | 0.7046207 | 163.78249 | 0.7683796 | 0.1327792 |
| healthyR | 3 | EARTH | Test | 0.5754901 | 109.96416 | 0.6720724 | 132.21595 | 0.7323525 | 0.1327792 |
| healthyR | 4 | NNAR | Test | 0.6064194 | 95.94974 | 0.7081924 | 143.11939 | 0.7723784 | 0.0031293 |
| healthyR.ts | 1 | ARIMA | Test | 0.5885542 | 98.80616 | 0.7414643 | 188.94006 | 0.7257519 | 0.0634741 |
| healthyR.ts | 2 | LM | Test | 0.8437510 | 239.91295 | 1.0629627 | 164.87106 | 0.9704507 | 0.0634741 |
| healthyR.ts | 3 | EARTH | Test | 0.5265473 | 203.86105 | 0.6633476 | 98.29694 | 0.7197321 | 0.0634741 |
| healthyR.ts | 4 | NNAR | Test | 0.6199836 | 111.50649 | 0.7810592 | 178.85254 | 0.7525769 | 0.0003671 |
| healthyverse | 1 | ARIMA | Test | 0.4731016 | 140.95773 | 0.7840468 | 71.78006 | 0.6451209 | 0.0698539 |
| healthyverse | 2 | LM | Test | 0.4503953 | 156.49765 | 0.7464168 | 68.01923 | 0.5778624 | 0.0504611 |
| healthyverse | 3 | EARTH | Test | 0.4776921 | 105.24990 | 0.7916544 | 78.60061 | 0.6462187 | 0.0504611 |
| healthyverse | 4 | NNAR | Test | 0.5022001 | 111.35751 | 0.8322702 | 84.84926 | 0.6638809 | 0.0023809 |
| healthyR.ai | 1 | ARIMA | Test | 0.5879808 | 125.53857 | 0.8732270 | 182.11192 | 0.7298612 | 0.0024067 |
| healthyR.ai | 2 | LM | Test | 0.4770934 | 97.18343 | 0.7085449 | 127.64796 | 0.6118752 | 0.0824138 |
| healthyR.ai | 3 | EARTH | Test | 7.1923767 | 3042.12008 | 10.6816028 | 189.85430 | 7.7570391 | 0.0824138 |
| healthyR.ai | 4 | NNAR | Test | 0.5915869 | 126.61755 | 0.8785824 | 132.65704 | 0.7476150 | 0.0447829 |
| TidyDensity | 1 | ARIMA | Test | 0.4208681 | 280.65585 | 0.8594028 | 114.92475 | 0.5255423 | 0.0224395 |
| TidyDensity | 2 | LM | Test | 0.5650163 | 436.42977 | 1.1537502 | 120.42845 | 0.6819294 | 0.0013624 |
| TidyDensity | 3 | EARTH | Test | 0.4186967 | 278.55748 | 0.8549688 | 114.39342 | 0.5157026 | 0.0013624 |
| TidyDensity | 4 | NNAR | Test | 0.3650748 | 163.70638 | 0.7454742 | 129.26024 | 0.4425991 | 0.0523569 |
| tidyAML | 1 | ARIMA | Test | 0.8232824 | 107.23716 | 0.9233202 | 113.29339 | 1.0869654 | 0.0059549 |
| tidyAML | 2 | LM | Test | 0.8418984 | 126.53541 | 0.9441983 | 106.19422 | 1.1152057 | 0.1970309 |
| tidyAML | 3 | EARTH | Test | 0.8077681 | 89.26213 | 0.9059207 | 124.80609 | 1.0388110 | 0.1970309 |
| tidyAML | 4 | NNAR | Test | 0.8758559 | 127.38003 | 0.9822819 | 112.77835 | 1.1381798 | 0.1548225 |
| RandomWalker | 1 | ARIMA | Test | 1.3857045 | 213.57991 | 0.7459748 | 139.69817 | 1.6848490 | 0.0077847 |
| RandomWalker | 2 | LM | Test | 1.1821986 | 97.88637 | 0.6364202 | 189.57826 | 1.3939728 | 0.0009462 |
| RandomWalker | 3 | EARTH | Test | 1.1663334 | 104.75931 | 0.6278794 | 173.80217 | 1.4090329 | NA |
| RandomWalker | 4 | NNAR | Test | 1.1161361 | 167.09555 | 0.6008564 | 129.19092 | 1.3735538 | 0.0621070 |

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
    ## 1 healthyR.data         2 LM          Test  0.617 141.  0.709 110.  0.816 0.0145
    ## 2 healthyR              3 EARTH       Test  0.575 110.  0.672 132.  0.732 0.133 
    ## 3 healthyR.ts           3 EARTH       Test  0.527 204.  0.663  98.3 0.720 0.0635
    ## 4 healthyverse          2 LM          Test  0.450 156.  0.746  68.0 0.578 0.0505
    ## 5 healthyR.ai           2 LM          Test  0.477  97.2 0.709 128.  0.612 0.0824
    ## 6 TidyDensity           4 NNAR        Test  0.365 164.  0.745 129.  0.443 0.0524
    ## 7 tidyAML               3 EARTH       Test  0.808  89.3 0.906 125.  1.04  0.197 
    ## 8 RandomWalker          4 NNAR        Test  1.12  167.  0.601 129.  1.37  0.0621

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1610|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1603|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1547|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1517|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1342|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1193|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [801|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [223|28]>  <mdl_tm_t [1 × 5]>

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
