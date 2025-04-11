Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
11 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 136,451
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

The last day in the data set is 2025-04-09 23:25:37, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5891.83
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 136451        |
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
| r_version     |     98120 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     98120 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     98120 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11472 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-09 | 2023-05-30 | 1599 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134638.13 | 1523272.13 | 355 | 14701 | 271509 | 2367770 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10356.33 | 18367.95 | 1 | 303 | 3064 | 11700 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-09 23:25:37 | 2023-05-30 09:27:16 | 83019 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 13S |       60 |

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
    ## -149.28  -35.47  -10.59   26.54  812.69 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.802e+02  7.301e+01
    ## date                                                1.100e-02  3.873e-03
    ## lag(value, 1)                                       1.078e-01  2.477e-02
    ## lag(value, 7)                                       9.728e-02  2.565e-02
    ## lag(value, 14)                                      9.638e-02  2.577e-02
    ## lag(value, 21)                                      6.571e-02  2.584e-02
    ## lag(value, 28)                                      5.992e-02  2.574e-02
    ## lag(value, 35)                                      6.834e-02  2.580e-02
    ## lag(value, 42)                                      4.906e-02  2.589e-02
    ## lag(value, 49)                                      7.703e-02  2.586e-02
    ## month(date, label = TRUE).L                        -1.013e+01  5.136e+00
    ## month(date, label = TRUE).Q                         2.853e+00  5.194e+00
    ## month(date, label = TRUE).C                        -1.252e+01  5.221e+00
    ## month(date, label = TRUE)^4                        -6.608e+00  5.190e+00
    ## month(date, label = TRUE)^5                        -1.212e+01  5.201e+00
    ## month(date, label = TRUE)^6                        -3.347e+00  5.271e+00
    ## month(date, label = TRUE)^7                        -6.105e+00  5.159e+00
    ## month(date, label = TRUE)^8                        -4.451e+00  5.175e+00
    ## month(date, label = TRUE)^9                         5.220e+00  5.211e+00
    ## month(date, label = TRUE)^10                        4.751e+00  5.279e+00
    ## month(date, label = TRUE)^11                       -6.004e+00  5.333e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.167e+01  2.386e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.098e+00  2.513e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.468 0.013686 *  
    ## date                                                 2.839 0.004581 ** 
    ## lag(value, 1)                                        4.353 1.43e-05 ***
    ## lag(value, 7)                                        3.792 0.000155 ***
    ## lag(value, 14)                                       3.739 0.000191 ***
    ## lag(value, 21)                                       2.543 0.011075 *  
    ## lag(value, 28)                                       2.328 0.020056 *  
    ## lag(value, 35)                                       2.649 0.008158 ** 
    ## lag(value, 42)                                       1.895 0.058293 .  
    ## lag(value, 49)                                       2.979 0.002938 ** 
    ## month(date, label = TRUE).L                         -1.973 0.048665 *  
    ## month(date, label = TRUE).Q                          0.549 0.582919    
    ## month(date, label = TRUE).C                         -2.398 0.016606 *  
    ## month(date, label = TRUE)^4                         -1.273 0.203188    
    ## month(date, label = TRUE)^5                         -2.331 0.019884 *  
    ## month(date, label = TRUE)^6                         -0.635 0.525506    
    ## month(date, label = TRUE)^7                         -1.183 0.236890    
    ## month(date, label = TRUE)^8                         -0.860 0.389905    
    ## month(date, label = TRUE)^9                          1.002 0.316634    
    ## month(date, label = TRUE)^10                         0.900 0.368219    
    ## month(date, label = TRUE)^11                        -1.126 0.260378    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.890 1.12e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.222 0.001298 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.64 on 1527 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2492, Adjusted R-squared:  0.2384 
    ## F-statistic: 23.04 on 22 and 1527 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.4605999291108"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.22818519607452"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.04293324949501"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.04293324949501"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.72910166137642"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.72910166137642"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.61752754789613"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.61752754789613"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.63741167603083"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.33745144167375"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 14, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.14602503669746"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 14, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.14602503669746"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 14, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.77998405987744"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 14, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.77998405987744"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 14, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.37128596065478"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 14, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.37128596065478"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.33226868166057"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.33226868166057"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.00927044048526"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.00927044048526"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.48681324534234"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.48681324534234"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.19322637740499"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.87795208958926"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.85181992037234"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.85181992037234"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.47336374556674"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.47336374556674"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.01468613993666"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.01468613993666"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.12792410582759"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.04177133196249"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 77, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.0173504878282"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 77, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.0173504878282"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 77, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.83957083220953"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 77, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.83957083220953"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 77, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.30076855871128"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 77, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.30076855871128"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.33515260157398"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.37067690859068"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.37067690859068"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.5274963344604"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.5274963344604"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.36679323729339"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.36679323729339"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.64940696752501"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.64940696752501"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.77136564456471"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.77136564456471"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.30686925763388"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.30686925763388"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.26199540300914"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.08377091573659"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.08377091573659"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.99383755748851"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.99383755748851"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.705336817284"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.705336817284"

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
    ## 1 healthyR.data <tibble [1,592 × 2]> <tibble [28 × 2]> <split [1564|28]>
    ## 2 healthyR      <tibble [1,585 × 2]> <tibble [28 × 2]> <split [1557|28]>
    ## 3 healthyR.ts   <tibble [1,529 × 2]> <tibble [28 × 2]> <split [1501|28]>
    ## 4 healthyverse  <tibble [1,500 × 2]> <tibble [28 × 2]> <split [1472|28]>
    ## 5 healthyR.ai   <tibble [1,324 × 2]> <tibble [28 × 2]> <split [1296|28]>
    ## 6 TidyDensity   <tibble [1,175 × 2]> <tibble [28 × 2]> <split [1147|28]>
    ## 7 tidyAML       <tibble [783 × 2]>   <tibble [28 × 2]> <split [755|28]> 
    ## 8 RandomWalker  <tibble [205 × 2]>   <tibble [28 × 2]> <split [177|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7436009 | 130.33433 | 0.5975899 | 169.68192 | 0.9069345 | 0.0059306 |
| healthyR.data | 2 | LM | Test | 0.7046728 | 151.51313 | 0.5663056 | 132.01739 | 0.8543863 | 0.0089369 |
| healthyR.data | 3 | EARTH | Test | 0.8117096 | 245.43437 | 0.6523250 | 120.97699 | 0.9681787 | 0.0089369 |
| healthyR.data | 4 | NNAR | Test | 0.7152495 | 100.52123 | 0.5748055 | 167.21076 | 0.9397298 | 0.1152529 |
| healthyR | 1 | ARIMA | Test | 0.7626202 | 109.54876 | 0.7674796 | 184.61537 | 0.9346342 | 0.0000855 |
| healthyR | 2 | LM | Test | 0.7289824 | 98.35088 | 0.7336275 | 187.10202 | 0.9072665 | 0.0008778 |
| healthyR | 3 | EARTH | Test | 0.7282391 | 97.35095 | 0.7328795 | 185.72884 | 0.9074503 | 0.0008778 |
| healthyR | 4 | NNAR | Test | 0.7250992 | 96.37693 | 0.7297196 | 157.83230 | 0.9055094 | 0.0025560 |
| healthyR.ts | 1 | ARIMA | Test | 0.9316868 | 254.03514 | 0.7369536 | 139.36039 | 1.1779015 | 0.0000945 |
| healthyR.ts | 2 | LM | Test | 0.9447402 | 267.71454 | 0.7472788 | 138.48348 | 1.1932182 | 0.0000945 |
| healthyR.ts | 3 | EARTH | Test | 0.9427428 | 265.56401 | 0.7456989 | 138.75289 | 1.1902297 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.8086374 | 96.51198 | 0.6396230 | 166.12274 | 1.0433176 | 0.0181451 |
| healthyverse | 1 | ARIMA | Test | 0.6558847 | 275.15099 | 0.7538098 | 103.76508 | 0.8086369 | 0.0580710 |
| healthyverse | 2 | LM | Test | 0.5807283 | 333.54869 | 0.6674323 | 88.61647 | 0.7189316 | 0.0099278 |
| healthyverse | 3 | EARTH | Test | 0.6319869 | 208.44716 | 0.7263440 | 108.77358 | 0.7974668 | 0.0099278 |
| healthyverse | 4 | NNAR | Test | 0.6346542 | 207.25085 | 0.7294094 | 107.89860 | 0.8074876 | 0.0059245 |
| healthyR.ai | 1 | ARIMA | Test | 0.6964085 | 110.97693 | 0.6865459 | 163.70088 | 0.9279461 | 0.0184877 |
| healthyR.ai | 2 | LM | Test | 0.6639196 | 136.52603 | 0.6545171 | 140.36726 | 0.8925612 | 0.0212423 |
| healthyR.ai | 3 | EARTH | Test | 0.6847046 | 116.49295 | 0.6750077 | 159.48002 | 0.9118266 | 0.0212423 |
| healthyR.ai | 4 | NNAR | Test | 0.6653280 | 122.13700 | 0.6559054 | 142.30517 | 0.8839967 | 0.0296323 |
| TidyDensity | 1 | ARIMA | Test | 0.5722613 | 276.52092 | 0.6341875 | 111.54782 | 0.7505078 | 0.0857623 |
| TidyDensity | 2 | LM | Test | 0.6605630 | 395.81802 | 0.7320447 | 109.81239 | 0.8522558 | 0.0000002 |
| TidyDensity | 3 | EARTH | Test | 0.6067350 | 294.86773 | 0.6723917 | 114.05814 | 0.7749502 | 0.0000002 |
| TidyDensity | 4 | NNAR | Test | 0.5594024 | 130.62810 | 0.6199371 | 137.56698 | 0.7175381 | 0.0881017 |
| tidyAML | 1 | ARIMA | Test | 0.5775201 | 343.66663 | 0.6481901 | 89.51329 | 0.7533862 | 0.0122860 |
| tidyAML | 2 | LM | Test | 0.5987339 | 372.37156 | 0.6719997 | 90.80607 | 0.7562266 | 0.0000084 |
| tidyAML | 3 | EARTH | Test | 0.6170513 | 198.11885 | 0.6925586 | 108.22757 | 0.8338002 | 0.0000084 |
| tidyAML | 4 | NNAR | Test | 0.5775564 | 358.75763 | 0.6482308 | 92.34123 | 0.7346679 | 0.0635629 |
| RandomWalker | 1 | ARIMA | Test | 1.2998827 | 103.84411 | 0.6764931 | 171.68661 | 1.4656051 | 0.0168351 |
| RandomWalker | 2 | LM | Test | 1.3022312 | 99.73987 | 0.6777153 | 197.73577 | 1.4565209 | 0.0014226 |
| RandomWalker | 3 | EARTH | Test | 1.2668280 | 91.90972 | 0.6592906 | 167.33633 | 1.4399705 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2292242 | 89.74897 | 0.6397206 | 158.00487 | 1.4388345 | 0.0193943 |

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
    ## 1 healthyR.da…         2 LM          Test  0.705 152.  0.566 132.  0.854 0.00894
    ## 2 healthyR             4 NNAR        Test  0.725  96.4 0.730 158.  0.906 0.00256
    ## 3 healthyR.ts          4 NNAR        Test  0.809  96.5 0.640 166.  1.04  0.0181 
    ## 4 healthyverse         2 LM          Test  0.581 334.  0.667  88.6 0.719 0.00993
    ## 5 healthyR.ai          4 NNAR        Test  0.665 122.  0.656 142.  0.884 0.0296 
    ## 6 TidyDensity          4 NNAR        Test  0.559 131.  0.620 138.  0.718 0.0881 
    ## 7 tidyAML              4 NNAR        Test  0.578 359.  0.648  92.3 0.735 0.0636 
    ## 8 RandomWalker         4 NNAR        Test  1.23   89.7 0.640 158.  1.44  0.0194

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1564|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1557|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1501|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1472|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1296|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1147|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [755|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [177|28]>  <mdl_tm_t [1 × 5]>

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
