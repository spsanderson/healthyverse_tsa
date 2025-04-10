Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
10 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 136,319
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

The last day in the data set is 2025-04-08 23:35:00, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5867.99
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 136319        |
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
| r_version     |     98023 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     98023 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     98023 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11472 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-08 | 2023-05-29 | 1598 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1134857.91 | 1523493.34 | 355 | 14701 | 271502 | 2367772 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10355.21 | 18365.39 | 1 | 300 | 3064 | 11700 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-08 23:35:00 | 2023-05-29 00:35:46 | 82940 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 12S |       60 |

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
    ## -149.29  -35.50  -10.64   26.53  812.64 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.791e+02  7.316e+01
    ## date                                                1.094e-02  3.881e-03
    ## lag(value, 1)                                       1.081e-01  2.480e-02
    ## lag(value, 7)                                       9.746e-02  2.567e-02
    ## lag(value, 14)                                      9.663e-02  2.580e-02
    ## lag(value, 21)                                      6.580e-02  2.585e-02
    ## lag(value, 28)                                      5.991e-02  2.575e-02
    ## lag(value, 35)                                      6.812e-02  2.582e-02
    ## lag(value, 42)                                      4.905e-02  2.590e-02
    ## lag(value, 49)                                      7.706e-02  2.587e-02
    ## month(date, label = TRUE).L                        -1.010e+01  5.139e+00
    ## month(date, label = TRUE).Q                         2.878e+00  5.197e+00
    ## month(date, label = TRUE).C                        -1.256e+01  5.226e+00
    ## month(date, label = TRUE)^4                        -6.582e+00  5.193e+00
    ## month(date, label = TRUE)^5                        -1.209e+01  5.204e+00
    ## month(date, label = TRUE)^6                        -3.380e+00  5.274e+00
    ## month(date, label = TRUE)^7                        -6.087e+00  5.161e+00
    ## month(date, label = TRUE)^8                        -4.415e+00  5.178e+00
    ## month(date, label = TRUE)^9                         5.166e+00  5.217e+00
    ## month(date, label = TRUE)^10                        4.791e+00  5.283e+00
    ## month(date, label = TRUE)^11                       -6.018e+00  5.335e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.164e+01  2.388e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.071e+00  2.516e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.448 0.014475 *  
    ## date                                                 2.818 0.004897 ** 
    ## lag(value, 1)                                        4.358  1.4e-05 ***
    ## lag(value, 7)                                        3.797 0.000152 ***
    ## lag(value, 14)                                       3.745 0.000187 ***
    ## lag(value, 21)                                       2.546 0.010997 *  
    ## lag(value, 28)                                       2.327 0.020109 *  
    ## lag(value, 35)                                       2.638 0.008413 ** 
    ## lag(value, 42)                                       1.894 0.058391 .  
    ## lag(value, 49)                                       2.979 0.002935 ** 
    ## month(date, label = TRUE).L                         -1.966 0.049506 *  
    ## month(date, label = TRUE).Q                          0.554 0.579745    
    ## month(date, label = TRUE).C                         -2.404 0.016333 *  
    ## month(date, label = TRUE)^4                         -1.268 0.205152    
    ## month(date, label = TRUE)^5                         -2.323 0.020328 *  
    ## month(date, label = TRUE)^6                         -0.641 0.521759    
    ## month(date, label = TRUE)^7                         -1.179 0.238417    
    ## month(date, label = TRUE)^8                         -0.853 0.394017    
    ## month(date, label = TRUE)^9                          0.990 0.322227    
    ## month(date, label = TRUE)^10                         0.907 0.364568    
    ## month(date, label = TRUE)^11                        -1.128 0.259496    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.875  1.2e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.208 0.001366 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.66 on 1526 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2491, Adjusted R-squared:  0.2382 
    ## F-statistic: 23.01 on 22 and 1526 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.55830973189113"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.35162492019213"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 21, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.29889701147799"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 21, 70, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.29209089193648"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 21, 70, 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.25479723314496"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35, 21, 70, 98, 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.22889409564284"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35, 21, 70, 98, 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.22889409564284"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35, 21, 70, 98, 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.23995117672509"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35, 21, 70, 98, 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.23995117672509"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35, 21, 70, 98, 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.57468082127516"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35, 21, 70, 98, 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.57468082127516"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.18044457897878"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.82036742159732"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.73976449154883"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 98, 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.71804067345993"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 98, 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.71804067345993"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 98, 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.99827433930984"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 98, 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.99827433930984"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 98, 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.78152973286141"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 98, 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.78152973286141"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.22133658045638"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.22133658045638"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.75155094087699"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.75155094087699"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.30501768778668"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.30501768778668"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.11640847416067"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.63106204284143"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.63106204284143"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.41711482890799"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.41711482890799"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.8922382456548"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.8922382456548"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.69664636092278"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.31446337823792"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.31446337823792"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.18368717578478"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.18368717578478"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.03900683358733"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.03900683358733"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.1953027841495"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.38626570684529"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.38626570684529"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.67553951062464"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.67553951062464"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.44973801883228"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.44973801883228"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.88949890625311"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.88949890625311"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.82722189873563"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.82722189873563"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.45702764745559"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.45702764745559"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.90221874971107"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.66257797871703"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.66257797871703"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.02319036177597"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.02319036177597"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.06730071241346"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.06730071241346"

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
    ## 1 healthyR.data <tibble [1,591 × 2]> <tibble [28 × 2]> <split [1563|28]>
    ## 2 healthyR      <tibble [1,584 × 2]> <tibble [28 × 2]> <split [1556|28]>
    ## 3 healthyR.ts   <tibble [1,528 × 2]> <tibble [28 × 2]> <split [1500|28]>
    ## 4 healthyverse  <tibble [1,499 × 2]> <tibble [28 × 2]> <split [1471|28]>
    ## 5 healthyR.ai   <tibble [1,323 × 2]> <tibble [28 × 2]> <split [1295|28]>
    ## 6 TidyDensity   <tibble [1,174 × 2]> <tibble [28 × 2]> <split [1146|28]>
    ## 7 tidyAML       <tibble [782 × 2]>   <tibble [28 × 2]> <split [754|28]> 
    ## 8 RandomWalker  <tibble [204 × 2]>   <tibble [28 × 2]> <split [176|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7404602 | 144.21087 | 0.6218258 | 163.10691 | 0.9014255 | 0.0129704 |
| healthyR.data | 2 | LM | Test | 0.6943058 | 158.11056 | 0.5830662 | 131.98103 | 0.8463414 | 0.0000121 |
| healthyR.data | 3 | EARTH | Test | 0.9307919 | 324.93502 | 0.7816632 | 124.29844 | 1.0724472 | 0.0000121 |
| healthyR.data | 4 | NNAR | Test | 0.7172334 | 103.82548 | 0.6023204 | 160.09314 | 0.9544814 | 0.0741678 |
| healthyR | 1 | ARIMA | Test | 0.7591965 | 106.71392 | 0.7636875 | 183.31831 | 0.9289878 | 0.0421955 |
| healthyR | 2 | LM | Test | 0.7461445 | 98.04744 | 0.7505582 | 185.84683 | 0.9202118 | 0.0091085 |
| healthyR | 3 | EARTH | Test | 0.7407999 | 95.83445 | 0.7451821 | 178.26840 | 0.9170035 | 0.0091085 |
| healthyR | 4 | NNAR | Test | 0.7000448 | 90.75385 | 0.7041859 | 144.91116 | 0.8930830 | 0.0424469 |
| healthyR.ts | 1 | ARIMA | Test | 0.9494619 | 240.96328 | 0.7477397 | 142.66105 | 1.1809057 | 0.0213361 |
| healthyR.ts | 2 | LM | Test | 0.9736193 | 266.50807 | 0.7667646 | 141.11191 | 1.2069591 | 0.0213361 |
| healthyR.ts | 3 | EARTH | Test | 3.3756522 | 1500.46330 | 2.6584628 | 156.77244 | 3.7777058 | 0.0213361 |
| healthyR.ts | 4 | NNAR | Test | 0.7942982 | 97.52861 | 0.6255420 | 160.45209 | 1.0367298 | 0.1097396 |
| healthyverse | 1 | ARIMA | Test | 0.6134869 | 268.11322 | 0.7009456 | 96.93242 | 0.7762219 | 0.0263398 |
| healthyverse | 2 | LM | Test | 0.5853961 | 336.98207 | 0.6688502 | 90.00317 | 0.7215294 | 0.0303763 |
| healthyverse | 3 | EARTH | Test | 0.6371912 | 210.88792 | 0.7280293 | 110.54715 | 0.7974079 | 0.0303763 |
| healthyverse | 4 | NNAR | Test | 0.6401738 | 216.32784 | 0.7314371 | 108.94801 | 0.8038323 | 0.0117022 |
| healthyR.ai | 1 | ARIMA | Test | 0.7029275 | 110.58047 | 0.6801153 | 162.96271 | 0.9281925 | 0.0413673 |
| healthyR.ai | 2 | LM | Test | 0.6815105 | 134.06257 | 0.6593934 | 139.86808 | 0.9025950 | 0.0648514 |
| healthyR.ai | 3 | EARTH | Test | 0.6961975 | 116.89726 | 0.6736037 | 153.65177 | 0.9160133 | 0.0648514 |
| healthyR.ai | 4 | NNAR | Test | 0.6625093 | 133.01744 | 0.6410088 | 129.81042 | 0.8803137 | 0.0578217 |
| TidyDensity | 1 | ARIMA | Test | 0.5885455 | 267.60616 | 0.6446922 | 114.23258 | 0.7554384 | 0.1060437 |
| TidyDensity | 2 | LM | Test | 0.6766969 | 393.60419 | 0.7412532 | 111.34966 | 0.8581201 | 0.0111741 |
| TidyDensity | 3 | EARTH | Test | 0.6274129 | 286.17067 | 0.6872676 | 117.14478 | 0.7839303 | 0.0111741 |
| TidyDensity | 4 | NNAR | Test | 0.5880433 | 125.24341 | 0.6441421 | 141.63630 | 0.7410911 | 0.1133579 |
| tidyAML | 1 | ARIMA | Test | 0.5603892 | 336.20067 | 0.5851339 | 94.35568 | 0.7433217 | 0.0157214 |
| tidyAML | 2 | LM | Test | 0.5885937 | 376.87065 | 0.6145838 | 91.90221 | 0.7481794 | 0.0008123 |
| tidyAML | 3 | EARTH | Test | 0.5708051 | 208.72523 | 0.5960097 | 101.05466 | 0.7921408 | 0.0008123 |
| tidyAML | 4 | NNAR | Test | 0.5551373 | 353.59216 | 0.5796501 | 91.23745 | 0.7202149 | 0.0719697 |
| RandomWalker | 1 | ARIMA | Test | 1.2410061 | 97.19404 | 0.6654961 | 168.50898 | 1.4090227 | 0.0051994 |
| RandomWalker | 2 | LM | Test | 1.2748041 | 101.83529 | 0.6836204 | 197.78205 | 1.4270118 | 0.0014425 |
| RandomWalker | 3 | EARTH | Test | 1.2240633 | 91.38568 | 0.6564103 | 166.78691 | 1.3968766 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2492147 | 102.88456 | 0.6698980 | 183.75494 | 1.4097388 | 0.0107817 |

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
    ## 1 healthyR.d…         2 LM          Test  0.694 158.  0.583 132.  0.846  1.21e-5
    ## 2 healthyR            4 NNAR        Test  0.700  90.8 0.704 145.  0.893  4.24e-2
    ## 3 healthyR.ts         4 NNAR        Test  0.794  97.5 0.626 160.  1.04   1.10e-1
    ## 4 healthyver…         2 LM          Test  0.585 337.  0.669  90.0 0.722  3.04e-2
    ## 5 healthyR.ai         4 NNAR        Test  0.663 133.  0.641 130.  0.880  5.78e-2
    ## 6 TidyDensity         4 NNAR        Test  0.588 125.  0.644 142.  0.741  1.13e-1
    ## 7 tidyAML             4 NNAR        Test  0.555 354.  0.580  91.2 0.720  7.20e-2
    ## 8 RandomWalk…         3 EARTH       Test  1.22   91.4 0.656 167.  1.40  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1563|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1556|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1500|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1471|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1295|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1146|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [754|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [176|28]>  <mdl_tm_t [1 × 5]>

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
